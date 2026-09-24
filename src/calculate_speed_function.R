# Function to calculate movement speed between consecutive stations. It is based on an 'under the hood' function in Hugo Flavio's actel package
# https://github.com/hugomflavio/actel
# For more questions, ask Hugo
# hflavio@wlu.ca

library(assertthat)

movementSpeeds <- function(movements, speed.method, dist.mat) {
  #appendTo("debug", "Running movementSpeeds.")
  movements$swimtime_s <- NA_real_
  movements$swimdistance_m <- NA_real_
  movements$speed_m_s <- NA_real_
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[i - 1] & 
          all(!grep("^Unknown$", movements$station_name[(i - 1):i]))) {
        if (speed.method == "last to first"){
          a.sec <- as.vector(difftime(movements$arrival[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], movements$station_name[i - 1]]
        }
        if (speed.method == "last to last"){
          a.sec <- as.vector(difftime(movements$departure[i], movements$departure[i - 1], units = "secs"))
          my.dist <- dist.mat[movements$station_name[i], movements$station_name[i - 1]]
        }
        assertthat::assert_that(!is.null(a.sec),
          msg = paste0("a.sec must be not NULL. ",
                       "Tag serial number: ",
                       movements$tag_serial_number[i],
                       ". Row: ",i, ".")
        )
        assertthat::assert_that(!is.null(my.dist),
          msg = paste0("my.dist must be not NULL. ",
                       "Acoustic tag ID: ",
                       movements$tag_serial_number[i],
                       ". Row: ",i, ".")
        )
        movements$swimtime_s[i] <<- round(a.sec, 6)
        movements$swimdistance_m[i] <<- round(my.dist, 6)
        movements$speed_m_s[i] <<- round(my.dist/a.sec, 6)
        rm(a.sec, my.dist)
      }
    })
  }
  return(movements)
}


