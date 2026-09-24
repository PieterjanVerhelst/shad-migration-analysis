# Function to calculate the station distance from a 'source' station. It is
# based on an 'under the hood' function in Hugo Flavio's actel package
# https://github.com/hugomflavio/actel For more questions, ask Hugo
# hflavio@wlu.ca

library(assertthat)
library(dplyr)


distanceSource <- function(movements,
                           distance.method,
                           dist.mat,
                           upstream) {
  #appendTo("debug", "Running movementSpeeds.")
  
  # get release station as a character (in case it would be a factor)
  release_stat <- as.character(unique(movements$release_station))
  assert_that(length(release_stat) == 1,
              msg = "Only one release station per acoustic tag is allowed."
  )
  
  # get upstream stations for the specific released fish
  upstream_stations <- upstream %>%
    filter(release_station == release_stat) %>%
    pull(.data$upstream_stations)
  
  # initialize distance with sign
  dist.mat_sign <- dist.mat
  for (stat in upstream_stations) {
    dist.mat_sign[stat, release_stat] <- dist.mat[stat, release_stat]*(-1)
    dist.mat_sign[release_stat, stat] <- dist.mat[release_stat, stat]*(-1)
  }
  rm(dist.mat)
  
  # create column for collecting the distance to source
  movements$distance_to_source_m <- NA
  if (nrow(movements) > 1) {
    capture <- lapply(2:nrow(movements), function(i) {
      if (movements$station_name[i] != movements$station_name[1] & all(!grep("^Unknown$", movements$station_name[(1):i]))) {
        if (distance.method == "last to first"){
          dist <- dist.mat_sign[movements$station_name[i], movements$station_name[1]]
        }
        movements$distance_to_source_m[i] <<- round(dist, 6)
        rm(dist)
      } else {
        movements$speed_m_s[i] <<- NA_real_
      }
    })
  }
  # remove release station column from output
  movements$release_station <- NULL
  
  return(movements)
}
