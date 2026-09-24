#' Functions for detecting spawning and upstream/downstream migration
#'
#' Damiano Oldoni (damiano.oldoni@inbo.be) Pieterjan Verhelst
#' (pieterjan.verhelst@inbo.be)
#'
#' R script with functions used in detect_spawning_migration.R script for
#' identifying shad migration behavior based on discussion in
#' https://github.com/PieterjanVerhelst/shad-analysis/issues/13

library(assertthat) # to declare the conditions the code should satisfy
library(lubridate)  # to work with datetime
library(dplyr)      # to do data wrangling
library(tidylog)    # to get useful messages about dplyr and tidyr operations


#' This function returns the very first value of a vector(and corresponding
#' index) above a certain threshold. NA is returned if there are no values above
#' threshold.
#' 
#' @param x A numeric vector
#' @param threshold A numeric threshold
#' 
#' @return A list with two slots: `value` and `index`
#' @examples
#' # example 1: there are values above threshold
#' v <- c(1,3,5,2)
#' first_above(x = v, threshold = 3)
#' 
#' # example 2: no values above threshold
#' w <- c(1,3, 4)
#' first_above(x = w, threshold = 4)
first_above <- function(x, threshold) {
  value <- x[x > threshold][1]
  if (!is.na(value)) {
    index <- min(which(x > threshold))
  } else {
    index <- NA
  }
  return(list(value = value,
              index = index))
}

#' Function to get the very first value/index above distance threshold
#' 
#' @param df A data.frame. Column `distance_to_source_m` must be present.
#' @param row_idx A positive number which is used as row index.
#' @distance A numeric value indicating a distance in meters.
get_migration_dist_idx <- function(df, row_idx, distance) {
  
  ## check inputs
  # df is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # we make use of a column in df. So, it must be present
  assertthat::assert_that("distance_to_source_m" %in% names(df),
                          msg = "Column `distance_to_source_m` not found in df."
  )
  assertthat::assert_that(is.integer(row_idx),
                          msg = "row_idx must be an integer."
  )
  assertthat::assert_that(row_idx > 0,
                          msg = "row_idx must be positive."
  )
  # dist_threshold is a number
  assertthat::assert_that(is.numeric(distance))
  # dist_threshold is strictly positive
  assertthat::assert_that(distance > 0)

  # Start core function
  distance_to_source_m <- df$distance_to_source_m
  l <- length(distance_to_source_m)
  # Get the very first value/index above distance threshold
  first_value <- first_above(
    distance_to_source_m[(row_idx + 1):l], 
    threshold = distance_to_source_m[row_idx] + distance)
  first_value$index <- first_value$index + row_idx
  return(first_value)
}

#' Function to find the point to detect migration
#' 
#' @param df A data.frame.
#' @param dist_threshold A numeric value used as the minimum value to calculate
#'   the speed of the shad.
#' @return A data.frame. It contains the same columns as `df` plus the following
#'   ones:
#'   - `first_dist_to_use`
#'   - `first_dist_to_use_idx`
#'   - `time_first_dist_to_use`
find_distance_for_detection <- function(df, dist_threshold) {
  df$first_dist_to_use <- NA
  df$first_dist_to_use_idx <- NA
  df$time_first_dist_to_use <- NA_POSIXct_
  for (i in seq_len(nrow(df))) {
    # first distance to use for detecting downstream migration
    first_dist_to_use <- get_migration_dist_idx(df = df, 
                                                row_idx = i, 
                                                distance = dist_threshold)
    df$first_dist_to_use[i] <- first_dist_to_use$value
    df$first_dist_to_use_idx[i] <- first_dist_to_use$index
    df$time_first_dist_to_use[i] <- df$arrival[first_dist_to_use$index]
  }
  return(df)
}

#' apply_speed_threshold for point- wisespawning migration assessment 
#'
#' This function calculates the speed of the shad and apply the speed threshold
#' to detect spawning migration point-wise.
#'
#' @param df A data.frame as returned by `find_distance_for_detection()`. It
#'   must contain the following columns:
#' - `distance_to_source_m`
#' - `arrival`
#' - `first_dist_to_use`
#' - `time_first_dist_to_use`
#' - `first_dist_to_use_idx`
#' @param speed_threshold A numeric value: only periods with speed above this
#'   value are flagged as migration.
#' @return A data.frame. It contains all columns of input `df` except:
#' - `first_dist_to_use`
#' - `time_first_dist_to_use`
#' - `first_dist_to_use_idx`
#' It contains some extra columns:
#' - `migration_speed`: the migration speed
#' - `migration`:  a boolean
apply_speed_threshold <- function(df,
                            speed_threshold) {
  
  df <- df %>%
    mutate(delta_totdist = first_dist_to_use - distance_to_source_m) %>%
    mutate(delta_t = as.numeric(
      as.duration(time_first_dist_to_use - arrival)
    )
    ) %>%
    # Downstream migration cannot start if distance_to_source_m doesn't change
    mutate(distance_changed = distance_to_source_m != lag(distance_to_source_m)) %>%
    mutate(migration_speed = delta_totdist / delta_t)
  df <- df %>%
    mutate(migration = migration_speed >= speed_threshold) %>%
    mutate(migration = if_else(is.na(migration), FALSE, migration))
  
  # remove auxiliary columns
  df <- df %>%
    select(-c(delta_totdist, 
              delta_t,
              first_dist_to_use_idx, # Never used but useful while debugging
              first_dist_to_use,
              time_first_dist_to_use)
           )
  return(df)
}

#' Function to detect a migration phase. 
#' 
#' Depending on the sign, this
#' function can be used to detect upstream or downstream migration.
#' 
#' @param df A data.frame with migration data.
#' @param dist_threshold A numeric value used as the minimum value to calculate
#'   the speed of the shad.
#' @param speed_threshold A numeric value: only periods with speed above this
#'   value are flagged as migration.
#' @return df A data.frame as returned by `apply_speed_threshold()`. It contains
#'   all columns of input `df` except:
#'     - `first_dist_to_use`
#'     - `time_first_dist_to_use`
#'     - `first_dist_to_use_idx`
#'   It contains some extra columns:
#'     - `migration_speed`: the migration speed
#'     - `migration`:  a boolean
detect_migration_phase <- function(df,
                             dist_threshold, 
                             speed_threshold) {
  df <- find_distance_for_detection(df = df, dist_threshold = dist_threshold)
  df <- apply_speed_threshold(df = df, 
                              speed_threshold = speed_threshold)
  return(df)
}


#' Point-wise migration detection
#' 
#' This function detects the migration behavior of shad based on a point-wise
#' speed and direction assessment. The migration behavior is assessed for a
#' period defined as `spawning_period` days before and after the most downstream
#' detection.
#'
#' @param df A data.frame with migration data. Next columns must be present:
#' - `distance_to_source_m`
#' - `arrival`
#' - `location`
#' - `tagging_effect`
#' @param dist_threshold A numeric value used as the minimum value to calculate
#'  the speed of the shad.
#' @param speed_threshold A numeric value: only periods with speed above this
#' value are flagged as migration.
#' @param spawning_locations A vector containing the locations where spawning
#' could occur.
#' @param is_tagging (logical) IF  `TRUE`, the spawning migration is assessed
#' only for detections where no tagging effects occur, i.e. column
#' `tagging_effect` values is equal `FALSE`. Default: `TRUE`.
#' @param spawning_period A numeric value indicating the period in days where
#'   the spawning behavior is assessed. Default: 28 days.
#' @return df A data.frame with the following columns:
#' - `down_migration`: a boolean indicating if the shad is migrating downstream.
#' - `down_speed`: the speed of the downstream migration.
#' - `up_migration`: a boolean indicating if the shad is migrating upstream.
#' - `up_speed`: the speed of the upstream migration.
#' - `migration_pw`: the point-wise migration behavior of the shad.
#' - `start_spawning_period`: the start of the spawning period.
#' - `end_spawning_period`: the end of the spawning period.
point_wise_migration <- function(df, 
                                 dist_threshold, 
                                 speed_threshold,
                                 spawning_locations,
                                 is_tagging = TRUE,
                                 spawning_period = 28) {
  
  # Apply speed threshold algorithm for detecting point-wise downstream migration
  df <- df %>%
    detect_migration_phase(dist_threshold = dist_threshold,
                           speed_threshold = speed_threshold) %>%
    rename(down_migration = migration,
           down_speed = migration_speed)
  
  # Apply speed threshold algorithm for detecting point-wise upstream migration
  df <- df %>%
    mutate(distance_to_source_m = - distance_to_source_m) %>%
    detect_migration_phase(dist_threshold = dist_threshold,
                           speed_threshold = speed_threshold) %>%
    rename(up_migration = migration,
           up_speed = migration_speed) %>%
    mutate(distance_to_source_m = - distance_to_source_m)
  
  # Add migration point-wise behavior (one of downstream, upstream, spawning or
  # NA)
  df <- df %>%
    mutate(migration_pw = case_when(
      (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
        down_migration == TRUE ~ "downstream",
      (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
        up_migration == TRUE ~ "upstream",
      (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
        down_migration == FALSE & 
        up_migration == FALSE & 
        location %in% spawning_locations ~ "spawning",
      .default = NA_character_
      )
    )
  
  # Get the moment of the most upstream detection. If multiple detections are
  # found, the first one is taken.
  most_upstream <- df %>%
    filter(migration_pw == "spawning") %>%
    filter(distance_to_source_m == min(distance_to_source_m)) %>%
    filter(arrival == min(arrival)) %>%
    pull(arrival)
  
  # If both downstream and upstream migration behaviors are detected within the
  # center of the migration/spawning period (period/2), arise a warning.
  # Probably you should try a better speed/distance threshold?
  down_up <- df %>%
    filter(down_migration == TRUE & up_migration == TRUE & 
             arrival >= most_upstream - days(floor(spawning_period/2)) & 
             arrival <= most_upstream + days(floor(spawning_period/2)))
  
  if (nrow(down_up) > 0) {
    warning(
      sprintf(
        paste0("Both downstream and upstream migration detected point-wise ",
               "at the same time for %s rows. Arrival times: %s .",
               "Increase the speed threshold, please."),
        nrow(down_up),
        toString(down_up$arrival)
      )
    )
  }
  # Add columns `start_spawning_period` and `end_spawning_period` to the data
  # frame. These columns indicate the start and end of the spawning period which
  # is `spawning_period` days before and after `most_downstream`.
  df <- df %>%
    mutate(start_spawning_period = most_upstream - days(spawning_period),
           end_spawning_period = most_upstream + days(spawning_period))
  
  # Set the migration behavior to NA for detections before and after the
  # spawning period and return the data.frame
  df %>%
    mutate(migration_pw = case_when(
      arrival < start_spawning_period | arrival > end_spawning_period ~ NA_character_,
      TRUE ~ migration_pw
      )
    )
}

#' Smooth migration behavior
#' 
#' This function smooths the migration behavior of shad based on the point-wise
#' migration detection. Singular detections of migration behavior are replaced
#' with the migration behavior of the previous detection. In this way we avoid
#' a lot of back and forth between migration behaviors. The maximum length of
#' what we consider as a "singular" migration behavior can be set with the
#' `max_length` parameter.
#' 
#' @param x A factor with migration behavior. One of `spawning`, `downstream`, `migration`.
#' @param max_length A numeric value indicating the maximum length of a singular
#'  migration behavior.
#' @return A factor with smoothed migration behavior.
smooth_migration <- function(x, max_length) {
  # Check input
  assertthat::assert_that(is.factor(x))
  
  # Check levels
  assertthat::assert_that(
    all(levels(x) %in% c("spawning", "downstream", "upstream"))
  )
  
  # Get runs of values
  runs <- rle(as.character(x))
  
  # Find runs of length up to `max_length` that are different from both 
  # neighbors
  to_smooth <- which(
    (runs$lengths <= max_length) & 
      c(FALSE, runs$values[-1] != runs$values[-length(runs$values)]) & 
      c(runs$values[-1] != runs$values[-length(runs$values)], FALSE))
  
  # Replace these runs with the value of the previous run
  if (length(to_smooth) > 0) {
    runs$values[to_smooth] <- runs$values[to_smooth - 1]
  }
  
  # Reconstruct the vector
  smoothed <- inverse.rle(runs)
  
  # Convert back to factor with original levels
  factor(smoothed, levels = levels(x))
}


#' Truncate spurious downstream or upstream migration periods
#' 
#' Sometimes, we have short downstream or upstream migration periods out of the 
#' main downstream or upstream migration periods. This function set these short
#' periods as NA.
#' 
#' @param df A data.frame with migration data. Next columns must be present:
#' - `migration`: the migration behavior of the shad.
#' @param migration_values A character vector with the migration values to
#'  truncate. Default: `c("downstream", "upstream")`.
#' @return A data.frame with the same columns as input `df` but with the
#' migration periods truncated if needed.
truncate_migration <- function(df,
                               migration_values = c("downstream", "upstream")) {
  # Check input
  assertthat::assert_that(is.data.frame(df))
  # Check that the column `migration` is present
  assertthat::assert_that("migration" %in% names(df))
  
  # Check that `migration_values` is a character vector
  assertthat::assert_that(is.character(migration_values))
  
  # Check that `migration_values` values are present in column `migration`
  assertthat::assert_that(all(migration_values %in% unique(df$migration)))
  
  # Get the runs
  runs <- rle(df$migration)
  
  # Initialize the result `migration`
  result <- df$migration
  
  # Loop through each migration value
  for (value in migration_values) {
    # Check if the value is present in the migration column
    if (value %in% runs$values) {
      # Find the longest run of the current migration value
      max_run_length <- max(runs$lengths[runs$values == value], na.rm =  TRUE)
      
      # Replace values that are not part of the longest run with NA
      runs$values[which(runs$lengths < max_run_length & runs$values == value)] <- NA
      result <- inverse.rle(runs)
    }
  }
  df$migration <- result
  df
}

#' Global migration assessment function
#'
#' This function assesses the migration behavior of shad based on the point-wise
#' migration detection.
#'
#' @param df A data.frame with migration data. Next columns must be present:
#' - `migration_pw`: the point-wise migration behavior of the shad as returned by `detect_migration()` function.
#' - `arrival`: the datetime of the detections.
#' - `tagging_effect`: a logical value. Do tagging effects occur?
#' - `distance_to_source_m`: the distance to the source.
#' - `start_spawning_period`: the start of the spawning period. See documentation of `point_wise_migration()` for more details.
#' - `end_spawning_period`: the end of the spawning period. See documentation of `point_wise_migration()` for more details.
#' - `down_speed`: the speed of the downstream migration.
#' - `up_speed`: the speed of the upstream migration.
#' @param max_smooth_length A numeric value indicating the maximum length of a
#'   "singular" migration behavior to be changed via `smooth_migration()`.
#' @param is_tagging (logical) IF  `TRUE`, the spawning migration is assessed
#'   only for detections where no tagging effects occur, i.e. column
#'   `tagging_effect` is equal `FALSE`.  Default: `TRUE`.
#' @return df A data.frame with an extra column `migration` with the following
#'   values:
#'  - `"upstream"`
#'  - `"spawning"`
#'  - `"downstream"`
global_migration <- function(df, max_smooth_length, is_tagging = TRUE) {
  
  # Check data.frame
  assertthat::assert_that(is.data.frame(df))
  # Check max_smooth_length is a numeric scalar
  assertthat::assert_that(assertthat::is.number(max_smooth_length))
  # Check `is_tagging` is a boolean scalar
  assertthat::assert_that(assertthat::is.flag(is_tagging))
  
  # Initialize migration column
  df <- df %>%
    mutate(migration = as.factor(migration_pw))
  # Apply `smooth_migration()` to remove singularities in migration behavior
  # Do it recursively until no more singularities are found
  # Do it recursively by using incremental `max_smooth_length`
  for (i in seq_len(max_smooth_length)) {
    smoothed_migration <- smooth_migration(x = df$migration,
                                           max_length = i)
    while(!all(df$migration == smoothed_migration | 
               (is.na(df$migration) & is.na(smoothed_migration)))) {
      df <- df %>%
        mutate(migration = smoothed_migration)
      smoothed_migration <- smooth_migration(x = df$migration,
                                             max_length = i)
    }
  }
  
  # Get the upstream part of the data.frame before spawning
  upstream_df <- df %>%
    filter(migration == "upstream")
  
  # Get first upstream moment
  first_upstream <- if (nrow(upstream_df) > 0) {
    upstream_df %>%
      filter(arrival == min(arrival)) %>%
      pull(arrival)
  } else {
    NA
  }
  
  # Get the most "upstream" detection of the upstream movement
  last_upstream <- if (nrow(upstream_df) > 0) {
    upstream_df %>%
      filter(distance_to_source_m == min(distance_to_source_m)) %>%
      pull(arrival) %>%
      min()
  } else {
    NA
  }
  
  # Spawning part of data.frame
  df_spawning <- df %>%
    filter(migration == "spawning")
  
  # Get first spawning moment
  first_spawning <- if (nrow(df_spawning) > 0) {
    df_spawning %>%
      filter(arrival == min(arrival)) %>%
      pull(arrival)
  } else {
    NA
  }
  
  # Get last spawning moment
  last_spawning <- if (nrow(df_spawning) > 0) {
    df_spawning %>%
      filter(arrival == max(arrival)) %>%
      pull(arrival)
  } else {
    NA
  }
  
  # Get the downstream part of the data.frame after spawning
  downstream_df <- if (!is.na(last_spawning)) {
    df %>%
      filter(arrival > last_spawning) %>%
      filter(migration == "downstream")
  } else {
    data.frame()
  }
  
  # Get the most upstream detection of the last downstream movement
  first_downstream <- if (nrow(downstream_df) > 0) {
    downstream_df %>%
      filter(arrival == min(arrival)) %>%
      pull(arrival)
  } else {
    NA
  }

  # Get the most downstream detection of the last downstream movement
  last_downstream <- if (nrow(downstream_df) > 0) {
    downstream_df %>%
      filter(distance_to_source_m == max(distance_to_source_m)) %>%
      pull(arrival) %>%
      max()
  } else {
    NA
  } 
  
  # Assess a global migration behavior for "upstream", "downstream" and spawning
  if (!is.na(first_upstream) > 0 && !is.na(last_upstream) > 0) {
    df <- 
      df %>%
      mutate(migration = dplyr::if_else(
        condition = (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
          arrival >= first_upstream & arrival <= last_upstream,
        true = "upstream",
        false = migration
        )
      )
  }
  
  if (!is.na(first_downstream) > 0 && !is.na(last_downstream) > 0) {
    df <- 
      df %>%
      mutate(migration = dplyr::if_else(
        condition = (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
          arrival > first_downstream & arrival < last_downstream,
        true = "downstream",
        false = migration
      ))
  }
  
  if (!is.na(first_spawning) > 0 && !is.na(last_spawning) > 0) {
    df <- 
      df %>%
      mutate(migration = dplyr::if_else(
        condition = (!is_tagging | !tagging_effect | is.na(tagging_effect)) & 
          arrival >= first_spawning & arrival <= last_spawning,
        true = "spawning",
        false = migration
      ))
  }
  
  # Handle NAs which typically occur when a `down_speed` is low, but still
  # higher than `up_speed`, if any. These detections should be set as
  # "downstream" if they occur within the spawning period. Viceversa, detections
  # with `migration` = `NA` where `up_speed` is low, but still higher than
  # `down_speed`, should be set as "upstream" if they occur within the spawning
  # period. Adding column `low_speed_flag` to flag these situations.
  df <- df %>%
    mutate(low_speed_flag = case_when(
      is.na(migration) & arrival >= start_spawning_period & arrival <= end_spawning_period & 
        (down_speed > up_speed | (!is.na(down_speed) & is.na(up_speed))) ~ "low down_speed",
      is.na(migration) & arrival >= start_spawning_period & arrival <= end_spawning_period & 
        (up_speed > down_speed | (!is.na(up_speed) & is.na(down_speed))) ~ "low up_speed",
      TRUE ~ NA_character_
    )) %>%
    mutate(migration = case_when(
      low_speed_flag == "low down_speed" ~ "downstream",
      low_speed_flag == "low up_speed" ~ "upstream",
      TRUE ~ migration
      )
    )
  
  # Truncate spurious downstream or upstream migration periods.
  truncate_migration(df)
}

#' Wrap-up general function to find spawning and download/upstream migration
#' periods
#'
#' @param df A data.frame with migration data. Next columns must be present:
#' - `distance_to_source_m`
#' - `arrival`
#' @param dist_threshold A numeric value used as the minimum value to calculate
#'   the speed of the shad.
#' @param speed_threshold A numeric value: only periods with speed above this
#'   value are flagged as migration.
#' @param max_smooth_length A numeric value indicating the maximum length of a
#'   "singular" migration behavior to be changed via `smooth_migration()`
#'   (within `global_migration()`).
#' @param spawning_locations A vector containing the locations where spawning
#'   could occur.
#' @param is_tagging (logical) IF  `TRUE`, the spawning migration is assessed
#'   only for detections where no tagging effects occur, i.e. column
#'   `tagging_effect` values is equal `FALSE`.  Default: `TRUE`.
#' @param spawning_period A numeric value indicating the period in days where
#'  the spawning migration is assessed. Default: 28 days.
get_spawning_migrations <- function(df, 
                           dist_threshold, 
                           speed_threshold,
                           max_smooth_length,
                           spawning_locations,
                           is_tagging = TRUE,
                           spawning_period = 28) {
  ## check inputs
  # `df` is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # `dist_threshold` is a number (scalar)
  assertthat::assert_that(assertthat::is.number(dist_threshold))
  # `speed_threshold` is a number (scalar)
  assertthat::assert_that(assertthat::is.number(speed_threshold))
  # `max_smooth_length` is a number (scalar)
  assertthat::assert_that(assertthat::is.number(max_smooth_length))
  # `spawning_locations` is a character vector
  assertthat::assert_that(is.character(spawning_locations))
  # `is_tagging` is a boolean scalar
  assertthat::assert_that(assertthat::is.flag(is_tagging))
  # `spawning_period` is a number (scalar)
  assertthat::assert_that(assertthat::is.number(spawning_period))
  # Column `distance_to_source_m` in df 
  assertthat::assert_that("distance_to_source_m" %in% names(df),
                          msg = "Column `distance_to_source_m` not found in df."
  )
  # Column `arrival` in df
  assertthat::assert_that("arrival" %in% names(df),
                          msg = "Column `arrival` not found in df."
  )
  
  # Assess point-wise migration
  df <- point_wise_migration(df = df, 
                             dist_threshold = dist_threshold, 
                             speed_threshold = speed_threshold,
                             spawning_locations = spawning_locations,
                             is_tagging,
                             spawning_period)
  
  # Assess global migration
  df <- global_migration(df,
                         max_smooth_length = max_smooth_length,
                         is_tagging = is_tagging)
  
  # Add column with migration speed: `down_speed` if the migration is downstream,
  # `-up_speed` (negative) if upstream.
  df %>%
    mutate(migration_speed = case_when(
      migration == "downstream" ~ down_speed,
      migration == "upstream" ~ -up_speed,
      .default = NA)
    )
}
