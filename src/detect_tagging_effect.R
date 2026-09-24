# LifeWatch 2022
# Damiano Oldoni: damiano.oldoni@inbo.be

library(dplyr)
library(lubridate)
library(assertthat)

#' Detect tagging effect in a detection time series
#' 
#' @param df data.frame with a detection time series of a single shad. It MUST
#'   contain at least two columns: `arrival`, the datetime of the detections,
#'   and `location`, the place where the detection occurred.
#' @param n_days Number of days used to define tagging effects. Default: 40.
#' @param tagging_location Location where tagging took place. Typically the very first value in column `location`.
#' @param end_location Location used to define the end of tagging effects.
#' @return the input data.frame with a column extra, `taggin_effect`, filled in
#'   with logical values: _NA_ for detections after  `n_days` and `TRUE` or
#'   `FALSE` before `n_days`.
detect_tagging_effect <- function(
  df,
  n_days = 40,
  tagging_location = "zeeschelde",
  end_location = "bpns"
) {
  
  # Check input data.frame has the needed columns
  assertthat::assert_that("arrival" %in% names(df),
                          msg = "Column with name 'arrival' is missing in df."

  )
  assertthat::assert_that("location" %in% names(df),
                          msg = "Column with name 'location' is missing in df."
                          
  )
  
  # Create and initialize tagging_effect column
  df$tagging_effect <- NA
  
  # number of detections during the first n days
  n_detects_first_n_days <- df %>%
    filter(.data$arrival < (.data$arrival[1] + lubridate::days(n_days))) %>%
    nrow
  # Get the index (row number) of the last detection for each location within
  # first n_days
  last_occ <- tapply(seq_along(df$location[1:n_detects_first_n_days]),
                     df$location[1:n_detects_first_n_days],
                     max
  )
  
  # get row index of the last detection from tagging location
  last_detect_tagging_location <- last_occ[[tagging_location]]
  
  # Get the first index (row number) at end_location within
  # first n days and after the last detection at tagging location
  last_occ <- tapply(seq_along(df$location[last_detect_tagging_location+1:n_detects_first_n_days]),
                     df$location[last_detect_tagging_location+1:n_detects_first_n_days],
                     min) + last_detect_tagging_location
  
  # get row index of the first detection at end_location
  if (end_location %in% names(last_occ)) {
    end_tagging_effect_idx <- last_occ[[end_location]]
  } else {
    end_tagging_effect_idx <- NA
  }
  
  if (length(end_tagging_effect_idx) == 1 & !is.na(end_tagging_effect_idx)) {
    df$tagging_effect[1:(end_tagging_effect_idx-1)] <- TRUE
    if (n_detects_first_n_days > end_tagging_effect_idx) {
      df$tagging_effect[(end_tagging_effect_idx):n_detects_first_n_days] <- FALSE
    }
  } else {
    df$tagging_effect[1:n_detects_first_n_days] <- TRUE
  }
  df
}