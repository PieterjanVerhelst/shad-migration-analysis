# Flag spawning migration
# Year: 2025
# Damiano Oldoni: damiano.oldoni@inbo.be
# Pieterjan Verhelst: pieterjan.verhelst@inbo.be

library(assertthat)
library(dplyr)
library(mgcv)
library(ggplot2)
library(data.table)

#' Get the most upstream detection
#' 
#' Get the moment of the most upstream detection. If multiple detections are
#' found, the first one is taken. if column `tagging_effect` is present, do not take into account the data where `tagging_effect` is `TRUE`.
#' 
#' @param df A data.frame with at least the columns `arrival` and
#'   `distance_to_source_m`.
get_most_upstream <- function(df) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  # Remove tagging effect data
  df <- df %>%
    dplyr::filter(is.na(tagging_effect) | tagging_effect == FALSE)
  # Get first arrival time of the most upstream detection
  df %>%
    dplyr::filter(distance_to_source_m == min(distance_to_source_m)) %>%
    dplyr::filter(arrival == min(arrival)) %>%
    dplyr::pull(arrival)
}

#' Build a detection and a smoothing window around the most upstream detection.
#' 
#' The detection window is defined by the spawning period. The window is used to
#' determine if a detection is within the spawning period, which is the period
#' we want to analyse. The smoothing window is 30% broader than the detection
#' window. It is 15% broader at both sides. This is done to avoid boundary
#' effects while smoothing.
#' 
#' @param df (data.frame) the data to build the detection window for. It must
#' contain the following columns:
#' - `arrival`: the datetime of the detection.
#' @param most_upstream (datetime) the datetime of the most upstream detection.
#' @param spawning_period (numeric) the spawning period in days.
#' @return a data.frame with two additional columns:
#' - `detection_window` which is a logical indicating if the detection is 
#' within the detection window.
#' - `smoothing_window` which is a logical indicating if the detection is
#' within the smoothing window.
build_detection_window <- function(df, most_upstream, spawning_period) {
  # Check input data.frame has the needed columns: `arrival`
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  # `most_upstream` is a datetime
  assertthat::assert_that(
    lubridate::is.POSIXct(most_upstream),
    msg = "most_upstream is not a datetime."
  )
  # Add detection window
  df <- df %>%
    dplyr::mutate(
      detection_window = dplyr::if_else(
        arrival >= most_upstream - lubridate::days(floor(spawning_period/2)) & 
          arrival <= most_upstream + lubridate::days(floor(spawning_period/2)),
        TRUE,
        FALSE
      )
    )
  # Add smoothing window
  df %>%
    dplyr::mutate(
      smoothing_window = dplyr::if_else(
        arrival >= most_upstream - lubridate::days(floor((1.3*spawning_period)/2)) & 
          arrival <= most_upstream + lubridate::days(floor((1.3*spawning_period)/2)),
        TRUE,
        FALSE
      )
    )
}

#' Apply a loess smoother to the data
#' 
#' @param df (data.frame) the data to smooth. It must contain the following columns:
#' - `distance_to_source_m`: the distance to the reference station.
#' - `arrival`: the datetime of the detection.
#' - `smoothing_window`: a logical indicating if the detection is within the
#' smoothing window.
#' @param span (numeric) the parameter α which controls the degree of smoothing.
#' @return the input data.frame with the following columns added:
#' - `smooth_upper`: the upper bound of the smoothed distance (95% confidence interval)
#' - `smooth_lower`: the lower bound of the smoothed distance (95% confidence interval)
#' - `smoothed_distance`: the smoothed distance
apply_loess <- function(df, span) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "smoothing_window" %in% names(df),
    msg = "Column with name 'smoothing_window' is missing in df."
  )
  
  # Initialize the smoothed columns
  df$smooth_upper <- NA_real_
  df$smooth_lower <- NA_real_
  df$smoothed_distance <- NA_real_
  
  # Convert arrival (datetime) to numeric for loess
  df$arrival_numeric <- as.numeric(df$arrival)
  # Apply basic LOESS smoothing
  result <- tryCatch(expr = {
    smooth_loess <- loess(
      distance_to_source_m ~ arrival_numeric,
      data = df,
      span = span
    )
    pred_loess <- predict(smooth_loess, se = TRUE)
    # Calculate confidence bands (using 1.96 for 95% confidence interval)
    df$smooth_upper <- pred_loess$fit + 1.96 * pred_loess$se.fit
    df$smooth_lower <- pred_loess$fit - 1.96 * pred_loess$se.fit
    df$smoothed_distance <- pred_loess$fit
  },
  error = function(e) e,
  warning = function(w) w
  )
  # Notify users about errors and warnings
  if (class(result)[1] %in% c("simpleWarning", "simpleError")) {
    warning(paste0(
      "Issues applying LOESS smoothing.\n",
      result$message,
      ".\n")
    )
  }
  return(df)
}

#' Apply GAM smoothing to the data
#' 
#' Apply GAM smoothing with P-spline to the data. The number of knots is set by
#' the user. 
#' 
#' @param df (data.frame) the data to smooth. It must contain the following
#'   columns:
#' - `distance_to_source_m`: the distance to the reference station.
#' - `arrival`: the datetime of the detection.
#' - `smoothing_window`: a logical indicating if the detection is within the
#' smoothing window.
#' @param knots (numeric) the number of knots to use in the GAM.
#' @return the input data.frame with the following columns added:
apply_gam <- function(df, knots) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  
  # Initialize the smoothed columns
  df$smooth_upper <- NA_real_
  df$smooth_lower <- NA_real_
  df$smoothed_distance <- NA_real_
  
  # Convert arrival (datetime) to numeric for loess
  df$arrival_numeric <- as.numeric(df$arrival)
  # Fit GAM with P-spline. Notice that  k-1 is maximum number of knots
  result <- tryCatch(
    expr = {
      fit <- gam(
        distance_to_source_m ~ s(arrival_numeric, bs = "ps", k = knots),
        data = df
      )
      # Get predictions with standard errors
      pred <- predict(fit, se.fit = TRUE)
      
      # Get predictions with confidence levels (95%)
      df$smoothed_distance <- pred$fit
      df$smooth_lower <- pred$fit - 1.96 * pred$se.fit
      df$smooth_upper <- pred$fit + 1.96 * pred$se.fit
      
      # You can see where the knots were placed
      summary_gam <- summary(fit)
    },
    error = function(e) e,
    warning = function(w) w
  )
  # Notify users about errors and warnings
  if (class(result)[1] %in% c("simpleWarning", "simpleError")) {
    warning(paste0(
      "Issues applying GAM smoothing.\n",
      result$message,
      ".\n")
    )
  }
  return(df)
}

#' Apply a smoother to the data
#' 
#' @param df (data.frame) the data to smooth. It must contain the following columns:
#'  - `distance_to_source_m`: the distance to the reference station.
#'  - `arrival`: the datetime of the detection.
#'  - `smoothing_window`: a logical indicating if the detection is within the
#'  smoothing window.
#' @param method_smooth (character) the method to use for smoothing. It must be
#' one of "loess" or "gam".
#' @param span (numeric) the parameter α which controls the degree of smoothing.
#'   Passed to `loess()`. `NULL` if `method_smooth` is "gam".
#' @param knots (numeric) the number of knots to use in the GAM. Passed to
#'   `apply_gam()`. `NULL` if `method_smooth` is "loess".
#' @return the input data.frame with the following columns added:
#' - `smooth_upper`: the upper bound of the smoothed distance (95% confidence interval)
#' - `smooth_lower`: the lower bound of the smoothed distance (95% confidence interval)
#' - `smoothed_distance`: the smoothed distance
apply_smooth <- function(df, method_smooth, span = NULL, knots = NULL) {
  ## Check inputs
  # `df` is a data.frame
  assertthat::assert_that(is.data.frame(df))
  # Column `distance_to_source_m` in df 
  assertthat::assert_that("distance_to_source_m" %in% names(df),
                          msg = "Column `distance_to_source_m` not found in df."
  )
  # Column `arrival` in df
  assertthat::assert_that("arrival" %in% names(df),
                          msg = "Column `arrival` not found in df."
  )
  # Column `detection_window` in df  
  assertthat::assert_that("detection_window" %in% names(df),
                          msg = "Column `detection_window` not found in df."
  )
  # `method_smooth` is one of "loess" or "gam"
  assertthat::assert_that(
    method_smooth %in% c("loess", "gam"),
    msg = "method_smooth must be one of 'loess' or 'gam'."
  )
  # `span` is a numeric value if `method_smooth` is "loess"
  if (method_smooth == "loess") {
    assertthat::assert_that(
      is.numeric(span),
      msg = "span must be a numeric value if `method_smooth` is \"loess\"."
    )
  } else {
    # `span` is NULL if `method_smooth` is "gam"
    assertthat::assert_that(
      is.null(span),
      msg = "span must be NULL if `method_smooth` is \"gam\"."
    )
  }
  # `knots` is a numeric value if `method_smooth` is "gam"
  if (method_smooth == "gam") {
    assertthat::assert_that(
      is.numeric(knots),
      msg = "knots must be a numeric value if `method_smooth` is \"gam\"."
    )
  } else {
    # `knots` is NULL if `method_smooth` is "loess"
    assertthat::assert_that(
      is.null(knots),
      msg = "knots must be NULL if `method_smooth` is \"loess\"."
    )
  }
  
  # Smooth only the detections within the smoothing_window
  df_to_smooth <- df %>% dplyr::filter(smoothing_window == TRUE)
  
  # Apply the smoother
  if (method_smooth == "loess") {
    df_to_smooth <- apply_loess(df_to_smooth, span)
  } else {
    if (method_smooth == "gam") {
      df_to_smooth <- apply_gam(df_to_smooth, knots)
    }
  }
  
  # Add fitted values to original data.frame `df`
  df <- df %>%
    left_join(df_to_smooth %>%
                select(arrival, smooth_upper, smooth_lower, smoothed_distance),
              by = "arrival"
    )
  # Delete helper data.frame
  remove(df_to_smooth)
  
  return(df)
}

#' Get central difference for calculating the first derivative
#' 
#' @param x (numeric) the x values.
#' @param y (numeric) the y values.
#' @return the first derivative of y with respect to x.
central_diff <- function(x, y) {
  n <- length(x)
  derivative <- numeric(n)
  
  # Central difference for interior points
  derivative[2:(n-1)] <- (y[3:n] - y[1:(n-2)]) / (x[3:n] - x[1:(n-2)])
  
  # Forward difference for first point
  derivative[1] <- (y[2] - y[1]) / (x[2] - x[1])
  
  # Backward difference for last point
  derivative[n] <- (y[n] - y[n-1]) / (x[n] - x[n-1])
  
  return(derivative)
}

#' Get first derivative of the smoothed data
#' 
#' Calculate the first derivative of the smoothed distance using central
#' differences. The point before and after are used.
#' 
#' @param df (data.frame) the data to calculate the first derivative for. It must
#' contain the following columns:
#' - `arrival`: the datetime of the detection.
#' - `smoothed_distance`: the smoothed distance.
#' @return the input data.frame with an additional column `first_derivative`
#' containing the first derivative of the smoothed distance, i.e. the speed.
get_first_derivative <- function(df) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "smoothed_distance" %in% names(df),
    msg = "Column with name 'smoothed_distance' is missing in df."
  )
  # Convert arrival (datetime) to numeric for difference calculation
  df$arrival_numeric <- as.numeric(df$arrival)
  # Calculate the first derivative using `diff()`
  # df$first_derivative <- NA_real_
  # lag <- 1
  # df$first_derivative[1:(nrow(df) - lag)] <- diff(df$smoothed_distance, lag) /
  #   diff(df$arrival_numeric, lag)
  
  # Calculate the first derivative using central differences 
  df$first_derivative <- central_diff(df$arrival_numeric, df$smoothed_distance)
  
  return(df)
}

#' Assign spawning status based on the first derivative
#' 
#' @param df (data.frame) the data to assign the spawning status to. It must
#' contain the following columns:
#' - `first_derivative`: the first derivative of the smoothed distance.
#' @param upstream_speed_threshold (numeric) the maximum value of `first_derivative` (speed) to consider a detection as
#' upstream migration. It must be a negative value.
#' @param downstream_speed_threshold (numeric) the minimum value of `first_derivative` (speed) to consider a detection as
#' downstream migration. It must be a positive value.
#' @return the input data.frame with an additional column `migration` which is a
#' factor with levels `upstream`, `downstream`, `spawning` and `NA`.
#' The factor `spawning` is returned if `first_derivative` is between
#' `upstream_speed_threshold` and `downstream_speed_threshold`.
assign_spawning_status <- function(df,
                                   upstream_speed_threshold,
                                   downstream_speed_threshold) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "first_derivative" %in% names(df),
    msg = "Column with name 'first_derivative' is missing in df."
  )
  
  # Check `upstream_speed_threshold` is negative
  assertthat::assert_that(
    upstream_speed_threshold < 0,
    msg = "upstream_speed_threshold must be a negative value."
  )
  # Check `downstream_speed_threshold` is positive
  assertthat::assert_that(
    downstream_speed_threshold > 0,
    msg = "downstream_speed_threshold must be a positive value."
  )
  
  # Assign migration status (first derivative)
  df %>%
    dplyr::mutate(
      migration = dplyr::case_when(
        first_derivative < upstream_speed_threshold ~ "upstream",
        first_derivative > downstream_speed_threshold ~ "downstream",
        first_derivative >= upstream_speed_threshold & first_derivative <= downstream_speed_threshold ~ "spawning",
        TRUE ~ NA_character_
      )
    )
}

#' Clean up spurious periods.
#'
#' Step 1: upstream/downstream periods start only if `distance_to_source_m` changes.
#' Step 2: spawning cannot occur before the first upstream detection. Set to
#' `NA.`
#' Step 3: spawning cannot occur after the last downstream detection. Set to
#' `NA`.
#' Step 4: Detect start, end of the longest spawning period. Find also the
#' minimum `distance_to_source_m` of it. Values of `migration` below this
#' minimum minus the buffer `spawning_distance_limit` are converted to upstream or downstream if their `arrival` value
#' falls during the upstream phase (after first upstream and before the longest
#' spawning period) or downstream phase (after the longest spawning and before
#' last downstream).
#' Step 5: Convert the NAs after last downstream to downstream if `distance_to_source_m` is lower than the last downstream.
#' @param df (data.frame) It must contain the following column `migration` with
#'   factor levels `upstream`, `downstream`, `spawning` or `NA`.
#' @param spawning_distance_limit (numeric) Buffer below the minimum
#'   `distance_to_source_m` of the main spawning period. Used for cleaning up
#'   spurious periods.
#' @return The input data.frame. Values in column `migration` could have be
#'   changed.
clean_up_spurious_periods <- function(df, spawning_distance_limit) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "migration" %in% names(df),
    msg = "Column with name 'migration' is missing in df."
  )
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  # Step 1
  # Avoid upstream/downstream periods starting if distance_to_source_m does not
  # change
  df <- df %>%
    # Identify migration periods
    dplyr::mutate(period = data.table::rleid(migration)) %>%
    dplyr::group_by(migration, period) %>%
    # Identify distance periods (i.e. consecutive rows with same distance)
    dplyr::mutate(distance_period = data.table::rleid(distance_to_source_m)) %>%
    # Within each `distance_period`, mark the last row as TRUE, the rest as
    # FALSE
    dplyr::group_by(migration, period, distance_period) %>%
    dplyr::mutate(
      is_last_in_distance_period = dplyr::row_number() == dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    # If `migration` is "upstream"  or "downstream", distance_period == 1
    # and is_last_in_distance_period is FALSE, then set `migration` to previous
    # value
    dplyr::mutate(
      migration = dplyr::case_when(
        migration %in% c("upstream", "downstream") & 
          distance_period == 1 & 
          !is_last_in_distance_period ~ dplyr::lag(migration),
        TRUE ~ migration
      )
    ) %>%
    # Remove help column `distance_period` and `is_last_in_distance_period`
    dplyr::select(-c(period, distance_period, is_last_in_distance_period))
  
  # Step 2
  # Find first upstream detection
  first_upstream <- df %>%
    dplyr::filter(migration == "upstream") %>%
    dplyr::filter(arrival == min(arrival)) %>%
    dplyr::pull(arrival)
  # Set migration = NA if spawning or downstream occurs before first upstream
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("spawning", "downstream")  & arrival < first_upstream,
      NA,
      migration
    ))
  
  # Step 3
  # Find last downstream detection
  last_downstream_df <- df %>%
    dplyr::filter(migration == "downstream")
  if (nrow(last_downstream_df) > 0) {
    # Take the last if multiple detections
    last_downstream_df <- last_downstream_df %>%
      dplyr::filter(arrival == max(arrival))
    last_downstream <- last_downstream_df %>%
      dplyr::pull(arrival)
    distance_last_downstream <- last_downstream_df %>%
      dplyr::pull(distance_to_source_m)
  } else {
    # If no downstream detection, set to NA
    last_downstream <- NA_POSIXct_
    distance_last_downstream <- NA_real_
  }
  
  # Set migration = NA if spawning occurs after last downstream
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      !is.na(last_downstream) &
        migration %in% c("spawning", "upstream") &
        arrival > last_downstream,
      NA,
      migration
    )
    )
  
  # Step 4
  # # Identify consecutive migration periods
  df <- df %>%
    dplyr::mutate(period = data.table::rleid(migration))
  # Find main spawning period
  main_spawning_period <- df %>%  
    # Filter only 'spawning' periods
    dplyr::filter(migration == "spawning") %>%                         
    dplyr::group_by(period) %>%
    dplyr::summarise(
      # Min `distance_to_source_m` in each 'spawning' period
      min_distance = min(-distance_to_source_m),  
      # Count the duration in each 'spawning' period
      duration = max(arrival) - min(arrival),
      # Find first value of arrival
      first_arrival = min(arrival),
      # Find last value of arrival
      last_arrival = max(arrival),
      .groups = "drop"
    ) %>%
    # Select the longest 'spawning' period based on its duration
    dplyr::filter(duration == max(duration))
  # Get the minimum distance of the longest 'spawning' period
  min_distance_spawning <- main_spawning_period$min_distance
  # Define lower limit for spawning distance
  lower_spawning_buffer <- min_distance_spawning - spawning_distance_limit
  
  # Convert spawning values to "upstream" if they occur when distance is below
  # `lower_spawning_buffer`, after `first_upstream` and before main spawning
  # period
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("spawning", "downstream") & 
        -distance_to_source_m < lower_spawning_buffer & 
        arrival < main_spawning_period$first_arrival & 
        arrival > first_upstream,
      "upstream",
      migration
    )
    )
  # Convert spawning values to "downstream" if they occur when distance is below
  # `lower_spawning_buffer`, after main spawning period and before
  # `last_downstream`
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("spawning", "upstream") & 
        -distance_to_source_m < lower_spawning_buffer & 
        arrival > main_spawning_period$last_arrival & 
        arrival < last_downstream,
      "downstream",
      migration
    )
    )
  
  # Step 5
  # Now that all spurious spawning values have been converted to upstream or
  # downstream, convert all the upstream or downstream between first and last
  # spawning to spawning
  first_last_spawning_df <- df %>%
    dplyr::filter(migration == "spawning")
  if (nrow(first_last_spawning_df) > 0) {
    # Take the first if multiple detections
    first_spawning <- first_last_spawning_df %>%
      dplyr::filter(arrival == min(arrival)) %>%
      dplyr::pull(arrival)
    last_spawning <- first_last_spawning_df %>%
      dplyr::filter(arrival == max(arrival)) %>%
      dplyr::pull(arrival)
  } else {
    first_spawning <- NA_POSIXct_
    last_spawning <- NA_POSIXct_
  }
  
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      !is.na(first_spawning) & 
        !is.na(last_spawning) & 
        arrival > first_spawning & 
        arrival < last_spawning &
        migration %in% c("upstream", "downstream"),
      "spawning",
      migration
    ))
  
  # Step 6
  # Set end downstream at the (first) most downstream distance. 
  # Convert downstream values to NA after it
  end_downstream_distance_df <- df %>%
    dplyr::filter(migration == "downstream")
  if (nrow(end_downstream_distance_df) > 0) {
    end_downstream_distance_df <- end_downstream_distance_df %>%
      # Take the most downstream distance among the downstream detections
      dplyr::filter(-distance_to_source_m == min(-distance_to_source_m))
    end_downstream_distance_arrival <- end_downstream_distance_df %>%
      # Take the first if multiple detections
      dplyr::filter(arrival == min(arrival)) %>%
      dplyr::select(arrival, distance_to_source_m) %>%
      dplyr::pull(arrival)
  } else {
    end_downstream_distance_arrival <- NA_POSIXct_
  }
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      !is.na(end_downstream_distance_arrival) & 
        arrival > end_downstream_distance_arrival,
      NA,
      migration
    )
    )
  
  # Step 7
  # Remove final spurious misclasifications of upstream, downstream, spawning.
  # This step is similar to Step 4, but less restrictive. For this reason, it
  # must be done at the very end, when the upstream/downstream/spawning periods
  # are well defined.
  
  # Correct misclassifications within downstream migration phase
  first_downstream_df <- df %>%
    # Select from last spawning so that we are sure we are taking "real
    # downstream", no spurious downstream detections
    dplyr::filter(arrival >= last_spawning) %>%
    dplyr::filter(migration == "downstream")
  if (nrow(first_downstream_df) > 0) {
    first_downstream <- first_downstream_df %>%
      # Take the first if multiple detections
      dplyr::filter(arrival == min(arrival)) %>%
      dplyr::pull(arrival)
  } else {
    first_downstream <- NA_POSIXct_
  }
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("spawning", "upstream") & 
        !is.na(first_downstream) & 
        arrival > first_downstream,
      "downstream",
      migration
    )
    )
  
  # Correct misclassifications within upstream migration phase
  last_upstream_df <- df %>%
    dplyr::filter(arrival <= first_spawning) %>%
    dplyr::filter(migration == "upstream")
  # Take the first if multiple detections
  if (nrow(last_upstream_df) > 0) {
    last_upstream <- last_upstream_df %>% 
      dplyr::filter(arrival == max(arrival)) %>%
      dplyr::pull(arrival)
  } else { 
    last_upstream <- NA_POSIXct_
  }
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("spawning", "downstream") &
        !is.na(last_upstream) &
        arrival < last_upstream,
      "upstream",
      migration
    )
  )
  
  # Correct misclassifications within spawning phase
  df <- df %>%
    dplyr::mutate(migration = dplyr::if_else(
      migration %in% c("upstream", "downstream") &
        !is.na(last_upstream) &
        !is.na(first_downstream) &
        arrival > last_upstream &
        arrival < first_downstream,
      "spawning",
      migration
    )
  )
  
  # Remove help column `period`
  df <- df %>%
    dplyr::select(-period)
  return(df)
}

#' Wrap-up function to smooth the data
#'
#' @param df (data.frame) the data to smooth. It must contain the following columns:
#' - `arrival`: the datetime of the detection.
#' - `distance_to_source_m`: the distance to the reference station.
#' - `tag_serial_number`: the serial number of the tag.
#' @param spawning_period (numeric) the spawning period in days. It is equally divided in two halves around the most upstream detection. If multiple detections are found, the first one is taken.
#' @param method_smooth (character) the method used for smoothing. It must be
#' one of `"loess"` or `"gam"`.
#' @param span (numeric) the parameter α which controls the degree of loess smoothing. If `method_smooth` = `"gam"` is used, it must be `NULL`.
#' @param knots (numeric) the number of knots to use in the GAM. It must be an
#'   integer greater than 1. If `method_smooth` = `"loess"` is used, it must be
#'   `NULL`.
#' @param upstream_speed_threshold (numeric) the maximum value of `first_derivative` (speed) to consider a detection as
#' upstream migration. It must be a negative value.
#' @param downstream_speed_threshold (numeric) the minimum value of `first_derivative` (speed) to consider a detection as
#' downstream migration. It must be a positive value.
#' @param spawning_distance_limit (numeric) the buffer below the minimum
#' `distance_to_source_m` of the main spawning period. Used for cleaning up
#' spurious periods.
#' @return the input data.frame with the following columns added:
#' - `detection_window`: a logical indicating if the detection is within the
#' detection window.
#' - `smoothing_window`: a logical indicating if the detection is within the
#' smoothing window.
#' - `smooth_upper`: the upper bound of the smoothed distance (95% confidence interval)
#' - `smooth_lower`: the lower bound of the smoothed distance (95% confidence interval)
#' - `smoothed_distance`: the smoothed distance
#' - `first_derivative`: the first derivative of the smoothed distance.
#' - `migration`: a factor with levels `upstream`, `downstream`, `spawning` and `NA`.
get_spawning_migrations <- function(df,
                                    spawning_period,
                                    method_smooth,
                                    span = NULL,
                                    knots = NULL,
                                    upstream_speed_threshold,
                                    downstream_speed_threshold,
                                    spawning_distance_limit) {
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  assertthat::assert_that(
    "tag_serial_number" %in% names(df),
    msg = "Column with name 'tag_serial_number' is missing in df."
  )
  
  # Check `spawning_period` is numeric
  assertthat::assert_that(
    is.numeric(spawning_period),
    msg = "spawning_period must be a numeric value."
  )
  
  # Check `method_smooth` is one of "loess" or "gam"
  assertthat::assert_that(
    method_smooth %in% c("loess", "gam"),
    msg = "method_smooth must be one of 'loess' or 'gam'."
  )
  
  # Check `span` is a positive number  between 0 and 1 if
  # `method_smooth` = "loess"
  if (method_smooth == "loess") {
    assertthat::assert_that(
      is.numeric(span),
      msg = "span must be a numeric value if `method_smooth` is \"loess\"."
    )
    assertthat::assert_that(
      span > 0,
      msg = "span must be a positive value."
    )
    assertthat::assert_that(
      span <= 1,
      msg = "span must be less than or equal to 1."
    )
  }
  
  # Check `knots` is an integer > 1 if `method_smooth` = "gam"
  if (method_smooth == "gam") {
    assertthat::assert_that(
      knots == round(knots),
      msg = "knots must be an integer value if `method_smooth` is \"gam\"."
    )
    assertthat::assert_that(
      knots > 1,
      msg = "knots must be a value > 1."
    )
  }
  # Check `upstream_speed_threshold` is numeric
  assertthat::assert_that(
    is.numeric(upstream_speed_threshold),
    msg = "upstream_speed_threshold must be a numeric value."
  )
  
  # Return a warning if `upstream_speed_threshold` is not negative
  if (upstream_speed_threshold >= 0) {
    warning(paste0("upstream_speed_threshold must be a negative value.",
                   "Did you forgot to add the negative sign?")
    )
  }
  
  # Check `downstream_speed_threshold` is numeric
  assertthat::assert_that(
    is.numeric(downstream_speed_threshold),
    msg = "downstream_speed_threshold should be a numeric value."
  )
  
  # Return a warning if `downstream_speed_threshold` is not positive
  if (downstream_speed_threshold <= 0) {
    warning(paste0("downstream_speed_threshold should be a positive value. ",
                   "Did you forgot to delete the negative sign?")
    )
  }
  
  # Get the most upstream detection
  most_upstream <- get_most_upstream(df)
  
  # Build the detection window
  df <- build_detection_window(df, most_upstream, spawning_period)
  
  # Apply the smoother
  df <- apply_smooth(df, method_smooth, span, knots)
  
  # Calculate first derivative
  df <- get_first_derivative(df)
  
  # Assign spawning status
  df <- assign_spawning_status(df,
                               upstream_speed_threshold,
                               downstream_speed_threshold)
  
  # Clean up spurious spawning periods (begin of the window)
  df <- clean_up_spurious_periods(df, spawning_distance_limit)
  
  return(df)
}
#' Plot the migration/spawning detection, smoothed distance and confidence interval
#' 
#' @param df (data.frame) the data to plot. It must contain the following columns:
#' - `tag_serial_number`: the serial number of the tag.
#' - `arrival`: the datetime of the detection.
#' - `distance_to_source_m`: the distance to the reference station.
#' - `smoothed_distance`: the smoothed distance.
#' - `smooth_lower`: the lower bound of the smoothed distance (95% confidence interval)
#' - `smooth_upper`: the upper bound of the smoothed distance (95% confidence interval)
#' - `detection_window`: a logical indicating if the detection is within the
#' detection window.
#' - `migration`: a factor with levels `upstream`, `downstream`, `spawning` and `NA`.
#' @param start_spawning_period (datetime) the start of the spawning period.
#' @param end_spawning_period (datetime) the end of the spawning period.
#' @return a ggplot object with the migration/spawning detection plot.
plot_migration_detection <- function(df,
                                     start_spawning_period,
                                     end_spawning_period) {
  
  # Check input data.frame has the needed columns
  assertthat::assert_that(
    "tag_serial_number" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "arrival" %in% names(df),
    msg = "Column with name 'arrival' is missing in df."
  )
  assertthat::assert_that(
    "distance_to_source_m" %in% names(df),
    msg = "Column with name 'distance_to_source_m' is missing in df."
  )
  assertthat::assert_that(
    "smoothed_distance" %in% names(df),
    msg = "Column with name 'smoothed_distance' is missing in df."
  )
  assertthat::assert_that(
    "smooth_lower" %in% names(df),
    msg = "Column with name 'smooth_lower' is missing in df."
  )
  assertthat::assert_that(
    "smooth_upper" %in% names(df),
    msg = "Column with name 'smooth_upper' is missing in df."
  )
  assertthat::assert_that(
    "detection_window" %in% names(df),
    msg = "Column with name 'detection_window' is missing in df."
  )
  assertthat::assert_that(
    "migration" %in% names(df),
    msg = "Column with name 'migration' is missing in df."
  )
  
  # Check `start_spawning_period` is a Datetime object
  assertthat::assert_that(
    lubridate::is.POSIXct(start_spawning_period),
    msg = "start_spawning_period is not a datetime."
  )
  
  # Check `end_spawning_period` is a Datetime object
  assertthat::assert_that(
    lubridate::is.POSIXct(end_spawning_period),
    msg = "end_spawning_period is not a datetime."
  )
  
  # Create plot to check the flagging of spawning migration
  ggplot2::ggplot() +
    ggplot2::geom_hline(yintercept = -1*df$distance_to_source_m,
                        colour = "gray",
                        linewidth = 0.1,
                        linetype = "dashed") +
    ggplot2::geom_line(ggplot2::aes(x = arrival, y = -1*distance_to_source_m), 
                       colour = "black",
                       linewidth = 0.5,
                       data = df) + 
    ggplot2::geom_point(ggplot2::aes(x = arrival,
                                     y = -1*distance_to_source_m),
                        color = "black",
                        size = 1,
                        data = df) + 
    ggplot2::geom_point(ggplot2::aes(x = arrival,
                                     y = -1*distance_to_source_m,
                                     shape = migration,
                                     fill = migration),
                        color = "black",
                        size = 3,
                        data = df %>% dplyr::filter(detection_window == TRUE)) +
    ggplot2::scale_shape_manual(
      values = c("spawning" = 22, # solid square
                 "upstream" = 24, # solid triangle pointing up
                 "downstream" = 25), # solid triangle pointing down
      na.value = 4 # 'x' shape
    ) +
    ggplot2::scale_fill_manual(
      values = c("spawning" = "black",
                 "upstream" = "blue",
                 "downstream" = "green"),
      na.value = "grey"
    ) +
    ggplot2::geom_line(ggplot2::aes(x = arrival, y = - smoothed_distance),
                       color = "red",
                       data = df %>% 
                         dplyr::filter(detection_window == TRUE)) + 
    ggplot2::geom_point(ggplot2::aes(x = arrival, y = - smoothed_distance),
                        color = "red",
                        data = df %>%
                          dplyr::filter(detection_window == TRUE)) +
    ggplot2::geom_ribbon(ggplot2::aes(x = arrival, 
                                      ymin = - smooth_lower, 
                                      ymax = - smooth_upper), 
                         alpha = 0.2,  # transparency
                         fill = "red",
                         data = df %>%
                           dplyr::filter(detection_window == TRUE)) + 
    ggplot2::geom_vline(xintercept = as.numeric(start_spawning_period),
                        colour = "red", 
                        linewidth = 0.5, 
                        linetype = "dashed") +
    ggplot2::geom_vline(xintercept = as.numeric(end_spawning_period),
                        colour = "red",
                        linewidth = 0.5,
                        linetype = "dashed") + 
    ggplot2::ggtitle(
      sprintf("Tag serial number: %s - method smooth: %s",
              unique(df$tag_serial_number),
              method_smooth)
    )
}
