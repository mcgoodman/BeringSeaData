

#' Compute week-of-year means for ROMS output
#'
#' @param x A multiband `stars` object returned by `get_level2`
#' @param start integer start year. NA defaults to all available years.
#' @param end integer end year. NA defaults to all available years.
#'
#' @return A `stars` object
#' @export
weight_weeks <- function(x, start = NA, end = NA) {

  # Subset time, if applicable
  if(!is.na(start) | !is.na(end)) {
    if (!is.na(start)) {
      if(!(start == as.integer(start))) stop("start should be an integer year")
      start_date <- as.POSIXct(paste0(start, "-01-01 00:00:00"), tz = "UTC")
      dates_in <- which(stars::st_get_dimension_values(x, "ocean_time") >= start_date)
      x <- x |> dplyr::slice(dates_in, along = "ocean_time")
    }
    if (!is.na(end)) {
      if(!(end == as.integer(end))) stop("end should be an integer year")
      end_date <- as.POSIXct(paste0(end, "-12-31 23:59:00"), tz = "UTC")
      dates_in <- which(stars::st_get_dimension_values(x, "ocean_time") <= end_date)
      x <- x |> dplyr::slice(dates_in, along = "ocean_time")
    }
  }

  # Extract dates from raster
  days <- as.Date(stars::st_get_dimension_values(x, "ocean_time"))

  # Matrix to hold distance between raster dates and week-of-year midpoints
  roms_weights <- matrix(NA_real_, nrow = length(days), ncol = 52)

  # Loop over raster layers, store distance between raster layer date and week midpoints
  for (i in 1:length(days)) {

    # Midpoints for corresponding year
    wk_mdpts <- seq(as.Date(paste0(lubridate::year(days[i]), "-01-04")), by = "week", length.out = 52)

    # Handle special cases - e.g. distance from January dates to week 52 of last year
    if(lubridate::month(days)[i] == 1) wk_mdpts[52] <- seq(as.Date(paste0(lubridate::year(days[i]) - 1, "-01-04")), by = "week", length.out = 52)[52]
    if(lubridate::month(days)[i] == 12) wk_mdpts[1] <- as.Date(paste0(lubridate::year(days[i]) + 1, "-01-04"))

    # Distance between raster layer date and week midpoints (>7 days set to zero)
    roms_weights[i,] <- pmax(1 - abs((days[i] - wk_mdpts) / 7), 0)

  }

  # Loop over weeks, store weighted mean of applicable raster layers for each week
  for (i in 1:ncol(roms_weights)) {

    # Raster layers to compute weighted mean from, and corresponding weights
    weeks <- which(roms_weights[,i] > 0)
    weights <- roms_weights[weeks,i]

    if (i == 1) {
      roms_weekly <- x |> dplyr::slice(weeks, along = "ocean_time") |>
        stars::st_apply(1:2, weighted.mean, weights = weights, na.rm = TRUE)
    } else {
      roms_week <- x |> dplyr::slice(weeks, along = "ocean_time") |>
        stars::st_apply(1:2, weighted.mean, weights = weights, na.rm = TRUE)
      roms_weekly <- c(roms_weekly, roms_week)
    }

  }

  # Set time dimension to week-of-year
  roms_weekly <- roms_weekly |> merge(name = "week") |> stars::st_set_dimensions("week", values = 1:52)

  # Format names and units and return
  names(roms_weekly) <- names(x)
  if ("units" %in% class(x[[names(x)]])) units(roms_weekly[[names(x)]]) <- units(x[[names(x)]])
  roms_weekly

}

#' Delta-correct ROMS level 2 outputs
#'
#' @param x ROMS level 2 `stars` object returned by `get_level2`
#' @param hindcast Weekly means for ROMS level 2 hindcast, returned by `weight_weekly`
#' @param historical Weekly means for ROMS level 2 historical run, returned by `weight_weekly`
#' @param lower If applicable, lower threshold for returned variable
#' @param upper If applicable, upper thresdhold for returned variable
#'
#' @return A `stars` object
#' @export
delta_correct <- function(x, hindcast, historical, lower = NA, upper = NA) {

  days <- as.Date(stars::st_get_dimension_values(x, "ocean_time"))

  for (i in 1:length(days)) {

    # Midpoints for corresponding year
    wk_mdpts <- seq(as.Date(paste0(lubridate::year(days[i]), "-01-04")), by = "week", length.out = 52)
    if(lubridate::month(days)[i] == 1) wk_mdpts[52] <- seq(as.Date(paste0(lubridate::year(days[i]) - 1, "-01-04")), by = "week", length.out = 52)[52]
    if(lubridate::month(days)[i] == 12) wk_mdpts[1] <- as.Date(paste0(lubridate::year(days[i]) + 1, "-01-04"))

    # Week bin corresponding to date
    wk_bin <- which.min(abs(wk_mdpts - days[i]))

    # Delta correction
    if (i == 1) {
      bc_rast <- x[,,,i, drop = TRUE] + (hindcast[,,,wk_bin, drop = TRUE] - historical[,,, wk_bin, drop = TRUE])
    } else {
      bc_rast_day <- x[,,,i, drop = TRUE] + (hindcast[,,,wk_bin, drop = TRUE] - historical[,,, wk_bin, drop = TRUE])
      bc_rast <- c(bc_rast, bc_rast_day)
    }

  }

  # Set dimensions and attributes
  bc_rast <- bc_rast |> merge(name = "ocean_time") |> stars::st_set_dimensions("ocean_time", days)
  names(bc_rast) <- names(x)

  # Deal with bounds
  if (!is.na(lower)) bc_rast[bc_rast < lower] <- lower
  if (!is.na(upper)) bc_rast[bc_rast > upper] <- upper

  bc_rast

}
