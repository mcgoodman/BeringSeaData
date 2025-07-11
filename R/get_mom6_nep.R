
#' Download gridded estimates from the CEFI MOM6 NEP hindcast
#'
#' @param var variable name. See https://psl.noaa.gov/cefi_portal/ for options.
#' @param freq "monthly" or "daily". Note that a matching category must also be
#'   specified, e.g. "ocean_daily" for daily frequency.
#' @param category data category
#' @param release release code. If NA, returns latest available release.
#' @param extent Either (1) a shapefile from which to compute the extent,
#'  (2) a bounding box created using `sf::st_bbox()` with accompanying CRS,
#'  or (3) NA for the full MOM6 NEP grid. If a shapefile is provided, it will be
#'  used to mask (crop) the output prior to returning. Defaults to NOAA AFSC
#'  Eastern Bering Sea (including NBS) survey region.
#' @param start_date Initial date to query data for. If frequency = "monthly",
#'   only years and months of provided date is used, date of month is ignored.
#'   If NA, all available dates are returned. Can be NA even if `end_date` is provided.
#' @param end_date Final date to query data for. If frequency = "monthly",
#'   only years and months of provided date is used, date of month is ignored.
#'   If NA, all available dates are returned. Can be NA even if `start_date` is provided.
#' @param target_crs CRS to transform output to. Defaults to UTM zone 2N (EPSG 32602).
#'   NA defaults to WGS84 lat/long (EPSG 4326).
#'
#' @return A `stars` object
#' @export
#'
get_mom6_nep <- function(
    var = "tob",
    freq = c("monthly", "daily"),
    category = c(
      "ocean_monthly", "ice_monthly", "ocean_cobalt_omip_sfc", "ocean_cobalt_btm",
      "ocean_cobalt_sfc", "ocean_cobalt_tracers_int", "ocean_cobalt_omip_2d",
      "ocean_monthly_z", "ocean_cobalt_tracers_month_z", "ocean_cobalt_fluxes_int",
      "ocean_cobalt_daily_2d", "ocean_daily"
    ),
    release = NA,
    extent = get_ebs_shapefile("EBS"),
    start_date = NA,
    end_date = NA,
    target_crs = 32602
) {

  # Query available datasets
  available <- jsonlite::fromJSON("https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northeast_pacific.full_domain.hindcast.json")
  available <- do.call("rbind", lapply(available, as.data.frame))

  # Process arguments
  freq <- match.arg(freq)
  category <- match.arg(category)

  # Subset available datasets to match arguments
  available <- available[
    available$cefi_output_frequency == freq & available$cefi_ori_category == category &
      available$cefi_experiment_type == "hindcast" & available$cefi_grid_type == "regrid",
  ]

  # Argument checking
  if (!(nrow(available) > 0)) stop(paste0("specified data category not available for freq = ", freq))
  if (!(var %in% available$cefi_variable)) stop(paste0("specified variable not available freq = ", freq, " and category = ", category))

  # Subset to requested variable
  available <- available[available$cefi_variable == var,]

  # Get latest release available or specified release
  if (is.na(release)) {
    releases <- unique(available$cefi_release)
    newest_release <- releases[which.max(as.Date(gsub("r", "", releases), format = "%Y%m%d"))]
    var_info <- available[available$cefi_release == newest_release,]
    cat(paste0("Using release ", as.Date(gsub("r", "", newest_release), format = "%Y%m%d"), "\n"))
  } else {
    var_info <- available[available$cefi_release == release,]
    if (nrow(var_info) == 0) stop(paste0("release ", release, " not available"))
  }

  # Link to netcdf file
  if (nrow(var_info) != 1) stop("Problem with query")
  url <- var_info$cefi_opendap
  nc <- suppressMessages(tidync::tidync(url))

  # Clip spatial extent, if necessary
  if (inherits(extent, "sf")) {

    extent_wgs84 <- sf::st_transform(extent, crs = 4326)
    bbox <- sf::st_bbox(extent_wgs84)
    bbox[c("xmin", "xmax")] <- rotate_lon(bbox[c("xmin", "xmax")])

    nc <- nc |> tidync::hyper_filter(
      lon = lon >= bbox["xmin"] & lon <= bbox["xmax"],
      lat = lat >= bbox["ymin"] & lat <= bbox["ymax"]
    )

  } else if (inherits(extent, "bbox")) {

    bbox <- sf::st_transform(extent, crs = 4326)
    bbox[c("xmin", "xmax")] <- rotate_lon(bbox[c("xmin", "xmax")])

    nc <- nc |> tidync::hyper_filter(
      lon = lon >= bbox["xmin"] & lon <= bbox["xmax"],
      lat = lat >= bbox["ymin"] & lat <= bbox["ymax"]
    )

  } else if (!is.na(bbox)) {

    stop("extent must either be an sf object, bbox, or NA")

  }

  # Start and end dates
  var_dates <- as.Date(paste0(strsplit(var_info$cefi_date_range, split = "-")[[1]], c("01", "31")), format = "%Y%m%d")

  # Expand to date dimension
  if (freq == "monthly") {
    var_dates <- seq(var_dates[1], var_dates[2], by = "month")
    var_dates <- lubridate::floor_date(var_dates, "month")
  } else {
    var_dates <- seq(var_dates[1], var_dates[2], by = "day")
  }

  # Clip temporal extent, if necessary
  if(!is.na(start_date) | !is.na(end_date)) {

    dates_in <- rep(TRUE, length(var_dates))

    if (!is.na(start_date)) {

      if (!inherits(start_date, "Date")) stop("`start_date` must be class 'Date'")
      if (freq == "monthly") start_date <- lubridate::floor_date(start_date, "month")

      dates_in <- var_dates >= start_date

    } else {

      start_date <- var_dates[1]

    }

    if (!is.na(end_date)) {

      if (!inherits(end_date, "Date")) stop("`start_date` must be class 'Date'")
      if (freq == "monthly") end_date <- lubridate::floor_date(end_date, "month")

      dates_in <- dates_in & (var_dates <= end_date)

    } else {

      end_date <- var_dates[length(var_dates)]

    }

    if (!any(dates_in)) stop(paste(
      "Specified date range is invalid. Available range is",
      var_dates[1], "-", var_dates[length(var_dates)]
    ))

    nc <- nc |> tidync::hyper_filter(
      time = index >= Position(isTRUE, dates_in, right = FALSE) &
        index <= Position(isTRUE, dates_in, right = TRUE)
    )

    var_dates <- var_dates[dates_in]

  }

  # Retrieve data and convert to stars
  nc <- nc |>
    st_as_stars.tidync() |>
    sf::st_set_crs(4326) |>
    stars::st_set_dimensions("time", values = var_dates)

  # Resample on target CRS, if applicable
  if (!is.na(target_crs)) {
    nc <- stars::st_warp(nc, crs = target_crs)
  }

  # Crop to shapefile extent, if applicable
  if (inherits(extent, "sf")) {
    if (sf::st_crs(extent) != target_crs) {
      extent <- sf::st_transform(extent, crs = target_crs)
    }
    nc <- nc[extent]
  }

  return(nc)

}
