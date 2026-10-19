#' CEFI variable lists and catalog links
#' @noRd
cefi_variable_lists <- data.frame(
  region = c("NEP", "NWA", "NWA", "NWA", "NWA"),
  experiment = c("hindcast", "hindcast", "forecast", "reforecast", "decadal_forecast"),
  link = c(
    "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northeast_pacific.full_domain.hindcast.json", 
    "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.hindcast.json",
    "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.seasonal_forecast.json",
    "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.seasonal_reforecast.json",
    "https://psl.noaa.gov/cefi_portal/data_index/cefi_data_indexing.Projects.CEFI.regional_mom6.cefi_portal.northwest_atlantic.full_domain.decadal_forecast.json"
  )
)

#' Retry an expression with exponential backoff
#'
#' @param expr The expression to evaluate
#' @param max_attempts Maximum number of times to try
#' @param base_sleep Base number of seconds to sleep before the first retry
#'
#' @return The result of the expression if successful
#' @noRd
with_retries <- function(expr, max_attempts = 5, base_sleep = 2) {
  attempt <- 1
  while (attempt <= max_attempts) {
    result <- tryCatch(
      expr,
      error = function(e) e
    )
    
    if (!inherits(result, "error")) {
      return(result)
    }
    
    if (attempt == max_attempts) {
      stop(sprintf("Failed after %d attempts. Final error: %s", max_attempts, result$message), call. = FALSE)
    }
    
    sleep_time <- base_sleep * (2 ^ (attempt - 1)) + stats::runif(1, min = 0, max = 1)
    message(sprintf("Request failed (Attempt %d/%d). Retrying in %.1f seconds...", attempt, max_attempts, sleep_time))
    Sys.sleep(sleep_time)
    
    attempt <- attempt + 1
  }
}

#' List available CEFI MOM6 datasets
#' 
#' @param region MOM6 model region ("NEP" or "NWA")
#' @param experiment Model experiment ("hindcast", "forecast", "reforecast", "decadal_forecast")
#' @param var (optional) variable name; will return all variables if missing
#' @param freq (optional) frequency ("daily" or "monthly")
#' @param category (optional) CEFI data category
#' @param release (optional) CEFI release code
#' 
#' @return A `data.frame` with available datasets
#' @export
#' 
list_mom6 <- function(
  region = c("NEP", "NWA"),
  experiment = c("hindcast", "forecast", "reforecast", "decadal_forecast"),
  var = NULL,
  freq = NULL,
  category = NULL,
  release = NULL
) {
  
  region <- match.arg(region)
  experiment <- match.arg(experiment)
  
  # Query available datasets
  available <- jsonlite::fromJSON(cefi_variable_lists$link[cefi_variable_lists$region == region & cefi_variable_lists$experiment == experiment])
  available <- do.call("rbind", lapply(available, as.data.frame))
  
  # Filter by arguments
  if (!missing(var)) available <- available[available$cefi_variable == var,]
  if (!missing(freq)) available <- available[available$cefi_output_frequency == freq,]
  if (!missing(category)) available <- available[available$cefi_ori_category == category,]
  if (!missing(release)) available <- available[available$cefi_release == release,]
  
  return(available)
  
}

#' Download gridded estimates from CEFI MOM6 experiments
#'
#' @param var variable name. See [CEFI portal](https://psl.noaa.gov/cefi_portal/) or use
#'   `list_mom6()` for options.
#' @param freq "monthly" or "daily"
#' @param grid_type "regrid" or "raw". Regridded data is on a regular grid, while raw data is on the model's native curvilinear grid.
#' @param region CEFI region. "NEP" for Northeast Pacific or "NWA" for Northwest Atlantic.
#' @param experiment CEFI experiment. Defaults to "hindcast".
#' @param release release code. If NA, returns latest available release.
#' @param extent Either (1) a shapefile from which to compute the extent,
#'  (2) a bounding box created using `sf::st_bbox()` with accompanying CRS,
#'  or (3) NA for the full MOM6 grid. If a shapefile is provided, it will be
#'  used to mask (crop) the output prior to returning. NULL defaults to
#'  to the NOAA AFSC EBS (including NBS) survey region for NEP, and full domain for NWA.
#' @param start_date Initial date to query data for. If frequency = "monthly",
#'   only years and months of provided date is used, date of month is ignored.
#'   If NA, all available dates are returned. Can be NA even if `end_date` is provided.
#' @param end_date Final date to query data for. If frequency = "monthly",
#'   only years and months of provided date is used, date of month is ignored.
#'   If NA, all available dates are returned. Can be NA even if `start_date` is provided.
#' @param category (optional) data category; should not be required unless the same variable name
#'   is used in multiple categories. See [CEFI portal](https://psl.noaa.gov/cefi_portal/) or use
#'   `list_mom6()` for options.
#' @param chunk Time interval to chunk requests by (e.g., "10 years"); "none" for no chunking.
#'   Using no chunks may result in request hitting server data limits; using too many chunks 
#'   may result in hitting server rate limits. Accepts any string that `seq.Date` can parse.
#'
#' @return A `stars` object
#' @export
#'
get_mom6 <- function(
  var = "tob",
  freq = c("monthly", "daily"),
  grid_type = c("regrid", "raw"),
  region = c("NEP", "NWA"),
  experiment = c("hindcast", "forecast", "reforecast", "decadal_forecast"),
  release = NA,
  extent = NULL,
  start_date = NA,
  end_date = NA,
  category = NULL,
  chunk = "none"
) {
  
  grid_type <- match.arg(grid_type)
  region <- match.arg(region)
  experiment <- match.arg(experiment)
  freq <- match.arg(freq)
  
  # Default to EBS survey region if requesting NEP data
  if (is.null(extent)) {
    if (region == "NEP") {
      extent <- get_ebs_shapefile("EBS")
    } else {
      extent <- NA
    }
  }
  
  if (grid_type == "regrid") {
    
    get_mom6_regrid(
      var = var,
      freq = freq,
      region = region,
      experiment = experiment,
      category = category,
      release = release,
      extent = extent,
      start_date = start_date,
      end_date = end_date,
      chunk = chunk
    )

  } else {
    
    get_mom6_raw(
      var = var,
      freq = freq,
      region = region,
      experiment = experiment,
      category = category,
      release = release,
      extent = extent,
      start_date = start_date,
      end_date = end_date,
      chunk = chunk
    )

  }

}

#' Internal function to query regridded MOM6 data
#' @noRd
get_mom6_regrid <- function(
  var = "tob",
  freq = "monthly",
  region = "NEP",
  experiment = "hindcast",
  release = NA,
  extent = NA,
  start_date = NA,
  end_date = NA,
  category = NULL,
  chunk = "none"
) {
  
  # Query available datasets
  available <- list_mom6(region = region, experiment = experiment, freq = freq)
  available <- available[available$cefi_grid_type == "regrid",]
  if (!is.null(category)) available <- available[available$cefi_ori_category == category,]
  
  # Argument checking
  if (!(nrow(available) > 0)) stop(paste0("no variables for freq = ", freq, " and experiment = ", experiment))
  if (!(var %in% available$cefi_variable)) stop(paste0("specified variable not available freq = ", freq, " and experiment = ", experiment))
  
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
  nc <- with_retries(suppressWarnings(suppressMessages(tidync::tidync(url))), max_attempts = 5)
  
  if (inherits(extent, "bbox")) extent <- sf::st_as_sfc(extent) 

  if (inherits(extent, c("sf", "sfc"))) {
    
    extent_wgs84 <- sf::st_transform(extent, crs = 4326)

    # Wrap dateline
    if (region == "NEP") extent_wgs84 <- sf::st_shift_longitude(extent_wgs84)

    bbox <- sf::st_bbox(extent_wgs84)
    
    nc <- nc |> tidync::hyper_filter(
      lon = lon >= bbox["xmin"] & lon <= bbox["xmax"],
      lat = lat >= bbox["ymin"] & lat <= bbox["ymax"]
    )
    
  } else if (!is.na(extent)) {
    
    stop("extent must either be an sf object, bbox, or NA")
    
  }
  
  # Netcdf file start and end dates
  var_dates <- strsplit(var_info$cefi_date_range, split = "-")[[1]]
  dom_end <- lubridate::days_in_month(as.Date(paste0(var_dates[2], "15"), format = "%Y%m%d"))
  var_dates <- as.Date(paste0(var_dates, c("01", dom_end)), format = "%Y%m%d")
  
  # Expand to date dimension
  if (freq == "monthly") {
    var_dates <- seq(var_dates[1], var_dates[2], by = "month")
    var_dates <- lubridate::floor_date(var_dates, "month")
  } else {
    var_dates <- seq(var_dates[1], var_dates[2], by = "day")
  }
  
  all_dates <- var_dates
  
  # Clip temporal extent, if necessary
  if(!is.na(start_date) | !is.na(end_date)) {
    
    dates_in <- rep(TRUE, length(all_dates))
    
    if (!is.na(start_date)) {
      
      if (!inherits(start_date, "Date")) stop("`start_date` must be class 'Date'")
      if (freq == "monthly") start_date <- lubridate::floor_date(start_date, "month")
      
      dates_in <- all_dates >= start_date
      
    }
    
    if (!is.na(end_date)) {
      
      if (!inherits(end_date, "Date")) stop("`end_date` must be class 'Date'")
      if (freq == "monthly") end_date <- lubridate::floor_date(end_date, "month")
      
      dates_in <- dates_in & (all_dates <= end_date)
      
    }
    
    if (!any(dates_in)) stop(paste(
      "Specified date range is invalid. Available range is",
      all_dates[1], "-", all_dates[length(all_dates)]
    ))
    
    var_dates <- all_dates[dates_in]
    
  }
  
  # Optionally, retrieve data in chunks to avoid server data limits
  if (chunk != "none") {
    
    if (is.na(start_date)) start_date <- min(var_dates)
    if (is.na(end_date)) end_date <- max(var_dates)
    
    start_dates <- seq(start_date, end_date, by = chunk)
    end_dates <- c(start_dates[-1] - 1, end_date)
    
    stars_list <- vector("list", length(start_dates))
    
    for (i in seq_along(start_dates)) {
      
      chunk_dates <- var_dates[var_dates >= start_dates[i] & var_dates <= end_dates[i]]
      indices <- match(chunk_dates, all_dates)
      
      nc_chunk <- nc |> tidync::hyper_filter(time = index >= min(indices) & index <= max(indices))
      
      cube_chunk <- with_retries(tidync::hyper_tbl_cube(nc_chunk), max_attempts = 5)
      cube_chunk$dims$lon <- rotate_lon(cube_chunk$dims$lon, from = "0/360")
      
      # Cell corners
      if (i == 1) {
        delta <- vapply(cube_chunk$dims[1:2], \(x) diff(x)[1], numeric(1))
        offset <- vapply(cube_chunk$dims[1:2], \(x) x[1], numeric(1)) - delta / 2
      }
      
      stars_list[[i]] <- stars::st_as_stars(cube_chunk$mets[[1]])
      
    }
    
    # Combine chunks
    nc <- do.call(c, c(stars_list, along = 3))
    
  } else {
    
    indices <- match(var_dates, all_dates)
    
    nc <- nc |> tidync::hyper_filter(time = index >= min(indices) & index <= max(indices))
    nc <- with_retries(tidync::hyper_tbl_cube(nc), max_attempts = 5)
    nc$dims$lon <- rotate_lon(nc$dims$lon, from = "0/360")
    
    delta <- vapply(nc$dims[1:2], \(x) diff(x)[1], numeric(1))
    offset <- vapply(nc$dims[1:2], \(x) x[1], numeric(1)) - delta / 2
    
    nc <- stars::st_as_stars(nc$mets[[1]])
    
  }
  
  # Convert to stars
  nc <- stats::setNames(nc, var)
  nc <- stars::st_set_dimensions(nc, which = 1, offset = offset["lon"], delta = delta["lon"], names = "lon", point = FALSE)
  nc <- stars::st_set_dimensions(nc, which = 2, offset = offset["lat"], delta = delta["lat"], names = "lat", point = FALSE)
  if (length(dim(nc)) == 3) nc <- stars::st_set_dimensions(nc, which = 3, values = var_dates, names = "time")
  nc <- stars::st_set_dimensions(nc, xy = c("lon", "lat"))
  sf::st_crs(nc) <- sf::st_crs(4326)
  
  # Crop to shapefile extent, if applicable
  if (inherits(extent, "sf")) {
    if (sf::st_crs(extent) != sf::st_crs(nc)) {
      extent <- sf::st_transform(extent, crs = sf::st_crs(4326))
    }
    nc <- nc[extent]
  }
  
  return(nc)
  
}

#' Internal function to query raw MOM6 data
#' @noRd
get_mom6_raw <- function(
  var = "tob",
  freq = "monthly",
  region = "NEP",
  experiment = "hindcast",
  release = NA,
  extent = NA,
  start_date = NA,
  end_date = NA,
  category = NULL,
  chunk = "none"
) {
  
  # Query available datasets
  available <- list_mom6(region = region, experiment = experiment, freq = freq)
  available <- available[available$cefi_grid_type == "raw",]
  if (!is.null(category)) available <- available[available$cefi_ori_category == category,]
  
  # Argument checking
  if (!(nrow(available) > 0)) stop(paste0("no variables for freq = ", freq, " and experiment = ", experiment))
  if (!(var %in% available$cefi_variable)) stop(paste0("specified variable not available freq = ", freq, " and experiment = ", experiment))
  
  # Subset to requested variable
  available <- available[available$cefi_variable == var,]
  
  # Get latest release available or specified release
  if (is.na(release)) {
    releases <- unique(available$cefi_release)
    release <- releases[which.max(as.Date(gsub("r", "", releases), format = "%Y%m%d"))]
    var_info <- available[available$cefi_release == release,]
    cat(paste0("Using release ", as.Date(gsub("r", "", release), format = "%Y%m%d"), "\n"))
  } else {
    var_info <- available[available$cefi_release == release,]
    if (nrow(var_info) == 0) stop(paste0("release ", release, " not available"))
  }
  
  # Link to netcdf file
  if (nrow(var_info) != 1) stop("Problem with query")
  url <- var_info$cefi_opendap
  nc <- with_retries(suppressWarnings(suppressMessages(tidync::tidync(url))), max_attempts = 5)
  
  # Link to corresponding static grid
  region_long <- switch(region, "NEP" = "northeast_pacific", "NWA" = "northwest_atlantic")
  experiment_long <- ifelse(experiment %in% c("forecast", "reforecast"), paste0("seasonal_", experiment), experiment)
  thredds_base <- 'http://psl.noaa.gov/thredds/dodsC/Projects/CEFI/regional_mom6/cefi_portal'
  url_static <- paste(thredds_base, region_long, "full_domain", experiment_long, freq, "raw", release, "ocean_static.nc", sep = "/")
  nc_static <- with_retries(suppressWarnings(suppressMessages(tidync::tidync(url_static))), max_attempts = 5)
  
  # Activate dimensions in nc_static matching dimensions for requested variable
  dims <- nc$dimension$name[nc$dimension$active]
  dims <- dims[dims != "time"]
  dims_use <- nc_static$dimension$id[nc_static$dimension$name %in% dims]
  dims_use <- paste0("D", sort(dims_use), collapse = ",")
  nc_static <- nc_static |> tidync::activate(dims_use)
  
  # Extract coordinates corresponding to grid location (h/v/u/c) depending on variable
  grid_vars <- nc_static$grid$variables[nc_static$grid$grid == dims_use][[1]][[1]]
  coord_vars <- c(lon = grid_vars[grepl("geolon", grid_vars)], lat = grid_vars[grepl("geolat", grid_vars)])
  coords <- stats::setNames(tidync::hyper_array(nc_static, select_var = coord_vars), names(coord_vars))
  coords <- simplify2array(coords)
  
  # geolon / geolat are sometimes NA over land - these need to be appended to enable reprojection with `stars`
  if (any(is.na(coords))) {
    X <- cbind(1, stats::poly(I(seq_len(dim(coords)[2])), 3))
    for (i in seq_len(dim(coords)[1])) {
      coords[i, is.na(coords[i,,"lon"]), "lon"] <- (X %*% stats::coef(stats::lm(I(coords[i,,"lon"]) ~ 0 + I(X))))[is.na(coords[i,,"lon"])]
      coords[i, is.na(coords[i,,"lat"]), "lat"] <- (X %*% stats::coef(stats::lm(I(coords[i,,"lat"]) ~ 0 + I(X))))[is.na(coords[i,,"lat"])]
    }
  }
  
  # Clip spatial extent, if necessary
  if (!all(is.na(extent))) {
    
    if (inherits(extent, "bbox")) extent <- sf::st_as_sfc(extent) 

    # Construct bounding box
    if (inherits(extent, c("sf", "sfc"))) { 

      extent_wgs84 <- sf::st_transform(extent, crs = 4326)

      if (region == "NEP") extent_wgs84 <- sf::st_shift_longitude(extent_wgs84)

      bbox <- sf::st_bbox(extent_wgs84)

    } else {
      
      stop("extent must either be an sf object, bbox, or NA")

    }
     
    # Use matrices of lat/long to figure out which MOM6 coordinate range to use
    mask <- (
      coords[,,"lon"] >= bbox["xmin"] & coords[,,"lon"] <= bbox["xmax"] & 
      coords[,,"lat"] >= bbox["ymin"] & coords[,,"lat"] <= bbox["ymax"]
    )
    
    if (!sum(mask)) stop("no data within requested extent")
    
    x_idx <- range(which(apply(mask, 1, any)))
    y_idx <- range(which(apply(mask, 2, any)))
    
    # Restrict query to this range
    nc <- nc |> tidync::hyper_filter(
      !!dims[1] := index >= x_idx[1] & index <= x_idx[2],
      !!dims[2] := index >= y_idx[1] & index <= y_idx[2]
    )
    
    # Subset coords for use downstream
    coords <- coords[x_idx[1]:x_idx[2], y_idx[1]:y_idx[2], ]
    
  } else if (!all(is.na(extent))) {
    
    stop("extent must either be an sf object, bbox, or NA")
    
  }
  
  # Netcdf file start and end dates
  var_dates <- strsplit(var_info$cefi_date_range, split = "-")[[1]]
  dom_end <- lubridate::days_in_month(as.Date(paste0(var_dates[2], "15"), format = "%Y%m%d"))
  var_dates <- as.Date(paste0(var_dates, c("01", dom_end)), format = "%Y%m%d")
  
  # Expand to date dimension
  if (freq == "monthly") {
    var_dates <- seq(var_dates[1], var_dates[2], by = "month")
    var_dates <- lubridate::floor_date(var_dates, "month")
  } else {
    var_dates <- seq(var_dates[1], var_dates[2], by = "day")
  }
  
  all_dates <- var_dates
  
  # Clip temporal extent, if necessary
  if(!is.na(start_date) | !is.na(end_date)) {
    
    dates_in <- rep(TRUE, length(all_dates))
    
    if (!is.na(start_date)) {
      
      if (!inherits(start_date, "Date")) stop("`start_date` must be class 'Date'")
      if (freq == "monthly") start_date <- lubridate::floor_date(start_date, "month")
      
      dates_in <- all_dates >= start_date
      
    }
    
    if (!is.na(end_date)) {
      
      if (!inherits(end_date, "Date")) stop("`end_date` must be class 'Date'")
      if (freq == "monthly") end_date <- lubridate::floor_date(end_date, "month")
      
      dates_in <- dates_in & (all_dates <= end_date)
      
    }
    
    if (!any(dates_in)) stop(paste(
      "Specified date range is invalid. Available range is",
      all_dates[1], "-", all_dates[length(all_dates)]
    ))
    
    var_dates <- all_dates[dates_in]
    
  }
  
  # Optionally, retrieve data in chunks to avoid server data limits
  if (chunk != "none") {
    
    if (is.na(start_date)) start_date <- min(var_dates)
    if (is.na(end_date)) end_date <- max(var_dates)
    
    start_dates <- seq(start_date, end_date, by = chunk)
    end_dates <- c(start_dates[-1] - 1, end_date)
    
    stars_list <- vector("list", length(start_dates))
    
    for (i in seq_along(start_dates)) {
      
      chunk_dates <- var_dates[var_dates >= start_dates[i] & var_dates <= end_dates[i]]
      indices <- match(chunk_dates, all_dates)
      
      nc_chunk <- nc |> tidync::hyper_filter(time = index >= min(indices) & index <= max(indices))
      
      cube_chunk <- with_retries(tidync::hyper_tbl_cube(nc_chunk), max_attempts = 5)
      cube_chunk$dims$lon <- rotate_lon(cube_chunk$dims$lon, from = "0/360")
      
      # Cell corners
      if (i == 1) {
        delta <- vapply(cube_chunk$dims[1:2], \(x) diff(x)[1], numeric(1))
        offset <- vapply(cube_chunk$dims[1:2], \(x) x[1], numeric(1)) - delta / 2
      }
      
      stars_list[[i]] <- stars::st_as_stars(cube_chunk$mets[[1]])
      
    }
    
    # Combine chunks
    nc <- do.call(c, c(stars_list, along = 3))
    
  } else {
    
    indices <- match(var_dates, all_dates)
    
    nc <- nc |> tidync::hyper_filter(time = index >= min(indices) & index <= max(indices))
    nc <- with_retries(tidync::hyper_tbl_cube(nc), max_attempts = 5)
    nc$dims$lon <- rotate_lon(nc$dims$lon, from = "0/360")
    
    delta <- vapply(nc$dims[1:2], \(x) diff(x)[1], numeric(1))
    offset <- vapply(nc$dims[1:2], \(x) x[1], numeric(1)) - delta / 2
    
    nc <- stars::st_as_stars(nc$mets[[1]])
    
  }
  
  # Convert to stars
  nc <- stats::setNames(nc, var)
  coords[, , "lon"] <- rotate_lon(coords[, , "lon"], from = "0/360")
  nc <- stars::st_as_stars(nc, curvilinear = stats::setNames(asplit(coords, 3), dims))
  sf::st_crs(nc) <- sf::st_crs(4326)
  
  # Crop to shapefile extent, if applicable
  if (inherits(extent, c("sf", "sfc"))) {
    if (sf::st_crs(extent) != sf::st_crs(nc)) {
      extent <- sf::st_transform(extent, crs = sf::st_crs(nc))
    }
    nc <- nc[extent]
  }
  
  return(nc)
  
}
