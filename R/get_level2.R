
#' Download Bering 10K level 2 gridded ROMS output
#'
#' @param var variable name
#' @param type "hindcast", "projection", or "historical" run
#' @param scenario "SSP126" or "SSP585" for CMIP6 (`version = "K20P19"` or `version = "K20"`) outputs, "RCP45" or "RCP85" for CMIP5 (`version = "H16"`) outputs
#' @param start integer start year. NA defaults to all available years.
#' @param end integer end year. NA defaults to all available years.
#' @param version Either (1) "K20P19" for 30-layer ROMs model from \insertCite{K20;textual}{BeringSeaData}, with carbonate dynamics from
#' \insertCite{P19;textual}{BeringSeaData} (default), (2) "K20" for 30-layer model without carbonate dynamics, or (3) "H16" for 10-layer
#' ROMS model from \insertCite{H16;textual}{BeringSeaData}
#' @param earth_model "GFDL", "CESM", or "MIROC"
#' @param crop_ebs if TRUE, crop to EBS/NBS survey boundary
#' @param write_dir path to write downloaded raster (as GEOTIF). if NA, object is not written to disk.
#'
#'
#' @references
#' \insertAllCited
#'
#' @importFrom Rdpack reprompt
#' @return A `stars` object
#' @export
get_level2 <- function(var,
                       type = c("hindcast", "projection", "historical"),
                       scenario = c("SSP126", "SSP585", "RCP45", "RCP85"),
                       start = NA,
                       end = NA,
                       version = c("K20P19", "K20", "H16"),
                       earth_model = c("GFDL", "CESM", "MIROC"),
                       crop_ebs = TRUE,
                       write_dir = NA) {

  require("curl")
  require("dplyr")
  require("stringr")
  require("stars")

  # Base directory url for Thredds server
  url <- "https://data.pmel.noaa.gov/aclim/thredds"

  # Check argument validity
  type <- match.arg(type)
  scenario <- tolower(match.arg(scenario))
  earth_model <- match.arg(earth_model)
  version <- match.arg(version)
  if(version == "H16" & grepl("ssp", scenario)) stop("CMIP5 / H16 options are RCP45 or RCP85")
  if((version == "K20" | version == "K20P19") & grepl("rcp", scenario)) stop("CMIP6 / K20 options are SSP126 or SSP585")
  if(version == "K20" | version == "K20P19") earth_model <- tolower(earth_model)

  # Derived arguments
  cmip <- switch(version, K20P19 = "CMIP6", K20 = "CMIP6", H16 = "CMIP5")
  var_base <- strsplit(var, "_")[[1]][1]

  # Paste together sim directory based on arguments
  sim <- type |> switch(
    hindcast = paste0("B10K-", version, "_CORECFS"),
    historical = paste0("B10K-", paste(version, cmip, earth_model, "historical", sep = "_")),
    projection =  paste0("B10K-", paste(version, cmip, earth_model, scenario, sep = "_"))
  )

  # List directories on sim page using html scraping
  var_yrs <- check_availability(var, type, toupper(scenario), version, toupper(earth_model), print_console = FALSE)
  sim_dirs <- unique(var_yrs$data_links$years)

  # Subset directories to download using start and end years
  if(!is.na(start) | !is.na(end)) {
    if (!is.na(start)) if(!(start == as.integer(start))) stop("start should be an integer year")
    if (!is.na(end)) if(!(end == as.integer(end))) stop("end should be an integer year")
    dir_yrs <- lapply(sim_dirs, \(x) as.integer(strsplit(x, "-")[[1]][1]):as.integer(strsplit(x, "-")[[1]][2]))
    start_sub <- vapply(dir_yrs, \(x) any(ifelse(is.na(start), dir_yrs[[1]][1], start) <= x), logical(1))
    end_sub <- vapply(dir_yrs, \(x) any(x <= ifelse(is.na(end), max(dir_yrs[[length(dir_yrs)]]), end)), logical(1))
    sim_dirs <- sim_dirs[start_sub & end_sub]
  }

  # Paste urls for all years in sim
  sim_urls <- paste0(url, "/fileServer/", sim, "/Level2/", sim_dirs, "/", sim, "_", sim_dirs, "_average_", var, ".nc")

  # Download data
  dir.create(tmp <- tempdir())
  dir.create(save_dir <- paste(tmp, sim, sep = "/"))
  file_names <- paste0(save_dir, "/", var, "_", sim_dirs, ".nc")
  for (i in 1:length(sim_urls)) {
    tryCatch(
      curl::curl_download(sim_urls[i], file_names[i]),
      error = function(x) {
        stop(paste(
          "Download failed: variable likely not avaialable for selected dataset.\n",
          "Browse available outputs for selected sim by visitng:\n",
          paste0(url, "/catalog/files/", sim, "/Level2.html")
        ))
      }
    )
  }

  # Read in rasters, join together
  suppressWarnings(suppressMessages({
    roms <- read_ncdf(file_names[1], var = var_base)
    roms_dates <- st_get_dimension_values(roms, "ocean_time")
    if (length(file_names) > 1) {
      for (i in 2:length(file_names)) {
        roms_i <- read_ncdf(file_names[i], var = var_base)
        roms <- c(roms, roms_i, along = "ocean_time")
        roms_dates <- c(roms_dates, st_get_dimension_values(roms_i, "ocean_time"))
      }
    }
    st_crs(roms) <- "+proj=longlat +datum=WGS84 +no_defs"
  }))

  # Fix issue with `stars` where if first raster is single band, all bands receive same date
  roms <- st_set_dimensions(roms, "ocean_time", values = roms_dates)

  # Subset time, if applicable
  if(!is.na(start) | !is.na(end)) {
    if (!is.na(start)) {
      start_date <- as.POSIXct(paste0(start, "-01-01 00:00:00"), tz = "UTC")
      dates_in <- which(st_get_dimension_values(roms, "ocean_time") >= start_date)
      roms <- roms |> slice(dates_in, along = "ocean_time")
    }
    if (!is.na(end)) {
      end_date <- as.POSIXct(paste0(end, "-12-31 23:59:00"), tz = "UTC")
      dates_in <- which(st_get_dimension_values(roms, "ocean_time") <= end_date)
      roms <- roms |> slice(dates_in, along = "ocean_time")
    }
  }

  # Crop to eastern Bering Sea survey region, if applicable
  if(crop_ebs) {
    ebs <- get_ebs_shapefile() |> st_transform("+proj=longlat +datum=WGS84") |> st_shift_longitude()
    roms <- roms |> st_crop(ebs)
  }

  # Write out stars object, if applicable
  if(!is.na(write_dir)) {
    write_stars(roms, file.path(write_dir, paste0(sim, "_", var, ".tif")), driver = "GTiff")
  }

  on.exit({
    unlink(tmp, recursive = TRUE, force = TRUE)
    dir.create(tmp)
  }, add = TRUE)

  return(roms)

}
