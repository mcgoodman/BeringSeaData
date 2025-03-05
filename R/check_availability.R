
#' Check which, if any, years a variable is available for from a given ROMS simulation
#'
#' @param var variable name
#' @param type "hindcast", "projection", or "historical" run
#' @param scenario "SSP126" or "SSP585" for CMIP6 (`version = "K20P19"` or `version = "K20"`) outputs, "RCP45" or "RCP85" for CMIP5 (`version = "H16"`) outputs
#' @param version Either (1) "K20P19" for 30-layer ROMs model from Kearney et al. 2020, with carbonate dynamics from
#' Pilcher et al. 2019 (default), (2) "K20" for 30layer model without carbonate dynamics, or (3) "H16" for 10-layer
#' ROMS model from Hermann et al. 2016
#' @param earth_model "GFDL", "CESM", or "MIROC"
#' @param print_console Whether to print available time blocks to the console instead of returning output
#'
#' @return If `print_console = FALSE`, a list of available time blocks and links to datasets for each time block
#' @export
check_availability <- function(var,
                               type = c("hindcast", "projection", "historical"),
                               scenario = c("SSP126", "SSP585", "RCP45", "RCP85"),
                               version = c("K20P19", "K20", "H16"),
                               earth_model = c("GFDL", "CESM", "MIROC"),
                               print_console = TRUE) {


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

  datasets <- list_level2_datasets(option = "sims")

  if (!(sim %in% datasets)) stop("specified combination of dataset type, scenario, version, and earth model does not exist. Use `list_level2_datasets(type = 'sim') to show available datasets.`")

  var_yrs <- list_level2_datasets(option = "all", sims = sim, quiet = TRUE)

  if (!(var %in% var_yrs$var)) {
    stop(paste(
      "Variable does not exist for any time block in specified simulation.",
      "Browse available outputs for selected sim by visitng:\n",
      paste0("https://data.pmel.noaa.gov/aclim/thredds/catalog/files/", sim, "/Level2.html")
    ))
  }

  time_blocks <- unique(var_yrs$years[var_yrs$var == var])

  if (print_console) {

    cat(paste0(
      "variable ", var, " available for the following time blocks:\n",
      paste0(time_blocks, collapse = "\n")
    ))

  } else {

    return(list(
      sim = sim,
      var = var,
      time_blocks = time_blocks,
      data_links = var_yrs[var_yrs$var == var,]
    ))

  }

}
