
#' Get data on AFSC survey hauls.
#'
#' @param survey Survey to return data for. Codes differ from the official GAP documentation:
#'  \itemize{
#'    \item "All" = all surveys
#'    \item "EBS" = Full Bering Sea Shelf (Southeastern + Northern Bering Sea) survey
#'    \item "SEBS" = Southeastern Bering Sea survey
#'    \item "NBS" = Northern Bering Sea survey
#'    \item "EBS Slope" = Southeastern Bering Sea Slope survey
#'    \item "GOA" = Gulf of Alaska survey
#'    \item "AI" = Aleutian Islands survey
#'  }
#' @param years Years to filter data to. If missing, returns all years.
#'
#' @returns A data.frame
#' @source <https://afsc-gap-products.github.io/gap_products/content/foss-api-r.html>
#' @export
#'
get_hauldata <- function(survey = c("All", "EBS", "SEBS", "NBS", "EBS Slope", "AI", "GOA"), years) {

  survey <- match.arg(survey)

  query <- survey |> switch(
    "All" = "?offset=",
    "EBS" = c(
      "?q={\"srvy\":\"EBS\"}&offset=",
      "?q={\"srvy\":\"NBS\"}&offset="
    ),
    "SEBS" = "?q={\"srvy\":\"EBS\"}&offset=",
    "NBS" = "?q={\"srvy\":\"NBS\"}&offset=",
    "EBS Slope" = "?q={\"srvy\":\"BSS\"}&offset=",
    "AI" = "?q={\"srvy\":\"AI\"}&offset=",
    "GOA" = "?q={\"srvy\":\"GOA\"}&offset="
  )

  survey_query <- paste0("srvy=", survey, collapse = "&")

  api_link_haul <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_haul/'

  haul_data <- data.frame()

  for (q in seq_along(query)) {

    for (i in seq(0, 500000, 10000)){

      ## query the API link
      res <- httr::GET(url = paste0(api_link_haul, query[q], i, "&limit=10000"))

      ## convert from JSON format
      haul_data_i <- jsonlite::fromJSON(base::rawToChar(res$content))$items

      ## if there are no data, stop the loop
      if (is.null(nrow(haul_data_i))) break

      ## bind sub-pull to data
      haul_data <- haul_data |> dplyr::bind_rows(dplyr::select(haul_data_i, -links))
    }

  }

  if (!missing(years)) {
    haul_data <- haul_data |> dplyr::filter(year %in% years)
  }

  haul_data

}

#' Get AKFIN species data, or return species code corresponding to a species scientific name(s).
#'
#' @param sciname Vector of species scientific names for which to return codes. If missing, returns
#' all species data. Can be a vector.
#'
#' @returns A named vector if sciname is non-missing, otherwise a data.frame.
#' @source <https://afsc-gap-products.github.io/gap_products/content/foss-api-r.html>
#' @export
#'
get_species_codes <- function(sciname) {

  api_link_species <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_species/'

  res <- httr::GET(url = paste0(api_link_species, "?offset=0&limit=10000"))

  ## convert from JSON format
  species_data <- jsonlite::fromJSON(base::rawToChar(res$content))
  species_data <- species_data$items |> dplyr::select(-links)

  if (missing(sciname)) {
    return(species_data)
  } else {
    setNames(species_data$species_code[match(sciname, species_data$scientific_name)], sciname)
  }

}

#' Get catch for a species and (optionally) add in implicit zeroes using haul data.
#'
#' @inheritParams get_hauldata
#' @param species_code AKFIN species code returned by `get_species_code` (length 1)
#' @param zero_expand Whether to add in implicit zeroes (TRUE) or return only hauls with positive catch (FALSE)
#' @param haul_data Haul data returned by `get_hauldata`. If `zero_expand` is TRUE and `haul_data` is missing,
#' data corresponding to the `survey` argument will be downloaded, however if calling `get_catch` multiple times
#' for different species, it is more efficient to download the haul data first and pass it to each `get_catch` call.
#' @param years Years to subset data to. If missing, returns all years.
#'
#' @returns A data.frame
#' @export
#'
get_catch <- function(species_code, zero_expand = FALSE, haul_data, survey = c("All", "EBS", "SEBS", "NBS", "EBS Slope", "AI", "GOA"), years) {

  survey <- match.arg(survey)

  survey_sub <- survey |> switch(
    "All" = c("EBS", "NBS", "BSS", "AI", "GOA"),
    "EBS" = c("EBS", "NBS"),
    "SEBS" = "EBS",
    "NBS" = "NBS",
    "EBS Slope" = "BSS",
    "AI" = "AI",
    "GOA" = "GOA"
  )

  if (isTRUE(zero_expand)) {
    if (missing(haul_data)) {
      haul_data <- get_hauldata(survey = survey)
    } else {
      haul_data <- haul_data |> dplyr::filter(srvy %in% survey_sub)
    }
    if (!missing(years)) {
      haul_data <- haul_data |> dplyr::filter(year %in% years)
    }
  }

  api_link_catch <- 'https://apps-st.fisheries.noaa.gov/ods/foss/afsc_groundfish_survey_catch/'

  catch_data <- data.frame()

  for (i in seq(0, 500000, 10000)) {

    url <- paste0(api_link_catch, "?q={\"species_code\":", species_code, "}&offset=", i, "&limit=10000")
    res <- httr::GET(url)

    catch_data_i <- jsonlite::fromJSON(base::rawToChar(res$content))$items

    if (is.null(nrow(catch_data_i))) break

    ## bind sub-pull to data
    catch_data <- catch_data |> dplyr::bind_rows(dplyr::select(catch_data_i, -links))

    if (nrow(catch_data_i) < 10000) break

  }

  if (!isTRUE(zero_expand)) {
    if (!missing(years)) {
      warning("`years` argument ignored if haul data is missing.")
    }
    return(catch_data)
  }

  catch_data <- haul_data |>
    dplyr::rename(station_id = station) |>
    dplyr::left_join(catch_data, by = "hauljoin") |>
    dplyr::mutate(across(
      c(cpue_kgkm2, cpue_nokm2, weight_kg, count),
      \(x) ifelse(is.na(x), 0, x)
    )) |>
    dplyr::arrange(year, station_id)

  return(catch_data)

}

