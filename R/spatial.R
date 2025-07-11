
# Functions for reading in package-associated GIS files -------------------------------------------

#' @title EBS survey shapefiles
#' @param region Either "EBS" for full survey region including NBS, or "SEBS"
#' @param type Whether to return multipolygon corresponding to survey grid ("grid") or region boundary only ("boundary")
#' @source \href{https://github.com/afsc-gap-products/akgfmaps}{QTL Archive}
#'
#' @return An "sf" object
#' @export
get_ebs_shapefile <- function(region = c("EBS", "SEBS"), type = c("boundary", "grid")) {

  region <- match.arg(region)
  type <- match.arg(type)

  option <- paste(region, type, sep = "_")

  option |> switch(
    EBS_grid = sf::st_read(system.file(package = "BeringSeaData", "GIS", "EBS grid"), quiet = TRUE),
    EBS_boundary = sf::st_read(system.file(package = "BeringSeaData", "GIS", "EBS boundary"), quiet = TRUE),
    SEBS_grid = sf::st_read(system.file(package = "BeringSeaData", "GIS", "SEBS grid"), quiet = TRUE),
    SEBS_boundary = sf::st_read(system.file(package = "BeringSeaData", "GIS", "SEBS boundary"), quiet = TRUE)
  )

}


#' @title Alakska coastline shapefile
#' @return An "sf" object
#' @export
get_ak_coast <- function() {

  sf::st_read(system.file(package = "BeringSeaData", "GIS", "Alaska Shoreline", "ak_russia.shp"))

}


#' @title NOAA 15-arcsecond EBS digitial bedrock elevation model
#' @return A "stars" object
#' @export
get_bathymetry <- function() {

  stars::read_stars(system.file(package = "BeringSeaData", "GIS", "etopo_bedrock_15arcsecond.tif"))

}

#' @title NOAA 15-arcsecond EBS digitial bedrock elevation model
#' @return A "stars" object
#' @export
get_sediment <- function() {

  phi <- stars::read_stars(system.file(package = "BeringSeaData", "GIS", "phi.grd"))

  phi |> st_warp(crs = "+proj=longlat +datum=WGS84 +no_defs")

}

#' @title Simple function to convert longitude from 0/360 to -180/180 and vice-versa
#' @param x A numeric vector of longitudes
#' @param from Whether to convert from -180/180 or 0/360
#'
#' @return A numeric vector
#' @export
rotate_lon <- function(x, from = c("-180/180", "0/360")) {

  from <- match.arg(from)

  from |> switch(
    `-180/180` = (x + 360) %% 360,
    `0/360` = ((x + 180) %% 360) - 180
  )

}
