
# Functions for reading in package-associated GIS files -------------------------------------------

#' @title EBS survey shapefiles
#' @param region Either "EBS" for full survey region including NBS, or "SEBS"
#' @param type Whether to return multipolygon corresponding to survey grid ("grid") or region boundary only ("boundary")
#' @source \href{https://github.com/afsc-gap-products/akgfmaps}{QTL Archive}
#'
#' @return An "sf" object
#' @export
get_ebs_shapefile <- function(region = c("EBS", "SEBS"), type = c("boundary", "grid")) {

  require("sf")

  region <- match.arg(region)
  type <- match.arg(type)

  option <- paste(region, type, sep = "_")

  option |> switch(
    EBS_grid = sf::st_read(system.file(package = "Bering10KThredds", "GIS", "EBS grid"), quiet = TRUE),
    EBS_boundary = sf::st_read(system.file(package = "Bering10KThredds", "GIS", "EBS boundary"), quiet = TRUE),
    SEBS_grid = sf::st_read(system.file(package = "Bering10KThredds", "GIS", "SEBS grid"), quiet = TRUE),
    SEBS_boundary = sf::st_read(system.file(package = "Bering10KThredds", "GIS", "SEBS boundary"), quiet = TRUE)
  )

}


#' @title Alakska coastline shapefile
#' @return An "sf" object
#' @export
get_ak_coast <- function() {

  require("sf")

  sf::st_read(system.file(package = "Bering10KThredds", "GIS", "Alaska Shoreline", "ak_russia.shp"))

}


#' @title NOAA 15-arcsecond EBS digitial bedrock elevation model
#' @return A "stars" object
#' @export
get_bathymetry <- function() {

  require("stars")

  stars::read_stars(system.file(package = "Bering10KThredds", "GIS", "etopo_bedrock_15arcsecond.tif"))

}

#' @title NOAA 15-arcsecond EBS digitial bedrock elevation model
#' @return A "stars" object
#' @export
get_sediment <- function() {

  require("stars")

  phi <- stars::read_stars(system.file(package = "Bering10KThredds", "GIS", "phi.grd"))

  phi |> st_warp(crs = "+proj=longlat +datum=WGS84 +no_defs")

}
