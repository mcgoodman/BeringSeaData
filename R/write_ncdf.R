
#' @title Save ROMS or MOM6 output as netcdf
#' @param roms Object to save (non-cropped)
#' @param path File path
#' @param type "resampled" for object resampled to a regular UTM zone 2 grid, or "native" for original ROMS grid (currently only implemented for ROMS)
#' @param cellsize cell size (in km) used for resampling
#'
#' @export
write_ncdf <- function(roms, path, type = c("resampled", "native"), cellsize = 5e+03) {

  vars <- names(roms)

  type <- match.arg(type)

  if (type == "native") {

    B10K_grid <- ncdf4::nc_open(system.file(package = "BeringSeaData", "GIS", "Bering10K_extended_grid.nc"))

    roms_xy <- dplyr::mutate(setNames(roms[,,,1,drop = TRUE], "var"), var = 1)
    coords <- suppressWarnings(sf::st_coordinates(sf::st_centroid(sf::st_as_sf(roms_xy))))

    # Time dimension
    origin <- "1970-01-01"
    dates <- as.double(as.Date(stars::st_get_dimension_values(roms, 3)) - as.Date(origin))
    time_units <- paste("days since", origin, "00:00:00.0 -0:00")

    # Define RHO coordinates
    xi_rho <- ncdf4::ncdim_def("xi_rho", units = "", B10K_grid$dim$xi_rho$vals)
    eta_rho <- ncdf4::ncdim_def("eta_rho", units = "", B10K_grid$dim$eta_rho$vals)
    time <- ncdf4::ncdim_def("ocean_time", units = time_units, dates)

    # Define cell-wise coordinates
    lon_array <- rotate_lon(matrix(coords[,"X"], nrow = nrow(roms), ncol = ncol(roms)))
    lon <- ncdf4::ncvar_def("lon_rho", units = "degree_east", list(xi_rho, eta_rho), longname = "longitude of RHO-points")
    lat_array <- matrix(coords[,"Y"], nrow = nrow(roms), ncol = ncol(roms))
    lat <- ncdf4::ncvar_def("lat_rho", units = "degree_north", list(xi_rho, eta_rho), longname = "latitude of RHO-points")

    # Variable definitions
    var_defs <- setNames(vector("list", length(vars)), vars)
    for (i in seq_along(vars)) {
      var_defs[[vars[i]]] <- ncdf4::ncvar_def(vars[i], units = "", list(xi_rho, eta_rho, time), missval = NA)
    }

    # Create netcdf file
    var_defs <- append(var_defs, list(lon_rho = lon, lat_rho = lat))
    ncout <- ncdf4::nc_create(path, var_defs, force_v4 = FALSE)

    # Write variables to netcdf file
    for (i in seq_along(vars)) ncdf4::ncvar_put(ncout, var_defs[[vars[i]]], roms[[vars[i]]])
    ncdf4::ncvar_put(ncout, lon, lon_array)
    ncdf4::ncvar_put(ncout, lat, lat_array)

    # Write dimension and variable attributes to netcdf file
    ncdf4::ncatt_put(ncout, "xi_rho", "axis", "xi_rho")
    ncdf4::ncatt_put(ncout, "xi_rho", "standard_name", "projection_x_coordinate")
    ncdf4::ncatt_put(ncout, "xi_rho", "_CoordinateAxisType", "GeoX")
    ncdf4::ncatt_put(ncout, "eta_rho", "axis", "eta_rho")
    ncdf4::ncatt_put(ncout, "eta_rho", "standard_name", "projection_y_coordinate")
    ncdf4::ncatt_put(ncout, "eta_rho", "_CoordinateAxisType", "GeoY")

    # Write global attributes to netcdf file
    ncdf4::ncatt_put(ncout, 0, "title", basename(tools::file_path_sans_ext(path)))
    ncdf4::ncatt_put(ncout, 0, "Conventions", "CF-1.5")
    ncdf4::ncatt_put(ncout, 0, "GDAL", "GDAL 3.8.4, released 2024/02/08")

    # Close connection to netcdf file
    ncdf4::nc_close(ncout)


  } else if (type == "resampled") {

    # Resample on regular UTM-2 grid
    roms <- roms |> sf::st_transform(crs = "+proj=utm +zone=2 +datum=WGS84 +units=m +no_defs")
    roms <- stars::st_warp(roms, crs = sf::st_crs(roms), cellsize = cellsize)

    # Time dimension
    origin <- "1970-01-01"
    dates <- as.double(as.Date(stars::st_get_dimension_values(roms, 3)) - as.Date(origin))
    time_units <- paste("days since", origin, "00:00:00.0 -0:00")

    # Define dimensions
    x <- ncdf4::ncdim_def("x", units = "m", longname = "UTM zone 2 eastings", stars::st_get_dimension_values(roms, "x"))
    y <- ncdf4::ncdim_def("y", units = "m", longname = "UTM zone 2 eastings", stars::st_get_dimension_values(roms, "y"))
    time <- ncdf4::ncdim_def("time", units = time_units, dates)

    # Variable definitions
    var_defs <- setNames(vector("list", length(vars)), vars)
    for (i in seq_along(vars)) {
      var_defs[[vars[i]]] <- ncdf4::ncvar_def(vars[i], units = "", list(x, y, time), missval = NA)
    }

    # Define projection
    projname <- "transverse_mercator"
    var_defs$proj <- ncdf4::ncvar_def(projname, "1", NULL, NULL, longname = projname, prec = "char")

    # Create netcdf file
    ncout <- ncdf4::nc_create(path, var_defs, force_v4 = TRUE)

    # Write variables to netcdf file
    for (i in seq_along(vars)) ncdf4::ncvar_put(ncout, var_defs[[vars[i]]], roms[[vars[i]]])

    # Write dimension and variable attributes to netcdf file
    ncdf4::ncatt_put(ncout, "x", "axis","X")
    ncdf4::ncatt_put(ncout, "x", "standard_name", "projection_x_coordinate")
    ncdf4::ncatt_put(ncout, "x", "_CoordinateAxisType", "GeoX")
    ncdf4::ncatt_put(ncout, "y", "axis", "Y")
    ncdf4::ncatt_put(ncout, "y", "standard_name", "projection_y_coordinate")
    ncdf4::ncatt_put(ncout, "y", "_CoordinateAxisType", "GeoY")
    for (i in seq_along(vars)) ncdf4::ncatt_put(ncout, vars[i], "grid_mapping", projname)

    # Write CRS attributes to netcdf file
    ncdf4::ncatt_put(ncout, projname, "name", projname)
    ncdf4::ncatt_put(ncout, projname, "grid_mapping_name", projname)
    ncdf4::ncatt_put(ncout, projname, "longitude_of_central_meridian", -171)
    ncdf4::ncatt_put(ncout, projname, "false_easting", 500)
    ncdf4::ncatt_put(ncout, projname, "false_northing", 0)
    ncdf4::ncatt_put(ncout, projname, "latitude_of_projection_origin", 0)
    ncdf4::ncatt_put(ncout, projname, "scale_factor_at_central_meridian", 0.9996)
    ncdf4::ncatt_put(ncout, projname, "long_name", "CRS definition")
    ncdf4::ncatt_put(ncout, projname, "longitude_of_prime_meridian", 0)
    ncdf4::ncatt_put(ncout, projname, "semi_major_axis", 6378137)
    ncdf4::ncatt_put(ncout, projname, "inverse_flattening", 298.257223563)
    ncdf4::ncatt_put(ncout, projname, "spatial_ref", "PROJCS['unknown',GEOGCS['unknown',DATUM['WGS_1984',SPHEROID['WGS 84',6378137,298.257223563],AUTHORITY['EPSG','6326']],PRIMEM['Greenwich',0,AUTHORITY['EPSG','8901']],UNIT['degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['latitude_of_origin',0],PARAMETER['central_meridian',-171],PARAMETER['scale_factor',0.9996],PARAMETER['false_easting',500],PARAMETER['false_northing',0],UNIT['metre',1,AUTHORITY['EPSG','9036']],AXIS['Easting',EAST],AXIS['Northing',NORTH]]")
    ncdf4::ncatt_put(ncout, projname, "crs_wkt", "PROJCS['unknown',GEOGCS['unknown',DATUM['WGS_1984',SPHEROID['WGS 84',6378137,298.257223563],AUTHORITY['EPSG','6326']],PRIMEM['Greenwich',0,AUTHORITY['EPSG','8901']],UNIT['degree',0.0174532925199433]],PROJECTION['Transverse_Mercator'],PARAMETER['latitude_of_origin',0],PARAMETER['central_meridian',-171],PARAMETER['scale_factor',0.9996],PARAMETER['false_easting',500],PARAMETER['false_northing',0],UNIT['metre',1,AUTHORITY['EPSG','9036']],AXIS['Easting',EAST],AXIS['Northing',NORTH]]")
    ncdf4::ncatt_put(ncout, projname, "GeoTransform", "-192.6105578478245 5 0 7461.274939415367 0 -5")
    ncdf4::ncatt_put(ncout, projname, "_CoordinateAxisTypes","GeoX GeoY")

    # Write global attributes to netcdf file
    ncdf4::ncatt_put(ncout, 0, "title", basename(tools::file_path_sans_ext(path)))
    ncdf4::ncatt_put(ncout, 0, "Conventions", "CF-1.5")
    ncdf4::ncatt_put(ncout, 0, "GDAL", "GDAL 3.8.4, released 2024/02/08")

    # Close connection to netcdf file
    ncdf4::nc_close(ncout)

  }

}
