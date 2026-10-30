test_that(
  "NEP Regrid monthly querying works", {

    # Basic Querying works
    mom6 <- get_mom6(
      "sob",
      grid_type = "regrid",
      region = "NEP",
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2020-02-28")
    )

    expect_equal(dim(mom6), c(lon = 73, lat = 128, time = 2))
    expect_equal(sf::st_crs(mom6)$epsg, 4326)

    # Alternative argument classes work
    mom6 <- get_mom6(
      "sob",
      grid_type = "regrid",
      region = "NEP",
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2020-02-28"),
      extent = sf::st_bbox(get_ebs_shapefile("EBS"))
    )

    expect_equal(dim(mom6), c(lon = 89, lat = 130, time = 2))

  }
)

test_that("NEP Regrid daily frequency querying works", {
  
  mom6_daily <- get_mom6(
    "tob",
    grid_type = "regrid",
    region = "NEP",
    freq = "daily",
    category = "ocean_daily",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-01-03")
  )

  expect_equal(dim(mom6_daily), c(lon = 73, lat = 128, time = 3))

})

test_that("NEP Raw querying works", {

  ebs_bbox <- sf::st_bbox(get_ebs_shapefile("EBS"))
  
  mom6_raw <- get_mom6(
    "tob",
    grid_type = "raw",
    region = "NEP",
    extent = ebs_bbox,
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-02-28")
  )
  
  expect_true("time" %in% names(dim(mom6_raw)))
  expect_equal(dim(mom6_raw), c(ih = 182, jh = 186, time = 2))

})

test_that("NWA Regrid monthly querying works", {

  nwa_bbox <- sf::st_bbox(c(xmin = -75, xmax = -70, ymin = 35, ymax = 40), crs = sf::st_crs(4326))

  mom6_nwa <- get_mom6(
    "tob",
    grid_type = "regrid",
    region = "NWA",
    extent = nwa_bbox,
    start_date = as.Date("2015-01-01"),
    end_date = as.Date("2015-02-28")
  )

  expect_equal(dim(mom6_nwa), c(lon = 62, lat = 80, time = 2))

})

test_that("NWA Raw monthly querying works", {

  nwa_bbox <- sf::st_bbox(c(xmin = -75, xmax = -70, ymin = 35, ymax = 40), crs = sf::st_crs(4326))

  mom6_nwa_raw <- get_mom6(
    "tob",
    grid_type = "raw",
    region = "NWA",
    extent = nwa_bbox,
    start_date = as.Date("2015-01-01"),
    end_date = as.Date("2015-02-28")
  )

  expect_equal(dim(mom6_nwa_raw), c(xh = 63, yh = 79, time = 2))

})

test_that(
  "Error checking works", {

    expect_error(
      get_mom6("temp_bottom5m", grid_type = "regrid", region = "NEP"),
      "specified variable not available"
    )

    expect_error(
      get_mom6(start_date = as.Date("2040-01-01"), grid_type = "regrid", region = "NEP"),
      "Specified date range is invalid"
    )

    expect_error(
      get_mom6(region = "NWA", experiment = "forecast"),
      "not currently supported"
    )

  }
)

test_that("Empty OPeNDAP bands are caught", {

  # Valid data: land cells (first row) are NA in every time step
  good <- array(as.numeric(1:36), dim = c(x = 3, y = 4, time = 3))
  good[1, , ] <- NA

  # Valid all-zero ocean (e.g., ice-free summer) keeps the land mask
  ice_free <- good
  ice_free[2:3, , 2] <- 0

  # Failed transfer: trailing time steps fully zero-filled, including land
  failed <- good
  failed[, , 3] <- 0

  # Truncated transfer: zero-filling starts partway through a time step
  truncated <- good
  truncated[, 3:4, 3] <- 0

  # Fully failed request: no NA cells anywhere
  all_failed <- array(0, dim = c(x = 3, y = 4, time = 3))

  # Additional non-spatial dimension (e.g., ice thickness category)
  with_ct <- array(rep(good, 2), dim = c(x = 3, y = 4, time = 3, ct = 2)) |> aperm(c(1, 2, 4, 3))
  with_ct_failed <- with_ct
  with_ct_failed[, , 2, 3] <- 0

  expect_silent(check_opendap_bands(stars::st_as_stars(good)))
  expect_silent(check_opendap_bands(stars::st_as_stars(ice_free)))
  expect_silent(check_opendap_bands(stars::st_as_stars(good[, , 1])))
  expect_silent(check_opendap_bands(stars::st_as_stars(with_ct)))
  expect_error(check_opendap_bands(stars::st_as_stars(failed)), "1 of 3 time steps")
  expect_error(check_opendap_bands(stars::st_as_stars(truncated)), "1 of 3 time steps")
  expect_error(check_opendap_bands(stars::st_as_stars(all_failed)), "3 of 3 time steps")
  expect_error(check_opendap_bands(stars::st_as_stars(all_failed[, , 1])), "opendap errors")
  expect_error(check_opendap_bands(stars::st_as_stars(with_ct_failed)), "1 of 3 time steps")

})

test_that("Chunked retrieval works", {

  mom6_chunked <- get_mom6(
    "sob",
    grid_type = "regrid",
    region = "NEP",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-03-31"),
    chunk = "1 month"
  )

  expect_equal(dim(mom6_chunked), c(lon = 73, lat = 128, time = 3))

})

test_that("NA extent works", {

  mom6_full <- get_mom6(
    "sob",
    grid_type = "regrid",
    region = "NEP",
    start_date = as.Date("2020-01-01"),
    end_date = as.Date("2020-01-31"),
    extent = NA
  )

  expect_gt(dim(mom6_full)["lon"], 200)
  
})
