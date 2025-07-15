
test_that(
  "Querying MOM6 works", {

    # Basic Querying works

    mom6 <- get_mom6_nep(
      "sob",
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2020-02-28")
    )

    expect_equal(dim(mom6), c(x = 99, y = 98, time = 2))

    expect_equal(st_crs(mom6)$epsg, 32602)

    # Error checking works

    expect_error(
      get_mom6_nep("temp_bottom5m"),
      "specified variable not available"
    )

    expect_error(
      get_mom6_nep(start_date = as.Date("2040-01-01")),
      "Specified date range is invalid"
    )

    # Alternative argument classes work

    mom6 <- get_mom6_nep(
      "sob",
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2020-02-28"),
      target_crs = st_crs(mom6),
      extent = st_bbox(get_ebs_shapefile("EBS"))
    )

    expect_equal(dim(mom6), c(x = 135, y = 111, time = 2))

    mom6 <- get_mom6_nep(
      "sob",
      start_date = as.Date("2020-01-01"),
      end_date = as.Date("2020-02-28"),
      target_crs = NA
    )

    expect_equal(dim(mom6), c(lon = 73, lat = 128, time = 2))

  }
)
