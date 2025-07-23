

test_that(
  "Querying ROMS works", {

    # Basic Querying works

    roms <- get_roms_b10k(
      "temp_bottom5m",
      type = "projection",
      scenario = "SSP126",
      start = 2040,
      end = 2042
    )

    expect_true(
      all(lubridate::year(st_get_dimension_values(roms, "ocean_time")) %in% 2040:2042)
    )

    expect_equal(sum(!is.na(roms$temp)), 839322)

    expect_equal(dim(roms), c(xi_rho = 182, eta_rho = 258, ocean_time = 157))

    expect_equal(st_crs(roms)$proj4string, "+proj=longlat +datum=WGS84 +no_defs")

    roms <- get_roms_b10k(
      "oxygen_bottom5m",
      type = "projection",
      scenario = "SSP126",
      start = 2040,
      end = 2042,
      crop_ebs = FALSE
    )

    expect_equal(sum(!is.na(roms$oxygen)), 4889294)

    # Error checking works

    expect_error(
      get_roms_b10k("totally a real variable"),
      "Variable does not exist for any time block in specified simulation"
    )

    expect_error(
      get_roms_b10k("temp_bottom5m", start = 2200),
      "Download failed"
    )

    # Listing datasets works

    expect_equal(
      capture.output(BeringSeaData::check_availability("pH_bottom5m", type = "hindcast"))[-1],
      capture.output(BeringSeaData::check_availability("temp_bottom5m", type = "hindcast"))[-1]
    )

    expect_true(
      all(
        c(
          "B10K-K20P19_CMIP6_cesm_historical", "B10K-K20P19_CMIP6_cesm_ssp126", "B10K-K20P19_CMIP6_cesm_ssp585",
          "B10K-K20P19_CMIP6_gfdl_historical", "B10K-K20P19_CMIP6_gfdl_ssp126", "B10K-K20P19_CMIP6_gfdl_ssp585",
          "B10K-K20P19_CMIP6_miroc_historical", "B10K-K20P19_CMIP6_miroc_ssp126", "B10K-K20P19_CMIP6_miroc_ssp585",
          "B10K-K20P19_CORECFS", "B10K-K20_CORECFS"
        ) %in%
          BeringSeaData::list_roms_datasets()
      )
    )


  }
)
