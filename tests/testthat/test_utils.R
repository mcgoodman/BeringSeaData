
test_that(
  "stars utilities work", {

    phi <- get_sediment()

    phi2 <- st_replicate(phi, name = "year", values = 1:2)

    expect_equal(dim(phi2), c(dim(phi), year = 2))

    expect_error(
      st_replicate(phi2, name = "year", values = 1:2),
      "`x` must be a 2-dimensional `stars` object"
    )

    expect_error(
      st_replicate(phi, name = "year", values = 1),
      "`values` must be length 2 or more"
    )

    phi3 <- st_reband(phi2)

    expect_equal(dim(phi), dim(phi3))
    expect_equal(length(names(phi3)), 2)

    expect_error(
      st_reband(phi),
      "`x` must be a 3-dimensional `stars` object"
    )

    phi4 <- st_reband(c(phi2, setNames(phi2, "depth")))

    expect_equal(
      c("phi.1", "phi.2", "depth.1", "depth.2"),
      names(phi4)
    )

    expect_equal(dim(phi), dim(phi4))

  }
)
