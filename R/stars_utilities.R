
#' Change multi-band, single attribute `stars` object to single-band, multi-attribute
#'
#' @param x stars object with three dimensions
#'
#' @returns A stars object
#'
#' @export
st_reband <- function(x) {

  if (!(inherits(x, "stars") & length(dim(x)) == 3)) {
    stop("`x` must be a 3-dimensional `stars` object")
  }

  x_list <- vector("list", dim(x)[3])

  band_names <- stars::st_get_dimension_values(x, 3)

  for (i in 1:(dim(x)[3])) {

    x_list[[i]] <- do.call(dplyr::slice, list(x, i, along = dimnames(x)[3]))
    names(x_list[[i]]) <- band_names[i]

  }

  Reduce("c", x_list)

}


#' Expand a 2-dimensional `stars` object over time
#'
#' @param x 2-dimensional `stars` object
#' @param name Name of time dimension
#' @param values Values to use for time dimension
#'
#' @returns A 3-dimensional `stars` object
#' @export
st_replicate <- function(x, name, values = 1:2) {

  if (!(inherits(x, "stars") & length(dim(x)) == 2)) {
    stop("`x` must be a 2-dimensional `stars` object")
  }

  if (length(values) <= 1) {
    stop("`values` must be length 2 or more")
  }

  if (missing(name)) name <- "z"

  xi <- stats::setNames(vector("list", length(names(x))), names(x))

  for (i in seq_along(names(x))) {

    xi[[i]] <- stats::setNames(rep(list(x[i]), length(values)), values)
    xi[[i]] <- Reduce("c", xi[[i]])
    xi[[i]] <- stars::st_redimension(xi[[i]])
    xi[[i]] <- stars::st_set_dimensions(xi[[i]], which = 3, values = values, names = name)
    names(xi[[i]]) <- names(x)[i]

  }

  if (length(xi) > 1) {
    return(Reduce("c", xi))
  } else {
    return(xi[[1]])
  }

}

#' Convert tidync object to `stars`
#'
#' @param .x tidync object
#' @param ... ignored
#'
#' @return A `stars` object
#' @source <https://github.com/ropensci/tidync/issues/68>
#'
#' @importFrom stars st_as_stars
#' @method st_as_stars tidync
#' @aliases st_as_stars
#' @export
st_as_stars.tidync <- function(.x, ...) {

  ## ignore unit details for the moment
  data <- lapply(tidync::hyper_array(.x, drop = FALSE),
                 units::as_units)
  ## this needs to be a bit easier ...
  transforms <- tidync:::active_axis_transforms(.x)
  dims <- lapply(names(transforms), function(trname) {
    transform <- transforms[[trname]] |> dplyr::filter(selected)
    values <- transform[[trname]]
    if (length(values) > 1) {
      stars:::create_dimension(
        values = values)
    } else {
      ## a hack for now when there's only one value
      structure(list(from = values, to = values,
                     offset = values, delta = NA_real_,
                     geotransform = rep(NA_real_, 6),
                     refsys = NA_character_,
                     point = NA,
                     values = NULL),
                class = "dimension")
    }
  })
  names(dims) <- names(transforms)
  if (length(transforms)>= 2L) {
    r <- structure(list(affine = c(0, 0),
                        dimensions = names(dims)[1:2],
                        curvilinear = FALSE, class = "stars_raster"))

    attr(dims, "raster") <- r
  }
  geotransform_xy <- c(dims[[1]]$offset, dims[[1]]$delta, 0, dims[[2]]$offset, 0, dims[[2]]$delta)
  dims[[1]]$geotransform <- dims[[2]]$geotransform <- geotransform_xy
  structure(data, dimensions =   structure(dims, class = "dimensions"),
            class = "stars")

}
