
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

  if (length(names(x)) == 1) {

    x_list <- vector("list", dim(x)[3])

    band_names <- stars::st_get_dimension_values(x, 3)

    for (i in 1:(dim(x)[3])) {

      x_list[[i]] <- do.call(dplyr::slice, list(x, i, along = dimnames(x)[3]))
      names(x_list[[i]]) <- band_names[i]

    }

    return(Reduce("c", x_list))

  } else {

    x_list <- vector("list", length(names(x)))

    for (i in seq_along(names(x))) {

      x_list[[i]] <- st_reband(x[names(x)[i]])
      names(x_list[[i]]) <- paste(names(x)[i], names(x_list[[i]]), sep = ".")

    }

    return(Reduce("c", x_list))

  }

}


#' Expand a `stars` object across a new dimension
#'
#' @param x A `stars` object
#' @param name Name of new dimension
#' @param values Values to use for new dimension
#'
#' @returns A `stars` object with an additional dimension
#' @export
st_replicate <- function(x, name, values = 1:2) {

  if (!inherits(x, "stars")) {
    stop("`x` must be a `stars` object")
  }

  if (length(values) <= 1) {
    stop("`values` must be length 2 or more")
  }

  if (missing(name)) name <- "z"

  along_arg <- stats::setNames(list(values), name)

  do.call(c, c(rep(list(x), length(values)), list(along = along_arg)))

}
