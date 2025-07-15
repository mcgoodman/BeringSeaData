
#' List simulations / datasets available on the thredds server
#'
#' @param option Whether to list simulations ("sims") or all available years / variables for a given
#' simulation ("all"). If using `option = "all"` with `sims = NA`, it may take a long time to query the
#' thredds server to list all combinations of available simulations, years, and variables.
#' @param sims Optional; if using `option = "all"`, will return a list of all years and variables
#' available for the provided sim(s)
#' @param quiet Set to FALSE to display progress if listing all available years / variables for a given simulation.
#'
#' @return Either a vector of simulation names for `option = "sims"` or a data frame for `option = "all"`
#' @export
list_level2_datasets <- function(option = c("sims", "all"), sims = NA, quiet = TRUE) {

  option <- match.arg(option)

  url <- "https://data.pmel.noaa.gov/aclim/thredds/catalog/"

  html <- rvest::read_html(paste0(url, "files.html")) |> rvest::html_elements("a") |> rvest::html_attr("href")
  dirs <- html[grepl("files/", html, fixed = TRUE)]
  dirs <- gsub("files/", "", gsub(".html", "", dirs, fixed = TRUE), fixed = TRUE)

  if (option == "all") {

    if (!is.na(sims)) dirs <- dirs[dirs %in% sims]

    dirlist <- setNames(vector("list", length(dirs)), dirs)

    for (i in seq_along(dirs)) {

      if (isFALSE(quiet)) cat(paste0("parsing ", dirs[i], " (", i, "/", length(dirs), ")"))

      dir_url <- paste0(url, "files/", dirs[i], "/Level2.html")

      html <- rvest::read_html(dir_url) |> rvest::html_nodes("a") |> rvest::html_attr("href")
      subdirs <- html[grepl(dirs[i], html, fixed = TRUE) & !grepl("file:", html, fixed = TRUE)]
      subdirs <- basename(dirname(subdirs))

      dirlist[[i]] <- setNames(vector("list", length(subdirs)), subdirs)

      for (j in seq_along(subdirs)) {

        subdir_url <- paste0(url, dirs[i], "/Level2/", subdirs[j], "/catalog.html")

        html <- rvest::read_html(subdir_url) |> rvest::html_nodes("a") |> rvest::html_attr("href")
        datasets <- html[grepl("dataset=", html, fixed = TRUE)]
        varnames <- gsub(paste0(dirs[i], "_", subdirs[j], "_"), "", basename(datasets))
        varnames <- gsub("average_", "", gsub(".nc", "", varnames[grepl(".nc", varnames, fixed = TRUE)], fixed = TRUE), fixed = TRUE)

        dirlist[[i]][[j]] <- data.frame(
          var = varnames,
          url = paste0(gsub(basename(subdir_url), "", subdir_url, fixed = TRUE), datasets)
        )

      }

    }

    varlist <- dplyr::bind_rows(lapply(dirlist, dplyr::bind_rows, .id = "years"), .id = "sim")

    return(varlist)

  } else {

    return(dirs)

  }

}

