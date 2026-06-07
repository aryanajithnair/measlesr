#' Load measles dataset of user's choice (helper function)
#'
#' @return A tibble containing measles data
#' @importFrom stringr str_c
#' @export
load_data <- function(file) {
  if (!file %in% c("cases_month", "cases_year")) {
    stop("Data file not found. Enter either 'cases_month' or 'cases_year'")
  }

  path <- system.file("extdata", str_c(file, ".parquet"), package = "measlesr")

  if (path == "") {
    stop("Data file not found. Make sure the package is installed correctly")
  }

  arrow::read_parquet(path)
}

#' Load measles cases_year dataset
#'
#' @return A tibble containing measles data
#' @export
load_year <- function() {
  load_data("cases_year")
}

#' #' Load measles cases_month dataset
#'
#' @return A tibble containing measles data
#' @importFrom stringr str_c
#' @export
load_month <- function() {
  load_data("cases_year")
}


