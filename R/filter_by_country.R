#' Filter measles dataset by country
#'
#' @param string country to be filtered by
#' @param string dataset to be filtered (year or month)
#' @return tibble
#'
#' @examples
#' filter_by_country(data, "United States")
#'
#' @export

filter_by_country <- function(country = "Madagascar", by = "cases_year") {
  if (!by %in% c("cases_year", "cases_month")) {
    stop("'by' data not found. Enter either 'cases_year' or 'cases_month'")
  }
  data <- load_data(by)

  if (!("country" %in% names(data))) {
    stop("Data must contain a 'country' column.")
  }

  filtered <- data[data$country == country, ]

  if (nrow(filtered) == 0) {
    stop("No data found for country = '", country, "'.")
  }

  return(filtered)
}
