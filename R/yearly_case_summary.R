#' Breaks down yearly measles cases for a single country
#'
#' @param string a country
#'
#' @return tibble
#' @examples
#' yearly_case_summary(cases_year, coumtry = "United States")
#'
#' @export

yearly_case_summary <- function(country = "Madagascar") {
  cases_year <- load_year()

  filtered <- filter_by_country(country, "cases_year")

  summary <- filtered |>
    dplyr::group_by(year) |>
    dplyr::summarise(total_cases = sum(measles_total, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(year)

  summary <- summary |>
    dplyr::mutate(
      pct_change = dplyr::case_when(
        dplyr::row_number() == 1 ~ 0,
        TRUE ~ round((total_cases - dplyr::lag(total_cases)) /
                       dplyr::lag(total_cases) * 100, 1)
      ),
      trend = dplyr::case_when(
        pct_change > 0  ~ "surging",
        pct_change < 0  ~ "declining",
        TRUE            ~ "stable"
      )
    )

  return(summary)
}
