#' Plot measles detection method proportions by year
#'
#' Creates a stacked bar chart showing the proportion of measles cases
#' identified by clinical, epidemiologically-linked, and lab-confirmed
#' detection methods for each year.
#'
#' @param start_year integer year for data to start from
#' @param end_year integer year for data to end
#'
#' @return a ggplot object
#'
#' @examples
#' plot_detection_methods(load_data())
#'
#' @importFrom dplyr filter group_by summarise mutate select
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot aes geom_bar scale_x_continuous scale_fill_manual theme_minimal theme element_text labs
#'
#' @export
plot_detection_methods <- function(start_year = 2012, end_year = 2025) {
  data <- load_year()

  if ((!start_year %in% data$year) || (!end_year %in% data$year)) {
    stop("Enter a valid year range. Years must be between 2012 and 2025")
  }

  if (start_year > end_year) {
    stop("Enter a valid year range. `start_year` must be before `end_year`")
  }

  data <- load_year() |>
    dplyr::filter(year >= start_year & year <= end_year)

  cases_filtered <- data |>
    dplyr::filter(
      !is.na(measles_clinical),
      !is.na(measles_total),
      !is.na(measles_epi_linked),
      !is.na(measles_lab_confirmed)
    )

  if (nrow(cases_filtered) == 0) {
    stop("No complete detection method data found.")
  }

  yearly_totals <- cases_filtered |>
    dplyr::group_by(year) |>
    dplyr::summarise(
      total    = sum(measles_total),
      clinical = sum(measles_clinical),
      epi      = sum(measles_epi_linked),
      lab      = sum(measles_lab_confirmed),
      .groups  = "drop"
    )

  propsbyyear <- yearly_totals |>
    dplyr::mutate(
      clin_prop = clinical / total,
      epi_prop  = epi / total,
      lab_prop  = lab / total
    ) |>
    dplyr::select(year, clin_prop, epi_prop, lab_prop)

  plot_long <- propsbyyear |>
    tidyr::pivot_longer(
      cols      = c(clin_prop, epi_prop, lab_prop),
      names_to  = "method",
      values_to = "proportion"
    )

  ggplot2::ggplot(plot_long, ggplot2::aes(x = year, y = proportion, fill = method)) +
    ggplot2::geom_bar(position = "fill", stat = "identity") +
    ggplot2::scale_x_continuous(breaks = unique(plot_long$year)) +
    ggplot2::scale_fill_manual(
      values = c(clin_prop = "#2B6A4D", epi_prop = "#7DAF8E", lab_prop = "#C8DFC8"),
      labels = c(clin_prop = "Clinical", epi_prop = "Epi-Linked", lab_prop = "Lab Confirmed")
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(size = 8)) +
    ggplot2::labs(
      title    = "Measles Detection Method by Year",
      subtitle = "Clinical vs. Epi-linked vs. Lab Cases",
      x        = "Year",
      y        = "Proportion",
      fill     = "Detection Method"
    )
}
