#' Summarize a WHO measles dataset (monthly or yearly)
#'
#' Automatically detects whether the input is the monthly or yearly dataset
#' based on column structure, and returns a relevant summary tibble.
#' If a country is specified, uses filter_by_country() to subset the data.
#'
#' @param data A data frame loaded from cases_month.csv or cases_year.csv
#' @param country Optional character string to filter to a specific country
#'
#' @return A tibble summarizing measles and rubella cases
#' @examples
#' monthly <- read.csv("cases_month.csv")
#' yearly  <- read.csv("cases_year.csv")
#' summarize_measles(monthly)
#' summarize_measles(yearly, country = "Nigeria")

library(dplyr)

summarize_measles <- function(data, country = NULL) {
  
  # Filter by country using filter_by_country() if specified
  if (!is.null(country)) {
    by <- if ("month" %in% colnames(data)) "cases_month" else "cases_year"
    data <- filter_by_country(country = country, by = by)
  }
  
  # Detect dataset type based on columns
  is_monthly <- "month" %in% colnames(data)
  is_yearly  <- "measles_incidence_rate_per_1000000_total_population" %in% colnames(data)
  
  if (!is_monthly && !is_yearly) {
    stop("Unrecognized dataset. Expected columns from cases_month.csv or cases_year.csv.")
  }
  
  if (is_monthly) {
    # Monthly summary: aggregate by country and year
    summary <- data |>
      group_by(region, country, iso3, year) |>
      summarise(
        months_reported    = n(),
        measles_total      = sum(measles_total,      na.rm = TRUE),
        measles_confirmed  = sum(measles_lab_confirmed, na.rm = TRUE),
        rubella_total      = sum(rubella_total,      na.rm = TRUE),
        discarded          = sum(discarded,           na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(country, year)
    
    message("Monthly dataset detected: summarized by country and year.")
    
  } else {
    # Yearly summary: one row per country-year already; compute key stats
    summary <- data |>
      select(
        region, country, iso3, year,
        total_population,
        measles_total,
        measles_confirmed  = measles_lab_confirmed,
        measles_incidence  = measles_incidence_rate_per_1000000_total_population,
        rubella_total,
        rubella_incidence  = rubella_incidence_rate_per_1000000_total_population,
        discarded_cases
      ) |>
      arrange(country, year)
    
    # Rename rubella incidence col if it exists under its full name
    if ("rubella_incidence_rate_per_1000000_total_population" %in% colnames(data) &&
        !"rubella_incidence_per_1000000_total_population" %in% colnames(data)) {
      summary <- data |>
        select(
          region, country, iso3, year,
          total_population,
          measles_total,
          measles_confirmed  = measles_lab_confirmed,
          measles_incidence  = measles_incidence_rate_per_1000000_total_population,
          rubella_total,
          rubella_incidence  = rubella_incidence_rate_per_1000000_total_population,
          discarded_cases
        ) |>
        arrange(country, year)
    }
    
    message("Yearly dataset detected: key columns selected and renamed.")
  }
  
  return(summary)
}