cases_month <- load_data("cases_month")
cases_year  <- load_data("cases_year")

test_that("summarize_measles works correctly on monthly dataset", {
  result <- summarize_measles(cases_month)
  expect_true(is.data.frame(result))
  expect_true(all(c("region", "country", "iso3", "year", "months_reported",
                    "measles_total", "measles_confirmed", "rubella_total",
                    "discarded") %in% colnames(result)))
  algeria_2012 <- result |> filter(country == "Algeria", year == 2012)
  expect_equal(algeria_2012$measles_total, 55)
  expect_equal(algeria_2012$months_reported, 10)
})

test_that("summarize_measles works correctly on yearly dataset", {
  result <- summarize_measles(cases_year)
  expect_true(is.data.frame(result))
  expect_true(all(c("region", "country", "iso3", "year", "total_population",
                    "measles_total", "measles_confirmed", "measles_incidence",
                    "rubella_total", "rubella_incidence", "discarded_cases") %in% colnames(result)))
  algeria_2012 <- result |> filter(country == "Algeria", year == 2012)
  expect_equal(algeria_2012$measles_total, 55)
  expect_equal(round(algeria_2012$measles_incidence, 2), 1.46)
})