test_that("cases by year summary works", {
  result <- yearly_case_summary(country = "Madagascar")

  # check object type
  expect_s3_class(result, "data.frame")

  # check expected column names
  expect_equal(names(result), c("year", "total_cases", "pct_change", "trend"))

  # check expected values
  expect_equal(result[1, ], tibble::tibble(
    year = 2012,
    total_cases = 3,
    pct_change = 0,
    trend = 'stable'
  ))

  expect_equal(result[5, ], tibble::tibble(
    year = 2016,
    total_cases = 17,
    pct_change = 183.3,
    trend = 'surging'))
})
