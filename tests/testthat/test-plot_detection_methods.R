test_that("plot detection method works", {
  detect_methods <- plot_detection_methods(start_year = 2020, end_year = 2025)
  expect_s3_class(detect_methods, "ggplot")
})
