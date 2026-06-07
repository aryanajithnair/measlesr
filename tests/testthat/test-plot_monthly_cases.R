test_that("plot_monthly_cases works", {
  monthly_cases_plot <- plot_monthly_cases(year = 2018,
                                           country = "Madagascar")
  expect_s3_class(monthly_cases_plot, "ggplot")
})
