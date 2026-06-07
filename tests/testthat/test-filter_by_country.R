test_that("filter by country works", {
  madagascar <- filter_by_country("madagascar")

  expect_s3_class(madagascar, "tbl")
})
