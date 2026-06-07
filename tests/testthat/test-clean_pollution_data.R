test_that("clean_pollution_data removes missing values", {
  dat <- tibble::tibble(
    country = c("Philippines", NA, "India"),
    total_plastic = c(1000, 800, NA),
    avg_aqi = c(90, 70, 50)
  )

  result <- clean_pollution_data(dat)

  expect_equal(nrow(result), 1)
  expect_equal(result$country, "Philippines")
})

test_that("clean_pollution_data returns a data frame", {
  dat <- tibble::tibble(
    country = c("Philippines", "India", "Mexico"),
    total_plastic = c(1000, 800, 600),
    avg_aqi = c(90, 70, 40)
  )

  result <- clean_pollution_data(dat)

  expect_s3_class(result, "data.frame")
})

test_that("clean_pollution_data errors on missing columns", {
  bad <- tibble::tibble(
    country = "Philippines",
    total_plastic = 1000
  )

  expect_error(
    clean_pollution_data(bad),
    "missing required column"
  )
})

test_that("clean_pollution_data errors on empty data", {
  empty <- tibble::tibble(
    country = character(),
    total_plastic = numeric(),
    avg_aqi = numeric()
  )

  expect_error(
    clean_pollution_data(empty),
    "no rows"
  )
})
