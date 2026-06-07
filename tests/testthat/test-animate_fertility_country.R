test_that("animate_fertility_country works", {
  dat <- tibble::tibble(
    country = "Philippines",
    `2000` = 3.5,
    `2001` = 3.4,
    `2002` = 3.3
  )

  result <- animate_fertility_country(dat)

  expect_s3_class(result, "gif_image")
})

test_that("animate_fertility_country errors on non-data-frame input", {
  expect_error(
    animate_fertility_country("nope"),
    "must be a data frame"
  )
})

test_that("animate_fertility_country errors on missing columns", {
  bad <- tibble::tibble(
    `2000` = 3.5,
    `2001` = 3.4
  )

  expect_error(
    animate_fertility_country(bad),
    "country"
  )
})

test_that("animate_fertility_country errors when country is not Philippines", {
  dat <- tibble::tibble(
    country = "United States",
    `2000` = 2.1,
    `2001` = 2.0,
    `2002` = 1.9
  )

  expect_error(
    animate_fertility_country(dat)
  )
})

test_that("animate_fertility_country errors with fewer than 2 years", {
  dat <- tibble::tibble(
    country = "Philippines",
    `2000` = 3.5
  )

  expect_error(
    animate_fertility_country(dat)
  )
})
