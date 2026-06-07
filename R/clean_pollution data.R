#' Clean Pollution Data
#'
#' Cleans the merged pollution dataset by removing rows with missing key values
#' and filtering out extreme outliers in total plastic waste using the IQR rule.
#'
#' @param dat A data frame containing `country`, `total_plastic`, and `avg_aqi`
#'   columns.
#'
#' @return A cleaned tibble with missing values and extreme plastic waste
#'   outliers removed.
#'
#' @importFrom dplyr filter
#' @export
#'
#' @examples
#' \dontrun{
#' dat <- load_data()
#' clean_pollution_data(dat)
#' }
clean_pollution_data <- function(dat) {

  validate_cols(
    dat,
    required = c("country", "total_plastic", "avg_aqi"),
    arg_name = "dat"
  )

  if (nrow(dat) == 0) {
    stop("`dat` has no rows.", call. = FALSE)
  }

  Q1 <- stats::quantile(dat$total_plastic, 0.25, na.rm = TRUE)
  Q3 <- stats::quantile(dat$total_plastic, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1

  dat |>
    dplyr::filter(
      !is.na(country),
      !is.na(total_plastic),
      !is.na(avg_aqi),
      total_plastic >= Q1 - 1.5 * IQR,
      total_plastic <= Q3 + 1.5 * IQR
    )
}
