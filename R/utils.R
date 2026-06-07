#' Validate Required Data Frame Columns
#'
#' Checks that a data frame contains all required columns.
#'
#' @param df A data frame to validate.
#' @param required A character vector of required column names.
#' @param arg_name Name of the argument being validated for error messages.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
validate_cols <- function(df, required, arg_name = "data") {
  if (!is.data.frame(df)) {
    stop("`", arg_name, "` must be a data frame, not ",
         class(df)[1], ".", call. = FALSE)
  }
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("`", arg_name, "` is missing required column(s): ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  invisible(TRUE)
}
