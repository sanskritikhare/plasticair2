capitals <- function(country_data) {
  country_data |>
    dplyr::select(c(1:4)) |>
    dplyr::rename(
      country = CountryName,
      capital_city = CapitalName,
      capital_lat = CapitalLatitude,
      capital_long = CapitalLongitude
    )
}
air_vs_plastic <- function(plastics, capitals, air_data) {
  pollution_data <- dplyr::left_join(plastics, capitals, by = "country")
  pollution_data <- dplyr::left_join(pollution_data, air_data, by = "country")

  pollution_data |>
    dplyr::filter(!parent_company %in% c("Grand Total", "Unbranded", "null", "NULL")) |>
    dplyr::group_by(country) |>
    dplyr::summarize(
      total_plastic = sum(grand_total, na.rm = TRUE),
      total_volunteers = sum(volunteers, na.rm = TRUE),
      avg_aqi = mean(overall_aqi, na.rm = TRUE),
      capital_city = dplyr::first(capital_city),
      capital_lat = dplyr::first(capital_lat),
      capital_long = dplyr::first(capital_long),
      .groups = "drop"
    ) |>
    dplyr::filter(!is.na(avg_aqi), total_plastic > 0) |>
    dplyr::arrange(dplyr::desc(total_plastic))
}

air_vs_plastic_clean <- function(air_vs_plastic) {
  Q1 <- stats::quantile(air_vs_plastic$total_plastic, 0.25)
  Q3 <- stats::quantile(air_vs_plastic$total_plastic, 0.75)
  IQR <- Q3 - Q1

  air_vs_plastic |>
    dplyr::filter(
      total_plastic >= Q1 - 1.5 * IQR,
      total_plastic <= Q3 + 1.5 * IQR
    )
}

combined <- function(air_vs_plastic, fertility) {
  fertility_clean <- fertility |>
    dplyr::rename(country = `Country Name`)

  dplyr::left_join(
    air_vs_plastic,
    fertility_clean,
    by = "country"
  )
}
