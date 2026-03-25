pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df

head(temp_df)

temp_df |>
  pivot_longer(
    cols = contains("X20"),
    names_to = "year",
    values_to = "DataSeries")
    