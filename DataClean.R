pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df

head(temp_df)

temp_df |>
  pivot_longer(
    cols = contains("X["),
    names_to = "year",
    