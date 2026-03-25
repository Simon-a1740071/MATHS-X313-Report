pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble()
