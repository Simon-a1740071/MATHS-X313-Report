pacman::p_load(fpp3)

read.csv(
  here::here("AnnualFertilityData_Cleaned.csv"), check.names = FALSE
) |>
  tsibble(index = Year) -> df

# Summary statistics
df |>
  summary()
