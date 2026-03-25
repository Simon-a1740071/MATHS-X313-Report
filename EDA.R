pacman::p_load(fpp3)

read.csv(
  here::here("AnnualFertilityData_Cleaned.csv"), check.names = FALSE
) |>
  tsibble(index = Year) -> df

head(df)

# Summary statistics
df |>
  summary()

#time plot

df |>
  autoplot(TFR)

df |>
  autoplot(TLB)

# seasonal plot not possible

# relationships between variables
df |>
  GGally::ggpairs(columns = 2:18)
