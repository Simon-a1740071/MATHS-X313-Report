#Modelling

##Load Libraries
pacman::p_load(fpp3)

##Load data
read.csv(
  here::here(
    "AnnualFertilityData_Cleaned.csv"),
  check.names = FALSE
) |>
  tsibble(index = Year) -> fert_df

##Split into Train and Test
train <- 
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year <= 2012)

test <-
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year > 2012)  


