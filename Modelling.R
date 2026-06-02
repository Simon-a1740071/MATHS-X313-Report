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

zodiac <- c(
  "Rat", "Ox", "Tiger", "Rabbit",
  "Dragon", "Snake", "Horse", "Goat",
  "Monkey", "Rooster", "Dog", "Pig"
)

fert_df |>
  mutate(Zodiac = zodiac[((Year - 1900) %% 12 + 1)]) |>
  relocate(Zodiac, .after = Year) -> fert_df

##Split into Train and Test
train <- 
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year <= 2012)

test <-
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year > 2012)  

#Fitting models

##TFR
fert_fit <-
  train |> 
  model(
    arima1510001 = ARIMA(TFR ~ 1 + pdq(15,1,0) + PDQ(0,0,1)),
    arima0113001 = ARIMA(TFR ~ 1 + pdq(0,1,13) + PDQ(0,0,1)),
    auto = ARIMA(TFR, stepwise = FALSE, approx = FALSE)
  )

fert_fit |>
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Order")

glance(fert_fit) |>
  arrange(AICc) |>
  select(.model:BIC) 

fert_fit |>
  select(arima0113001) |>
  gg_tsresiduals(lag = 36) 

augment(fert_fit) |>
  filter(.model == "arima1510001") |>
  features(.innov, ljung_box, lag = 24, dof = 16)

##TLB
fert_fit <-
  train |>
  model(
    auto = ARIMA(TLB, stepwise = FALSE, approx = FALSE)
  )
