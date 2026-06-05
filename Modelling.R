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
  select(Year, TFR, TLB, Zodiac) |>
  filter(Year <= 2012)

test <-
  fert_df |>
  select(Year, TFR, TLB, Zodiac) |>
  filter(Year > 2012)  

#Fitting models

##TFR
TFR_fit <-
  train |> 
  model(
    arima410110 = ARIMA(log(TFR) ~ pdq(4,1,0) + PDQ(1,1,0, period = 12)),
    arima0113110 = ARIMA(log(TFR) ~ pdq(0, 1, 13) + PDQ(1,1,0, period = 12)),
    arima018110 = ARIMA(log(TFR) ~ pdq(0, 1, 8) + PDQ(1,1,0, period = 12)),
    arima1520100 = ARIMA(log(TFR) ~ pdq(15,2,0) + PDQ(1,0,0, period = 12)),
    arima1320100 = ARIMA(log(TFR) ~ pdq(13,2,0) + PDQ(1,0,0, period = 12)),
    arima0213100 = ARIMA(log(TFR) ~ pdq(0,2,13) + PDQ(1,0,0, period = 12)),
    arima120100 = ARIMA(log(TFR) ~ pdq(1,2,0) + PDQ(1,0,0, period = 12)),
    auto = ARIMA(log(TFR) ~ PDQ(period = 12), stepwise = FALSE, approx = FALSE)
  )

TFR_fit |>
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Order")

glance(TFR_fit) |>
  arrange(AICc) |>
  select(.model:BIC) 

TFR_fit |>
  select(arima410110) |>
  gg_tsresiduals(lag = 60) 

augment(TFR_fit) |>
  filter(.model == "arima410110") |>
  features(.innov, ljung_box, lag = 24, dof = 5)

##TLB
TLB_fit <-
  train |>
  model(
    arima410110 = ARIMA(log(TLB) ~ pdq(4,1,0) + PDQ(1,1,0, period = 12)),
    arima1310110 = ARIMA(log(TLB) ~ pdq(13,1,0) + PDQ(1,1,0, period = 12)),
    arima0110110 = ARIMA(log(TLB) ~ pdq(0,1,10) + PDQ(1,1,0, period = 12)),
    arima1310100 = ARIMA(log(TLB) ~ pdq(13,1,0) + PDQ(1,0,0, period = 12)),
    arima0113100 = ARIMA(log(TLB) ~ pdq(0,1,13) + PDQ(1,0,0, period = 12)),
    arima0111100 = ARIMA(log(TLB) ~ pdq(0,1,11) + PDQ(1,0,0, period = 12)),
    arima1310001 = ARIMA(log(TLB) ~ pdq(13,1,0) + PDQ(0,0,1, period = 12)),
    arima0113001 = ARIMA(log(TLB) ~ pdq(0,1,13) + PDQ(0,0,1, period = 12)),
    arima0111001 = ARIMA(log(TLB) ~ pdq(0,1,11) + PDQ(0,0,1, period = 12)),
    auto = ARIMA(log(TLB) ~ PDQ(period = 12), stepwise = FALSE, approx = FALSE)
  )

TLB_fit |>
  pivot_longer(everything(),
               names_to = "Model name",
               values_to = "Order")


glance(TLB_fit) |>
  arrange(AICc) |>
  select(.model:BIC) 

TLB_fit |>
  select(arima410110) |>
  gg_tsresiduals(lag = 60) 

augment(TLB_fit) |>
  filter(.model == "arima410110") |>
  features(.innov, ljung_box, lag = 24, dof = 5)
