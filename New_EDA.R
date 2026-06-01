#New EDA

## Load Libraries

pacman::p_load(fpp3)

## Load Cleaned Dataset

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

## Time plot TFR

fert_df |>
  ggplot(aes(x = Year, y = TFR, label = Year)) +
  geom_line() +
  geom_point(data = filter(fert_df, Zodiac %in% c("Dragon", "Tiger")),
             aes(colour = Zodiac)) +
  geom_text(data = filter(fert_df, Zodiac %in% c("Dragon", "Tiger")),
            vjust = -1) 

fert_df |>
  ggplot(aes(x = Year, y = TFR, label = Year, )) +
  geom_line() +
  geom_point(colour = "gray") +
  geom_point(aes(colour = Zodiac),
             data = filter(fert_df, Zodiac %in% c("Dragon", "Tiger"))) +
  scale_x_continuous(breaks = filter(fert_df, 
                                     Zodiac %in% c("Dragon", "Tiger"))$Year) +
  ggsci::scale_color_lancet("lanonc") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        legend.position = 'none')
  

## Train and Test

train <- 
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year <= 2012)

test <-
  fert_df |>
  select(Year, TFR, TLB) |>
  filter(Year > 2012)  

### TFR
train |>
  autoplot(TFR) +
  theme_light()

train |>
  gg_tsdisplay(TFR,  plot_type = 'partial', lag_max = 36)

train |>
  model(stl = STL(TFR)) |>
  components() |>
  autoplot()

train |>
  autoplot(difference(log(TFR), 12) |>
    difference(1))

train |>
  features(difference(log(TFR), 12) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))

train |>
  features(difference(log(TFR), 12) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))

train |>
  gg_tsdisplay(log(TFR) |>
                 difference(12) |>
                 difference(1), plot_type = 'partial', lag_max = 36)
#Seasonal PACF: there is a weak exponential decay at lags 12, 24,36
#Seasonal ACF: Only significant at lag 12. 
#Evident seasonality so D = 1
#(1,1,0)_12

#PACF: significant spikes at lag 1, 4. None beyond lag 4. Also has a damped sine wave manner. 
#ACF: Dies out in a sine wave like manner. Significant spikes at 13, none beyond it
#differenced once, d = 1
#possible model ARIMA models: (0,1,4), or (13,1,0)


train |>
  autoplot(log(TFR) |> 
             difference(1))
train |>
  features(difference(log(TFR), 1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))
train |>
  gg_tsdisplay(log(TFR) |>
                 difference(1), plot_type = 'partial', lag_max = 36)
#looks stationary however unitroot test says otherwise. 
#ACF: lags 12, 24 indicate an annual cycle cannot really say this is seasonal.
#However now knowing the context we can consider a SARIMA model

train |>
  gg_tsdisplay(log(TFR) |>
                 difference(1) |>
                 difference(1), plot_type = 'partial', lag_max = 36)
# lag 1 is strongly negative, over differenced, there are still spikes are seasonal lags
#so maybe need a seasonal differenced


### TLB
fert_df |>
  autoplot(TLB)

fert_df |>
  ggplot(aes(x = Year, y = TLB, label = Year)) +
  geom_line() +
  geom_point(colour = "gray") +
  geom_point(aes(colour = Zodiac),
             data = filter(fert_df, Zodiac %in% c("Dragon", "Tiger"))) +
  scale_x_continuous(breaks = filter(fert_df, 
                                     Zodiac %in% c("Dragon", "Tiger"))$Year) +
  ggsci::scale_color_lancet("lanonc") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 50, hjust = 1),
        legend.position = 'none')

train |>
  model(stl = STL(TLB)) |>
  components() |>
  autoplot()


train |>
  autoplot(log(TLB) |>
             difference(1))

train |>
  features(log(TLB) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))

train |>
  gg_tsdisplay(log(TLB) |>
                 difference(1), plot_type = 'partial', lag_max = 36)

train |>
  gg_tsdisplay(log(TLB) |>
                 difference(12) |>
                 difference(1), plot_type = 'partial', lag_max = 36)

train |> 
  PACF(log(TLB) |>
      difference(1), lag_max = 36) |>
  autoplot()
#there's spike at seasonal lags 12 24 for the ACF, PACF only has a lag spike at 12
#The data is already stationary, may consider a SARIMA with a D = 0 model
#ACF: spikes at seasonal lag 12, 24. Only 12 is significant. Exponential decays
#PACF: spikes at only 12, negative at 24, and 36
#(1,0,0)_12

#ACF: only significant at 13. There is almost a significant spike at 25. Dies out in an sine wave like manner
#PACF: last significant at 13. No spikes beyond 13. This also dies out in a sine wave like manner
#either an AR(13) or MA(13) but more inclined to pick a MA(13) model
#possible models ARIMA: (13,1,0) or (0,1,13) 


