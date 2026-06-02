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

## Time plot TFR TLB

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

fert_df |>
  ggplot(aes(x = Year, y = TLB, label = Year, )) +
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
  autoplot(log(TFR) |>
             difference(1) |>
             difference(1))

train |>
  features(log(TFR) |>
             difference(1) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs))

train |>
  gg_tsdisplay(log(TFR) |> 
                 difference(1) |>
                 difference(1), plot_type = 'partial', lag_max = 36)

#Seasonal ACF: there is a weak exponential decay at lags 12, 24,36
#Seasonal PACF: Only significant at lag 12. 
#Evident seasonality so D = 0
#(0,0,1)_12

#PACF: significant spikes at lag 1,2,11,15. None beyond lag 15. Also has a damped sine wave manner. 
#ACF: Signficant spikes at lag 1,11,13, Dies out in a sine wave like manner. 
#differenced once, d = 1
#possible model ARIMA models: (15,1,0), (0,1,13)

### TLB
train |>
  autoplot(TLB)

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
#there's spike at seasonal lags 12 24 for the ACF, PACF only has a lag spike at 12
#The data is already stationary, may consider a SARIMA with a D = 0 model
#ACF: spikes at seasonal lag 12, 24. Only 12 is significant. Exponential decays
#PACF: spikes at only 12, negative at 24, and 36
#(1,0,0)_12

#ACF: only significant at 13. There is almost a significant spike at 25. Dies out in an sine wave like manner
#PACF: last significant at 13. No spikes beyond 13. This also dies out in a sine wave like manner
#either an AR(13) or MA(13) but more inclined to pick a MA(13) model
#possible models ARIMA: (13,1,0) or (0,1,13) 


