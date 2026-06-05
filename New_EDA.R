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
  select(Year, TFR, TLB, Zodiac) |>
  filter(Year <= 2012)

test <-
  fert_df |>
  select(Year, TFR, TLB, Zodiac) |>
  filter(Year > 2012)  

### TFR
train |>
  autoplot(log(TFR) |>
             difference(12) |>
             difference(1))

train |>
  autoplot(log(TFR) |>
             difference(1) |>
             difference(1))

train |>
  features(log(TFR) |>
             difference(12) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs))

train |>
  features(log(TFR) |>
             difference(1) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs))

train |>
  gg_tsdisplay(log(TFR) |> 
                 difference(12) |>
                 difference(1), plot_type = 'partial', lag_max = 45)

train |>
  ACF(log(TFR) |> 
        difference(12) |>
        difference(1), lag_max = 45) |>
  autoplot()

train |>
  PACF(log(TFR) |> 
        difference(12) |>
        difference(1), lag_max = 45) |>
  autoplot()

# d = 1, D = 12
### Seasonal ACF: only 12
### Seasonal PACF: Exponential decay, no significance
### (0,1,1)_12
### ACF: last significance is 13 but maybe 8 as 12 13 is affected by seasonal lags. Damped Sine-wave. 
### PACF: last significant is 4
### AR(4,1,0) or MA(0,1,8) or MA(0,1,13)

train |>
  gg_tsdisplay(log(TFR) |> 
                 difference(1) |>
                 difference(1), plot_type = 'partial', lag_max = 45)

train |>
  PACF(log(TFR) |>
         difference(1) |>
         difference(1), lag_max = 45) |>
  autoplot()

train |>
  ACF(log(TFR) |>
        difference(1) |>
        difference(1), lag_max = 45) |>
  autoplot()
# d = 2, D = 0
### Seasonal ACF: 12, 24, exponential decay
### Seasonal PACF: only 12 significant
### (1,0,0)_12
### ACF: last significant is 1, lags 11 and 13 are affected by the seasonal lag
### PACF: last significant is 15, damped sine wave
### AR(15,2,0), or AR(13,2,0) or MA(0,2,13) or MA(1,2,0)

### TLB
train |>
  autoplot(TLB)

train |>
  autoplot(log(TLB) |>
             difference(12)) 

train |>
  autoplot(log(TLB) |>
             difference(12) |>
             difference(1))

train |>
  features(log(TLB) |>
             difference(12) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))

train |>
  gg_tsdisplay(log(TLB) |>
                 difference(12) |>
                 difference(1), plot_type = 'partial', lag_max = 45)

train |>
  PACF(log(TLB) |>
         difference(12) |>
         difference(1), lag_max = 45) |>
  autoplot()

train |>
  ACF(log(TLB) |>
        difference(12) |>
        difference(1), lag_max = 45) |>
  autoplot()
# d = 1 D = 1
### Seasonal ACF: 12, exponential decay
### Seasonal PACF: no significant, exponential decay 
### (1,1,0)_12
### ACF: last significant 13. This lag could be affected by seasonals. damped sine wave. However lag 4 is correlated with PACF Lag 4
### PACF: last significant is 10. damped sine wave.
### AR(4,1,0) or AR(13,1,0) or MA(0,1,10)


train |>
  gg_tsdisplay(log(TLB) |>
                 difference(1), plot_type = 'partial', lag_max = 45)

train |>
  PACF(log(TLB) |>
         difference(1), lag_max = 45) |>
  autoplot()

train |>
  ACF(log(TLB) |>
        difference(1), lag_max = 45) |>
  autoplot()
# d = 1 D = 0
### Seasonal ACF: only 12, 24 may look significant but it's not. exponential decay
### Seasonal PACF: only 12, exponential decay
###(1,0,0)_12 or (0,0,1)_12
#ACF: last significant 13, damped sine wave
#PACF: last significant 13, damped sine wave
### AR(13,1,0) or (0,1,13)


