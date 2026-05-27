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
  geom_segment(aes(x = Year, xend = Year, y = 0, yend = TFR),
               data = filter(fert_df, Zodiac %in% c("Dragon", "Tiger")), 
               linetype = "dashed") +
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
  autoplot(log(TFR) |>
             difference(1) |>
             difference(1))

train |>
  features(log(TFR) |>
             difference(1) |>
             difference(1), list(unitroot_kpss, 
                                 unitroot_ndiffs,
                                 unitroot_nsdiffs))

train |>
  gg_tsdisplay(log(TFR) |>
                 difference(12) |>
                 difference(1), plot_type = 'partial', lag_max = 36)
#Seasonal PACF: there is a weak exponential decay at lags 12, 24,36
#Seasonal ACF: Only significant at lag 12. 
#Very weak sesonaility so D = 0
#(1,0,0)_12

#PACF: significant spikes at lag 1, 4. None beyond lag 4. Also has a damped sine wave manner. 
#ACF: Dies out in a sine wave like manner. Significant spikes at 13, none beyond it
#differenced once, d = 1
#possible model ARIMA models: (0,1,4), or (13,1,0)


### TLB
fert_df |>
  autoplot(TLB)
