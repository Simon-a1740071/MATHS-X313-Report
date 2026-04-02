pacman::p_load(fpp3, patchwork)

read.csv(
  here::here("AnnualFertilityData_Cleaned.csv"), check.names = FALSE
) |>
  tsibble(index = Year) -> df

df |>
  select(Year, TFR, TLB) |>
  filter(Year <= 2012) -> train

df |>
  select(Year, TFR, TLB) |>
  filter(Year > 2012) -> test

head(df)
colnames(df)
# 6 rows 18 columns
# Frequency of observations 1 year apart
# index by year
# measured variables TFR TLB
# no key variables that uniquely identifies time series, e.g. countries

# Summary statistics 
df |>
  features(TFR, list(mean = mean, 
                     variance = var,
                     sd = sd,
                     quantile))

df |>
  features(TLB, list(mean = mean,
                     variance = var,
                     sd = sd,
                     quantile))
#time plot
df |>
  autoplot(TFR, colour = "orange") +
  labs( y = "Total Fertility Rate", x = "Year") -> a1

df |>
  autoplot(TLB, colour = "blue") +
  labs(y = "Total Live Births", x = "Year") -> a2

a1 / a2

# Time decomposition
df |>
  model(stl = STL(TFR)) |>
  components() |>
  autoplot()

df |>
  model(stl = STL(TLB/1e4)) |>
  components() |>
  autoplot()


#TFR
#decreasing trend
#non-stationary
#no seasonality

#stationarity formal check

### TFR 
train |>
  autoplot(TFR)

train |>
  autoplot(log(TFR))

train |>
  autoplot(sqrt(TFR))

train |>
  autoplot(TFR^(1/3))

train |>
  features(TFR, features = guerrero)

train |>
  autoplot(box_cox(TFR, 0.119))

train |>
  ACF(TFR) |>
  autoplot() # can only interpret if time series is stationary, this has a trend
# maybe influencing these plots too much
# however significant at each lag until 8 but tails off

train |>
  PACF(TFR) |>
  autoplot() # only lag 1 significant sharp cut off suggests AR(1)

train |>
  features(TFR, unitroot_kpss) # <0.05 reject H0: Stationary

train |>
  features(TFR, unitroot_ndiffs) #need to take 2nd order differences 

train |>
  features(TFR |>
             difference(2), unitroot_kpss) # <0.05 reject H0: stationary

train |>
  autoplot(TFR |>
             difference(2)) 
#ACF: tails off
#PACF:: significant lags 1, 3, 5, 13. seems to suggest an AR model. 
#Large significant spike at 1, then tapers off after it. Maybe an AR(1) model
#ARIMA(0,2,1)

train |>
  features(TFR |>
             difference(2), unitroot_ndiffs)

train |>
  gg_tsdisplay(difference(TFR, 2), plot_type = 'partial')


train |>
  autoplot(difference(TFR, 2) |>
             difference(1)) -> pl0

 train |>
   features(difference(TFR, 2) |>
              difference(1), unitroot_ndiffs) # no more differencing needed
 
train |>
  ACF(difference(TFR, 2) |>
        difference(1)) |>
  autoplot() -> pl1 #lag 2, 12, 14 are significant
# 

train |>
  PACF(difference(TFR, 2) |>
         difference(1)) |>
  autoplot() -> pl2 #lags 2, 4, 6, 8, 9, 11, 12, 15 significant. 


pl0 / pl1 / pl2
#maybe over fitting
#time series non random maybe because of several autocorrealtions are nonzero
#high correlation in lag 2
#ACF: sharp cut off after lag 1
#PACF: no sharp cut off, seems to decay gradually
#overall suggests an MA model
#ARIMA(0,3,1)

### TLB

train |>
  mutate(TLB_S = TLB / 1e4) |>
  autoplot(log(TLB/1e4) |>
             difference(1))

train |>
  mutate(TLB_S = TLB / 1e4) |>
  gg_tsdisplay(TLB_S, plot_type = 'partial')
#significant spike at lag 1 pacf, decays in acf, maybe an AR(1) ARIMA(1,1,0)

train |>
  mutate(TLB_S = TLB / 1e4) |>
  gg_tsdisplay(difference(TLB_S, 1), plot_type = 'partial')
#significant spike at 12, no clear indication of AR or MA

train |>
  ACF(TLB/10000) |>
  autoplot() # gradually decay

train |>
  PACF(TLB/10000) |>
  autoplot() # sharp cutoff after lag 1, maybe AR(1)
  
train |>
  features(TLB/10000, unitroot_kpss) # non-stationary

train |>
  features(TLB/10000, unitroot_ndiffs) # need first order difference

train |>
  mutate(mod_TLB = TLB/10000) |>
  autoplot(difference(mod_TLB, 1)) -> pl0a

train |>
  mutate(mod_TLB = TLB/10000) |>
  features(difference(mod_TLB, 1), unitroot_kpss) # stationary

train |>
  mutate(mod_TLB = TLB/10000) |>
  features(difference(mod_TLB, 1), unitroot_ndiffs)

train |>
  mutate(mod_TLB = TLB/10000) |>
  ACF(difference(mod_TLB, 1)) |>
  autoplot() -> pl1a

train |>
  mutate(mod_TLB = TLB/10000) |>
  PACF(difference(mod_TLB, 1)) |>
  autoplot() -> pl2a


pl0a / pl1a / pl2a
#TLB
#decreasing trend
#non-stationary
#no seasonaility
#spikes from mid 1980 to 2000 corresponds with a slight increase in TFR in that same period
#transformation needed

#acf no clear pattern suggesting MA
#pacf no clear pattern suggesting AR
#possible random walk
#may consider an arima model as only differenced ARIMA(0,1,0)

#Canidate models
#TFR: ARIMA(0,3,1) overfitting is a concern may not use this
#TFR: ARIMA(0,2,1)
#TLB: ARIMA(0,1,0)

fit_TFR <-
  train |>
  select(Year, TFR) |>
  model(ARIMA(TFR ~ pdq(0,2,1)))

report(fit_TFR)

fit_TLB <-
  train |>
  mutate(TLB_S = TLB / 1e4) |>
  select(Year, TLB_S) |>
  model(ARIMA(TLB_S ~ 1 + pdq(0,1,0)))

fit_TLB2 <-
  train |>
  mutate(TLB_S = TLB / 1e4) |>
  select(Year, TLB_S) |>
  model(ARIMA(TLB_S ~ pdq(1,1,0)))

report(fit_TLB)
report(fit_TLB2) #no clear pattern favouring a random walk,
# arima(1,1,0) seems more likely as a starting model

# Age groups
df |>
  select(Year, "15-19Y":"45-49Y") |>
  pivot_longer(
    cols = -Year,
    names_to = "age_group",
    values_to = "TFR") |>
  ggplot(aes(Year, y = TFR, colour =
               age_group)) +
  geom_line() +
  labs(y = "Total Fertility Rate") -> a3

# Ethnicity
df |>
  select(Year, "Chinese":"Indians") |>
  pivot_longer(
    cols = -Year,
    names_to = "ethnicity",
    values_to = "TFR") |>
  ggplot(aes(Year, TFR, colour = ethnicity)) +
  geom_line() +
  labs(y = "Total Fertility Rate") -> a4
  
a1 / a3 / a4

# Birth and reproduction rates
df |>
  autoplot(CBR, colour = "red") +
  labs( y = "Crude Birth Rate", x = "Year") -> a5
  
df |>
  autoplot(GRR, colour = "purple") +
  labs(y = "Gross Reproduction Rate", x = "Year") -> a6

df |>
  autoplot(NRR, colour = "blue") +
  labs(y = "Net Reproduction Rate", x = "Year") -> a7

a5 / a6 / a7
# seasonal plot, 
# no seasonaility 

# scatterplots
# relationships between variables

# lag plots
df |>
  gg_lag(TFR)
# not too useful

# ACF, cant determine AR and MA from these, 
#the original time series has dependence on trend
#need to look at acf and pacf of the transformed TFR and TLB instead
df |> 
  ACF(TFR) |>
  autoplot()

df |>
  features(TFR, feat_acf)

df |>
  ACF(TLB) |>
  autoplot()

df |>
  features(TLB, feat_acf)

# PACF
df |>
  PACF(TFR) |>
  autoplot()

df |>
  features(TFR, feat_pacf)

df |>
  PACF(TLB) |>
  autoplot()

df |>
  features(TLB, feat_pacf)


