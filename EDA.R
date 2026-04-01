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
  select(TFR) |>
  summary()

df |>
  features(TFR, list(mean = mean, 
                     variance = var,
                     sd = sd,
                     quantile))
  
df |>
  select(TLB) |>
  summary()

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
#TFR
#decreasing trend
#non-stationary
#no seasonality

#stationarity formal check

### TFR 
train |>
  autoplot(TFR)

train |>
  ACF(TFR) |>
  autoplot() # suggests an MA model

train |>
  PACF(TFR) |>
  autoplot() # no AR model suggested

train |>
  features(TFR, unitroot_kpss) # <0.05 reject H0: Stationary

train |>
  features(TFR, unitroot_ndiffs) #need to take 2nd order differences 

train |>
  autoplot(difference(TFR, 2))

train |>
  ACF(difference(TFR, 2)) |>
  autoplot()

train |>
  features(difference(TFR, 2), unitroot_kpss) # <0.05 reject H0: stationary

train |>
  autoplot(difference(TFR, 2) |>
           difference(1))
 train |>
   features(difference(TFR, 2) |>
              difference(1), unitroot_ndiffs) # no more differencing needed
 
 train |>
   features(difference(TFR, 2) |>
              difference(1), unitroot_kpss) #>0.05 accept H0: stationary
 


train |>
  ACF(difference(TFR, 2) |>
        difference(1)) |>
  autoplot()

train |>
  PACF(difference(TFR, 2) |>
         difference(1)) |>
  autoplot()

### TLB

train |>
  autoplot(TLB/10000) 
  
train |>
  features(TLB/10000, unitroot_kpss) #non-stationary

train |>
  features(TLB/10000, unitroot_ndiffs) # need first order difference

train |>
  mutate(mod_TLB = TLB/10000) |>
  autoplot(
    mod_TLB |>
      difference(1)
  )

train |>
  mutate(diff = 1) |>
  features(diff, unitroot_ndiffs) #suggests ARIMA model

#TLB
#decreasing trend
#non-stationary
#no seasonaility
#spikes from mid 1980 to 2000 corresponds with a slight increase in TFR in that same period
#transformation needed



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

# Time decomposition
df |>
  model(stl = STL(TFR)) |>
  components() |>
  autoplot()

df |>
  model(stl = STL(TLB)) |>
  components() |>
  autoplot()

