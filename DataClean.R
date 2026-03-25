pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df

head(temp_df)
temp_df 
sapply(temp_df, class) #checking year columns if theyre numeric
colnames(temp_df)

temp_df |> 
  mutate(across(where(is.character) & -any_of("DataSeries"),
                as.numeric)) -> rates_df
    
