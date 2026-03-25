pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df

head(temp_df)
temp_df 
sapply(temp_df, class) #checking year columns if theyre numeric
colnames(temp_df)

temp_df |> 
  filter(!stringr::str_detect(DataSeries, "Live-Births")) |>
  mutate(across(where(is.character) & -any_of("DataSeries"), as.numeric))


  pivot_longer(
    cols = contains("X20"),
    names_to = "year",
    values_to = "DataSeries")
    
