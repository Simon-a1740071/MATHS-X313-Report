pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df

head(temp_df)
sapply(temp_df, class) #checking year columns if theyre numeric
colnames(temp_df)

temp_df |> 
  mutate(across(where(is.character) & -any_of("DataSeries"),
                as.numeric)) |>
  pivot_longer(
    cols = -DataSeries,
    names_to = "Year",
    values_to = "Value"
  ) |>
  pivot_wider(
    names_from = "DataSeries",
    values_from = "Value"
  ) |>
  mutate(Year = stringr::str_remove(Year, "X"))
