pacman::p_load(fpp3)

read.csv(
  here::here("BirthsAndFertilityRatesAnnual.csv")) |>
  as.tibble() -> temp_df #reading data as temp

sapply(temp_df, class) #checking year columns if theyre numeric
colnames(temp_df) #check colnames 
#these names are have blank spaces
#will need to clean them up so it's easier to analyse 


temp_df <-
  temp_df |> 
  mutate(across(where(is.character) & -any_of("DataSeries"),
                as.numeric)) |> #converting all variables as numeric except first column
  pivot_longer( #pivoting year col names so their a single column, gives too many rows
    cols = -DataSeries,
    names_to = "Year",
    values_to = "Value"
  ) |>
  pivot_wider( # pivot the DataSeries to be colnames, fixes the obscene number of rows
    names_from = "DataSeries",
    values_from = "Value"
  ) |> #converts years to integers, not necessary for time series but just did it anyways
  mutate(Year = as.integer(stringr::str_remove(Year, "X")))|>
  rename( #renaming variables so it's easier to use in later anaylsis
    #newName = oldName
    "GRR" = "Gross Reproduction Rate",
    "CBR" = "Crude Birth Rate",
    "RLB" = "Resident Live-Births",
    "TFR" = "Total Fertility Rate (TFR)",
    "TLB" = "Total Live-Births",
    "CLB" = "Citizen Live-Births",
    "NPR" = "Net Reproduction Rate",
    "15-19Y" = "    15 - 19 Years",
    "20-24Y" = "    20 - 24 Years",
    "25-39Y" = "    25 - 29 Years",
    "30-34Y" = "    30 - 34 Years",
    "35-39Y" = "    35 - 39 Years",
    "40-44Y" = "    40 - 44 Years",
    "45-49Y" = "    45 - 49 Years"
  )


temp_df |> #converts to time series dateframe
  as_tsibble(index = Year) -> fertility_tsd
  
fertility_tsd |> #exports as CSV
  write.csv("AnnualFertilityData_Cleaned.csv", row.names = FALSE)
