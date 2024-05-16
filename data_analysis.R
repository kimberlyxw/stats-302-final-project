library(tidyverse)
library(here)

airline <- read_csv("data/Airline_customer_satisfaction.csv") |> 
  janitor::clean_names()

airline |> 
  skimr::skim_without_charts()

airline |> 
  filter(is.na(arrival_delay_in_minutes)) |> 
  select(arrival_delay_in_minutes, everything()) |> 
  glimpse()


airline |> 
  glimpse()
# no missing 
# 129880 rows aka observations
# 22 columns 
# 4 character variables 
# 18 numeric but many of them are still categorical 