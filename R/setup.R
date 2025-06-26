library(shiny)
library(bslib)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(DT)
library(stringr)
library(leaflet)
library(sf)


csv <- read.csv("data/app_data_test.csv")

constituencies <- read.csv("data/station_accessibility.csv") |> 
  select(ConstituencyName) |> 
  distinct() |> 
  arrange(ConstituencyName)

table_data <- csv |>
  mutate(across(where(is.logical), ~ ifelse(.x, "Yes", "No")))|> 
  mutate(StepFreeAccess = str_replace(StepFreeAccess, "^wholeStation$", "Whole station")) |> 
  mutate(StepFreeAccess = str_replace(StepFreeAccess, "^partialStation$", "Partial station")) |> 
  mutate(StepFreeAccess = str_replace(StepFreeAccess, "^noPartOfStation$", "Unavailable")) |> 
  select("Station Name" = StationName,
         "Accessible Ticket Machines" = AccessibleTicketMachines,
         "Ramp for Train Access" = RampForTrainAccess,
         "National Key Toilet" = NationalKeyToilet,
         "Impaired Mobility Setdown" = ImpairedMobilitySetDown,
         "Induction Loop" = InductionLoop,
         "Step Free Access" = StepFreeAccess,
         "Constituency" = ConstituencyName)

  

# stations <- read.csv("data/station_accessibility.csv") |> 
#   select(StationName) |> 
#   distinct()
# 
# geography <- read.csv("data/constituency_region_country_2023.csv") |>
#   select(new_constituency_name)
# 
# 
# unmatched <- anti_join(geography, constituencies, join_by(new_constituency_name == ConstituencyName))
