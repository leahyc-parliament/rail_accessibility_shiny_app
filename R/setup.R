library(shiny)
library(bslib)
library(shinyWidgets)
library(magrittr)
library(dplyr)
library(DT)
library(stringr)
library(leaflet)
library(sf)

load("data/app_data.RData")
csv <- app_data |>
  rename(ImpairedMobilitySetDown = ImpariedMobilitySetDown) |>
  st_transform(crs = 4326)

# load("data/OLD_to_be_removed/data_with_geom.RData")
# csv <- final_output |>
#   rename(ImpairedMobilitySetDown = ImpariedMobilitySetDown)

# Add in const with no stations
constituencies <- read.csv("data/constituency_region_country_2023.csv") |> 
  select(ConstituencyName = new_constituency_name) |> 
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
         "ConstituencyName")
  

# stations <- read.csv("data/station_accessibility.csv") |> 
#   select(StationName) |> 
#   distinct()
# 
# geography <- read.csv("data/constituency_region_country_2023.csv") |>
#   select(new_constituency_name)
# 
# 
# unmatched <- anti_join(geography, constituencies, join_by(new_constituency_name == ConstituencyName))
