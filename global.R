
library('rgdal')
library('dplyr')

# load data
util_summary <- readRDS('data/util_summary.rds')
water_byMonth <- readRDS('data/water_byMonth.rds')
statewide_byMonth <- readRDS('data/statewide_byMonth.rds')
water_districts <- readRDS('data/water_districts.rds')

water_quality_summary <- readRDS('data/water_quality_summary.rds')
violations_summary <- readRDS('data/violations_summary.rds')

# other data
appliance_data <- list(name = "Appliance", data = list(list(y = 27.21)))
#foodservice_data <- list(name = "Food Service", data = list(list(y = 0.44)))
hvac_data <- list(name = "HVAC", data = list(list(y = 50.76), list(y = 0)))
indoorlighting_data <- list(name = "Indoor Lighting", data = list(list(y = 236.89), list(y = 0)))
other_data <- list(name = "Other", data = list(list(y = 35.67 + 0.44 + 0.39 + 0.26), list(y = 0))) # also include: foodservice + Plug Loads + Water Heating
outdoorlighting_data <- list(name = "Outdoor Lighting", data = list(list(y = 18.61), list(y = 0)))
#plugloads_data <- list(name = "Plug Loads", data = list(list(y = 0.39), list(y = 0)))
process_data <- list(name = "Process", data = list(list(y = 36.62), list(y = 0)))
refrigeration_data <- list(name = "Refrigeration", data = list(list(y = 22.90), list(y = 0)))
#waterheating_data <- list(name = "Water Heating", data = list(list(y = 0.26), list(y = 0)))
wholebuilding_data <- list(name = "Whole Building", data = list(list(y = 29.69), list(y = 0)))

q3_2015_we_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(GWh_saved = sum(MWh_saved) / 1000)
waterenergy_data <- list(name = "Water Conservation", data = list(list(y = 0), list(y = q3_2015_we_sav$GWh_saved)))

# load modules
source('modules/water_district_map.R')
source('modules/water_quality.R')

