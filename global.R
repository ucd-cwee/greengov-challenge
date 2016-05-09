
library('sp')
library('dplyr')

# load data
util_summary <- readRDS('data/util_summary.rds')
water_byMonth <- readRDS('data/water_byMonth.rds')
statewide_byMonth <- readRDS('data/statewide_byMonth.rds')
water_districts <- readRDS('data/water_districts.rds')

water_quality_summary <- readRDS('data/water_quality_summary.rds')
violations_summary <- readRDS('data/violations_summary.rds')

# Energy Savings data
appliance_data <- list(name = "Appliance", data = list(list(y = 27.21 / 4), list(y = 0)))
hvac_data <- list(name = "HVAC", data = list(list(y = 50.76 / 4), list(y = 0)))
indoorlighting_data <- list(name = "Indoor Lighting", data = list(list(y = 236.89 / 4), list(y = 0)))
other_data <- list(name = "Other", data = list(list(y = (35.67 + 0.44 + 0.39 + 0.26) / 4), list(y = 0))) # also includes: foodservice + Plug Loads + Water Heating
outdoorlighting_data <- list(name = "Outdoor Lighting", data = list(list(y = 18.61 / 4), list(y = 0)))
process_data <- list(name = "Process", data = list(list(y = 36.62 / 4), list(y = 0)))
refrigeration_data <- list(name = "Refrigeration", data = list(list(y = 22.90 / 4), list(y = 0)))
wholebuilding_data <- list(name = "Whole Building", data = list(list(y = 29.69 / 4), list(y = 0)))

q3_2015_we_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(GWh_saved = sum(MWh_saved) / 1000)
waterenergy_data <- list(name = "Water Conservation", data = list(list(y = 0), list(y = q3_2015_we_sav$GWh_saved)))

# Cost Savings data
appliance_cost_data <- list(name = "Appliance", data = list(list(y = 9.6), list(y = 0)))
hvac_cost_data <- list(name = "HVAC", data = list(list(y = 41.7), list(y = 0)))
indoorlighting_cost_data <- list(name = "Indoor Lighting", data = list(list(y = 56.8), list(y = 0)))
other_cost_data <- list(name = "Other", data = list(list(y = (9.3 + 1.3 + 0.6 + 3.7)), list(y = 0))) # also includes: foodservice + Plug Loads + Water Heating
outdoorlighting_cost_data <- list(name = "Outdoor Lighting", data = list(list(y = 6.9), list(y = 0)))
process_cost_data <- list(name = "Process", data = list(list(y = 12.6), list(y = 0)))
refrigeration_cost_data <- list(name = "Refrigeration", data = list(list(y = 9.7), list(y = 0)))
wholebuilding_cost_data <- list(name = "Whole Building", data = list(list(y = 20.4), list(y = 0)))

q3_2015_we_cost_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(change_af = sum(change_gal) / 325851)
waterenergy_cost_data <- list(name = "Water Conservation", data = list(list(y = 0), list(y = q3_2015_we_cost_sav$change_af * 75 / 1e6)))


# load modules
source('modules/water_district_map.R')
source('modules/water_quality.R')

