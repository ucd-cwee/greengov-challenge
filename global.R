
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
appliance_data <- list(name = "Appliance", data = list(list(y = 27.21), list(y = 0, color = '#393CB8')))
foodservice_data <- list(name = "Food Service", data = list(list(y = 0.44), list(y = 0, color = '#393CB8')))
hvac_data <- list(name = "HVAC", data = list(list(y = 50.76), list(y = 0, color = '#393CB8')))
indoorlighting_data <- list(name = "Indoor Lighting", data = list(list(y = 236.89), list(y = 0, color = '#393CB8')))
other_data <- list(name = "Other", data = list(list(y = 35.67), list(y = 0, color = '#393CB8')))
outdoorlighting_data <- list(name = "Outdoor Lighting", data = list(list(y = 18.61), list(y = 0, color = '#393CB8')))
plugloads_data <- list(name = "Plug Loads", data = list(list(y = 0.39), list(y = 0, color = '#393CB8')))
process_data <- list(name = "Process", data = list(list(y = 36.62), list(y = 0, color = '#393CB8')))
refrigeration_data <- list(name = "Refrigeration", data = list(list(y = 22.90), list(y = 0, color = '#393CB8')))
waterheating_data <- list(name = "Water Heating", data = list(list(y = 0.26), list(y = 0, color = '#393CB8')))
wholebuilding_data <- list(name = "Whole Building", data = list(list(y = 29.69), list(y = 0, color = '#393CB8')))

q3_2015_we_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(GWh_saved = sum(MWh_saved) / 1000)
waterenergy_data <- list(name = "Water Conservation", data = list(list(y = 0), list(y = q3_2015_we_sav$GWh_saved, color = '#393CB8')))


# energy_by_program <- list(
#   list(y = 215.05,  name = 'Appliance'),
#   list(y = 6.23,  name = 'Food Service'),
#   list(y = 702.57,  name = 'HVAC'),
#   list(y = 2111.29, name = 'Indoor Lighting'),
#   list(y = 96.13, name = 'Other'),
#   list(y = 249.35, name = 'Outdoor Lighting'),
#   list(y = 46.52, name = 'Plug Loads'),
#   list(y = 617.73, name = 'Process'),
#   list(y = 233.10, name = 'Refrigeration'),
#   list(y = 4.44, name = 'Water Heating'),
#   list(y = 454.09, name = 'Whole Building')
# )

# load modules
source('modules/water_district_map.R')
source('modules/water_quality.R')

