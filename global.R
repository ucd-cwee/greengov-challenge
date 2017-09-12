
library('sp')
library('dplyr')

# load data
util_summary <- readRDS('data/util_summary_new.rds')
water_byMonth <- readRDS('data/water_byMonth_new.rds')
statewide_byMonth <- readRDS('data/statewide_byMonth_new.rds')
water_districts <- readRDS('data/water_districts_new.rds')

water_quality_summary <- readRDS('data/water_quality_summary.rds')
violations_summary <- readRDS('data/violations_summary.rds')

# Energy Savings data
appliance_data <- list(name = "Appliance", color = '#f45b5b', data = list(list(y = 27.21), list(y = 0)))
hvac_data <- list(name = "HVAC", color = '#434348', data = list(list(y = 50.76), list(y = 0)))
indoorlighting_data <- list(name = "Indoor Lighting", color = '#90ed7d', data = list(list(y = 236.89), list(y = 0)))
other_data <- list(name = "Other", color = '#f7a35c', data = list(list(y = 35.67 + 0.44 + 0.39 + 0.26), list(y = 0))) # also includes: foodservice + Plug Loads + Water Heating
outdoorlighting_data <- list(name = "Outdoor Lighting", color = '#8085e9', data = list(list(y = 18.61), list(y = 0)))
process_data <- list(name = "Process", color = '#f15c80', data = list(list(y = 36.62), list(y = 0)))
refrigeration_data <- list(name = "Refrigeration", color = '#e4d354', data = list(list(y = 22.90), list(y = 0)))
wholebuilding_data <- list(name = "Whole Building", color = '#2b908f', data = list(list(y = 29.69), list(y = 0)))

q3_2015_we_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(GWh_saved = sum(MWh_saved) / 1000)
waterenergy_data <- list(name = "Water Conservation", color = '#3E7DC1', data = list(list(y = 0), list(y = q3_2015_we_sav$GWh_saved)))

# Cost Savings data
appliance_cost_data <- list(name = "Appliance", color = '#f45b5b', data = list(list(y = 9.6), list(y = 0)))
hvac_cost_data <- list(name = "HVAC", color = '#434348', data = list(list(y = 41.7), list(y = 0)))
indoorlighting_cost_data <- list(name = "Indoor Lighting", color = '#90ed7d', data = list(list(y = 56.8), list(y = 0)))
other_cost_data <- list(name = "Other", color = '#f7a35c', data = list(list(y = (9.3 + 1.3 + 0.6 + 3.7)), list(y = 0))) # also includes: foodservice + Plug Loads + Water Heating
outdoorlighting_cost_data <- list(name = "Outdoor Lighting", color = '#8085e9', data = list(list(y = 6.9), list(y = 0)))
process_cost_data <- list(name = "Process", color = '#f15c80', data = list(list(y = 12.6), list(y = 0)))
refrigeration_cost_data <- list(name = "Refrigeration", color = '#e4d354', data = list(list(y = 9.7), list(y = 0)))
wholebuilding_cost_data <- list(name = "Whole Building", color = '#2b908f', data = list(list(y = 20.4), list(y = 0)))

q3_2015_we_cost_sav <- statewide_byMonth %>%
  filter(date %in% lapply(c('2015-07-15', '2015-08-15', '2015-09-15'), as.Date)) %>%
  summarise(change_af = sum(change_gal) / 325851)
waterenergy_cost_data <- list(name = "Water Conservation", color = '#3E7DC1', data = list(list(y = 0), list(y = q3_2015_we_cost_sav$change_af * 75 / 1e6)))

# Cost per kWh Savings data
ee_costperkwh_data <- list(name = "Energy Efficiency Programs", color = '#F4855E',
                           data = list(list(y = (9.6 + 41.7 + 56.8 + 9.3 + 1.3 + 0.6 + 3.7 + 6.9 + 12.6 + 9.7 + 20.4) / (27.21 + 50.76 + 236.89 + 35.67 + 0.44 + 0.39 + 0.26 + 18.61 + 36.62 + 22.90 + 29.69)), list(y = 0)))
waterenergy_costperkwh_data <- list(name = "Water Conservation", color = '#3E7DC1',
                                    data = list(list(y = 0), list(y = (q3_2015_we_cost_sav$change_af * 75 / 1e6) / q3_2015_we_sav$GWh_saved)))


# load modules
source('modules/water_district_map.R')
source('modules/water_quality.R')

