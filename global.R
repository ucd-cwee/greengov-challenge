
library('rgdal')

# load data
util_summary <- readRDS('data/util_summary.rds')
water_byMonth <- readRDS('data/water_byMonth.rds')
statewide_byMonth <- readRDS('data/statewide_byMonth.rds')
water_districts <- readRDS('data/water_districts.rds')

water_quality_summary <- readRDS('data/water_quality_summary.rds')
violations_summary <- readRDS('data/violations_summary.rds')

# other data
ghg_by_sector <- list(
  list(y = 8,  name = 'Agriculture'),
  list(y = 7,  name = 'Residential'),
  list(y = 5,  name = 'Commercial'),
  list(y = 37, name = 'Transportation'),
  list(y = 23, name = 'Industrial'),
  list(y = 20, name = 'Electricity<br/>Generation')
)

# load modules
source('modules/water_district_map.R')
source('modules/water_quality.R')

