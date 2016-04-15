
library(shiny)
library(rgdal)

# load data
util_summary <- readRDS('data/util_summary.rds')
water_byMonth <- readRDS('data/water_byMonth.rds')
water_districts <- readRDS('data/water_districts.rds')

# load modules
source('modules/water_district_map.R')

