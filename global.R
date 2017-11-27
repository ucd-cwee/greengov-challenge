
library('shiny')
library('sf')
library('dplyr')

# get info about current environment -------------------------------------------

islocal <- Sys.getenv('SHINY_PORT') == ""
if (!islocal) options(shiny.sanitize.errors = TRUE)


# conversion factors -----------------------------------------------------------

# pounds per 1 kilogram
lbs_per_kg <- 2.20462262185

# eGRID emissions factor estimate; CAMX subgrid & imported electricity (MT CO2e/MWh)
ghg_factor_mt_mwh <- 0.285

# annual emissions from a typical passenger vehicle
avg_car_mt_CO2 <- 4.7


# load data --------------------------------------------------------------------

water_byMonth <- readRDS('data/water_byMonth_pub.rds')
statewide_byMonth <- readRDS('data/statewide_byMonth_pub.rds')
water_districts <- readRDS('data/water_districts_pub.rds')
eedata_sav_summary <- readRDS('data/eedata_sav_summary.rds')
elec_ce_plot_data <- readRDS('data/elec_ce_plot_data.rds')


# load modules -----------------------------------------------------------------
source('modules/water_district_map.R')
