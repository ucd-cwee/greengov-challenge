
library(shiny)
library(flexdashboard)
library(rgdal)

# load data
util_summary <- readRDS('data/util_summary.rds')
water_byMonth <- readRDS('data/water_byMonth.rds')
water_districts <- readRDS('data/water_districts.rds')

# load modules
source('modules/water_district_map.R')


#  ------------------------------------------------------------------------

ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        waterConservationInput1('water_districts'),
        waterConservationInput2('water_districts')
      ),
      
      mainPanel(
        waterConservationOutput1('water_districts'),
        waterConservationOutput2('water_districts'),
        textOutput('savings'),
        textOutput('vs_std'),
        textOutput('ei'),
        textOutput('ghg')
      )
   )
))


server <- shinyServer(function(input, output, session) {
  util <- callModule(waterConservation, "water_districts", water_summary = util_summary,
                     water_monthly = water_byMonth, map_data = water_districts,
                     id_field = 'PWS_ID', name_field = 'PWS_name')
  
  savings <- reactive({
    util() %>%
      filter(selected) %>%
      mutate(change_1000m3 = (gal_2013 - gal_cur) / 264172.052) %>% 
      group_by(PWS_ID) %>% 
      summarise(proportionChangeGoal = mean(proportionChangeGoal),
                gal_2013 = sum(gal_2013),
                gal_cur = sum(gal_cur)
                ) %>% 
      mutate(change_gal = gal_2013 - gal_cur,
             change_prop = change_gal / gal_2013,
             sav_diff = proportionChangeGoal - change_prop,
             change_1000m3 = change_gal / 264172.052,
             kWh_saved = change_1000m3 * util_summary$ei_kWh_1000m3[match(PWS_ID, util_summary$PWS_ID)],
             MWh_saved = kWh_saved / 1e3,
             kg_CO2e_saved = MWh_saved * util_summary$ghgFactor_kg_MWh[match(PWS_ID, util_summary$PWS_ID)])
  })
  
  output$savings <- reactive({
    sprintf('%.1f%% (goal: %.f%%)', savings()$change_prop * 100, savings()$proportionChangeGoal * 100)
  })
  
  output$vs_std <- reactive({
    sprintf('%.1f%%', savings()$sav_diff * 100)
  })
  
  output$ei <- reactive({
    paste(format(round(savings()$kWh_saved), big.mark=",", scientific=FALSE), 'kWh energy')
  })
  
  output$ghg <- reactive({
    paste(format(round(savings()$kg_CO2e_saved), big.mark=",", scientific=FALSE), 'kg CO2e')
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

