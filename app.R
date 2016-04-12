
library(shiny)
library(flexdashboard)
library(rgdal)

# water district boundary
water_dist <- readRDS('data/water_dist.rds')

# water use summary
water_use <- readRDS('data/water_use.rds')

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
        textOutput('ghg')
      )
   )
))


server <- shinyServer(function(input, output, session) {
  util <- callModule(waterConservation, "water_districts", map_data = water_dist,
                     water_data = water_use, id_field = 'PWSID_1', name_field = 'Supplier_Name')
  
  output$savings <- reactive({
    util_data <- util()
    prop_change <- mean((util_data %>% filter(selected) %>% .$proportionChange), na.rm = TRUE)
    sprintf('%.1f%%', prop_change * 100)
  })
  
  output$ghg <- reactive({
    water_saved <- util() %>%
      filter(selected) %>%
      mutate(mg_change = (TotMonthlyH20Prod2013 - TotMonthlyH20ProdCurrent) / 1e6,
             ton_co2_saved = mg_change * ghg_factor)
    paste(format(round(sum(water_saved$ton_co2_saved, na.rm = TRUE)), big.mark=",", scientific=FALSE), 'tons CO2e')
  })
  
})

# Run the application 
shinyApp(ui = ui, server = server)

