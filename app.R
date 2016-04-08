
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

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Test"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        waterConservationInput('water_districts')
      ),
      
      mainPanel(
        waterConservationOutput1('water_districts'),
        waterConservationOutput2('water_districts'),
        textOutput('savings')
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  util <- callModule(waterConservation, "water_districts", map_data = water_dist,
                     water_data = water_use, id_field = 'PWSID_1', name_field = 'Supplier_Name')
  
  output$savings <- reactive({
    util_data <- util()
    sprintf('%.1f%%', util_data$proportionChange[1] * 100)
  })
})

# Run the application 
shinyApp(ui = ui, server = server)

