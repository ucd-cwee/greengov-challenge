
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
        waterDistMapInput('water_districts')
      ),
      
      mainPanel(
        waterDistMapOutput1('water_districts'),
        waterDistMapOutput2('water_districts')
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  util <- callModule(waterDistMap, "water_districts", map_data = water_dist,
                     water_data = water_use, id_field = 'PWSID_1', name_field = 'Supplier_Name', date_field = 'date')
  
  output$savings <- reactive({ util() })
})

# Run the application 
shinyApp(ui = ui, server = server)

