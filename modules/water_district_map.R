
# water district leaflet map
library("shiny")
library("dplyr")
library("leaflet")
library("lubridate")
library("highcharter")

# module UI function
waterConservationInput <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return input controls
  uiOutput(ns("controls"))
}

waterConservationOutput1 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return output
  leafletOutput(ns("map"))
}

waterConservationOutput2 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return output
  highchartOutput(ns("usage_chart"))
}

# module server function
waterConservation <- function(input, output, session,
                              map_data, water_data, id_field, name_field) {
  
  # all utilities
  allUtil_df <- unique(water_data[,c(id_field, name_field)])
  allUtil <- setNames(allUtil_df$PWSID_1, allUtil_df$Supplier_Name)
  
  # update UI controls
  output$controls <- renderUI({
    ns <- session$ns
    tagList(
      selectInput(ns("utility"), "Utility", choices = allUtil)
    )
  })
  
  # selected utility
  util <- reactive({
    validate(need(input$utility, message = FALSE))
    input$utility
  })
  
  # selected utility data
  util_data <- reactive({
    water_data[water_data[,id_field] == util(),]
  })
  
  # render map
  output$map <- renderLeaflet({
    leaflet(map_data[map_data@data[,id_field] == util(),]) %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>%
      addPolygons()
  })
  
  # render chart
  output$usage_chart <- renderHighchart({
    util_d <- util_data() %>% arrange(date)
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Water Production") %>% 
      hc_xAxis(Date = util_d$date) %>% 
      hc_add_series(data = util_d$TotMonthlyH20ProdCurrent,
                    name = "Production (gallons)")
  })
  
  
  # return (reactive function with) selected utility data
  util_data
}


