
# water district leaflet map
library(shiny, quietly = TRUE)
library(leaflet, quietly = TRUE)
library(lubridate, quietly = TRUE)

# module UI function
waterDistMapInput <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return input controls
  uiOutput(ns("controls"))
}

waterDistMapOutput1 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return output
  leafletOutput(ns("map"))
}

# module server function
waterDistMap <- function(input, output, session,
                         map_data, water_data, id_field, name_field, date_field) {
  
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
  
  # render map
  output$map <- renderLeaflet({
    leaflet(map_data[map_data@data[,id_field] == util(),]) %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>%
      addPolygons()
  })
  
  # return percent reduction
  reactive({
    savings <- water_data$proportionChange[water_data[,id_field] == util() & water_data[,date_field] == ymd('2016-02-15')]
    round(savings * 100, 1)
  })
}


