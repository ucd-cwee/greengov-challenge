
# water district leaflet map
library("shiny")
library("tidyr")
library("dplyr")
library("leaflet")
library("lubridate")
library("highcharter")


# module UI functions
waterConservationInput1 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return input control
  uiOutput(ns("utility"))
}

waterConservationInput2 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return input control
  uiOutput(ns("daterange"))
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
  
  # update UI control
  output$utility <- renderUI({
    ns <- session$ns
    selectInput(ns("utility"), "Utility", choices = allUtil)
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
  
  # update UI control
  output$daterange <- renderUI({
    ns <- session$ns
    dat <- util_data()
    sliderInput(ns('daterange'), label = "Date Range", min = min(dat$date), max = max(dat$date),
                value = c(min(dat$date), max(dat$date)), timeFormat = "%b %Y")
  })
  
  # render map
  output$map <- renderLeaflet({
    leaflet(map_data[map_data@data[,id_field] == util(),]) %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>%
      addPolygons()
  })
  
  # render chart
  output$usage_chart <- renderHighchart({
    
    validate(need(input$daterange, message = FALSE))
    
    # data
    util_d <- util_data() %>%
      mutate(month = month(date, label = TRUE),
             year = year(date)) %>% 
      select(year, month, TotMonthlyH20ProdCurrent) %>%
      spread(year, TotMonthlyH20ProdCurrent) %>%
      arrange(month)
    
    util_d_2014 <- util_d %>% select(month, values = `2014`) %>%
      mutate(month_start = as.Date(paste0('2014-',as.numeric(month),'-1')),
             month_end = ceiling_date(month_start + 1, unit = 'month') - 1,
             col = ifelse(between(month_start, input$daterange[1], input$daterange[2]) | between(month_end, input$daterange[1], input$daterange[2]), '#717CFF', '#B8BDFF'))
    util_d_2015 <- util_d %>% select(month, values = `2015`) %>%
      mutate(month_start = as.Date(paste0('2015-',as.numeric(month),'-1')),
             month_end = ceiling_date(month_start + 1, unit = 'month') - 1,
             col = ifelse(between(month_start, input$daterange[1], input$daterange[2]) | between(month_end, input$daterange[1], input$daterange[2]), '#69B245', '#99B28D'))
    util_d_2016 <- util_d %>% select(month, values = `2016`) %>%
      mutate(month_start = as.Date(paste0('2016-',as.numeric(month),'-1')),
             month_end = ceiling_date(month_start + 1, unit = 'month') - 1,
             col = ifelse(between(month_start, input$daterange[1], input$daterange[2]) | between(month_end, input$daterange[1], input$daterange[2]), '#FF9C71', '#FFCBB5'))
    
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Water Production") %>% 
      hc_xAxis(categories = util_d$month) %>% 
      hc_add_series(name = "2014", data =  util_d_2014$values, colorByPoint = TRUE, color = "717CFF", colors = util_d_2014$col, animation = FALSE) %>%
      hc_add_series(name = "2015", data =  util_d_2015$values, colorByPoint = TRUE, colors = util_d_2015$col, animation = FALSE) %>% 
      hc_add_series(name = "2016", data =  util_d_2016$values, colorByPoint = TRUE, colors = util_d_2016$col, animation = FALSE) %>% 
      #hc_colors(c('#717CFF','#69B245','#FF9C71')) %>% 
      hc_tooltip(crosshairs = TRUE, shared = TRUE) %>% 
      hc_credits(enabled = TRUE, text = "Source: State Water Board",
                 href = "http://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.shtml") %>% 
      hc_exporting(enabled = TRUE)
  })
  
  # return (reactive function with) selected utility data
  reactive({
    validate(need(input$daterange, message = FALSE))
    util_data() %>% mutate(selected = between(floor_date(date), input$daterange[1], input$daterange[2]) | between(ceiling_date(date, unit = 'month') - 1, input$daterange[1], input$daterange[2]))
  })
  
}

