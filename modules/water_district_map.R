
# water district leaflet map
library("shiny")
library("dplyr")
library("purrr")
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
  
  vals <- reactiveValues(last_util = NULL, from_menu = TRUE)
  
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
    sliderInput(ns('daterange'), label = "Date Range", min = as.Date('2015-6-1'), max = as.Date('2016-2-29'),
                value = c(as.Date('2015-6-1'), as.Date('2016-2-29')), timeFormat = "%b %Y")
  })
  
  # Map --------------------------------------------------------------------
  # render complete map
  output$map <- renderLeaflet({
    
    #colorBin("Spectral", domain = c(0,6))(0:5)
    pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")), domain = map_data$sav_diff, bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron", group = "Grayscale") %>% 
      addPolygons(layerId = ~PWSID_1, color = '#444', weight = 1,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7) %>%
      addLegend("bottomleft", pal = pal, values = ~sav_diff,
                title = "Missed Conservation Standard by",
                opacity = 1, labFormat = labelFormat(prefix = '(', suffix = ')%', between = ', ', transform = function(x) 100 * x))
  })
  
  # update selected utility in UI menu
  observeEvent(input$map_shape_click$id, {
    #ns <- session$ns
    vals$from_menu <- FALSE
    updateSelectInput(session, inputId = 'utility', selected = input$map_shape_click$id)
  })
  
  # update selected polygon on map
  observeEvent(util(), {
    
    ns <- session$ns
    last_selectedPoly <- map_data[map_data@data[,id_field] == vals$last_util,]
    selectedPoly <- map_data[map_data@data[,id_field] == util(),]
    b <- selectedPoly@bbox
    pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")), domain = map_data$sav_diff, bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
    
    leafletProxy(ns('map')) %>% 
      addPolygons(data = last_selectedPoly, layerId = ~PWSID_1, color = '#444', weight = 1,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7) %>% 
      addPolygons(data = selectedPoly, layerId = ~PWSID_1, color = '#000', weight = 2,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7)
    vals$last_util <- util()
    
    if (vals$from_menu) { leafletProxy(ns('map')) %>% fitBounds(b['x','min'], b['y','min'],b['x','max'], b['y','max']) }
    vals$from_menu <- TRUE
    
  }, priority = -1)
  
  
  # Chart -------------------------------------------------------------------
  # render chart
  output$usage_chart <- renderHighchart({
    
    validate(need(input$daterange, message = FALSE),
             need(util_data(), message = FALSE))
    
    isolate({
      # data
      util_d <- util_data() %>%
        mutate(month = month(date, label = TRUE),
               month_start = floor_date(date, 'month'),
               month_end = ceiling_date(date, unit = 'month') - 1,
               year = year(date),
               selected = between(month_start, input$daterange[1], input$daterange[2]) | between(month_end, input$daterange[1], input$daterange[2]),
               selected = ifelse(is.na(selected), FALSE, selected)) %>% 
        filter(selected) %>% 
        select(year, month, month_start, month_end, TotMonthlyH20ProdCurrent, selected) %>%
        split(.$year) %>% 
        map(~ right_join(., data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month'))
      
      util_d_2013 <- util_data() %>%
        mutate(month = month(date, label = TRUE)) %>% 
        select(month, TotMonthlyH20Prod2013) %>% 
        distinct() %>% 
        right_join(data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month')
      
      hc <- highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = "Water Production") %>% 
        hc_xAxis(categories = month.abb) %>% 
        hc_add_series(name = '2013', data =  util_d_2013$TotMonthlyH20Prod2013, color = '#000000', animation = (vals$last_util != util())) %>% 
        hc_tooltip(crosshairs = TRUE, shared = TRUE) %>% 
        hc_credits(enabled = TRUE, text = "Source: State Water Board",
                   href = "http://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.shtml") %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_plotOptions(column = list(pointPadding = 0))
      
      series_colors <- c('#717CFF','#69B245') # '#FF9C71'
      for (i in 1:length(util_d)) {
        #cols <- substr(ifelse(util_d[[i]]$selected, series_colors[i], adjustcolor(series_colors[i], red.f = 2, green.f = 2, blue.f = 2)), 1, 7)
        hc <- hc %>% hc_add_series(name = names(util_d)[i], data =  util_d[[i]]$TotMonthlyH20ProdCurrent,
                                   color = series_colors[i], animation = (vals$last_util != util()))
      }
      
      hc
    })
  })
  
  # return (reactive function with) selected utility data --------------------
  reactive({
    validate(need(input$daterange, message = FALSE))
    util_data() %>% mutate(selected = between(floor_date(date), input$daterange[1], input$daterange[2]) | between(ceiling_date(date, unit = 'month') - 1, input$daterange[1], input$daterange[2]))
  })
  
}

