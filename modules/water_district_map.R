
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
                              water_summary, water_monthly, map_data, id_field, name_field) {
  
  vals <- reactiveValues(last_util = NULL, from_menu = TRUE)
  
  # all utilities
  allUtil_df <- unique(water_summary[,c(id_field, name_field)])
  allUtil <- setNames(allUtil_df[[id_field]], allUtil_df[[name_field]])
  
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
    water_monthly[water_monthly[,id_field] == util(),]
  })
  
  # update UI control
  output$daterange <- renderUI({
    ns <- session$ns
    sliderInput(ns('daterange'), label = "Filter Date Range", min = as.Date('2015-6-1'), max = as.Date('2016-2-29'),
                value = c(as.Date('2015-6-1'), as.Date('2016-2-29')), timeFormat = "%b %Y")
  })
  
  # Map --------------------------------------------------------------------
  # render complete map
  output$map <- renderLeaflet({
    
    #colorBin("Spectral", domain = c(0,6))(0:5)
    pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")), domain = map_data$sav_diff, bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addPolygons(layerId = ~PWS_ID, color = '#444', weight = 1,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7, label= ~PWS_name_geo) %>%
      addProviderTiles("Stamen.TonerLabels") %>%
      addLegend("bottomleft", pal = pal, values = ~sav_diff,
                title = "Missed Conservation Standard by",
                opacity = 1, labFormat = labelFormat(prefix = '(', suffix = ')%', between = ', ', transform = function(x) 100 * x))
  })
  
  # update selected utility in UI menu
  observeEvent(input$map_shape_click$id, {
    #ns <- session$ns
    vals$from_menu <- FALSE
    
    # # handle overlapping districts
    # click_pt <- SpatialPoints(cbind(input$map_shape_click$lng, input$map_shape_click$lat),
    #                           proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
    # allPolys <- map_data[rownames(over(click_pt, map_data, returnList = TRUE)[[1]]),]
    # if (nrow(allPolys) > 1) cat(paste(allPolys$PWSID_1, allPolys$PWSNAME_1), sep = '\n')
    
    updateSelectInput(session, inputId = 'utility', selected = input$map_shape_click$id)
  })
  
  # update selected polygon on map
  observeEvent(util(), {
    
    ns <- session$ns
    last_selectedPoly <- map_data[map_data@data[,id_field] == vals$last_util,]
    selectedPoly <- map_data[map_data@data[,id_field] == util(),]
    b <- selectedPoly@bbox
    pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")), domain = map_data$sav_diff, bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
    
    if (!is.null(vals$last_util)) {
      leafletProxy(ns('map')) %>% 
      addPolygons(data = last_selectedPoly, layerId = ~PWS_ID, color = '#444', weight = 1,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7, label= ~PWS_name_geo)
    }
    
    leafletProxy(ns('map')) %>% 
      addPolygons(data = selectedPoly, layerId = ~PWS_ID, color = '#000', weight = 2,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7, label= ~PWS_name_geo)
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
        select(year, month, month_start, month_end, gal_cur, selected) %>%
        split(.$year) %>% 
        map(~ right_join(., data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month'))
      
      util_d_2013 <- util_data() %>%
        mutate(month = month(date, label = TRUE)) %>% 
        select(month, gal_2013) %>% 
        distinct() %>% 
        right_join(data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month')
      
      hc <- highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = "Water Production") %>% 
        hc_xAxis(categories = month.abb) %>% 
        hc_yAxis(title = list(text = "Million Gallons")) %>% 
        hc_add_series(name = '2013', data =  util_d_2013$gal_2013 / 1e6, color = '#000000', animation = (vals$last_util != util())) %>% 
        hc_tooltip(crosshairs = TRUE, shared = TRUE,
                   formatter = JS("function () {
                                    var s = '<b>' + this.x + '</b>';
                                    $.each(this.points, function (i, point) {
                                      s += '<br/><span style=\"color:'+ point.series.color +'\">\u25CF</span>: ' + point.series.name + ': ' + point.y.toFixed(1).replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',');
                                    });
                                    if (this.points.length > 1) {
                                      var n = ((this.points[this.points.length - 1].y - this.points[0].y) / this.points[0].y) * 100;
                                      n = (n<0?'':'+') + n.toFixed(1);
                                      s += '<br/><b>' + n + '%</b>';
                                    }
                                    return s; }")) %>% 
        hc_credits(enabled = TRUE, text = "Source: State Water Board",
                   href = "http://www.waterboards.ca.gov/water_issues/programs/conservation_portal/conservation_reporting.shtml") %>% 
        hc_exporting(enabled = TRUE) %>% 
        hc_plotOptions(column = list(pointPadding = 0))
      
      years <- c('2015','2016')
      series_colors <- c(`2015` = '#717CFF', `2016` = '#69B245') # '#FF9C71'
      for (yr in years) {
        #cols <- substr(ifelse(util_d[[yr]]$selected, series_colors[yr], adjustcolor(series_colors[yr], red.f = 2, green.f = 2, blue.f = 2)), 1, 7)
        hc <- hc %>% hc_add_series(name = yr, data =  util_d[[yr]]$gal_cur / 1e6,
                                   color = unname(series_colors[yr]), animation = (vals$last_util != util()))
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

