
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
                              water_summary, water_monthly, statewide_monthly, map_data, id_field, name_field, statewide) {
  
  vals <- reactiveValues(last_util = '', from_menu = TRUE, still_statewide = FALSE)
  
  # all utilities
  allUtil_df <- unique(water_summary[, c(id_field, name_field)])
  allUtil <- setNames(allUtil_df[[id_field]], allUtil_df[[name_field]])
  allUtil_ord <- allUtil[order(names(allUtil))]
  
  # update UI control
  output$utility <- renderUI({
    ns <- session$ns
    selectInput(ns("utility"), "Utility", choices = allUtil_ord)
  })
  
  # selected utility
  util <- reactive({
    validate(need(input$utility, message = FALSE))
    if (statewide()) return('statewide') else return(input$utility)
  })
  
  # selected utility data
  util_data <- reactive({
    selected_util = util()
    if (selected_util == 'statewide') {
      ret <- statewide_monthly
    } else {
      ret <- water_monthly[water_monthly[, id_field] == selected_util, ]
    }
    ret
  })
  
  # update UI control
  output$daterange <- renderUI({
    ns <- session$ns
    sliderInput(ns('daterange'), label = "Date Range", min = as.Date('2015-6-1'), max = as.Date('2016-5-31'),
                value = c(as.Date('2015-6-1'), as.Date('2016-5-31')), timeFormat = "%b %Y")
  })
  
  # Map --------------------------------------------------------------------
  # render complete map
  output$map <- renderLeaflet({
    
    #colorBin("Spectral", domain = c(0,6))(0:5)
    pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")),
                    domain = map_data$sav_diff,
                    bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% 
      addProviderTiles("Stamen.TonerLabels") %>%
      addPolygons(layerId = ~pwsid, color = '#444', weight = 1,
                  fillColor = ~pal(sav_diff), fillOpacity = 0.7, label = ~pwsname) %>%
      addLegend("bottomleft", pal = pal, values = ~sav_diff,
                title = "Missed Target by",
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
    
    selected_util <- util()
    
    if (selected_util != 'statewide') {
      ns <- session$ns
      pal <- colorBin(rev(c("#A50026","#F46D43","#FEE090","#E0F3F8","#74ADD1","#313695")), domain = map_data$sav_diff, bins = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))
      
      # update last poly
      if (vals$last_util != '') {
        last_selectedPoly <- map_data[map_data[[id_field]] == vals$last_util, ]
        if (nrow(last_selectedPoly) > 0) {
          leafletProxy(ns('map')) %>% 
          addPolygons(data = last_selectedPoly, layerId = ~pwsid, color = '#444', weight = 1,
                      fillColor = ~pal(sav_diff), fillOpacity = 0.7, label = ~pwsname)
        }
      }
      
      # update selected poly
      selectedPoly <- map_data[map_data[[id_field]] == selected_util, ]
      b <- st_bbox(selectedPoly)
      if (nrow(selectedPoly) > 0) {
        leafletProxy(ns('map')) %>% 
          addPolygons(data = selectedPoly, layerId = ~pwsid, color = '#000', weight = 2,
                      fillColor = ~pal(sav_diff), fillOpacity = 0.7, label = ~pwsname)
      }
      vals$last_util <- selected_util
      
      # if util was selected from menu, then update map view
      b <- unname(b)
      if (vals$from_menu) { leafletProxy(ns('map')) %>% fitBounds(b[1], b[2], b[3], b[4]) }
      vals$from_menu <- TRUE
    }
    
  }, priority = -1)
  
  
  # Chart -------------------------------------------------------------------
  # render chart
  output$usage_chart <- renderHighchart({
    
    validate(need(input$daterange, message = FALSE),
             need(util_data(), message = FALSE))
    
    isolate({
      # data
      util_d <- util_data() %>%
        mutate(year = year(ReportingMonth),
               month = month(ReportingMonth, label = TRUE),
               month_start = floor_date(ReportingMonth, 'month'),
               month_end = ceiling_date(ReportingMonth, unit = 'month') - 1) %>% 
        group_by(year, month, month_start, month_end) %>% 
        summarise(MG_2013 = sum(MG_2013),
                  MG_cur = sum(MG_cur)) %>% 
        mutate(selected = between(as.Date(month_start), input$daterange[1], input$daterange[2]) | between(as.Date(month_end), input$daterange[1], input$daterange[2]),
               selected = ifelse(is.na(selected), FALSE, selected)) %>% 
        filter(selected) %>% 
        select(year, month, month_start, month_end, MG_cur, selected) %>%
        split(.$year) %>% 
        map(~ right_join(., data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month'))
      
      util_d_2013 <- util_data() %>%
        mutate(month = month(ReportingMonth, label = TRUE)) %>% 
        group_by(month) %>% 
        summarise(MG_2013 = sum(MG_2013)) %>% 
        select(month, MG_2013) %>% 
        distinct() %>% 
        right_join(data.frame(month = factor(month.abb, levels = month.abb, ordered = TRUE)), by = 'month')
      
      # animate chart or not
      animate <- TRUE
      if (util() == 'statewide') {
        if (vals$still_statewide | vals$last_util != '') { animate <- FALSE }
        vals$still_statewide <- TRUE
      } else {
        if (vals$last_util == util()) { animate <- FALSE }
        vals$still_statewide <- FALSE
      }
      
      # build chart
      hc <- highchart() %>% 
        hc_chart(type = "column") %>% 
        hc_title(text = "Water Production") %>% 
        hc_xAxis(categories = month.abb) %>% 
        hc_yAxis(title = list(text = "Million Gallons")) %>% 
        hc_add_series(name = '2013', data =  util_d_2013$MG_2013, color = '#000000', animation = animate) %>% 
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
      series_colors <- c(`2015` = '#717CFF', `2016` = '#69B245')
      for (yr in years) {
        #cols <- substr(ifelse(util_d[[yr]]$selected, series_colors[yr], adjustcolor(series_colors[yr], red.f = 2, green.f = 2, blue.f = 2)), 1, 7)
        hc <- hc %>% hc_add_series(name = yr, data =  util_d[[yr]]$MG_cur,
                                   color = unname(series_colors[yr]), animation = animate)
      }
      
      hc
    })
  })
  
  # return (reactive function with) selected utility data --------------------
  reactive({
    validate(need(input$daterange, message = FALSE))
    util_data() %>%
      mutate(selected = between(floor_date(as.Date(ReportingMonth), unit = 'month'),
                                input$daterange[1],
                                input$daterange[2]) | 
                        between(ceiling_date(as.Date(ReportingMonth), unit = 'month') - 1,
                                input$daterange[1],
                                input$daterange[2]))
  })
  
}

