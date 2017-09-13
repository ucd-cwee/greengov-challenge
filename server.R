
function(input, output, session) {
  
  # handle selection of statewide vs utility view
  vals <- reactiveValues(statewide = TRUE)
  observeEvent(input$view_statewide, {vals$statewide <- TRUE})
  observeEvent(input$view_utility, {vals$statewide <- FALSE})
  
  # call modules
  util <- callModule(waterConservation, "water_districts", water_summary = util_summary,
                     water_monthly = water_byMonth, statewide_monthly = statewide_byMonth,
                     map_data = water_districts, id_field = 'PWS_ID', name_field = 'PWS_name',
                     statewide = reactive(vals$statewide))
  
  # output values
  
  #hcdata_byQuarter <- function(x) { x$data[[1]]$y <- x$data[[1]]$y / 4; x }
  
  output$energy_barchart <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Electricity Savings from Statewide Water Conservation vs. Total First-Year Electricity Savings from Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
               style = list(fontSize = '14px', useHTML = TRUE)) %>% 
      hc_xAxis(categories = c('Energy Efficiency Programs<br/>by End Use Category', 'Energy Savings Resulting<br>from Water Conservation')) %>% 
      hc_yAxis(title = list(text = "GWh Energy Saved"),
               stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
                                  formatter = JS('function() { return this.total.toFixed(1) + " GWh"; }'))) %>% 
      hc_series(appliance_data,
                hvac_data,
                indoorlighting_data,
                other_data,
                outdoorlighting_data,
                process_data,
                refrigeration_data,
                wholebuilding_data,
                waterenergy_data) %>% 
      hc_plotOptions(column = list(stacking = 'normal')) %>% 
      hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + this.y.toFixed(1) + ' GWh'; }"))
  })
  
  output$cost_barchart <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Cost of Statewide Water Conservation vs. Expenditures on Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
               style = list(fontSize = '14px', useHTML = TRUE)) %>% 
      hc_xAxis(categories = c('Energy Efficiency Programs<br/>by End Use Category', 'Water Conservation')) %>% 
      hc_yAxis(title = list(text = "Million Dollars"),
               stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
                                  formatter = JS('function() { return "$" + this.total.toFixed(1) + "M"; }'))) %>% 
      hc_series(appliance_cost_data,
                hvac_cost_data,
                indoorlighting_cost_data,
                other_cost_data,
                outdoorlighting_cost_data,
                process_cost_data,
                refrigeration_cost_data,
                wholebuilding_cost_data,
                waterenergy_cost_data) %>% 
      hc_plotOptions(column = list(stacking = 'normal')) %>% 
      hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + '$' + this.y.toFixed(1) + 'M'; }"))
  })
  
  output$costperkwh_barchart <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "column") %>% 
      hc_title(text = "Cost per kWh of Statewide Water Conservation vs. Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
               style = list(fontSize = '14px', useHTML = TRUE)) %>% 
      hc_xAxis(categories = c('Energy Efficiency Programs', 'Water Conservation')) %>% 
      hc_yAxis(title = list(text = "Dollars"),
               stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
                                  formatter = JS('function() { return "$" + this.total.toFixed(2); }'))) %>% 
      hc_series(ee_costperkwh_data,
                waterenergy_costperkwh_data) %>% 
      hc_plotOptions(column = list(stacking = 'normal')) %>% 
      hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + '$' + this.y.toFixed(2); }"))
  })
  
  savings <- reactive({
    
    if (vals$statewide) {
      ret <- util() %>%
        filter(selected) %>%
        summarise(gal_2013 = sum(gal_2013, na.rm = TRUE),
                  gal_cur = sum(gal_cur, na.rm = TRUE),
                  change_gal = sum(change_gal, na.rm = TRUE),
                  kWh_saved = sum(kWh_saved, na.rm = TRUE),
                  MWh_saved = sum(MWh_saved, na.rm = TRUE),
                  kg_CO2e_saved = sum(kg_CO2e_saved, na.rm = TRUE)) %>% 
        mutate(change_prop = change_gal / gal_2013,
               proportionChangeGoal = 0.25,
               sav_diff = change_prop - proportionChangeGoal)
    } else {
      ret <- util() %>%
        filter(selected) %>%
        left_join(util_summary, by = 'PWS_ID') %>% 
        group_by(PWS_ID, ei_kWh_1000m3, ghgFactor_kg_MWh) %>% 
        summarise(proportionChangeGoal = mean(proportionChangeGoal),
                  gal_2013 = sum(gal_2013),
                  gal_cur = sum(gal_cur)) %>% 
        mutate(change_gal = gal_2013 - gal_cur,
               change_prop = change_gal / gal_2013,
               sav_diff = change_prop - proportionChangeGoal,
               change_1000m3 = change_gal / 264172.052,
               kWh_saved = change_1000m3 * ei_kWh_1000m3,
               MWh_saved = kWh_saved / 1e3,
               kg_CO2e_saved = MWh_saved * ghgFactor_kg_MWh)
    }
    
    ret
  })
  
  output$savings <- reactive({
    sprintf('%.1f%% (goal: %.f%%)', savings()$change_prop * 100, savings()$proportionChangeGoal * 100)
  })
  
  output$vs_std <- reactive({
    sprintf('%+.1f%%', savings()$sav_diff * 100)
  })
  
  output$vs_std_label <- reactive({
    switch(as.character(sign(savings()$sav_diff)), '-1' = '(Missed Target)', '0' = '(Met Target)', '1' = '(Savings Exceeding Target)', 'Vs Target')
  })
  
  output$energy <- reactive({
    paste(format(round(savings()$MWh_saved), big.mark=",", scientific=FALSE), 'MWh')
  })
  
  output$ghg <- reactive({
    paste(format(round(savings()$kg_CO2e_saved / 1000), big.mark=",", scientific=FALSE), 'MT CO2e')
  })
  
  output$n_cars <- reactive({
    format(round(savings()$kg_CO2e_saved * 2.20462 / 9737.44), big.mark=",", scientific=FALSE)
  })
  
}
