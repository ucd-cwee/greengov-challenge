
function(input, output, session) {
  
  # for easier debugging
  if (islocal) source('modules/water_district_map.R')
  
  
  # session state --------------------------------------------------------------
  
  # handle selection of statewide vs utility view
  vals <- reactiveValues(statewide = TRUE)
  observeEvent(input$view_statewide, {vals$statewide <- TRUE})
  observeEvent(input$view_utility, {vals$statewide <- FALSE})
  
  
  # module ---------------------------------------------------------------------
  util <- callModule(waterConservation, "water_districts",
                     water_monthly = water_byMonth,
                     statewide_monthly = statewide_byMonth,
                     map_data = water_districts,
                     id_field = 'pwsid',
                     name_field = 'pwsname',
                     statewide = reactive(vals$statewide))
  
  
  # summary panels -------------------------------------------------------------
  
  # total savings for current selection (util/state & time period)
  savings <- reactive({

    if (vals$statewide) {
      ret <- util() %>%
        filter(selected) %>%
        summarise(MG_2013 = sum(MG_2013, na.rm = TRUE),
                  MG_cur = sum(MG_cur, na.rm = TRUE),
                  MG_saved = sum(MG_saved, na.rm = TRUE),
                  MWh_saved_all = sum(MWh_saved_all, na.rm = TRUE),
                  MT_CO2e_saved_all = sum(MT_CO2e_saved_all, na.rm = TRUE)) %>%
        mutate(change_prop = MG_saved / MG_2013,
               proportionChangeGoal = 0.25,
               sav_diff = change_prop - proportionChangeGoal)
    } else {
      ret <- util() %>%
        filter(selected) %>%
        group_by(pwsid, out_iou_kWh_mg, out_all_kWh_mg) %>%
        summarise(proportionChangeGoal = mean(ConservationStandard),
                  MG_2013 = sum(MG_2013),
                  MG_cur = sum(MG_cur)) %>%
        mutate(MG_saved = MG_2013 - MG_cur,
               change_prop = MG_saved / MG_2013,
               sav_diff = change_prop - proportionChangeGoal,
               kWh_saved_all = MG_saved * out_all_kWh_mg,
               MWh_saved_all = kWh_saved_all / 1e3,
               MT_CO2e_saved_all = MWh_saved_all * ghg_factor_kg_mwh / 100)
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
    paste(format(round(savings()$MWh_saved_all), big.mark = ",", scientific = FALSE), 'MWh')
  })

  output$ghg <- reactive({
    paste(format(round(savings()$MT_CO2e_saved_all), big.mark = ",", scientific = FALSE), 'MT CO2e')
  })

  output$n_cars <- reactive({
    format(round(savings()$MT_CO2e_saved_all / avg_car_mt_CO2), big.mark = ",", scientific = FALSE)
  })
  
  
  # summary charts -------------------------------------------------------------
  
  # #hcdata_byQuarter <- function(x) { x$data[[1]]$y <- x$data[[1]]$y / 4; x }
  # 
  # output$energy_barchart <- renderHighchart({
  #   highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Electricity Savings from Statewide Water Conservation vs. Total First-Year Electricity Savings from Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
  #              style = list(fontSize = '14px', useHTML = TRUE)) %>% 
  #     hc_xAxis(categories = c('Energy Efficiency Programs<br/>by End Use Category', 'Energy Savings Resulting<br>from Water Conservation')) %>% 
  #     hc_yAxis(title = list(text = "GWh Energy Saved"),
  #              stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
  #                                 formatter = JS('function() { return this.total.toFixed(1) + " GWh"; }'))) %>% 
  #     hc_series(appliance_data,
  #               hvac_data,
  #               indoorlighting_data,
  #               other_data,
  #               outdoorlighting_data,
  #               process_data,
  #               refrigeration_data,
  #               wholebuilding_data,
  #               waterenergy_data) %>% 
  #     hc_plotOptions(column = list(stacking = 'normal')) %>% 
  #     hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + this.y.toFixed(1) + ' GWh'; }"))
  # })
  # 
  # output$cost_barchart <- renderHighchart({
  #   highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Cost of Statewide Water Conservation vs. Expenditures on Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
  #              style = list(fontSize = '14px', useHTML = TRUE)) %>% 
  #     hc_xAxis(categories = c('Energy Efficiency Programs<br/>by End Use Category', 'Water Conservation')) %>% 
  #     hc_yAxis(title = list(text = "Million Dollars"),
  #              stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
  #                                 formatter = JS('function() { return "$" + this.total.toFixed(1) + "M"; }'))) %>% 
  #     hc_series(appliance_cost_data,
  #               hvac_cost_data,
  #               indoorlighting_cost_data,
  #               other_cost_data,
  #               outdoorlighting_cost_data,
  #               process_cost_data,
  #               refrigeration_cost_data,
  #               wholebuilding_cost_data,
  #               waterenergy_cost_data) %>% 
  #     hc_plotOptions(column = list(stacking = 'normal')) %>% 
  #     hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + '$' + this.y.toFixed(1) + 'M'; }"))
  # })
  # 
  # output$costperkwh_barchart <- renderHighchart({
  #   highchart() %>% 
  #     hc_chart(type = "column") %>% 
  #     hc_title(text = "Cost per kWh of Statewide Water Conservation vs. Energy IOU Efficiency Programs<br/><b>(Jul - Sep 2015)</b>",
  #              style = list(fontSize = '14px', useHTML = TRUE)) %>% 
  #     hc_xAxis(categories = c('Energy Efficiency Programs', 'Water Conservation')) %>% 
  #     hc_yAxis(title = list(text = "Dollars"),
  #              stackLabels = list(enabled = TRUE, style = list(fontWeight = 'bold', color = 'gray'),
  #                                 formatter = JS('function() { return "$" + this.total.toFixed(2); }'))) %>% 
  #     hc_series(ee_costperkwh_data,
  #               waterenergy_costperkwh_data) %>% 
  #     hc_plotOptions(column = list(stacking = 'normal')) %>% 
  #     hc_tooltip(formatter = JS("function () { return this.point.series.name + '<br/>' + '$' + this.y.toFixed(2); }"))
  # })

}
