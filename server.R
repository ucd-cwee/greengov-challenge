
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
  
  callModule(waterQuality, "water_quality", quality_data = water_quality_summary,
             violation_data = violations_summary, id_field = 'PWS_ID',
             current_id = reactive(input$`water_districts-utility`))
  
  # output values
  output$ghg_pie <- renderHighchart({
    highchart() %>% 
      hc_chart(type = "pie") %>% 
      hc_title(text = "CA GHG Emissions By Sector") %>% 
      hc_series(list(name = "Sector",
                     size = "70%",
                     data = ghg_by_sector,
                     dataLabels = list(distance = 10, formatter = JS("function () { return this.point.name + '<br/>' + this.y + '%'; }")))) %>% 
      hc_tooltip(formatter = JS("function () { return this.point.name + '<br/>' + this.y + '%'; }"))
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
    switch(as.character(sign(savings()$sav_diff)), '-1' = '(Short of Target)', '0' = '(Met Target)', '1' = '(Savings Exceeding Target)', 'Vs Target')
  })
  
  output$energy <- reactive({
    paste(format(round(savings()$MWh_saved), big.mark=",", scientific=FALSE), 'MWh')
  })
  
  output$ghg <- reactive({
    paste(format(round(savings()$kg_CO2e_saved / 1000), big.mark=",", scientific=FALSE), 'MT CO2e')
  })
  
}
