
# water district leaflet map
library("dplyr")
library("DT")

# module UI functions
# ...

waterQualityOutput1 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return output
  dataTableOutput(ns("quality_summary"))
}

waterQualityOutput2 <- function(id) {
  
  # create namespace
  ns <- NS(id)
  
  # return output
  dataTableOutput(ns("violations_summary"))
}


# module server function
waterQuality <- function(input, output, session,
                         quality_data, violation_data, id_field, current_id) {
  
  output$quality_summary <- renderDataTable({
    dat <- quality_data[quality_data[[id_field]] == current_id(),] %>% 
      mutate(average_value = round(average_value, 1)) %>% 
      select(chemical, average_value, unit)
    datatable(dat, filter = 'none', rownames = FALSE, colnames = c('Chemical', 'measured value', 'unit'),
              style = 'bootstrap', selection = 'none', options = list(dom = 't', ordering = FALSE),
              class = 'table-condensed table-bordered table-striped table-hover')
  })
  
  output$violations_summary <- renderDataTable({
    dat <- violation_data[violation_data[[id_field]] == current_id(),] %>%
      filter(!is.na(PWS_ID)) %>% 
      select(violation_type, violations_count) %>% 
      arrange(desc(violations_count))
    datatable(dat, filter = 'none', rownames = FALSE, colnames = c('Type', '# of violations'),
              style = 'bootstrap', selection = 'none', options = list(dom = 't', ordering = FALSE),
              class = 'table-condensed table-bordered table-striped table-hover')
  })
  
  NULL
}
  
