
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
      mutate(perc_gt_RPHL = round(perc_gt_RPHL * 100, 1)) %>% 
      select(chemical, perc_gt_RPHL, n_violations)
    datatable(dat, filter = 'none', rownames = FALSE, colnames = c('Chemical', '% greater than RPH limit', '# violations'),
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
  
