
fluidPage(
  
  # Application title
  titlePanel("Test"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      waterConservationInput1('water_districts'),
      waterConservationInput2('water_districts')
    ),
    
    mainPanel(
      waterConservationOutput1('water_districts'),
      waterConservationOutput2('water_districts'),
      textOutput('savings'),
      textOutput('vs_std'),
      textOutput('ei'),
      textOutput('ghg')
    )
  )
)
