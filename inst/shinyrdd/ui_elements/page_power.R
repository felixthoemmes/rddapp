fluidRow(
  # Sidebar ()
  column(width = 5,
    input_powerUI('input_power')
  ),
  
  # Main Panel
  column(width = 7,
    tabsetPanel(
      tabPanel("Monte Carlo Simulation", icon = icon('random'), 
        simulate_powerUI('simulate_power')
      )
      ,
      tabPanel("R Code", icon = icon('code'),
        power_codeUI('power_code')
        )
    )
  )
)