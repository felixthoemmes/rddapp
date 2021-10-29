fluidRow(
  # Sidebar ()
  column(width = 3,
    input_fileUI('input_file'),
    input_parameterUI('input_parm')
  ),
  
  # Main Panel
  column(width = 9,
    
    # Show when `df_ready` (CURRENT: ALWAYS SHOW)
    conditionalPanel(
      condition = "output['df_ready']",
      # condition = 'true',
      
      tabsetPanel(id = 'model_tabsets',
        tabPanel("Data", icon = icon('table'), 
          # helpText('I show data in table (maybe the rawdata).'),
          # DT::dataTableOutput("raw_data")
          data_summaryUI('data_summary')
        ),
        
        tabPanel("Assumptions", icon = icon('check-square'),
          conditionalPanel(
            condition = "output['model_type'] != 'UNDEFINED' && output['par_ready']",
            assumption_checkUI('assumption_check')
          )
        ),
        
        tabPanel("Estimates", icon = icon('bar-chart'),
          conditionalPanel(
            condition = "output['model_type'] != 'UNDEFINED' && output['par_ready']",
            model_estimateUI('model_estimate')
          )
        ),
        
        tabPanel("Sensitivities", icon = icon('heartbeat'),
          conditionalPanel(
            condition = "output['model_type'] != 'UNDEFINED' && output['par_ready']",
            sensitivity_analysisUI('sensitivity_analysis')
          )
        )
        ,
        tabPanel("R Code", icon = icon('code'),
          conditionalPanel(
            condition = "output['model_type'] != 'UNDEFINED' && output['par_ready']",
            model_codeUI('model_code')
          )
        )
      )
    ),
    
    # Show when !`df_ready`
    conditionalPanel(
      condition = "!output['df_ready']"
      
      # NOTHING IS DONE YET.
      
    )
  )
)
