## Start Server
function(input, output, session) {
  ################
  ## MODEL PAGE ##
  ################
  
  ## Initialize dataframe
  inputfile <- callModule(input_file, "input_file", more_reactive = MORE_REACTIVE)
  
  # Signal UIs when file is ready
  output$df_ready <- reactive(is.data.frame(inputfile$dataframe()))
  output$df_mi <- reactive({
    req(is.data.frame(inputfile$dataframe()))
    !is.null(attr(inputfile$dataframe(), 'mi_id'))
  })
  outputOptions(output, 'df_ready', suspendWhenHidden = F)
  outputOptions(output, 'df_mi', suspendWhenHidden = F)
  
  ## Initialize parameter
  parameter <- callModule(input_parameter, "input_parm", dataframe = inputfile$dataframe) 

  # Signal UIs when there are two assignments
  output$par_ready <- reactive({req(parameter$assignment1(), parameter$cutoff1(), 
    parameter$treatment(), parameter$outcome()); TRUE})
  outputOptions(output, 'par_ready', suspendWhenHidden = F)
  output$is_frontier <- reactive({req(parameter$assignment2(), parameter$cutoff2()); TRUE})
  outputOptions(output, 'is_frontier', suspendWhenHidden = F)
  output$has_auxiliary <- reactive({length(parameter$auxiliary()) > 0})
  outputOptions(output, 'has_auxiliary', suspendWhenHidden = F)
  
  ## Initialize data summary
  # Show Data Summary, which also determines model type
  model_type <- callModule(data_summary, "data_summary", dataframe = inputfile$dataframe, 
    parameter = parameter)
  output$model_type <- reactive(model_type()$type)
  outputOptions(output, 'model_type', suspendWhenHidden = F)

  ## Initialize assumption check
  checks <- callModule(assumption_check, "assumption_check", dataframe = inputfile$dataframe, 
    parameter = parameter)

  ## Initialize model estiamte
  result <- callModule(model_estimate, "model_estimate", dataframe = inputfile$dataframe, 
    parameter = parameter, model_type = model_type)

  ## Initialize sensitivity analysis
  sensitivities <- callModule(sensitivity_analysis, "sensitivity_analysis", 
    dataframe = inputfile$dataframe, parameter = parameter, result = result)

  # Initiatize model code
  code <- callModule(model_code, 'model_code', inputfile = inputfile,
    parameter = parameter, result = result)

  ################
  ## POWER PAGE ##
  ################
 
  ## Initialized power input
  pw_pars <- callModule(input_power, "input_power") 
  
  ## Simulate power
  pw_sims <- callModule(simulate_power, 'simulate_power', parameter = pw_pars)
  
  ## Initialize power code
  pw_code <- callModule(power_code, 'power_code', 
    parameter = pw_pars)
}  
## End Server