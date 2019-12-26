# Module UI
input_parameterUI = function(id){
  ns = NS(id)
  
  conditionalPanel(
    condition = sprintf("output['df_ready']"),
    # condition = 'true',
    
    ## OUTCOME
    div(class='panel panel-default',
      div(class='panel-heading clearfix', 
        h5('Outcome', class='panel-title pull-left'),
        actionButton(ns('outcome_opt_btn'), label = NULL, icon = icon(name = 'cog'),
          class='pull-right btn-sm', `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
          title = 'options for outcome')
      ),
      
      div(class='panel-body',
        htmlOutput(ns('outcome_errmsg'), class='pull-right'),
        selectizeInput(ns('outcome'), NULL, choices = NULL)
      ),
      
      conditionalPanel(class='panel-footer',
        condition = sprintf("input['%s'] %% 2 == 1", ns('outcome_opt_btn')),
        
        h6('Kernel for local linear fitting'),
        selectizeInput(ns('kernel_type'),
          label = NULL,
          choices = c('Triangular' = 'triangular',
            'Rectangular' = 'rectangular',
            'Epanechnikov' = 'epanechnikov',
            'Quartic' = 'quartic', 
            'Triweight'= 'triweight', 
            'Tricube'='tricube', 
            'Gaussian'='gaussian',
            'Cosine'='cosine')
        ),
        
        h6('Type of SE'),
        selectizeInput(ns('se_type'),
          label = NULL,
          choices = list('Heteroskedasticity-Consistent' = 
              c('HC1', 'HC0', 'HC2', 'HC3', 'HC4', 'HC4m', 'HC5'), 
            'Clustering-Robust' = list('One-way Cluster' = 'cluster'),
            'Regular' = list('Constant Variance' = 'const'))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'cluster'", ns('se_type')),
          h6('Cluster ID', class='pull-left'),
          div(class='pull-right', htmlOutput(ns('cluster_id_errmsg'))),
          selectizeInput(ns('cluster_id'),
            label = NULL,
            choices = NULL
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
          h6('SE and CI for frontier estimate', class='pull-left'),
          div(class='input-group form-group shiny-input-container',
            span(class='input-group-addon', style = 'padding: 0 10px;',
              tags$input(id=ns('se_frontier_boot'), type = "checkbox")
              ),
            span(class='input-group-addon', tags$small('Bootstrap with'),
              style = 'border-right: 0px;border-left: 0;padding-left: 0px;'),
            numericInput(ns('se_frontier_boot_n'), label = NULL, width = '100%',
              min = 10, step = 1, value = 10),
            span(class = 'input-group-addon', tags$small('samples'))
          )
        ),
        div(
          h6('Auxiliary Variables', class='pull-left'),
          htmlOutput(ns('auxiliary_errmsg'), class='pull-right')
        ),
        selectizeInput(ns('auxiliary'), NULL, choices = NULL, multiple = T)
      )
      
    ),
    
    
    ## ASSIGNMENTS
    div(class='panel panel-default',
      div(class='panel-heading clearfix', 
        h5('Treatment Design', class='panel-title pull-left'),
        div(class='btn-group pull-right', 
          actionButton(ns('allow_frontier'), label = NULL, 
            icon = icon('plus-square'), class='btn-sm',
            `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
            title = 'allow a secondary assignment'),
          actionButton(ns('design_opt_btn'), class='btn-sm',label = NULL,icon = icon('cog'),
            `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
            title = 'options for experiment design')
        )
      ),
      div(class='panel-body',
        h6(span('Assign treatment if', class='badge badge-default')),
        div(style='margin-bottom:-10px',
          selectizeInput(ns('assignment1'), label = NULL, choices = NULL)
        ),
        div(class='input-group',
          span(class='input-group-btn', 
            actionButton(ns('operator1'), label= '>')
          ),
          numericInput(ns('cutoff1'), label = NULL, value = NA, min = NA, max = NA, width='100%'),
          htmlOutput(ns('assignment1_errmsg'), class='input-group-addon tail', inline= T)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
          h6(span('OR', class='badge')),
          div(style='margin-bottom:-10px',
            selectizeInput(ns('assignment2'), label = NULL, choices = NULL)
          ),
          div(class='input-group',
            span(class='input-group-btn', 
              actionButton(ns('operator2'), label= '>')
            ),
            numericInput(ns('cutoff2'), label = NULL, value = NA, min = NA, max = NA, width='100%'),
            htmlOutput(ns('assignment2_errmsg'), class = 'input-group-addon tail', inline = T)
          )
        ),
        ## Choosing Treatment
        hr(),
        div(
          h6(class='badge pull-left','Treatment Receipt'),
          div(class='pull-right',htmlOutput(ns('treatment_errmsg')))
        ),
        selectizeInput(ns('treatment'), NULL, choices = NULL)
      ),
      conditionalPanel(class='panel-footer',condition = sprintf("input['%s'] %% 2 == 1", ns('design_opt_btn')),
        checkboxInput(ns('treat_cutoff'),'Treatment at the cutoff(s)', value = T)
      )
    )
  )
}

# Module Server

input_parameter = function(input, output, session, dataframe){
  
  update_cutoff_inputs = function(assignment_widget, cutoff_widget, id){
    
    req(is.numeric(get_columns(dataframe(), assignment_widget)))
    assignment_data = get_columns(dataframe(), assignment_widget)
    req(length(unique(assignment_data))>2)
    
    current_cutoff = isolate(cutoff_widget)
    
    cutoff_range =
      range(setdiff(
        x = assignment_data,
        y = range(assignment_data)
      ))
    
    min_discrepancy = min(diff(sort(unique(assignment_data))), na.rm = T)
    fmt = format.info(as.numeric(min_discrepancy), 1)
    integer_len = fmt[1] - fmt[2] - 1
    decimal_len = fmt[2]
    
    cutoff_step = if(decimal_len>0) 10^(-decimal_len) else 10^(integer_len)
    
    cutoff_default =
      round(median(assignment_data, na.rm = T), digits = format.info(cutoff_step)[2])
    
    updateNumericInput(session, id,
      value =
        if(is.na(current_cutoff) | 
            current_cutoff <= cutoff_range[1] | 
            current_cutoff >= cutoff_range[2])
          cutoff_default else current_cutoff,
      min = cutoff_range[1],
      max = cutoff_range[2],
      step = cutoff_step
    )
  }
  
  
  # update operator
  
  observe(priority = 1, x = {
    label_options = if(input$treat_cutoff) c('\u2265','\u2264') else c('>','<')
    updateActionButton(session, 'operator1', 
      label = label_options[input$operator1 %% 2 + 1]
    )
    updateActionButton(session, 'operator2', 
      label = label_options[input$operator2 %% 2 + 1]
    )
  })
  
  ## Initialize Variable Options 
  all_vars = reactive({
    req(is.data.frame(dataframe()))
    return(
      setdiff(names(dataframe()), attr(dataframe(), 'mi_id')
      )
    )
  })
  
  # remain_vars = reactive({
  #   return(
  #     setdiff(
  #       x = all_vars(), 
  #       y = c(input$assignment1, input$assignment2,
  #         input$auxiliary, input$outcome, input$treatment, input$cluster_id)
  #     )
  #   )
  # })
  
  ## Ensure Mutual Exclusive Selections
  observe({
    
    current_selection = intersect(isolate(input$cluster_id), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$auxiliary, input$assignment2,input$assignment1,
        input$outcome, input$treatment)
    )
    
    updateSelectizeInput(session, 'cluster_id',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe())),
      selected = current_selection)
    
    if(input$se_type == 'cluster') {
      if(length(current_selection)>0) {
        
        if(min(table(get_columns(dataframe(),  input$cluster_id))) > 1) 
          output$cluster_id_errmsg = renderUI(h6(''))
        else 
          output$cluster_id_errmsg = renderUI(h6(icon('exclamation-triangle'), 'singlton',style='color:red'))
        validate(need(min(table(get_columns(dataframe(),  input$cluster_id))) > 1, message =F))
      } else {
        output$cluster_id_errmsg = renderUI(h6(icon('exclamation-triangle'), 'required', style='color:red'))
      }
    }
  })
  
  observe({
    current_selection = intersect(isolate(input$treatment), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$auxiliary, input$assignment1,
        input$outcome, input$assignment2, input$cluster_id)
    )
    
    updateSelectizeInput(session, 'treatment',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe()),  
        c('actual treatment' = '')),
      selected = current_selection)
    
    
    # treatment_data = get_columns(dataframe(),  current_selection)
    # if(length(current_selection)>0) {
    #   if(is.numeric(treatment_data)) {
    #     if(all(sort(unique(na.omit(treatment_data))) == 0:1)) 
    #       output$treatment_errmsg = renderUI(h6(''))
    #     else 
    #       output$treatment_errmsg = renderUI(h6(icon('exclamation-triangle'), 'non-binary',class='badge', style='background-color:red'))
    #   } else {
    #     output$treatment_errmsg = renderUI(h6(icon('exclamation-triangle'), 'non-numeric',class='badge', style='background-color:red'))
    #   }
    # }
  })
  
  observe({
    
    current_selection = intersect(isolate(input$outcome), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$auxiliary, input$assignment1,
        input$treatment, input$assignment2, input$cluster_id)
    )
    
    updateSelectizeInput(session, 'outcome',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe()),  
        c('outcome variable' = '')),
      selected = current_selection)
    
  }) 
  
  observe({
    
    current_selection = intersect(isolate(input$auxiliary), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$assignment1, input$assignment2,
        input$outcome, input$treatment, input$cluster_id)
    )
    
    updateSelectizeInput(session, 'auxiliary',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe()),  
        c('covariates for statistical control' = '')),
      selected = current_selection)
    
    output$auxiliary_errmsg = renderUI(h6('(optional)'))
  })
  
  # ASSIGNMENT1 AND CUTOFF1
  observe ({
    current_selection = intersect(isolate(input$assignment1), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$auxiliary, input$assignment2,
        input$outcome, input$treatment, input$cluster_id)
    )
    
    updateSelectizeInput(session, 'assignment1',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe()),  
        c('primary assignment' = '')),
      selected = current_selection)
    
    req(input$assignment1)
    assignment_data = get_columns(dataframe(),  isolate(input$assignment1))
    
    
    
    update_cutoff_inputs(isolate(input$assignment1), input$cutoff1, 'cutoff1')
    
  })
  
  
  # ASSIGNMENT2 AND CUTOFF2
  
  observe({
    current_selection = intersect(isolate(input$assignment2), all_vars())
    
    remain_vars = setdiff(
      x = all_vars(), 
      y = c(input$auxiliary, input$assignment1,
        input$outcome, input$treatment, input$cluster_id)
    )
    
    updateSelectizeInput(session, 'assignment2',
      choices = c(vars2selectChoices(c(current_selection, remain_vars), dataframe()),  
        c('secondary assignment' = '')),
      selected = current_selection)
    
    if((input$allow_frontier %% 2 == 1) & length(current_selection) == 0)
      output$assignment2_errmsg = renderUI(tags$small(icon('exclamation-triangle'), 'required', style='color:red;padding: 10px;'))
    
    req(input$assignment2)
    update_cutoff_inputs(isolate(input$assignment2), input$cutoff2, 'cutoff2')
    
  })
  
  parameter = reactiveValues()
  parameter$treatment = reactive({
    req(input$treatment)
    treatment_data = get_columns(dataframe(), input$treatment)
    output$treatment_errmsg = renderUI({
      if(is.numeric(treatment_data)) {
        if(all(sort(unique(na.omit(treatment_data))) == 0:1)) NULL
        else h6(icon('exclamation-triangle'), 'non-binary',class='badge', style='background-color:red')
      } else {
        h6(icon('exclamation-triangle'), 'non-numeric',class='badge', style='background-color:red')
      }
    })
    req(all(sort(unique(na.omit(treatment_data))) == 0:1))
    input$treatment
  })
  parameter$outcome = reactive({
    req(input$outcome,
      input$outcome %in% names(dataframe()))

    outcome_data = get_columns(dataframe(),  input$outcome)
    output$outcome_errmsg = renderUI({
      if(!is.numeric(outcome_data)) {
        h6(icon('exclamation-triangle'), 'non-numeric', style='color:red')
      } else {
        if(length(unique(outcome_data))<=2)
          h6(class='badge',icon('exclamation-triangle'), 'discrete values ≤', length(unique(outcome_data)), style='background-color:red')
        else NULL
      }
    })
    input$outcome
  })
  parameter$assignment1 = reactive({
    req(input$assignment1,
      input$assignment1 %in% names(dataframe()))
    assignment_data = get_columns(dataframe(),  isolate(input$assignment1))
    output$assignment1_errmsg = renderUI({
      if(!is.numeric(assignment_data)) {
        tags$small(icon('exclamation-triangle'), 'non-numeric', style='color:red;padding: 10px;')
      } else {
        if (length(unique(assignment_data))<3) 
          tags$small(icon('exclamation-triangle'), 'discrete values ≤', length(unique(assignment_data)) , style='color:red;padding: 10px;')
        else  NULL
      }
    })
    input$assignment1
  })
  parameter$cutoff1_range = reactive({
    c(sort(get_columns(dataframe(), parameter$assignment1()))[2],
      sort(get_columns(dataframe(), parameter$assignment1()), decreasing = T)[2]
      )
  })
  parameter$cutoff1 = reactive({
    req(
      is.numeric(input$cutoff1), 
      input$cutoff1<= parameter$cutoff1_range()[2],
      input$cutoff1>= parameter$cutoff1_range()[]
    )
    input$cutoff1
  })
  parameter$operator1 = reactive({
    if(input$operator1 %% 2 == 0)
      operator = 'g' else operator = 'l'
      paste0(operator, ifelse(input$treat_cutoff, 'eq',''))
  })
  parameter$is_frontier = reactive(input$allow_frontier %% 2==1)
  parameter$assignment2 = reactive({
    req(parameter$is_frontier() == TRUE, 
      input$assignment2,
      parameter$assignment1(),
      input$assignment2 %in% names(dataframe()))
    assignment_data = get_columns(dataframe(),  isolate(input$assignment2))
    output$assignment2_errmsg = renderUI({
      if(!is.numeric(assignment_data)) {
        tags$small(icon('exclamation-triangle'), 'non-numeric', style='color:red;padding: 10px;')
      } else {
        if (length(unique(assignment_data))<3) 
          tags$small(icon('exclamation-triangle'), 'discrete values ≤', length(unique(assignment_data)), style='color:red;padding: 10px;')
        else NULL
      }
    })
    
    input$assignment2
  })
   parameter$cutoff2_range = reactive({
    c(sort(get_columns(dataframe(), parameter$assignment2()))[2],
      sort(get_columns(dataframe(), parameter$assignment2()), decreasing = T)[2]
      )
  })
  parameter$cutoff2 = reactive({
    req(
      is.numeric(input$cutoff1),
      input$cutoff2, 
      input$cutoff2<= parameter$cutoff2_range()[2],
      input$cutoff2>= parameter$cutoff2_range()[1]
    )  
    input$cutoff2
  })
  parameter$operator2 = reactive({
    if(input$operator2 %% 2 == 0)
      operator = 'g' else operator = 'l'
      paste0(operator, ifelse(input$treat_cutoff, 'eq',''))
  })
  parameter$auxiliary = reactive({
    if(try(all(sapply(get_columns(dataframe(), input$auxiliary), is.numeric))) != T)
      output$auxiliary_errmsg = renderUI(h6(icon('exclamation-triangle'), 'non-numeric',style='color:red'))
    
    input$auxiliary
  })
  
  parameter$formula = reactive({
    lhs_str = parameter$outcome()
    cov_str = paste(parameter$auxiliary(), collapse = ' + ')
    
    if(!parameter$is_frontier()) {
      rhs_str = paste(parameter$assignment1(), parameter$treatment(), sep = ' + ')
    } else {
      rhs_str = paste(parameter$assignment1(),parameter$assignment2(), parameter$treatment(), sep = ' + ')
    }
    
    fml_str = sprintf("%s ~ %s ", lhs_str, rhs_str)
    if(length(parameter$auxiliary()) > 0) 
      fml_str = paste0(fml_str,  ' | ', cov_str)  
    
    Formula::as.Formula(fml_str)
  })
  parameter$se_type = reactive(input$se_type)
  parameter$kernel_type = reactive(input$kernel_type)
  parameter$cluster_id = reactive({
    if(input$se_type=='cluster') 
      req(input$cluster_id)
    input$cluster_id})
  
  parameter$se_frontier_boot = reactive(input$se_frontier_boot)
  parameter$se_frontier_boot_n = reactive(input$se_frontier_boot_n)
  
  return(parameter)
}



vars2selectChoices = function(vars, data){
  vars = setdiff(vars, c(''))
  nameit = function(lst) setNames(lst, lst[[1]])
  vars = sapply(data[vars], is.numeric)
  df = data.frame(var = names(vars), type = vars, stringsAsFactors = F)
  named_list = sapply(split.data.frame(df['var'], df$type), c)
  named_list = named_list[sort(names(named_list), decreasing = T)]
  named_list[sapply(named_list, length) == 1] = lapply(named_list[sapply(named_list, length) == 1], list)
  named_list[sapply(named_list, length) == 1] = lapply(named_list[sapply(named_list, length) == 1], nameit)
  nms = names(named_list)
  nms = sub('.var','', nms, fixed = T)
  nms = sub('TRUE','numeric', nms, fixed = T)
  nms= sub('FALSE','non-numeric',nms, fixed = T)
  
  result = setNames(named_list, nms)
  return(result)
}
