# Module UI
data_summaryUI = function(id){
  ns = NS(id)
  tagList(
    ## MODEL DATA
    tags$p(),
    conditionalPanel(
      condition = 'output["par_ready"]',
      fluidRow(
        column(7,  style='padding-right:5px;',
          div(class='panel panel-default',
            div(class='panel-heading clearfix',
              h6('Table 1.1','Descriptive Statistics', 
                conditionalPanel(condition = 'output["df_mi"]', '(first imputed dataset)',
            style = 'display: inline;'),
                align = 'left', class='panel-title pull-left'),
              div(class='pull-right', id='heading_1_1')
            ),
            div(class='panel-body',
              DT::dataTableOutput(ns('describe')),
              h6(uiOutput(ns('describe_note'), inline = T))
            )
            # div(class='panel-footer',
            #   )
          )
        ),
        column(5,  style='padding-left:5px;',
          div(class='panel panel-default',
            div(class='panel-heading clearfix', 
              h6(align = 'left','Table 1.2', 'Design Summary', 
               tags$small( htmlOutput(ns('crosstab_warning'))),
                conditionalPanel(condition = 'output["df_mi"]', '(first imputed dataset)',
            style = 'display: inline;'),
                class='panel-title pull-left'),
              div(class='pull-right', id='heading_1_2')
              ),
            div(class='panel-body', 
              DT::dataTableOutput(ns('crosstab')),
              h6(uiOutput(ns('crosstab_note'))))
            # div(class='panel-footer', )
            
          )
        )
      )
    ),
    
    ## RAW DATA
    
    div(class = 'panel panel-default',
      div(class = 'panel-heading', 
        h6('Raw Data', class='panel-title pull-left'),
        
        conditionalPanel(class='pull-right',
          condition = 'output["par_ready"]',
          actionButton(ns('show_model_data'),'Model Data' , class='btn-sm', 
            `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
        ),
        div(class='clearfix')
        
      )
      ,
      div(class = 'panel-body',
        conditionalPanel(
          condition = sprintf('input["%s"] %% 2 == 1', ns('show_model_data')),
          DT::dataTableOutput(ns("model_data"))
        ),
        conditionalPanel(
          condition = sprintf('input["%s"] %% 2 == 0', ns('show_model_data')),
          DT::dataTableOutput(ns("raw_data"))    
        )
      )
    )
    
    
  )
}

# Module Server
data_summary = function(input, output, session, dataframe, parameter){
  
  ## RAW DATA
  output$raw_data = DT::renderDataTable(
    extensions = c('Scroller'),
    options = list(
      scrollX = TRUE,
      scrollY = 350,
      scroller = TRUE,
      defRender  = TRUE,
      # pageLength = 5,
      searching = FALSE,
      dom = 'tip'
    ),
    server = TRUE,
    selection = 'none',
    rownames = FALSE,
    expr = {
      validate(need(is.data.frame(dataframe()), message = FALSE))
      dataframe()
    })
  
  #### 
  data_to_describe = reactive({
    data = dataframe()
    if(!is.null(attr(data, 'mi_id'))){
      mi_id = data[,attr(data, 'mi_id')]
      data = data[mi_id == unique(mi_id)[1],]
    }
    return(data)
  })
  
  ## MODEL DATA
  output$model_data = DT::renderDataTable(server = FALSE,
    expr = {
      # validate(need(parameter(), message = 'Awaiting for Model Specification'))
      df = setNames(
        get_columns(dataframe(),
          c(if(!is.null(attr(dataframe(),'mi_id'))) attr(dataframe(),'mi_id'),
            parameter$treatment(),
            parameter$outcome(),
            parameter$assignment1(),
            if(parameter$is_frontier()) parameter$assignment2(),
            parameter$auxiliary())),
        c(sprintf('%s (Dataset ID)', attr(dataframe(),'mi_id')),
          sprintf('%s (Treatment)', parameter$treatment()),
          sprintf('%s (Outcome)', parameter$outcome()),
          sprintf('%s (Assignment 1)', parameter$assignment1()),
          if(parameter$is_frontier()) sprintf('%s (Assignment 2)', parameter$assignment2()),
          parameter$auxiliary())
      )
      # print(df)
      dt = DT::datatable(
        data = df,
        extensions = 'Scroller',
        options = list(
          scrollX = TRUE,
          defRender  = TRUE,
          scrollY = 350,
          scroller = TRUE,
          searching = FALSE,
          dom = 'tip'
        ),
        selection = 'none',
        rownames = FALSE
      )
      
      DT::formatStyle(
        DT::formatStyle(dt, 
          columns = which(names(df) %in% 
              c(parameter$outcome())),
          fontWeight = 'bold'),
        columns = which(names(df) %in% 
            c(parameter$assignment1(),
              if(parameter$is_frontier()) parameter$assignment2())),
        fontWeight = 'bold'
      )
    })
  
  ### INSPECTION OF USER DATA ###
  crosstab = reactive({
    warn = NULL

    
    res = withCallingHandlers(
      rddapp:::rd_type(
        data_to_describe(), 
        treat         = parameter$treatment(),
        assign_1      = parameter$assignment1(),
        cutoff_1      = parameter$cutoff1(),
        operator_1    = parameter$operator1(),
        assign_2      = if(parameter$is_frontier()) parameter$assignment2(),
        cutoff_2      = if(parameter$is_frontier()) parameter$cutoff2(),
        operator_2    = if(parameter$is_frontier()) parameter$operator2()
      ),
      warning = function(w) warn <<- append(warn, conditionMessage(w))
    )
    c(res, warning = warn)
  })
  
  output$crosstab = DT::renderDataTable(server = FALSE,
    expr = {
      header = tags$table(class = 'display',
        tags$thead(
          tags$tr(
            lapply(paste0('A', 1:(ncol(crosstab()$crosstab) - 3)), 
              tags$th, rowspan = 2),
            tags$th(colspan = 2, paste0('Treatment Receipt (',parameter$treatment(),')'), 
              style = 'text-align: center'),
            tags$th('π',rowspan = 2, style = 'text-align: center')
          ),
          tags$tr(
            lapply(c('control', 'treatment'), tags$th, align = 'center')
          )
        )
      )
      
      dt = DT::datatable(
        data = crosstab()$crosstab,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            "$('#heading_1_2').children().remove();",
            "this.api().table().buttons().container().appendTo( $('#heading_1_2') );",
            "$('.dt-button').removeClass('dt-button');",
            "$('.dt-buttons').addClass('btn-group').removeClass('dt-buttons');",
            "}"),        
          scrollX = TRUE,
          buttons = list(list(
            extend = 'csv',
            text = '<i class="fa fa-file-text-o"></i>',
            titleAttr = 'Download table as CSV',
            filename = 'table_1_2_design_summary',
            className = 'btn btn-default btn-sm'
          )),          
          scrollX = TRUE,
          searching = FALSE,
          ordering = FALSE,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = 1:(ncol(crosstab()$crosstab)-1)))
        ),
        rownames = FALSE,
        selection = 'none'
        ,container = header
      )
      dt = DT::formatString(dt, columns = (ncol(crosstab()$crosstab)-2):(ncol(crosstab()$crosstab)-1), prefix = 'n = ')
      dt = DT::formatRound(dt, columns = ncol(crosstab()$crosstab), digits = 2)
      return(dt)
    })
  
  output$crosstab_note = renderUI(expr = {
    span(em('Note.'), 
      'A1 =', paste0(parameter$assignment1(),'.'),
      ifelse(parameter$is_frontier(), 
        paste0('A2 = ', parameter$assignment2(),'.'),''),
      'π = Probability conditioning on treatment status.',
      'This is a', em(tolower(crosstab()$type)), 'design.'
    )
  })
  
  output$crosstab_warning = renderUI({
    if(!is.null(crosstab()$warning)){
      span(icon('exclamation-triangle'), crosstab()$warning, style='color:red')
    }
  })
  
  ## DESCRIPTIVE STATS
  describe = reactive({

    warn = NULL
    df = withCallingHandlers(
      summarize_model_data(
        data_to_describe(),
        parameter$treatment(),
        parameter$outcome(),
        parameter$assignment1(),
        assignment2 = if(parameter$is_frontier())  parameter$assignment2(),
        auxiliary = if(length(parameter$auxiliary()) > 0) parameter$auxiliary() 
      ),
      warning = function(w) warn <<- append(warn, conditionMessage(w))
    )
    list(df = df, warning = warn)
  })
  
  output$describe = DT::renderDataTable(server = FALSE,
    expr = {
      req(describe()$df)
      # print()
      header = tags$table(class = 'display',
        tags$thead(
          tags$tr(
            tags$th('',rowspan = 2),
            tags$th(em('N'),rowspan = 2, style = 'text-align: right'),
            tags$th(em('M'),rowspan = 2, style = 'text-align: right'),
            tags$th(em('SD'),rowspan = 2, style = 'text-align: right'),
            tags$th('Correlation', colspan = nrow(describe()$df) + 1, style = 'text-align: center')
          ),
          tags$tr(
            lapply(c('T','O', 'A1', 
              if(parameter$is_frontier()) 'A2',
              if(length(parameter$auxiliary())>0) sprintf('C%g', 1:length(parameter$auxiliary()))
            )[-nrow(describe()$df)], tags$th)
          )
        )
      )
      
      dt = DT::datatable(
        data = describe()$df,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            "$('#heading_1_1').children().remove();",
            "this.api().table().buttons().container().appendTo( $('#heading_1_1') );",
            "$('.dt-button').removeClass('dt-button');",
            "$('.dt-buttons').addClass('btn-group').removeClass('dt-buttons');",
            "}"),        
          scrollX = TRUE,
          buttons = list(list(
            extend = 'csv',
            text = '<i class="fa fa-file-text-o"></i>',
            titleAttr = 'Download table as CSV',
            filename = 'table_1_1_descriptive_statistics',
            className = 'btn btn-default btn-sm'
          )),         
          scrollY = TRUE,
          searching = FALSE,
          ordering = FALSE,
          dom = 't',
          columnDefs = list(list(className = 'dt-right', targets = 1:(ncol(describe()$df)-1)))
        ),
        rownames = TRUE,
        selection = 'none'
        ,
        container = header
      )
      
      dt = DT::formatRound(dt, columns = 2:(ncol(describe()$df)))
      return(dt)
      
    })
  
  output$describe_note = renderUI(expr = {
    
    span(em('Note.'),
                'T = Treatment, O = Outcome, A = Assignment, C = Covariate.',
                
      ifelse(!is.null(describe()$warning),
        span(describe()$warning, style = 'color:red'),''))
    
  })
  
  return(crosstab)
}
