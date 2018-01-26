# Module UI function
input_fileUI = function(id) {
  # Create a namespace function using the provided id
  ns = NS(id)
  tagList(
    div(class='panel panel-default',
      div(class='panel-heading clearfix',
        h5( "Data", class='panel-title pull-left'),
        
        conditionalPanel(condition = sprintf("input['%s'] != ''", ns('type')),
          actionButton(ns('controls'), class='pull-right btn-sm', label = NULL, icon = icon(name = 'cog'),
            `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
            title = 'options for data file')
        )
      ),
      div(class='panel-body',
        ## INPUT DATA UI START ##
        selectizeInput(ns('type'), 
          label = NULL,
          choices = list(
            'choose data file type' = '',
            "SPSS Dataset" = c('SAV' = 'sav'), 
            "Delimited Text File" = c('CSV' = 'csv'),
            "Example Data" =  c('CARE' = 'CARE')
          )
        ),
        # Uploaders
        conditionalPanel(
          condition = sprintf("['sav','csv'].indexOf(input['%s']) != -1", ns('type')),
          fileInput(ns('file'), 
            label = NULL
            #, accept = c('text/csv', 'text/comma-separated-values,text/plain')
          )
        ),
        uiOutput(ns('error_status'))
      ), 
      
      # Control Panel
      conditionalPanel(class='panel-footer',
        condition = sprintf("input['%s'] %% 2 != 0", ns('controls')),
        
          # Controls for CSV
          conditionalPanel(
            condition = sprintf("input['%s'] == 'csv'", ns('type')),
            
            fluidRow(
              # Quote
              column(6, style='padding-right:5px;',
                h6(
                  # icon('quote-left'),
                  "Quote"),
                selectInput(ns("csv_quote"), label = NULL, 
                  choices = c(
                    "Double" = "\"",
                    "Single" = "'",
                    "None" = " "
                  )
                )
              ),
              # Separator
              column(6, style='padding-left:5px;',
                h6(
                  # icon('columns'),
                  "Separator"),
                selectInput(ns("csv_separator"), label = NULL, 
                  choices = c(
                    "," = ",",
                    ";" = ";",
                    "Tab" = "\t",
                    'White spaces' = ' '
                  )
                )
              )
            ),
            fluidRow(
              # NA string
              column(6,  style='padding-right:5px;',
                h6(
                  # icon('minus-circle'),
                  'Missing'),
                textInput(ns('csv_missing'), 
                  label = NULL, 
                  value = '',
                  placeholder = 'Blank'
                )
              ),
              # Heading
              column(6, style='padding-left:5px;',
                h6(
                  # icon('tags'),
                  'Header'),
                selectInput(ns('csv_header'),
                  label = NULL,
                  choices = c(
                    'First line' = 'TRUE',
                    'None' = 'FALSE'
                  )
                )
              )
            ),
            hr()
          ),
          
          # Controls for SAV
          conditionalPanel(
            condition = sprintf("input['%s'] == 'sav'", ns('type')),
            checkboxGroupInput(ns('sav_options'),label = NULL,
              choices = c(
                'Use Value Lables' = 'sav_vallabel',
                'Use User\'s Missing Value' = 'sav_missing'
              )
            ),
            hr()
          ),
          
          # Controls for Multiple Imputation after loading.
          
          h6(
            # icon('random'),
            'Multiple Imputation ID'),
          selectizeInput(ns('mi_var'),
            label = NULL,
            choices = c('Upload Data First' = '')
          ),
          htmlOutput(ns('mi_var_msg'), inline= T)
        )
      
    )
  )

}

# Module server function
input_file = function(input, output, session, more_reactive = F) {
  
  filename = reactiveVal("")
  # The selected file, if any
  userFile = reactive({
    # If no file is selected, don't do anything
    validate(need(input$file, message = FALSE))
    input$file
  })
  
  
  
  dataframe <- reactive({
    req(input$type)
    if(input$type %in% c('csv','sav')){
      filename(userFile()$name)
      filepath = userFile()$datapath
      withProgress(
        message = 'Processing',
        detail = userFile()$name,
        value = NULL,
        expr = try(if(more_reactive) {
          switch(isolate(input$type),
            csv = read.csv(
              file = filepath,
              header = input$csv_header == 'TRUE',
              quote = ifelse(input$csv_quote == ' ', '', 
                isolate(input$csv_quote)),
              sep = ifelse(input$csv_separator == ' ', '', 
                isolate(input$csv_separator)),
              na.strings = input$csv_missing,
              numeral = 'no.loss',
              stringsAsFactors = F
            ),
            sav = foreign::read.spss(
              file = filepath,
              use.value.labels = 'sav_vallabel' %in% input$sav_options,
              to.data.frame = T,
              use.missings = 'sav_missing' %in% input$sav_options
            )
          )
        } else {
          switch(isolate(input$type),
            csv = read.csv(
              file = filepath,
              header = isolate(input$csv_header) == 'TRUE',
              quote = ifelse(isolate(input$csv_quote) == ' ', '', 
                isolate(input$csv_quote)),
              sep = ifelse(isolate(input$csv_separator) == ' ', '', 
                isolate(input$csv_separator)),
              na.strings = isolate(input$csv_missing,
                numeral = 'no.loss')
            ),
            sav = foreign::read.spss(
              file = filepath,
              use.value.labels = isolate(input$sav_vallabel),
              to.data.frame = T,
              use.missings = isolate(input$sav_missing)
            )
          )
        })
      )
    } else {
      filename(input$type)
      get(input$type)
      # switch(input$type,
      #   s2nocov = s2nocov,
      #   s2cov = s2cov,
      #   s1nocov = s1nocov,
      #   s1cov = s1cov,
      #   f2nocov = f2nocov,
      #   f2cov = f2cov,
      #   f1nocov = f1nocov,
      #   f1cov = f1cov,
      #   CARE = CARE,
      #   mis1nocov = mis1nocov)
    }
  })
  
  # Show Post Hoc Controls for Multiple Imputed Files OR Error Message
  output$error_status = renderUI({
    ns = session$ns
    validate(need(class(dataframe()) == 'try-error', message = FALSE))
    wellPanel(style='padding:15px;',
      h6(icon('exclamation-triangle'),'Unable to parse file. Wrong format?', 
        align = 'center') 
    )
  })
  
  # We can run observers in here if we want to
  observe({
    # msg <- sprintf("File %s was uploaded in %s", userFile()$name, userFile()$datapath)
    validate(need(is.data.frame(dataframe()), message = FALSE))
    updateSelectizeInput(session, 'mi_var', 
      choices = c('Choose Imputation ID' = '', names(dataframe())),
      selected = intersect(names(dataframe()), input$mi_var)
    )
    # cat(msg, "\n")
  })
  
  dataframe_final = reactive({
    req(input$type, is.data.frame(dataframe()))
    
    df = dataframe()
    
    if(input$mi_var != '') {
      req(input$mi_var %in% names(df))
      mi_n = length(unique(df[, input$mi_var]))
      same_size = length(unique(table(df[, input$mi_var]))) == 1
      output$mi_var_msg = renderUI(
        if(mi_n == 1) {
          tags$small(icon('exclamation-triangle'), 'single dataset', style='color:red')
        } else {
          if(!same_size) {
            tags$small(icon('exclamation-triangle'), 'data sizes are not identical', style='color:red')
          } else {
            if(nrow(df)/mi_n == 1) {
              tags$small(icon('exclamation-triangle'), 'data sizes are not identical', style='color:red')
            }
          } 
        }        
      )

      req(mi_n > 1, same_size, nrow(df)/mi_n > 1)
      attr(df, 'mi_id') <- input$mi_var

    } 
    
    return(df)
  })
  
  return(list(filename = filename, dataframe = dataframe_final))
}