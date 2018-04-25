assumption_checkUI = function(id){
  ns = NS(id)
  tagList(
    ## 1ST ROW
    p(),
    div(class='panel panel-default',
      div(class='panel-heading clearfix', 
        h6(class='panel-title pull-left',
          'Figure 2.1', 'Sorting Test', 
          conditionalPanel(condition = 'output["df_mi"]', '(first imputed dataset)',
            style = 'display: inline;')
        ),
        div(class='btn-group pull-right',
          downloadLink(ns('assumption_plot_png'), icon('file-image-o'),
                       label = NULL, class='btn btn-default btn-sm',
                       title = 'Download plot as PNG'),  
          downloadLink(ns('assumption_plot_svg'), icon('file-o'),
                       label = NULL, class='btn btn-default btn-sm',
                       title = 'Download plot as SVG'),  
          downloadLink(ns('assumption_plot_pdf'), icon('file-pdf-o'),
            label = NULL, class='btn btn-default btn-sm',
            title = 'Download plot as PDF'),
          downloadLink(ns('assumption_plot_csv'),label = NULL, 
            class='btn btn-default btn-sm', 
            icon('file-text-o'),
            title = 'Download simulated results as CSV')
        )
        
      ),
      div(class='panel-body',
        fluidRow(
          column(8, plotOutput(ns('sort_plot'), height = '375px')),
          column(4,
            h6(class='badge','Assignment [Cutoff]'),
            selectInput(ns('sort_assignment'), label = NULL, choices = c()),
            div(class='input-group  input-group-sm', style='margin-bottom:5px',
              span(class='input-group-addon', style='width:30%', 'bin size'),
              numericInput(ns('sort_bins'), label = NULL, value = NULL, min = 0, 
                max = NA, step = .01, width = '100%'),
              span(class = 'input-group-addon', inline = T, style='padding:2.5px;')
            ),
            div(class='input-group input-group-sm',
              span(class='input-group-addon', style='width:30%','band width'),
              numericInput(ns('sort_banw'), label = NULL, value = NULL, min = 0, 
                max = NA, step = .01, width = '100%'),
              span(class = 'input-group-addon', inline = T, style='padding:2.5px;')
            ),
            hr(),
            h6(class='badge', 'Test Summary'),
            tableOutput(ns('sort_table'))
            
          )
        )
      )
    )  ,
    
    
    ## 2ND ROW
    div(class='panel panel-default',
      div(class='panel-heading clearfix', 
        h6('Table 2.1', class='panel-title pull-left', 'Attrition Analysis',
          conditionalPanel(condition = 'output["df_mi"]', '(first imputed dataset)',
            style = 'display: inline;')
          ),
        div(class='pull-right', id = 'heading_2_1')
        ),
      div(class='panel-body',
        DT::dataTableOutput(ns('attrition_table')),
        h6(em('Note.'),
          uiOutput(ns('attrition_table_note'), inline = T))
      )
      # div(class='panel-footer', 
      #   )
    )
  )
}

assumption_check = function(input, output, session, dataframe, parameter){
  
    data_to_describe = reactive({
    data = dataframe()
    if(!is.null(attr(data, 'mi_id'))){
      mi_id = data[,attr(data, 'mi_id')]
      data = data[mi_id == unique(mi_id)[1],]
    }
    return(data)
  })
  
  ## ATTRITION 
  attrition = reactive({
    rddapp:::attr_check(
        x1 = get_columns(data_to_describe(), parameter$assignment1()),
        y =  get_columns(data_to_describe(), parameter$outcome()),
        t =  get_columns(data_to_describe(), parameter$treatment()),
        x2 = if(parameter$is_frontier()) get_columns(data_to_describe(), parameter$assignment2())
      )
  })
  
  output$attrition_table = DT::renderDataTable(server = F,
    expr = {
      # DEFINE ROW NAMES
      dictionary = c(
        overallt = 'Total Sample Size',
        overallmisst = sprintf('- Treatment (%s)', parameter$treatment()),
        overallmissy = sprintf('- Outcome (%s)', parameter$outcome()),
        overallmissx1 = sprintf('- Assignment 1 (%s)', parameter$assignment1()),
        overallmissx2 = if(parameter$is_frontier()) sprintf('- Assignment 2 (%s)', parameter$assignment2()),
        missingness = 'Missingness'
      )
      if(!parameter$is_frontier()) 
        dictionary['overallmissx1'] = sprintf('- Assignment (%s)', parameter$assignment1())
      
      # TRANSPOSE / RESHAPE DATA
      df = data.frame(
        Condition = names(attrition()),
        N = unlist(attrition()), 
        Percent = unlist(attrition())/attrition()$overallt, stringsAsFactors = F)
      
      idx = as.data.frame(do.call(rbind,strsplit(df$Condition,split =  't(?!01)', perl = T)), 
        stringsAsFactors = F)
      idx$V2[!idx$V2%in%c('0','1')] = 'All'
      
      df = cbind(df, idx)
      df = reshape(df, direction = 'wide', v.names = c('N','Percent'), 
        timevar = 'V2', idvar = 'V1', drop = 'Condition')
      
      df[is.na(df)] = '-'
      df = rbind(df[1,], missingness = NA, df[2:nrow(df),])
      rownames(df) = dictionary[rownames(df)]
      
      # CREATE A MULTI COLUMN TABLE HEAD
      header = tags$table(class = 'display', 
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2, ''),
            tags$th(colspan = 2, 'Overall', tags$sup('a'), style = 'text-align: center'),
            tags$th(rowspan = 2, ''),
            tags$th(colspan = 2, 'Control', tags$sup('b'), style = 'text-align: center'),
            tags$th(rowspan = 2, ''),
            tags$th(colspan = 2, 'Treatment', tags$sup('b'), style = 'text-align: center')
          ),
          tags$tr(
            tags$th(em('N')),
            tags$th('%'),
            tags$th(em('N')),
            tags$th('%'),
            tags$th(em('N')),
            tags$th('%')
          )
        )
      )
      
      dt = DT::datatable(
        data = cbind(df[,2:3],gap1 = NA, df[,4:5], gap2 = NA, df[,6:7]),
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            "$('#heading_2_1').children().remove();",
            "this.api().table().buttons().container().appendTo( $('#heading_2_1') );",
            "$('.dt-button').removeClass('dt-button');",
            "$('.dt-buttons').addClass('btn-group').removeClass('dt-buttons');",
            "}"),        
          buttons = list(list(
            extend = 'csv',
            text = '<i class="fa fa-file-text-o"></i>',
            titleAttr = 'Download table as CSV',
            filename = 'table_2_1_attribution_analysis',
            className = 'btn btn-default btn-sm'
          )),         
          scrollX = TRUE,
          scrollY = TRUE,
          searching = FALSE,
          ordering = FALSE,
          dom = 't',
          columnDefs = list(list(className = 'dt-center', targets = 1:8))
        ),
        rownames = TRUE,
        selection = 'none',
        container = header
      )
      
      DT::formatPercentage(dt, c('Percent.All','Percent.0','Percent.1'), 2)
      
      
    })
  
  output$attrition_table_note = renderUI({
    complete_miss = rowSums(is.na(data_to_describe())) == ncol(data_to_describe())
    tags$span(
      ifelse(!is.null(attr(dataframe(), 'mi_id')), 
        'First of imputed datasets.',''),
      ifelse(sum(complete_miss) > 1,
        sprintf('Row %s are completely missing (n = %s).', 
          paste(which(complete_miss), collapse = ', '),
          sum(complete_miss)),
        ifelse(sum(complete_miss) == 1,
          sprintf('Row %s is completely missing (n = 1).', which(complete_miss)),
          '')),
      tags$sup('a'), 'Number of observations or missing cases marginalized 
      over treatment groups.',
      tags$sup('b'),'Number of observations or missing cases 
      in each treatment group.'
    )
  })
  
  ## SORTING TEST / LINEARITY CHECK (SHARE X-AXIS)
  # UPDATE ASSIGNMENT SELECTION 
  observe({
    # aux_cut = expand.grid(cut = na.omit(c(parameter$cutoff1(), if(parameter$is_frontier()) parameter$cutoff2())),aux = parameter$auxiliary())
    updateSelectInput(session, inputId = 'sort_assignment', 
      choices = 
        list('designed assignment' = 
            setNames(
              as.list(paste(c(parameter$assignment1(), if(parameter$is_frontier()) parameter$assignment2()),
                c(parameter$cutoff1(), if(parameter$is_frontier()) parameter$cutoff2()),sep='@')[1:length(c(parameter$assignment1(), if(parameter$is_frontier())  parameter$assignment2()))]
              ), paste0(c(parameter$assignment1(), if(parameter$is_frontier())  parameter$assignment2()),' [',
                c(parameter$cutoff1(), if(parameter$is_frontier()) parameter$cutoff2()),']')[1:length(c(parameter$assignment1(), if(parameter$is_frontier())  parameter$assignment2()))]
            ) 
          # ,'auxiliary variables' = 
          #   setNames(
          #     as.list(
          #       paste(aux_cut$aux, aux_cut$cut, sep = '@')[0:nrow(aux_cut)]
          #     ),
          #     paste0(aux_cut$aux, ' [', aux_cut$cut, ']')[0:nrow(aux_cut)]
          #   )
        )
    )
  })
  
  # INITIALIZE SORTING TEST
  init_sort = reactive({
    req(input$sort_assignment)
    # print(input$sort_assignment)
    assign_var = strsplit(input$sort_assignment, '@', fixed = T)[[1]][1]
    assign_cut = as.numeric(strsplit(input$sort_assignment, '@', fixed = T)[[1]][2])
    out = withProgress(message = sprintf('Initializing sorting test for %s...', input$sort_assignment), value = NULL,
      expr = tryCatch(
        dc_test(runvar = data_to_describe()[, assign_var],
          cutpoint = assign_cut, 
          plot = F,
          ext.out = T),
        error = function(e)
          return()
        # showNotification(paste('Sorting test:',   assign_var,assign_cut,
        #   as.character(e)), type = 'warning')
      ))
    validate(need(is.list(out), message = 'intitialization failed'))
    updateNumericInput(session, 'sort_bins', value = round(out$binsize,2))
    updateNumericInput(session, 'sort_banw', value = round(out$bw, 2))
    return(out)
  })
  
  # plot sorting test
  plot_sort = function(){
    req(init_sort(), isolate(input$sort_assignment))
    
    assign_var = strsplit(isolate(input$sort_assignment), '@', fixed = T)[[1]][1]
    assign_cut = as.numeric(strsplit(isolate(input$sort_assignment), '@', fixed = T)[[1]][2])
    
    out = withProgress(message = sprintf('Plot sorting test for %s...', isolate(input$sort_assignment)), value = NULL,
      expr = tryCatch(
        dc_test(runvar = data_to_describe()[, assign_var],
          cutpoint = assign_cut, plot = T,
          bin = input$sort_bins,
          bw = input$sort_banw,
          ext.out = T),
        error = function(e)
          return(e)
        # showNotification(paste('Sorting test:',  assign_var,assign_cut,
        #   as.character(e)), type = 'warning')
      ))
    mtext('Density Estimate', 2,2)
    mtext(assign_var, 1,2)
    grid(col='black')
    rug(data_to_describe()[, assign_var], side = 1, ticksize = .02)
    
    abline(v = assign_cut, col = "red", lty = 2)
    # if(!is.null(attr(dataframe(), 'mi_id'))) 
    #   legend(x = 'topleft', legend = '(first of imputed datasets)', bty = 'n')
    # 
    return(out)
    
    # return(out)
  }
  sort_table = reactiveValues(table = list())
  
  output$sort_plot = renderPlot(expr = {
    
    par(mar = c(3,3,1,.5))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
    par(new=T)
    
    out = plot_sort()
    
    # UPDATING TABLE

    output$sort_table = renderTable(
      colnames = F,rownames = T,striped = T,digits = 3,
      width = '100%',hover = T,spacing = 'xs',
      expr = {
        validate(need(is.list(out), message = 'test failed'))
        req(all(c('theta','se','z','p') %in% names(out)))
        mat = t(data.frame(out[c('theta','se','z','p')]))
        sort_table$table = mat 
        mat['p',] = ifelse(mat['p',]<.001, '<.001', mat['p',])
        return(mat)
      })
    
    
  }, bg = "transparent")
  
  # redo sorting test when numericInput changed.
  # observe({
  #   req(!is.na(input$sort_banw), !is.na(input$sort_bins))
  #   plot_params$sort_pars = list(bins = input$sort_bins,banw = input$sort_banw)
  #   
  #   # try({
  #   #   minimum = round(6/(min(abs(range(data_to_describe()[, plot_params$assignment],na.rm = T) - plot_params$cutoff)) / plot_params$sort_bins), 2)
  #   # updateNumericInput(session, 'lin_span',
  #   #   min = if(minimum > .99) .99 else minimum,
  #   #   value = if(isolate(input$lin_span) > minimum) isolate(input$lin_span) else minimum)
  #   # })
  # })
  
  
  ## LINEARITY CHECK 
  
  # plot
  # plot_lin = function() {
  #   validate(
  #     need(input$sort_bins, message = F),
  #     need(plot_params$assignment, message = F) %then%
  #       need(try(data_to_describe()[, plot_params$assignment]), message = F),
  #     need(input$lin_span, message = F),
  #     need(plot_params$cutoff, message = F)
  #   )
  #   withProgress(message = 'Plotting linearity check...',value = NULL,
  #     detail = isolate(paste(parameter$outcome(), plot_params$assignment, plot_params$cutoff, input$sort_bins, input$lin_span)),
  #     expr = tryCatch(
  #       plot_linearity_check(
  #         y = data_to_describe()[,parameter$outcome()],
  #         x = data_to_describe()[,plot_params$assignment],
  #         cutpoint = plot_params$cutoff,
  #         binw = input$sort_bins,
  #         span = input$lin_span,
  #         xaxt = 'n'),
  #       error = function(e)
  #         showNotification(paste('Linearity Check:',as.character(e)), type = 'warning')
  #     )
  #   )
  #   grid(col='black')
  #   axis(side=3, labels=T)
  #   
  #   mtext(isolate(parameter$outcome()), 2,2)
  #   legend(x = 'topleft', legend = 'Linearity Check', bty = 'n')
  #   rug(data_to_describe()[, parameter$outcome()], side = 2, ticksize = .02)
  # }
  # 
  # output$lin_plot = renderPlot(bg = 'transparent',
  #   expr= {
  #     par(mar = c(0.5,3,2,.5))
  #     rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
  #     par(new=T)
  #     
  #     plot_lin()
  #     # mtext(isolate(plot_params$assignment), 1,2)
  #   })
  output$assumption_plot_csv = downloadHandler(
    filename = 'figure_2_1_sorting_test.csv',
    content = function(file){
      req(sort_table$table)
      write.csv(sort_table$table, file)

    }
  )
  
  output$assumption_plot_png = downloadHandler(
    filename = 'figure_2_1_sorting_test.png',
    content = function(file) {
      png(file, width=700, height=600, res=150)
      par(mar=c(3,3,.5,.5))
      plot_sort()
      dev.off()
    }
  )

  output$assumption_plot_svg = downloadHandler(
    filename = 'figure_2_1_sorting_test.svg',
    content = function(file) {
      svg(file)
      par(mar=c(3,3,.5,.5))
      plot_sort()
      dev.off()
    }
  )
    
  output$assumption_plot_pdf = downloadHandler(
    filename = 'figure_2_1_sorting_test.pdf',
    content = function(file) {
      pdf(file)
      # par(mfcol = c(2,1), mar=c(.5,3,2,.5))
      # plot_lin()
      par(mar=c(3,3,.5,.5))
      plot_sort()
      dev.off()
    }
  )
  # may return whether assumptions hold
  return(NULL)
}