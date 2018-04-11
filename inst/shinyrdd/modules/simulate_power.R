# Module UI
simulate_powerUI = function(id){
  ns = NS(id)
  tagList(
    p(),
    div(class='panel panel-default',
      div(class='panel-heading',
        h6('Table 5.1',strong('Powers of RDD Estimates'), class='pull-left'),
        div(class='btn-toolbar input-group  pull-right',
              div(class='input-group input-group-sm',
                span(class='input-group-addon','Iterations'),
                eval({
                  tag=numericInput(ns('power_simulate_iter'), label = NULL, min = 0, max = 99.9, 
                    value = 500, width = '65px')
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-btn',
                  actionButton(ns('power_simulate'), label = NULL, class='btn-sm btn-primary pull-right', icon = icon('refresh'))
                  )
              )
          
        ),
        
        div(class='clearfix')
      ),
      div(class='panel-body',
        # fluidRow(
        #   column(9,
            DT::dataTableOutput(ns('power_table')),
            uiOutput(ns('power_table_note'))
        #   ),
        #   column(3,
        #     h6(class='badge','Sample Size'),
        #     numericInput(ns('power_n'), label = NULL, min = 1, value = 100),
        #     h6(class='badge','Iterations'),
        #     numericInput(ns('power_simulate_iter'), label = NULL, min = 1, value = 500)
        #   )
        # )
      )
    ),
    
    div(class='panel panel-default',
      div(class='panel-heading',
        h6('Figure 5.1',strong('Power by Sample Size'), class='pull-left'),
        div(class='btn-toolbar input-group  pull-right',
              div(class='input-group input-group-sm',
                span(class='input-group-addon','Steps'),
                eval({
                  tag=numericInput(ns('power_chart_step'), label = NULL, min = 0, max = 99.9, 
                    value = 5, width = '50px')
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-addon','Iterations in each step', style = 'border-left: 0;border-right:0;'),
                eval({
                  tag=numericInput(ns('power_chart_iter'), label = NULL, min = 0, max = 99.9, 
                    value = 500, width = '65px')
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-btn',
                  actionButton(ns('power_chart_simulate'), label = NULL, class='btn-sm btn-primary pull-right', icon = icon('refresh'))
                  )
              ),
          div(class='btn-group',
            downloadLink(ns('power_chart_png'),label = "PNG",
              class='btn btn-default btn-sm', 
              title = 'Download plot as PNG'),
            downloadLink(ns('power_chart_svg'),label = "SVG",
              class='btn btn-default btn-sm', 
              title = 'Download plot as SVG'),
            downloadLink(ns('power_chart_pdf'),label = NULL, icon('file-pdf-o'),
              class='btn btn-default btn-sm', 
              title = 'Download plot as PDF'),
            downloadLink(ns('power_chart_csv'),label = NULL, icon('file-text-o'),
              class='btn btn-default btn-sm', 
              title = 'Download simulated results as csv')
          )
          
        ),
        
    
        div(class='clearfix')
      ),
      div(class='panel-body',
        fluidRow(
          column(9, plotOutput(ns('power_chart'), height = '400px')),
          column(3, 
            h6(class='badge','Minimum Sample Size'),
            numericInput(ns('power_chart_n_min'), label = NULL, min = 1, value  = 50),
            h6(class='badge','Maximum Sample Size'),
            numericInput(ns('power_chart_n_max'), label = NULL, min = 1, value  = 500),
            hr(),
            h6(class='badge','Group Lines by'),
            selectizeInput(ns('power_chart_by'), label = NULL, 
              choices = c('Model Type' = 'type', 'Alpha Level' = 'alpha')),
            conditionalPanel(condition = sprintf("input['%s'] == 'alpha'", ns('power_chart_by')) ,
              h6(class='badge','Show Model Type'),
              selectizeInput(ns('power_chart_type'), label = NULL, selected = 'Linear',
                choices = c('Linear','Optimal' = 'Opt'))
            ),
            conditionalPanel(condition = sprintf("input['%s'] == 'type'", ns('power_chart_by')) ,
              h6(class='badge','Show Alpha Level'),
              selectizeInput(ns('power_chart_alpha'), label = NULL, 
                choices = c('.05' = 'a3', '.01' = 'a2', '.001' = 'a1' ))
            )
          )
        )
      )
      
      
    )
    
  )
  
  
}


# Module Server

simulate_power = function(input, output, session, parameter){
  
  ## SINGLE RUN SIMULATION
  counter = reactiveValues(val = 0)
  
  result = reactiveValues(summary = NULL,summaries = list())
  
  observeEvent(parameter(), {
    result$summary = matrix('-', nrow = 2, ncol = 6)
    result$summaries = list()
    counter$val = 0
  })
  
  observeEvent(input$power_simulate, {
    req(parameter())
    power_input = parameter()
    power_input$pars$num.rep = input$power_simulate_iter
    # power_input$pars$sample.size = input$power_n
    
    res = withProgress(message = 'Simulating...', value = NULL, 
      expr = do.call(power_input$cmd, power_input$pars)
    )
    # restructure for display
    result$summary = res
  })
  
  output$power_table = DT::renderDataTable(server = F, 
    expr = {
      req(is.matrix(result$summary))

      # CREATE A MULTI COLUMN TABLE HEAD
      header = tags$table(class = 'display',
        tags$thead(
          tags$tr(
            tags$th(rowspan = 2,  ''),
            tags$th(rowspan = 2,  'S'),
            tags$th(colspan = 2, 'Treatment Effects', style = 'text-align: center'),
            tags$th(rowspan = 2,  ''),
            tags$th(colspan = 3, 'Power', tags$sup('a'), style = 'text-align: center')
          ),
          tags$tr(
            lapply(c('Mean', 'Variance', '.001', '.01','.05'),
              tags$th, style = 'text-align:right')
          )
        )
      )

      dt = DT::datatable(
        data = {
          mat = cbind(result$summary[,1:3], NA, result$summary[,4:6])
          if(nrow(result$summary) == 2)
            rbind(NA, mat[1,], NA, mat[2,])
          else
            rbind(NA, mat[c(1,3,5),], NA, mat[c(2,4,6),])
        },
        options = list(
          responsive = T,
          pageLength = 8,
          scrollX = TRUE,
          searching = FALSE,
          ordering = FALSE,
          dom = 't',
          autoWidth = F,
          columnDefs = list(list(className = 'dt-right', targets = 1:7))
        ),
        rownames = if (nrow(result$summary) == 2) {
          c('Parametric',
          '- Linear',# '- Quadratic','- Cubic',
          'Nonparametric',
          '- Optimal'#, '- Half','- Double'
          ) } else {
          c('Parametric (Linear)',
          '- Centering', '- Univariate (A1)','- Univariate (A2)',
          'Nonparametric (Optimal)',
          '- Centering', '- Univariate (A1)','- Univariate (A2)'
          )
        }
          ,
        selection = 'none',
        container = header
      )
      DT::formatRound(dt, columns = 2:7, 3)
      # dt = DT::formatPercentage(dt, columns = 5:7, 2)
    })
  
  output$power_table_note = renderUI({
    req(result$summary)
    
    h6(em('Note.'), 
      'S = successful simulations.', 
      tags$sup('a'), '1 - Type II error rates across successful simulations, given a confidence level.')
  })
  
  ## POWER CHART
  power_chart = function(df){
    plot.new()
    plot.window(ylim = 0:1, xlim=c(input$power_chart_n_min, input$power_chart_n_max))
    box()
    axis(1); mtext('Sample Size', 1, 2)
    axis(2); mtext('Power', 2, 2)
    grid(lty = 3, col = 'black')
    df$type = as.character(df$type)
    if(nrow(df) > 0){
      if(input$power_chart_by == 'alpha'){
        points(a3 ~ n, subset(df, type == input$power_chart_type), type = 'b', col = 'black', pch = 0, lwd = 2)
        points(a2 ~ n, subset(df, type == input$power_chart_type), type = 'b', col = 'red4', pch = 1, lwd = 2)
        points(a1 ~ n, subset(df, type == input$power_chart_type), type = 'b', col = 'blue4', pch = 2, lwd = 2)
        legend(x = 'bottomright', 
          legend = c(expression(alpha == .001), expression(alpha == .01), expression(alpha == .05)),
          pch = 0:2,
          col = c('black','red4','blue4'),
          bty = 'n'
        )
      } else {
        fml = as.formula(sprintf('%s ~ n', input$power_chart_alpha)) 
        by(df, df$type, function(subdf){
          subdf$type = as.character(subdf$type)
          
          points(fml, data = subdf, type = 'b', 
            col = switch(unique(subdf$type), 
              'Linear' = 'red4', 'Opt' = 'blue4',
              'Linear-Centering' = 'red','Optimal-Centering' = 'red4',
              'Linear-Univariate (A1)' = 'blue','Optimal-Univariate (A1)' = 'blue4',
              'Linear-Univariate (A2)' = 'green','Optimal-Univariate (A2)' = 'green4'),
            pch = switch(unique(subdf$type), 
              'Linear' =  0, 'Opt' = 1,
              'Linear-Centering' = 0,'Optimal-Centering' = 15,
              'Linear-Univariate (A1)' = 1,'Optimal-Univariate (A1)' = 16,
              'Linear-Univariate (A2)' = 2,'Optimal-Univariate (A2)' = 17), 
            lwd = 2)
        })
        types = unique(df$type)
        
        legend(x = 'bottomright',
          legend = types,
          pch = sapply(types, switch,
            'Linear' =  0, 'Opt' = 1,
              'Linear-Centering' = 0,'Optimal-Centering' = 15,
              'Linear-Univariate (A1)' = 1,'Optimal-Univariate (A1)' = 16,
              'Linear-Univariate (A2)' = 2,'Optimal-Univariate (A2)' = 17),
          col = sapply(types, switch,
            'Linear' = 'red4', 'Opt' = 'blue4',
            'Linear-Centering' = 'red','Optimal-Centering' = 'red4',
            'Linear-Univariate (A1)' = 'blue','Optimal-Univariate (A1)' = 'blue4',
            'Linear-Univariate (A2)' = 'green','Optimal-Univariate (A2)' = 'green4'),

          # ncol = 2,
          bty = 'n'
        )
      }
    }
  }
  output$power_chart = renderPlot(bg = 'transparent', expr = {
    req(length(result$summaries) > 0)
    par(mar=c(3,3,0.5,0))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
    par(new = T)
    
    power_chart(do.call(rbind.data.frame, result$summaries))
    
  })
  
  N = reactive({seq(input$power_chart_n_min, input$power_chart_n_max,length.out =  input$power_chart_step)})
  
  observeEvent(input$power_chart_simulate, {
    result$summaries = list()
    counter$val = 1
  })
  
  observeEvent(input$power_chart_halt, {
    result$summaries = list()
    counter$val = 0
  })
  
  observe({
    req(parameter())
    input$power_chart_simulate
    if(isolate(counter$val <= length(N()) & counter$val > 0)) {
      isolate({
        n = N()[counter$val]
        power_input = parameter()
        power_input$pars$num.rep = input$power_chart_iter
        power_input$pars$sample.size = n
        
        res = as.data.frame(
          withProgress(message = 'Simulating...', value = NULL, detail = sprintf('N = %g', n),
            expr = do.call(power_input$cmd, power_input$pars)
          )
        )
        if (nrow(res) == 6){
          row.names(res) <- c(
            'Linear-Centering','Optimal-Centering',
            'Linear-Univariate (A1)','Optimal-Univariate (A1)',
            'Linear-Univariate (A2)','Optimal-Univariate (A2)')
          
        }
        updateSelectizeInput(session, 'power_chart_type',
            choices = row.names(res)
          )
        result$summaries[[counter$val]] = setNames(cbind(rownames(res), res, n = n),
          c('type','s','m','v','a1','a2','a3','n'))
        
        counter$val = counter$val + 1
      })
      
      invalidateLater(0, session)
    } 
  })
  
  output$power_chart_png = downloadHandler(
    filename = 'power_by_sample_size.png',
    content = function(file) {
      png(file, width=700, height=600, res=150)
      power_chart(do.call(rbind.data.frame, result$summaries))
      dev.off()
    }
  )
  output$power_chart_svg = downloadHandler(
    filename = 'power_by_sample_size.svg',
    content = function(file) {
      svg(file)
      power_chart(do.call(rbind.data.frame, result$summaries))
      dev.off()
    }
  )
  output$power_chart_pdf = downloadHandler(
    filename = 'power_by_sample_size.pdf',
    content = function(file) {
      pdf(file)
      power_chart(do.call(rbind.data.frame, result$summaries))
      dev.off()
    }
  )
  output$power_chart_csv = downloadHandler(
    filename = 'power_by_sample_size.csv',
    content = function(file) {
      write.csv(do.call(rbind.data.frame, result$summaries), file, row.names = F)
    }
  )
}