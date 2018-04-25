sensitivity_analysisUI = function(id){
  ns = NS(id)
  tagList(
    p(),
    fluidRow(
      column(6, 
        div(class='panel panel-default',
          div(class='panel-heading clearfix',
            h6(class = 'panel-title pull-left',
              'Figure 4.1 Cutpoint Sensitivity'),
            div(class='btn-toolbar input-group  pull-right',
              div(class='input-group input-group-sm',
                span(class='input-group-addon','CI'),
                eval({
                  tag=numericInput(ns('cutsens_level'), label = NULL, min = 0, max = 99.9, 
                    value = 95, width = '65px')
                  
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-addon','%')
              ),
              div(class='btn-group',
                downloadLink(ns('cutsens_png'),label = NULL, icon('file-image-o'),
                             class='btn btn-default btn-sm', 
                             title = 'Download plot as PNG'),  
                downloadLink(ns('cutsens_svg'),label = NULL, icon('file-o'),
                             class='btn btn-default btn-sm', 
                             title = 'Download plot as SVG'),
                downloadLink(ns('cutsens_pdf'),label = NULL, icon('file-pdf-o'),
                             class='btn btn-default btn-sm', 
                             title = 'Download plot as PDF'),
                downloadLink(ns('cutsens_csv'),label = NULL, class='btn btn-default btn-sm', 
                  icon('file-text-o'),
                  title = 'Download simulated results as CSV')
                # actionButton(ns('cut_sens_opt_btn'), label = NULL, icon = icon(name = 'cog'),
                #   class='pull-right btn-sm', `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
                #   title = 'options for outcome')
              )  
            )
            
          ),
          div(class='panel-body', style='min-height: 480px;',
            plotOutput(ns('cutsens_plot'), height = '450px'),
            selectizeInput(ns('cutsens_models'), label = NULL, multiple = T, width = '100%',
              choices = list('models' = '',
                'non-parametric' = c(),
                'parametric' = c()),
              selected = c())
          ),
          div(class='panel-footer',
            conditionalPanel(condition= "output['is_frontier']", 
              fluidRow(
                column(5, id = ns('cutsens_which_cutoff'), 
                  style='margin-bottom: 5px; padding-right:0', 
                  class='form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline shiny-bound-input',
                  div(class='btn-group input-group input-group-sm  shiny-options-group', 
                    `data-toggle`='buttons', 
                    span(class='input-group-addon','vary cutoff for'),
                    tags$label(class='btn btn-default active', "A1",#style='width:50%',
                      tags$input(name=ns('cutsens_which_cutoff'), type = "radio", value='A1', checked='checked')),
                    tags$label(class='btn btn-default', "A2",#style='width:50%',
                      tags$input(name=ns('cutsens_which_cutoff'), type = "radio", value='A2'))
                  )
                ),
                column(7, 
                  div(class='input-group input-group-sm ',
                    span(class='input-group-addon', 'hold'),
                    conditionalPanel(condition=sprintf("input['%s'] == 'A1' ", ns('cutsens_which_cutoff')),
                      class='input-group-addon',
                      'A2'
                    ),
                    conditionalPanel(condition=sprintf("input['%s'] == 'A2' ", ns('cutsens_which_cutoff')),
                      class='input-group-addon',
                      'A1'
                    ),
                    span(class='input-group-addon',style='border-right:0;border-left:0','cutoff at'),
                    numericInput(ns('cutsens_other_cutoff_at'), label = NULL, value = NA, width = '100%'),
                    span(class='input-group-addon tail')
                  )
                )
              )
            ),
            div(class='input-group', 
              span(class='input-group-addon', tags$small('min')),
              numericInput(ns('cutsens_range_min'), label = NULL, value = 10, step = .1, width = '95px'),
              span(class='input-group-addon', tags$small('max'), style = 'border-left:0; border-right:0'),
              numericInput(ns('cutsens_range_max'), label = NULL, value = 10, step = .1, width = '95px'),
              span(class='input-group-addon', tags$small('steps'), style = 'border-left:0; border-right:0'),
              numericInput(ns('cutsens_step'), label = NULL, value = 10, step = 1, width = '100%'),   
              span(class='input-group-btn',
                actionButton(ns('cutsens_simulate'), class='btn-primary',icon = icon('refresh'), 
                  label = NULL)
              ) 
            )
          )
        )
      ),
      column(6, 
        div(class='panel panel-default',
          div(class='panel-heading clearfix',
            h6(class = 'panel-title pull-left',
              'Figure 4.2 Bandwidth Sensitivity'),
            div(class='btn-toolbar input-group pull-right',
              div(class='input-group input-group-sm',
                span(class='input-group-addon','CI'),
                eval({
                  tag=numericInput(ns('bwsens_level'), label = NULL, min = 0, max = 99.9, 
                    value = 95, width = '65px')
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-addon','%')
              ),
              div(class='btn-group',
                downloadLink(ns('bwsens_png'),label = NULL, class='btn btn-default btn-sm', 
                             icon('file-image-o'), title = 'Download plot as PNG'),
                downloadLink(ns('bwsens_svg'),label = NULL, class='btn btn-default btn-sm', 
                             icon('file-o'), title = 'Download plot as SVG'),
                downloadLink(ns('bwsens_pdf'),label = NULL, class='btn btn-default btn-sm', 
                             icon('file-pdf-o'), title = 'Download plot as PDF'),
                downloadLink(ns('bwsens_csv'),label = NULL, class='btn btn-default btn-sm', 
                  icon('file-text-o'),
                  title = 'Download simulated results as CSV')
              )
            ) 
          ),
          div(class='panel-body', style='min-height: 480px;',
            plotOutput(ns('bwsens_plot'), height = '450px')
            
          ),
          div(class='panel-footer',
            conditionalPanel(condition= "output['is_frontier']", 
              
              div(id = ns('bwsens_which_est'), 
                style='margin-bottom: 5px',
                class='form-group shiny-input-radiogroup shiny-input-container shiny-input-container-inline shiny-bound-input',
                div(class='btn-group input-group input-group-sm  shiny-options-group', 
                  `data-toggle`='buttons', 
                  span(class='input-group-addon','approach'),
                  tags$label(class='btn btn-default ', "Centering",style='width:33%',
                    tags$input(name=ns('bwsens_which_est'), type = "radio", value='center')),
                  tags$label(class='btn btn-default active', "Univariate (A1)",style='width:33%',
                    tags$input(name=ns('bwsens_which_est'), type = "radio", value='univ1', checked='checked')),
                  tags$label(class='btn btn-default', "Univariate (A2)",style='width:34%',
                    tags$input(name=ns('bwsens_which_est'), type = "radio", value='univ2'))
                )
                
              )
            ),
            uiOutput(ns('bwsens_controls'))
          )
        )
      )
    )
  )   
}

sensitivity_analysis =  function(input, output, session, dataframe, parameter, result){
  
  ## CUTOFFS
  
  # UPDATE CUTOFF RANGE INPUT
  observe({
    if(input$cutsens_which_cutoff=='A1'){
      updateNumericInput(session, 'cutsens_range_min', 
        value = round(parameter$cutoff1() - sd(get_columns(dataframe(), parameter$assignment1()), na.rm = T), 2),
        min = parameter$cutoff1_range()[1],
        max = parameter$cutoff1()
      )
      updateNumericInput(session, 'cutsens_range_max', 
        value = round(parameter$cutoff1() + sd(get_columns(dataframe(), parameter$assignment1()), na.rm = T) , 2),
        min = parameter$cutoff1(),
        max = parameter$cutoff1_range()[2]
      )
      updateNumericInput(session, 'cutsens_other_cutoff_at', 
        value = parameter$cutoff2(),
        min = parameter$cutoff2_range()[1],
        max = parameter$cutoff2_range()[2]
      )
    } else {
      updateNumericInput(session, 'cutsens_other_cutoff_at', 
        value = parameter$cutoff1(),
        min = parameter$cutoff1_range()[1],
        max = parameter$cutoff1_range()[2]
      )
      updateNumericInput(session, 'cutsens_range_min', 
        value = round(parameter$cutoff2() - sd(get_columns(dataframe(), parameter$assignment2()), na.rm = T), 2),
        min = parameter$cutoff2_range()[1],
        max = parameter$cutoff2()
      )
      updateNumericInput(session, 'cutsens_range_max', 
        value = round(parameter$cutoff2() + sd(get_columns(dataframe(), parameter$assignment2()), na.rm = T), 2),
        min = parameter$cutoff2(),
        max = parameter$cutoff2_range()[2]
      )
    }
  })
  
  cut_sens_summary = eventReactive(input$cutsens_simulate, ignoreNULL = T, {
    
    req(class(result$model()) %in% c('rd','mrd'))
    req(input$cutsens_range_min, input$cutsens_range_max, input$cutsens_step)
    
    input$cutsens_simulate
    
    summ = withProgress(message = 'Simulating for cutoff sensitivity...', value = NULL,
      expr = {
        
        if(class(result$model()) == 'rd'){
          rd_sens_cutoff(result$model(),
            cutoffs = seq(
              from = isolate(input$cutsens_range_min),
              to = isolate(input$cutsens_range_max),
              length.out = isolate(input$cutsens_step)
            )
          )
        } else {
          cutsens_other_cutoff_at = isolate(input$cutsens_other_cutoff_at)
          if(is.na(cutsens_other_cutoff_at)) 
            cutsens_other_cutoff_at = switch(isolate(input$cutsens_which_cutoff), A2 = parameter$cutoff1(), parameter$cutoff2())
          
          
          mrd_sens_cutoff(result$model(),
            cutoffs = setNames(
              data.frame(
                seq(
                  from = isolate(input$cutsens_range_min),
                  to = isolate(input$cutsens_range_max),
                  length.out = isolate(input$cutsens_step)),
                cutsens_other_cutoff_at
              ),
              c(isolate(input$cutsens_which_cutoff), 
                switch(isolate(input$cutsens_which_cutoff), A1 = 'A2', A2 = 'A1')) 
            )[c('A1','A2')]
          )
        }
      }
    )
    
    models = unique(summ$model)
    default_selection = intersect(models, isolate(input$cutsens_models))
    
    if(length(default_selection)==0) 
      default_selection = models[grepl('linear|optimal',models)][1:2]
    
    updateSelectizeInput(session, 'cutsens_models', choices = list(
      "parametric" = models[grepl('linear|quadratic|cubic',models)],
      "non-parametric" = models[!grepl('linear|quadratic|cubic',models)]
    ),
      selected = default_selection
    )
    
    return(summ)
    
  })
  
  cutsens_plot = function(){
    req(
      class(result$model()) %in% c('rd','mrd'),
      cut_sens_summary(), 
      length(input$cutsens_models) > 0,
      all(input$cutsens_models %in% cut_sens_summary()$model)
    )
    
    sens_plot(cut_sens_summary(),
      level = input$cutsens_level / 100,
      x = isolate(input$cutsens_which_cutoff),
      plot_models = input$cutsens_models
    )
    grid(col = 'black')
    cutoff = switch(isolate(input$cutsens_which_cutoff),
      A1 = parameter$cutoff1(),
      A2 = parameter$cutoff2(),
      parameter$cutoff1()
    )
    
    min_y = min(subset(cut_sens_summary(), model %in% input$cutsens_models)$est, na.rm = T)
    abline(v = cutoff
      , col = 'red3', lty = 2)
    
    text(x =cutoff, y = min_y, 
      labels = 'designed cutoff', col = 'red3', adj = c(-.1, 0))
    
    mtext(
      switch(isolate(input$cutsens_which_cutoff),
        A1 = parameter$assignment1(),
        A2 = parameter$assignment2(),
        parameter$assignment1()
      ),
      side = 1, 2)
    mtext('Estimate', side = 2, 2)
  }
  output$cutsens_plot = renderPlot(bg = 'transparent', expr = {
    
    par(mar = c(4,3,.5,.5))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
    par(new=T)
    
    cutsens_plot()
    
  })
  output$cutsens_pdf = downloadHandler(
    filename = 'cutoff_sensitivity.pdf',
    content = function(file) {
      pdf(file)
      cutsens_plot()
      dev.off()
    }
  )
  output$cutsens_svg = downloadHandler(
    filename = 'cutoff_sensitivity.svg',
    content = function(file) {
      svg(file)
      cutsens_plot()
      dev.off()
    }
  )
  output$cutsens_png = downloadHandler(
    filename = 'cutoff_sensitivity.png',
    content = function(file) {
      png(file, width=700, height=600, res=150)
      cutsens_plot()
      dev.off()
    }
  )
  output$cutsens_csv = downloadHandler(
    filename = 'cutoff_sensitivity.csv',
    content = function(file) {
      write.csv(cut_sens_summary(), file, row.names = F)
    }
  )
  ## BWSENS
  
  ## UPDATE BW PER RESULT
  opt = reactiveVal()
  output$bwsens_controls = renderUI({
    ns = session$ns
    req(class(result$model()) %in% c('rd','mrd'))
    opt(if(class(result$model()) == 'rd'){
      result$model()$bw['Opt']
    } else {
      switch( input$bwsens_which_est,
        'center' =  result$model()$center$tau_MRD$bw['Opt'],
        'univ1'=  result$model()$univ$tau_R$bw['Opt'],
        'univ2'=  result$model()$univ$tau_M$bw['Opt']
      )
    })
    div(class='input-group',
      span(class='input-group-addon', tags$small('min')),
      numericInput(ns('bwsens_range_min'), label = NULL, value = round(opt()/3,2), min =  round(opt()/3,2), max = round(opt(),2), step = .1,  width = '95px'),
      span(class='input-group-addon', tags$small('max'), style = 'border-left:0; border-right:0'),
      numericInput(ns('bwsens_range_max'), label = NULL, value =  round(opt()*3,2), min = round(opt(),2), max = round(opt()*3, 2), step = .1,  width = '95px'),
      span(class='input-group-addon', tags$small('steps'), style = 'border-left:0; border-right:0'),
      numericInput(ns('bwsens_step'), label = NULL, min = 2, value = 10, step = 1, width = '100%'),   
      span(class='input-group-btn',
        actionButton(ns('bwsens_simulate'), class='btn-primary', icon = icon('refresh'), 
          label = NULL)
      )
    )
  })
  
  
  bw_sens_summary = eventReactive(input$bwsens_simulate, ignoreNULL = T, {
    req(class(result$model()) %in% c('rd','mrd'))
    req(input$bwsens_range_min, input$bwsens_range_max, input$bwsens_step)
    summ = withProgress(message = 'Simulating for bandwidth sensitivity...', value = NULL,
      expr = {
        if(class(result$model()) == 'rd'){
          rd_sens_bw(result$model(),
            bw = seq(
              from = isolate(input$bwsens_range_min),
              to = isolate(input$bwsens_range_max),
              length.out = isolate(input$bwsens_step)
            )
          )
        } else {
          bwsens_which_est = isolate(input$bwsens_which_est)
          if(is.null(bwsens_which_est)) 
            bwsens_which_est = 'center'
          
          mrd_sens_bw(result$model(),
            approach = bwsens_which_est,
            bw = seq(
              from = isolate(input$bwsens_range_min),
              to = isolate(input$bwsens_range_max),
              length.out = isolate(input$bwsens_step)
            )
          )
        }
      }
    )
    
    return(summ)
    
  })
  
  bwsens_plot = function(){
    req(
      class(result$model()) %in% c('rd','mrd'),
      bw_sens_summary()
    )
    yrange = bw_sens_summary()$est[grepl('origin',bw_sens_summary()$model)] + bw_sens_summary()$se[grepl('origin',bw_sens_summary()$model)] * qnorm(c(.005,.995))
    
    sens_plot(bw_sens_summary(),
      level = input$bwsens_level / 100,
      x = 'bw',
      yrange = yrange
    )
    grid(col = 'black')
    abline(v = opt(), col = 'red3', lty = 2)
    text(x = opt(), y = min(yrange), labels = 'optimal bandwidth', col = 'red3',  adj = c(-.1, 0))
    
    mtext('Bandwidth', side = 1, 2)
    mtext('Estimate', side = 2, 2)
    
    opt_bw = if(is.null(isolate(input$bwsens_which_est))){
      switch(isolate(input$bwsens_which_est),
        center = result$model()$center$tau_MRD$bw['Opt'],
        univ1 = result$model()$univ$tau_R$bw['Opt'],
        univ2 = result$model()$univ$tau_M$bw['Opt']
      )} else {
        result$model()$bw['Opt']
      }
    # print(opt_bw)
    # abline(v = opt_bw, col = 'red', lty = 2)
    # legend('topleft', legend = 'Optimal', lty = 2, col = 'red', pch = NA, bty='n')
    # 
  }
  output$bwsens_plot = renderPlot(bg = 'transparent', expr = {
    
    par(mar = c(4,3,.5,.5))
    rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
    par(new=T)
    
    bwsens_plot()
    
  })
  output$bwsens_pdf = downloadHandler(
    filename = 'bandwidth_sensitivity.pdf',
    content = function(file) {
      pdf(file)
      bwsens_plot()
      dev.off()
    }
  )
  output$bwsens_svg = downloadHandler(
    filename = 'bandwidth_sensitivity.svg',
    content = function(file) {
      svg(file)
      bwsens_plot()
      dev.off()
    }
  )
  output$bwsens_png = downloadHandler(
    filename = 'bandwidth_sensitivity.png',
    content = function(file) {
      png(file, width=700, height=600, res=150)
      bwsens_plot()
      dev.off()
    }
  )
  
  output$bwsens_csv = downloadHandler(
    filename = 'bandwidth_sensitivity.csv',
    content = function(file) {
      write.csv(subset(bw_sens_summary(), select = -model), file, row.names = F)
    }
  ) 
  
  
}