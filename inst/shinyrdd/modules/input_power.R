# Module UI
input_powerUI = function(id){
  ns = NS(id)
  tagList(
    
    ## FIRST BLOCK: ASSIGNMENT
    div(class='panel panel-default',
      div(class='panel-heading clearfix',
        h5(class='panel-title pull-left','Assignment'),
        div(class='btn-toolbar input-group  pull-right',
              div(class='input-group input-group-sm',
                span(class='input-group-addon','Sample Size'),
                eval({
                  tag=numericInput(ns('sample_size'), label = NULL, min = 1,
                    value = 100, width = '65px')
                  tag$attribs$class = paste(tag$attribs$class, 'input-group-sm')
                  tag
                }),
                span(class='input-group-addon tail')
              ),
          actionButton(ns('allow_frontier'), label = NULL, 
            icon = icon('plus-square'), class='btn-sm pull-right',
            `data-toggle`="button", `aria-pressed`="false", autocomplete="off",
            title = 'allow a secondary assignment')
        )
        
      ),
      div(class='panel-body',
        plotOutput(ns('assign_dist'), height = '250px'),
        
        h6(class='badge','Distribution'),
        
        div(class='input-group clearfix', style='margin-bottom:0;',
          div(class='input-group pull-left', 
            span(class='input-group-addon', 'A1'),
            span(class='input-group-btn',  actionButton(ns('assign1_dist'), label = 'normal',
              style = 'border-radius: 0'))
          ),
          div(class='input-group', style='margin-bottom:0;',
            conditionalPanel(condition = sprintf("input['%s'] %% 2 == 0", ns('assign1_dist')),class='input-group',
              span(class='input-group-addon', HTML('&mu;'), style='border-radius:0; border-left:0;'),
              numericInput(ns('assign1_mu'), label = NULL, value = 0, step = .1, width = '100%'),
              span(class='input-group-addon', HTML('&sigma;'), style='border-left:0;border-right:0;'),
              numericInput(ns('assign1_sigma'), label = NULL, value = 1, step = .1, width = '100%'),
              span(class='input-group-addon tail')
            ),
            conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('assign1_dist')),class='input-group',
              span(class='input-group-addon','min', style='border-radius:0; border-left:0;'),
              numericInput(ns('assign1_min'), label = NULL, value = -1, step = .1, width = '100%'),
              span(class='input-group-addon','max', style='border-left:0;border-right:0;'),
              numericInput(ns('assign1_max'), label = NULL, value = 1, step = .1,  width = '100%'),
              span(class='input-group-addon tail')
            )
          )
          
        ),
        
        conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
          div(class='input-group clearfix',  style='margin-bottom:0',
            div(class='input-group pull-left', 
              span(class='input-group-addon', 'A2'),
              span(class='input-group-btn',  actionButton(ns('assign2_dist'), label = 'normal',
                style = 'border-radius: 0'))
              
            ),
            div(class='input-group',
              conditionalPanel(condition = sprintf("input['%s'] %% 2 == 0", ns('assign2_dist')),class='input-group',
                span(class='input-group-addon', HTML('&mu;'), style='border-radius:0; border-left:0;'),
                numericInput(ns('assign2_mu'), label = NULL, value = 0, step = .1, width = '100%'),
                span(class='input-group-addon', HTML('&sigma;'), style='border-left:0;border-right:0;'),
                numericInput(ns('assign2_sigma'), label = NULL, value = 1, step = .1,  width = '100%'),
                span(class='input-group-addon tail')
              ),
              conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('assign2_dist')),class='input-group',
                span(class='input-group-addon','min', style='border-radius:0; border-left:0;'),
                numericInput(ns('assign2_min'), label = NULL, value = -1, step = .1, width = '100%'),
                span(class='input-group-addon','max', style='border-left:0;border-right:0;'),
                numericInput(ns('assign2_max'), label = NULL, value = 1, step = .1, width = '100%'),
                span(class='input-group-addon tail')
              )
            )
          )
          # ,
          # conditionalPanel(condition = sprintf("input['%s'] %% 2 == 0 && input['%s'] %% 2 == 0", 
          #   ns('assign1_dist'), ns('assign2_dist')), style='width:50%',
          #   div(class='input-group',
          #     span(class='input-group-addon', HTML('&rho;')),
          #     numericInput(ns('bivariate_rho'), label = NULL, value = 0, step = .1, min = -1, max = 1, width = '100%'),
          #     span(class='input-group-addon tail')
          #   )
          # )  
        ),
        
        
        h6(class='badge','Treatment Design'),
        div(class='input-group', 
          span(class='input-group-addon', 'A1'),
          span(class='input-group-btn', actionButton(ns('operator1'), label = '>',style='border-radius:0; border-right:0;')),
          numericInput(ns('assign1_cut'), label = NULL, value = 0, step = .1, width = '100%'),
          span(class='input-group-addon', 'fuzziness', style='border-left:0;border-right:0;'),
          numericInput(ns('assign1_p1'), label = NULL, value = 0, step = .1, min = 0, max = 1,width = '100%'),
          span(class='input-group-addon tail', style='border-left:0;border-right:0;padding:0'),
          numericInput(ns('assign1_p2'), label = NULL, value = 0, step = .1, min = 0, max = 1,width = '100%'),
          span(class='input-group-addon tail')
        ),
        
        conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
          div(class='input-group', 
            span(class='input-group-addon', 'A2'),
            span(class='input-group-btn', actionButton(ns('operator2'), label = '>',style='border-radius:0; border-right:0;')),
            numericInput(ns('assign2_cut'), label = NULL, value = 0, step = .1, width = '100%'),
            span(class='input-group-addon', 'fuzziness', style='border-left:0;border-right:0;'),
            numericInput(ns('assign2_p1'), label = NULL, value = 0, step = .1,min = 0, max = 1, width = '100%'),
            span(class='input-group-addon tail', style='border-left:0;border-right:0;padding:0'),
            numericInput(ns('assign2_p2'), label = NULL, value = 0, step = .1, min = 0, max = 1, width = '100%'),
            span(class='input-group-addon tail')
            
          )
          
        )
        
      )
    ),
    
    ## SECOND BLOCK
    div(class='panel panel-default', 
      div(class='panel-heading',
        h5(class='panel-title','Parametric Model for Outcome')
      ),
      div(class='panel-body',
        plotOutput(ns('par_model'), height = '325px'),
        conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
          fluidRow(
            column(6, h6(class='badge',icon('arrows-v')),
              sliderInput(ns('par_model_phi'), label = NULL, min = -180, 
                max = 180, value = 30, ticks = F, post = '°')
            ),
            column(6, h6(class='badge',icon('arrows-h')),
              sliderInput(ns('par_model_theta'), label = NULL,min = -180, 
                max = 180, value = -30, ticks = F, post = '°')
            )
          )
        )
      ),
      div(class='panel-footer',
        fluidRow(
          column(4, h6('Treatment'), 
            numericInput(ns('par_model_t'), label = NULL, value = 1, step = .05)),
          column(8, align = 'right', h6('Partial','η',tags$sup('2')), 
            sliderInput(
              ns('par_model_eta2'), 
              label = NULL, min = .01, max = .99, value = .5, step = .01, ticks = F)
          ),
          column(4, h6('Constant'),
            numericInput(ns('par_model_constant'), label = NULL, value = 1, step = .05)),
          conditionalPanel(condition = sprintf("input['%s'] %% 2 == 0", ns('allow_frontier')),
            column(4, h6('Slope'),
              numericInput(ns('par_model_slope'), label = NULL, value = .5, step = .05)),
            column(4, h6('Interaction'),
              numericInput(ns('par_model_interact'), label = NULL, value = .25, step = .05))
          ),
          conditionalPanel(condition = sprintf("input['%s'] %% 2 == 1", ns('allow_frontier')),
            column(4, h6('Treatment 1 (T1)'),
              numericInput(ns('par_model_t1'), label = NULL, value = .5, step = .05)),
            column(4, h6('Treatment 2 (T2)'),
              numericInput(ns('par_model_t2'), label = NULL, value = .25, step = .05)),
            column(4, h6('Assignment 1 (A1)'),
              numericInput(ns('par_model_x1'), label = NULL, value = .5, step = .05)),
            column(4, h6('Assignment 2 (A2)'),
              numericInput(ns('par_model_x2'), label = NULL, value = .5, step = .05)),
            column(4, h6('A1*A2'),
              numericInput(ns('par_model_x1x2'), label = NULL, value = .1, step = .05)),
            column(4, h6('T1*A1'),
              numericInput(ns('par_model_t1x1'), label = NULL, value = .1, step = .05)),
            column(4, h6('T1*A2'),
              numericInput(ns('par_model_t1x2'), label = NULL, value = .1, step = .05)),
            column(4, h6('T2*A1'),
              numericInput(ns('par_model_t2x1'), label = NULL, value = .1, step = .05)),
            column(4, h6('T2*A2'),
              numericInput(ns('par_model_t2x2'), label = NULL, value = .1, step = .05)),
            column(4, h6('T1*A1*A2'),
              numericInput(ns('par_model_t1x1x2'), label = NULL, value = .1, step = .05)),
            column(4, h6('T2*A1*A2'),
              numericInput(ns('par_model_t2x1x2'), label = NULL, value = .1, step = .05))
          )
        )
      )
      
    )
    
  )
}

# Module Server

input_power = function(input, output, session){
  
  observe({
    choice = input$operator1 %% 2 + 1
    updateActionButton(session, 'operator1', 
      label = c('\u2265','\u2264')[choice]
    )
  })
  
  observe({
    choice = input$operator2 %% 2 + 1
    updateActionButton(session, 'operator2', 
      label = c('\u2265','\u2264')[choice]
    )
  })  
  
  observe({
    choice = input$assign1_dist %% 2 + 1
    updateActionButton(session, 'assign1_dist', 
      label = c('normal','uniform')[choice]
    )
  })  
  
  observe({
    choice = input$assign2_dist %% 2 + 1
    updateActionButton(session, 'assign2_dist', 
      label = c('normal','uniform')[choice]
    )
  })  
  
  
  n = reactive(ifelse(input$allow_frontier, 200, 100))
  
  Var_noise = reactive({
    
    x1.para = if(input$assign1_dist %% 2 == 0) 
      c(input$assign1_mu, input$assign1_sigma)
    else 
      c(input$assign1_min, input$assign1_max)
    
    x1.fuzzy = c(input$assign1_p1, input$assign1_p2)
    x1.cut =  input$assign1_cut
    # coef = c(input$par_model_constant, input$par_model_t,input$par_model_slope, input$par_model_interact)
    eta.sq = input$par_model_eta2
    
    if(input$assign1_dist %% 2 == 0) {
      E_X1 <- x1.para[1]
      Var_X1 <- x1.para[2]^2
      E_T1 <- (1 - x1.fuzzy[1]) * 
        pnorm(x1.cut, mean = x1.para[1], sd = x1.para[2], lower.tail = FALSE) + 
        x1.fuzzy[2] * pnorm(x1.cut, mean = x1.para[1], sd = x1.para[2], lower.tail = TRUE)
      Var_T1 <- E_T1 - E_T1^2
      
    } else{
      E_X1 <- (x1.para[1] + x1.para[2]) / 2
      Var_X1 <- (x1.para[1] - x1.para[2])^2 / 12
      E_T1 <- (1 - x1.fuzzy[1]) * (x1.para[2] - x1.cut) / (x1.para[2] - x1.para[1]) + 
        x1.fuzzy[2] * (x1.cut - x1.para[1]) / (x1.para[2] - x1.para[1])
      Var_T1 <- E_T1 - E_T1^2
      
    } 
    
    if(input$allow_frontier %% 2 == 1){
      x2.para = if(input$assign2_dist %% 2 == 0) 
        c(input$assign2_mu, input$assign2_sigma)
      else 
        c(input$assign2_min, input$assign2_max)
      
      x2.fuzzy = c(input$assign2_p1, input$assign2_p2)
      x2.cut =  input$assign2_cut
      
      if(input$assign2_dist %% 2 == 0) {
        E_X2 <- x2.para[1]
        Var_X2 <- x2.para[2]^2
        E_T2 <- (1 - x2.fuzzy[1]) * 
          pnorm(x2.cut, mean = x2.para[1], sd = x2.para[2], lower.tail = FALSE) + 
          x2.fuzzy[2] * pnorm(x2.cut, mean = x2.para[1], sd = x2.para[2], lower.tail = TRUE)
        Var_T2 <- E_T2 - E_T2^2
        
      } else {
        E_X2 <- (x2.para[1] + x2.para[2]) / 2
        Var_X2 <- (x2.para[1] - x2.para[2])^2 / 12
        E_T2 <- (1 - x2.fuzzy[1]) * (x2.para[2] - x2.cut) / (x2.para[2] - x2.para[1]) + 
          x2.fuzzy[2] * (x2.cut - x2.para[1]) / (x2.para[2] - x2.para[1])
        Var_T2 <- E_T2 - E_T2^2
        
      } 
    } else {
      E_T2 = 0
    }
    
    E_T <- E_T1 + E_T2 - E_T1 * E_T2
    
    Var_T <- E_T - E_T^2
    Var_noise <- Var_T * (1 / eta.sq - 1)
    
    return(Var_noise)
  })
  
  # RENDER SLIDER FOR ETA2
  # output$par_model_eta2_slider = renderUI({
  #   ns = session$ns
  #   eta2_max = floor(vars()$Var_T / vars()$Var_Y * 1000) / 1000
  #   eta2_value = if(is.null(isolate(input$par_model_eta2))) signif(eta2_max /2, 3) else isolate(input$par_model_eta2) 
  #   sliderInput(
  #     ns('par_model_eta2'), 
  #     label = NULL, min = .001, max = eta2_max, value = eta2_value, step = .001)
  # })
  # 
  
  # input = list()
  # input$assign1_sigma = 1
  # input$assign1_mu = 0
  # input$assign1_min = -1
  # input$assign1_max = 1
  # input$assign1_cut = .5
  # input$assign1_dist = 'normal'
  # input$allow_frontier = F
  # input$assign2_sigma = 2
  # input$assign2_mu = 1
  # input$assign2_min = -1
  # input$assign2_max = 1
  # input$assign2_cut = .5
  # input$assign2_dist = 'normal'
  # input$bivariate_rho = .2
  # 
  # input$operator1 = 1
  # input$operator2 = 2
  # 
  sample_univariate = reactive({
    if(input$assign1_dist %% 2 == 0)
      data.frame(X = rnorm(input$sample_size, input$assign1_mu, input$assign1_sigma))
    else
      data.frame(X =  runif(input$sample_size, min = input$assign1_min, max = input$assign1_max))
  })
  
  sample_univariate_fuzzy = reactive({
    req(sample_univariate())
    within(sample_univariate(),
      {
        Z = ifelse(rddapp:::treat_assign(X, input$assign1_cut, ifelse(input$operator1 %% 2 == 0, 'geq', 'leq')), 
          rbinom(input$sample_size, 1, 1-input$assign1_p2), 
          1-rbinom(input$sample_size, 1, 1-input$assign1_p1))
      })
  })
  
  sample_bivariate =  reactive({
    if(input$assign1_dist %% 2 ==0& input$assign2_dist %% 2== 0) {
      mu = c(input$assign1_mu, input$assign2_mu)
      sigma = matrix(0, nrow = 2, ncol = 2)
      diag(sigma) = c(input$assign1_sigma, input$assign2_sigma)
      # sigma[1,2] = sigma[2,1] = sqrt(input$assign1_sigma * input$assign2_sigma) * input$bivariate_rho
      result = as.data.frame(MASS::mvrnorm(input$sample_size, mu, sigma))
      names(result) = c('X1', 'X2')
      result
    } else {
      data.frame(
        X1 = if(input$assign1_dist %% 2 == 0) rnorm(input$sample_size, input$assign1_mu, input$assign1_sigma)
        else runif(input$sample_size, min = input$assign1_min, max = input$assign1_max),
        X2 = if(input$assign2_dist %% 2 == 0) rnorm(input$sample_size, input$assign2_mu, input$assign2_sigma) 
        else runif(input$sample_size, min = input$assign2_min, max = input$assign2_max)
      )
      
    }
  })
  
  sample_bivariate_fuzzy = reactive({
    req(sample_bivariate())
    within(sample_bivariate(),
      {
        Z1 = ifelse(rddapp:::treat_assign(X1, input$assign1_cut, ifelse(input$operator1 %% 2 == 0, 'geq', 'leq')), 
          rbinom(input$sample_size, 1, 1-input$assign1_p2), 
          1-rbinom(input$sample_size, 1, 1-input$assign1_p1))
        Z2 = ifelse(rddapp:::treat_assign(X2, input$assign2_cut, ifelse(input$operator2 %% 2 == 0, 'geq', 'leq')), 
          rbinom(input$sample_size, 1, 1-input$assign2_p2), 
          1-rbinom(input$sample_size, 1, 1-input$assign2_p1))
        Z = ifelse(Z1 == 1 | Z2 ==1, 1, 0)
      })
  })
  
  output$assign_dist = renderPlot(bg = 'transparent', expr={
    
    if(input$allow_frontier %% 2 == 0) {
      par(mar = c(2,0,0.5,.5))
      left = if(input$assign1_dist %% 2 == 0) {
        curve(dnorm(x, mean = input$assign1_mu, sd = input$assign1_sigma), 
          from = input$assign1_mu - 3 * input$assign1_sigma,
          to =  input$assign1_mu + 3 * input$assign1_sigma, 
          ylim = c(0,1),
          axes = F, ylab = NA, xlab = NA, type = 'n')
        curve(dnorm(x, mean = input$assign1_mu, sd = input$assign1_sigma), 
          from = min(input$assign1_mu - 3 * input$assign1_sigma, input$assign1_cut),
          to = input$assign1_cut, ylab = NA, xlab = NA, add = T,
          lty = ifelse(input$operator1 %% 2 == 1, 1, 2))
      } else {
        curve(dunif(x, min = input$assign1_min, max = input$assign1_max), 
          from = input$assign1_min,
          to = input$assign1_max,
          ylim = c(0,1),
          axes = F, ylab = NA, xlab = NA, type = 'n')
        curve(dunif(x, min = input$assign1_min, max = input$assign1_max), 
          from = min(input$assign1_min, input$assign1_cut),
          to = max(input$assign1_cut, input$assign1_min), ylab = NA, xlab = NA, add = T,
          lty = ifelse(input$operator1 %% 2 == 1, 1, 2))
      }
      
      right = if (input$assign1_dist %% 2 == 0) {
        curve(dnorm(x, mean = input$assign1_mu, sd = input$assign1_sigma), 
          to = max(input$assign1_mu + 3 * input$assign1_sigma, input$assign1_cut),
          from = input$assign1_cut, ylab = NA, xlab = NA, add = T,
          lty = ifelse(input$operator1 %% 2 == 0, 1, 2))
      }  else {
        curve(dunif(x, min = input$assign1_min, max = input$assign1_max), 
          to = max(input$assign1_max, input$assign1_cut),
          from = min(input$assign1_cut, input$assign1_max), ylab = NA, xlab = NA,  add = T,
          lty = ifelse(input$operator1 %% 2 == 0, 1, 2))
        
      }
      
      
      polygon(c(min(left$x), left$x, max(left$x)), c(0,left$y, 0), border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator1 %% 2 ==1, .2, .1))
      )
      
      polygon(c(right$x[1],right$x, max(right$x)), c(0,right$y,0), border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator1 %% 2 == 0, .2, .1))
      )
      Ful = hist(sample_univariate_fuzzy()$X, plot = F)
      Trt = hist(sample_univariate_fuzzy()$X[sample_univariate_fuzzy()$Z==1], breaks = Ful$breaks, plot = F)
      Ctl = hist(sample_univariate_fuzzy()$X[sample_univariate_fuzzy()$Z==0], breaks = Ful$breaks, plot = F)
      Trt$density = Trt$density * sum(Trt$counts) / sum(Ful$counts)
      Ctl$density = Ctl$density * sum(Ctl$counts) / sum(Ful$counts)
      plot(Trt , freq = F, add = T, col = adjustcolor('black', alpha.f = .2))
      plot(Ctl , freq = F, add = T, lty = 2 )
      
      # text(input$assign1_cut, 0, 'treatment', adj = if(input$operator1 %% 2 == 1) c(1.1, 0) else c(-.1, 0))
      # text(input$assign1_cut, 0, 'control', adj = if(input$operator1 %% 2 == 0) c(1.1, 0) else c(-.1, 0))
      axis(1)
      abline(v = input$assign1_cut, col = 'black', lty=3)
      legend(x = 'topright', legend = c('treatment','control'), 
        fill = c(adjustcolor('black', alpha.f = .2), NA),  bty='n')
      
    } else {
      par(mar = c(2,1,0.5,0))
      X1_left = if(input$assign1_dist %% 2 == 0) {
        list(
          X = seq(from = min(input$assign1_mu - 3 * input$assign1_sigma, input$assign1_cut),
            to = input$assign1_cut, length.out = 20),
          Y = dnorm(
            x = seq(from = min(input$assign1_mu - 3 * input$assign1_sigma, input$assign1_cut),
              to = input$assign1_cut, length.out = 20), 
            mean = input$assign1_mu, sd = input$assign1_sigma)
        )
      } else {
        list(
          X = seq(from = min(input$assign1_min, input$assign1_cut),
            to = input$assign1_cut, length.out = 20),
          Y = dunif(
            x = seq(from = min(input$assign1_min, input$assign1_cut),
              to = input$assign1_cut, length.out = 20),
            min = input$assign1_min, max = input$assign1_max 
          )
        )
      }
      
      X1_right = if(input$assign1_dist %% 2 == 0) {
        list(
          X = seq(to = max(input$assign1_mu + 3 * input$assign1_sigma, input$assign1_cut),
            from = input$assign1_cut, length.out = 20),
          Y = dnorm(
            x = seq(to = max(input$assign1_mu + 3 * input$assign1_sigma, input$assign1_cut),
              from = input$assign1_cut, length.out = 20), 
            mean = input$assign1_mu, sd = input$assign1_sigma)
        )
      }else{
        list(
          X = seq(to = max(input$assign1_max, input$assign1_cut),
            from = input$assign1_cut, length.out = 20),
          Y = dunif(
            x = seq(to = max(input$assign1_max, input$assign1_cut),
              from = input$assign1_cut, length.out = 20),
            min = input$assign1_min, max = input$assign1_max 
          )
        )
      }
      
      X2_left = if(input$assign2_dist %% 2 == 0)
      {
        normal = list(
          X = seq(from = min(input$assign2_mu - 3 * input$assign2_sigma, input$assign2_cut),
            to = input$assign2_cut, length.out = 20),
          Y = dnorm(
            x = seq(from = min(input$assign2_mu - 3 * input$assign2_sigma, input$assign2_cut),
              to = input$assign2_cut, length.out = 20), 
            mean = input$assign2_mu, sd = input$assign2_sigma)
        )
      } else {
        list(
          X = seq(from = min(input$assign2_min, input$assign2_cut),
            to = input$assign2_cut, length.out = 20),
          Y = dunif(
            x = seq(from = min(input$assign2_min, input$assign2_cut),
              to = input$assign2_cut, length.out = 20),
            min = input$assign2_min, max = input$assign2_max 
          )
        )
      }
      
      X2_right = if(input$assign2_dist %% 2 == 0){
        normal = list(
          X = seq(to = max(input$assign2_mu + 3 * input$assign2_sigma, input$assign2_cut),
            from = input$assign2_cut, length.out = 20),
          Y = dnorm(
            x = seq(to = max(input$assign2_mu + 3 * input$assign2_sigma, input$assign2_cut),
              from = input$assign2_cut, length.out = 20), 
            mean = input$assign2_mu, sd = input$assign2_sigma)
        )
      } else {
        list(
          X = seq(to = max(input$assign2_max, input$assign2_cut),
            from = input$assign2_cut, length.out = 20),
          Y = dunif(
            x = seq(to = max(input$assign2_max, input$assign2_cut),
              from = input$assign2_cut, length.out = 20),
            min = input$assign2_min, max = input$assign2_max 
          )
        )
      }
      
      # X1_left$Y = X1_left$Y * (max(sample_bivariate_fuzzy()$X2)-min(sample_bivariate_fuzzy()$X2)) + min(sample_bivariate_fuzzy()$X2)
      # X1_right$Y = X1_right$Y * (max(sample_bivariate_fuzzy()$X2)-min(sample_bivariate_fuzzy()$X2)) + min(sample_bivariate_fuzzy()$X2)
      # 
      # X2_left$Y = X2_left$Y * (max(sample_bivariate_fuzzy()$X1)-min(sample_bivariate_fuzzy()$X1)) + min(sample_bivariate_fuzzy()$X1)
      # X2_right$Y = X2_right$Y * (max(sample_bivariate_fuzzy()$X1)-min(sample_bivariate_fuzzy()$X1)) + min(sample_bivariate_fuzzy()$X1)
      
      
      zones= matrix(c(2,4,1,3), ncol=2, byrow=TRUE)
      layout(zones, widths=c(4/5,1/5), heights=c(1/5,4/5))
      
      par(mar=c(2,2,.5,.5))
      
      plot.new()
      plot.window(ylim = range(X2_left$X, X2_right$X, input$assign2_cut), xlim = range(X1_left$X, X1_right$X, input$assign1_cut))
      
      points(X2 ~ X1, data = sample_bivariate_fuzzy(), 
        pch = ifelse(sample_bivariate_fuzzy()$Z , 19, 1))
      
      abline(v = input$assign1_cut, col = 'black', lty=3)
      abline(h = input$assign2_cut, col = 'black', lty=3)
      
      axis(1)
      axis(2)
      
      par(mar=c(0,2,.5,.5))
      plot.new()
      plot.window(xlim = range(X1_left$X, X1_right$X, input$assign1_cut), ylim = range(0, max(X1_left$Y), max(X1_right$Y)))
      
      lines(Y  ~ X, data = X1_left,
        lty = ifelse(input$operator1 %% 2 == 1, 1, 2))  
      lines(Y ~ X, data = X1_right,
        lty = ifelse(input$operator1 %% 2 == 0, 1, 2))  
      
      polygon(c(min(X1_left$X), X1_left$X, max(X1_left$X)), c(0,X1_left$Y, 0), border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator1 %% 2 == 1, .2, .1))
      )
      
      polygon(c(min(X1_right$X), X1_right$X, max(X1_right$X)), c(0,X1_right$Y, 0), border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator1 %% 2 == 0, .2, .1))
      )
      legend(x = 'center', legend = ('A1'), bty = 'n')
      
      par(mar=c(2,0,.5,.5))
      
      plot.new()
      plot.window(ylim = range(X2_left$X, X2_right$X, input$assign2_cut), xlim = range(0, max(X2_left$Y), max(X2_right$Y)))
      
      lines(X ~ Y, data = X2_left,
        lty = ifelse(input$operator2 %% 2 == 1, 1, 2))  
      lines(X ~ Y, data = X2_right,
        lty = ifelse(input$operator2 %% 2 == 0, 1, 2))  
      
      polygon(c(0,X2_left$Y, 0), c(min(X2_left$X), X2_left$X, max(X2_left$X)), border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator2 %% 2 == 1, .2, .1))
      )
      
      polygon(c(0,X2_right$Y,0), c(min(X2_right$X), X2_right$X, max(X2_right$X)) , border = NA, 
        col = adjustcolor('gray', alpha.f = ifelse(input$operator2 %% 2 == 0, .2, .1))
      )
      legend(x = 'center', legend = ('A2'), bty = 'n')
      par(mar=c(0, 0 , .5, .5))
      
      plot.new()
      legend(x = 'center', legend = c('treatment','control'), pch = c(19,1),bty = 'n')
      
      
      
    }
  })
  
  # PLOT MODEL
  output$par_model = renderPlot(bg = 'transparent', expr = {
    if(input$allow_frontier %% 2 == 0) {
      req(sample_univariate_fuzzy())
      
      model = function(x, z, c, op) {
        if(missing(z))
          z = rddapp:::treat_assign(x, c, op)
        x = x - c
        y = ifelse(x!=0, input$par_model_constant + input$par_model_t * z + input$par_model_slope * x + input$par_model_interact * z * x,  NA)
        return(y)
      }
      
      df = within(sample_univariate_fuzzy(), {
        # y_out <- y_out + rnorm(sample.size, 0, sqrt(Var_noise))
        Y_hat = model(X, Z, input$assign1_cut, ifelse(input$operator1 %% 2 == 0, 'geq', 'leq')) + rnorm(input$sample_size, 0, sqrt(Var_noise()))
        
      })
      
      par(mar = c(3,0,0,0))
      plot.new()
      plot.window(xlim = if(input$assign1_dist %% 2 == 0) range(c(3,-3)*input$assign1_sigma+input$assign1_mu, input$assign1_cut, df$X)
        else range(min(input$assign1_min,input$assign1_cut) - .1, max(input$assign1_max,input$assign1_cut)+.1)
        ,
        ylim = range(df$Y_hat, df$Y))
      
      curve(model(x, op= ifelse(input$operator1 %% 2 == 0, 'geq', 'leq'),  c = input$assign1_cut),
        from = if(input$assign1_dist %% 2 == 0) min(-3*input$assign1_sigma+input$assign1_mu, input$assign1_cut)
        else min(input$assign1_min,input$assign1_cut) - .1
        ,
        to= input$assign1_cut,  add= T,
        col = 'black', lwd = 2
        
       )
      curve(model(x, op= ifelse(input$operator1 %% 2 == 0, 'geq', 'leq'),  c = input$assign1_cut),
        from = input$assign1_cut, 
        to= if(input$assign1_dist %% 2 == 0) 
          max(3*input$assign1_sigma+input$assign1_mu, input$assign1_cut)
        else  max(input$assign1_max,input$assign1_cut) + .1
        , 
        add= T,
        col = 'black', lwd = 2)
      
      axis(4, lwd = 0, lwd.ticks = 1, pos = input$assign1_cut , at = c(input$par_model_constant, input$par_model_t + input$par_model_constant),
        labels = c(input$par_model_constant, NA))
      axis(2, pos = input$assign1_cut , at = c(input$par_model_constant, input$par_model_t + input$par_model_constant),
        labels = c(NA, input$par_model_t + input$par_model_constant))
      axis(1)
      abline(v= input$assign1_cut, col = 'black', lty=3)
      # segments(df$X[df$Z==0], df$Y_hat[df$Z==0], y1 = df$Y[df$Z==0], col = 'darkgray')
      # segments(df$X[df$Z==1], df$Y_hat[df$Z==1], y1 = df$Y[df$Z==1], col = 'black')
      mtext('Assignment', 1, 2)
      mtext('Outcome',2, 2, at = input$assign1_cut)
      legend(x = 'bottomright', bty = 'n', legend = c('treated','untreated'), pch = c(19,1))
      
      # empirical points
      points(Y_hat ~ X, data = df[df$Z==0,], col = 'black', pch = 1)
      points(Y_hat ~ X, data = df[df$Z==1,], col = 'black', pch = 19)
      
      # # empirical ci
      # level = .95
      # pred_ci = within(
      #   data.frame(x = if(input$assign1_dist %%2 == 0)
      #     seq(min(-3*input$assign1_sigma+input$assign1_mu, input$assign1_cut),
      #       max(3*input$assign1_sigma+input$assign1_mu, input$assign1_cut),
      #       length.out = 60)
      #     else seq(min(input$assign1_min,input$assign1_cut) - .1,
      #       max(input$assign1_max,input$assign1_cut) + .1,
      #       length.out = 60)),
      #   expr = {
      #     n_hat = c(input$sample_size*diff(
      #       if(input$assign1_dist %% 2 == 0)
      #       normal = pnorm(x, mean = input$assign1_mu, sd = input$assign1_sigma)
      #       else
      #       uniform = punif(x, min = input$assign1_min, max = input$assign1_max)
      #     ), NA)
      #     y_ub = model(x) + qt((1-level)/2 , df = n_hat - 1) * sqrt(Var_noise())
      #     y_lb = model(x) - qt((1-level)/2 , df = n_hat - 1) * sqrt(Var_noise())
      #       })
      # print(pred_ci)
      # lines(y_ub ~ x, pred_ci)
      # lines(y_lb ~ x, pred_ci)
      
    } else {
      req(sample_bivariate_fuzzy())
      
     
      
     
      coefs =  c(input$par_model_constant,
        input$par_model_t1,
        input$par_model_t2,
        input$par_model_t,
        input$par_model_x1,
        input$par_model_x2,
        input$par_model_x1x2,
        input$par_model_t1x1,
        input$par_model_t1x2,
        input$par_model_t2x1,
        input$par_model_t2x2,
        input$par_model_t1x1x2,
        input$par_model_t2x1x2
      )
      gran = 10
      
      c1 = input$assign1_cut
      c2 = input$assign2_cut 
      
      if(input$assign1_dist %% 2 == 0) {
        x1_min = -3 * input$assign1_sigma + input$assign1_mu
        x1_max = 3 * input$assign1_sigma + input$assign1_mu
      } else {
        x1_min = input$assign1_min
        x1_max = input$assign1_max
      }
      
      if(input$assign2_dist %% 2 == 0) {
        x2_min = -3 * input$assign2_sigma + input$assign2_mu
        x2_max = 3 * input$assign2_sigma + input$assign2_mu
      } else {
        x2_min = input$assign2_min
        x2_max = input$assign2_max
      }
      
      ratio1 <- (c1 - x1_min) / (x1_max - x1_min)
      ratio2 <- (c2 - x2_min) / (x2_max - x2_min)
      
      # empirical
      df = within(sample_bivariate_fuzzy(), {
        cx1 <- X1 - c1
        cx2 <- X2 - c2
        y_hat = cbind(1, 
          Z1, 
          Z2, 
          Z, 
          cx1, 
          cx2, 
          cx1 * cx2, 
          Z1 * cx1, 
          Z1 * cx2, 
          Z2 * cx1, 
          Z2 * cx2, 
          Z1 * cx1 * cx2, 
          Z2 * cx1 * cx2) %*% coefs + rnorm(input$sample_size, 0, sqrt(Var_noise()))
      })
      
      
      # theoretical
      X = expand.grid(
        x1 = c(seq(x1_min, c1 - 1e-10, length.out = ifelse(gran > 4, round(gran * ratio1), 2)),
          seq(c1 + 1e-10, x1_max, length.out = ifelse(gran > 4, round(gran * (1 - ratio1)), 2))), 
        x2 = c(seq(x2_min, c2 - 1e-10, length.out = ifelse(gran > 4, round(gran * ratio2), 2)),
          seq(c2 + 1e-10, x2_max, length.out = ifelse(gran > 4, round(gran * (1 - ratio2)), 2)))
      )
      
      X$tr1 <- rddapp:::treat_assign(X$x1, c1, ifelse(input$operator1 %% 2 == 0, 'geq', 'leq'))
      X$tr2 <- rddapp:::treat_assign(X$x2, c2, ifelse(input$operator2 %% 2 == 0, 'geq', 'leq'))
      X$tr <- as.integer(X$tr1 | X$tr2)
      X$cx1 <- X$x1 - c1
      X$cx2 <- X$x2 - c2
      
      X$y = 
        cbind(1, 
          X$tr1, 
          X$tr2, 
          X$tr, 
          X$cx1, 
          X$cx2, 
          X$cx1 * X$cx2, 
          X$tr1 * X$cx1, 
          X$tr1 * X$cx2, 
          X$tr2 * X$cx1, 
          X$tr2 * X$cx2, 
          X$tr1 * X$cx1 * X$cx2, 
          X$tr2 * X$cx1 * X$cx2) %*% coefs
      
      X$quandrant <- interaction(X$tr1, X$tr2)
      
      X <- merge(X, 
        data.frame(
          quandrant = c("0.0", "0.1", "1.0", "1.1"),
          color = c(NA, "black", "black", "black")
        )
      )
      
      X <- X[order(X$x1, X$x2, X$tr1, X$tr2), ]
      
      # plotting  
      preds <- list(
        x1 = sort(c(unique(X$x1))),
        x2 = sort(c(unique(X$x2))),
        y = matrix(X$y, ncol = sqrt(nrow(X)), byrow = TRUE)
      )
      par(mar = c(0,0,0,0))
      ele_3d <- persp(preds$x1, preds$x2, preds$y, 
        xlim = range(X$x1), xlab = 'A1',
        ylim = range(X$x2), ylab = 'A2',
        zlim = range(X$y), zlab = 'Outcome',
        ticktype = 'detailed', phi = input$par_model_phi, theta = input$par_model_theta )
      by(X, X$quandrant, 
        function(frame) {
          frame <- subset(frame, frame$x1 %in% range(frame$x1) & frame$x2 %in% range(frame$x2))
          poly_3d <- as.data.frame(trans3d(frame$x1, frame$x2, frame$y, pmat = ele_3d))
          poly_3d <- poly_3d[c(1, 2, 4, 3), ]
          polygon(poly_3d$x, poly_3d$y, col = adjustcolor(frame$color, alpha.f = .2), border = NA)
        }
      )
      pts_3d <- trans3d(df$X1, df$X2, df$y_hat, pmat = ele_3d)
      points(pts_3d, pch = ifelse(df$Z, 19, 1), cex = .7)
      legend(x = 'bottomright', legend = c('treatment','control'), pch = c(19,1),
        fill = c(adjustcolor('black', alpha.f = .2), NA),  bty='n')
    }
    
    
    
  })
  
  power_pars  = reactive({
    if(input$allow_frontier%%2==0){
      list(
        cmd = 'rd_power',
        pars = list(
          sample.size = input$sample_size,
          x.dist = ifelse(input$assign1_dist %% 2==0, 'normal','uniform'),
          x.para = if(input$assign1_dist %% 2 ==0) c(input$assign1_mu, input$assign1_sigma) else c(input$assign1_min, input$assign1_max),
          x.cut = input$assign1_cut,
          x.design = ifelse(input$operator1 %% 2 == 0, 'geq', 'leq'),
          x.fuzzy = c(input$assign1_p1, input$assign1_p2),
          coeff = c(input$par_model_constant, input$par_model_t,input$par_model_slope, input$par_model_interact),
          eta.sq = input$par_model_eta2  
        )
        
      )
      
    } else {
      list(
        cmd = 'mrd_power',
        pars= list(
          sample.size = input$sample_size,
          x1.dist = ifelse(input$assign1_dist %% 2==0, 'normal','uniform'),
          x1.para = if(input$assign1_dist %% 2 ==0) c(input$assign1_mu, input$assign1_sigma) else c(input$assign1_min, input$assign1_max),
          x2.dist = ifelse(input$assign2_dist %% 2==0, 'normal','uniform'),
          x2.para = if(input$assign2_dist %% 2 ==0) c(input$assign2_mu, input$assign2_sigma) else c(input$assign2_min, input$assign2_max),
          x1.cut = input$assign1_cut,
          x2.cut = input$assign2_cut,
          x1.fuzzy = c(input$assign1_p1, input$assign1_p2),
          x2.fuzzy = c(input$assign2_p1, input$assign2_p2),
          x1.design = ifelse(input$operator1 %% 2 == 0, 'geq', 'leq'),
          x2.design = ifelse(input$operator2 %% 2 == 0, 'geq', 'leq'),
          coeff = c(
            input$par_model_constant,
            input$par_model_t1,
            input$par_model_t2,
            input$par_model_t,
            input$par_model_x1,
            input$par_model_x2,
            input$par_model_x1x2,
            input$par_model_t1x1,
            input$par_model_t1x2,
            input$par_model_t2x1,
            input$par_model_t2x2,
            input$par_model_t1x1x2,
            input$par_model_t2x1x2
          ),
          eta.sq = input$par_model_eta2
        )
      )
    }
  })
  
  return(power_pars)
}


