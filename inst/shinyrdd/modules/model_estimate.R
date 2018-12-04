model_estimateUI = function(id){
  ns = NS(id)
  tagList(
    p(),
    div(class='panel panel-default',
      div(class='panel-heading clearfix', 
        ## UI for Table 3.1
        h6('Table 3.1','Summary of Estimates', class='panel-title pull-left'),
        ## Display buttons for various parts of Table 3.1
        div(class='btn-toolbar input-group pull-right', 
          conditionalPanel(condition= "output['is_frontier']", class='btn-group btn-group-sm input-group input-group-sm ',
            span(class='input-group-addon','approach'),
            actionButton(ns('est_approach_centering'), label = 'Centering', class='active',  title = 'show models using the centering approach',
              `data-toggle`="button", `aria-pressed`="true", autocomplete="off"),
            actionButton(ns('est_approach_univariate'), label = 'Univariate', title = 'show models using the univariate approach',
              `data-toggle`="button", `aria-pressed`="false", autocomplete="off"),
            actionButton(ns('est_approach_frontier'), label = 'Frontier', title = 'show models using the frontier approach',
              `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
            # tags$button(type='button', class='btn btn-default btn-sm', style='pointer-events:none;padding-left:0px;', tags$br())
          ),
          div(class='btn-group btn-group-sm input-group input-group-sm',
            span(class='input-group-addon','model'),
            actionButton(ns('est_family_par'), label = 'Parametric',class='active',  title = 'show parameteric models',
              `data-toggle`="button", `aria-pressed`="true", autocomplete="off"),
            actionButton(ns('est_family_npar'), label = 'Nonparametric',class='active',  title = 'show non-parametric models',
              `data-toggle`="button", `aria-pressed`="true", autocomplete="off")
          ),
          conditionalPanel(condition = 'output["model_type"] == "FUZZY" | output["model_type"] == "FUZZY FRONTIER" | output["has_auxiliary"]', 
            class='btn-group input-group btn-group-sm input-group-sm',
            span(class='input-group-addon', 'extra'),
            conditionalPanel(condition = 'output["model_type"] == "FUZZY" | output["model_type"] == "FUZZY FRONTIER"', 
              class='btn-group btn-group-sm',
              actionButton(ns('est_addition_itt'), label = 'ITT', title = 'show intention-to-treat estimates',
                `data-toggle`="button", `aria-pressed`="true", autocomplete="off")
            ),
            conditionalPanel(condition = 'output["has_auxiliary"]', class='btn-group btn-group-sm',
              actionButton(ns('est_addition_aux'), label = 'COV', title = 'show covariate estimates',
                `data-toggle`="button", `aria-pressed`="true", autocomplete="off")
            )
          ),
          downloadLink(ns('est_csv'),label = NULL, class='btn btn-default btn-sm pull-right', icon('file-text-o'),
            title = 'Download Estimates as CSV')
          #, div(class='btn-group input-group', id ='heading_3_1')
          
        )
      ),
      div(class='panel-body',
        DT::dataTableOutput(ns('estimate_table')),
        uiOutput(ns('estimate_table_note'))
      )
      
    ),
    
    div(class='panel panel-default',
      div(class='panel-heading clearfix',
        h6('Figure 3.1', 'Graphical Illustration', 
          conditionalPanel(condition = 'output["df_mi"]', '(first imputed dataset)',
            style = 'display: inline;'),
          style = 'padding-top:4px;',
          class='panel-title pull-left'), 
        downloadLink(ns('rdd_plot_pdf'),label = NULL, class='btn btn-default btn-sm pull-right', icon('file-pdf-o'),
                     title = 'Download plot as PDF'),
        downloadLink(ns('rdd_plot_svg'),label = NULL, class='btn btn-default btn-sm pull-right', icon('file-o'),
                     title = 'Download plot as SVG'),
        downloadLink(ns('rdd_plot_png'),label = NULL, class='btn btn-default btn-sm pull-right', icon('file-image-o'),
                     title = 'Download plot as PNG')
      ),
      div(class='panel-body',
        fluidRow(
          column(8,  
            plotOutput(ns('rdd_plot'), height = '500px')
          ),
          column(4, 
            conditionalPanel(
              condition = "'SHARP' != output['model_type']",
              h6(class='badge','Model Type'),
              selectizeInput(ns('plot_type'), label = NULL, choices = c()),
              hr()
            ),
            conditionalPanel(condition = sprintf('input["%s"] != "frontier"', ns('plot_type')) ,
              h6(class='badge','Predicted Lines'),
              selectizeInput(ns('rdd_which'), label = NULL, multiple = T,
                selected = 'Optimal', 
                choices = list('Hide' = '',
                  'non-parametric' = c('Optimal', 'Half', 'Double'),
                  'parametric' = c('Linear','Quadratic','Cubic'))),
              div(class='input-group',
                span(class='input-group-addon',  style='width:20%', tags$small('CI')),
                numericInput(ns('rdd_prd_level'), label = NULL, min = 0, max = 100, step = .01, value = 95, width = '100%'),
                span(class='input-group-addon', tags$small('%'), style='border-left:0;'),
                span(class='input-group-btn',
                  actionButton(ns('rdd_prd_level_area'), label = 'fill area', class='active',title = 'show CIs as filled areas',
                    `data-toggle`="button", `aria-pressed`="true", autocomplete="off")
                )
                
              ),
              hr(),
              h6(class='badge','Data Points'),
              selectizeInput(ns('rdd_point'), label = NULL,
                choices = list('Raw', 'Hide' = '', 'Binned' = c('Evenly','Quantile'))
              ),
              conditionalPanel(condition = sprintf('input["%s"] == "Evenly" | input["%s"] == "Quantile"', ns('rdd_point'), ns('rdd_point')),
                div(class='input-group', style='margin-bottom:5px;',
                  span(class='input-group-addon', style='width:20%', tags$small('bins')),
                  numericInput(ns('rdd_nbins'), label = NULL, value = 20, min = 2, step = 1, width='100%'),
                  span(class='input-group-btn',
                    actionButton(ns('rdd_point_binsize'), label = 'scale point size', class = 'active',
                      `data-toggle`="button", `aria-pressed`="true", autocomplete="off") 
                  )
                ),
                div(class='input-group',
                  span(class='input-group-addon', style='width:20%', tags$small('CI')),
                  numericInput(ns('rdd_bin_level'), label = NULL, 
                    value = 95, min = 0, max = 100, step = .01, width = '100%'),
                  span(class='input-group-addon', tags$small('%'), style='border-left:0;'),
                  span(class='input-group-btn',
                    actionButton(ns('rdd_bin_level_hide'), label = 'hide error bars',title = 'show CIs as filled areas',
                      `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
                  )
                )
              )
              
            ),
            conditionalPanel(condition = sprintf('input["%s"] == "frontier"', ns('plot_type')) ,
              h6(class='badge','Model Specification'),
              selectizeInput(ns('mfrd_model'), label = NULL, multiple = F,
                choices = c('Unconstrained'='m_s','Heterogeneous Effect' = 'm_h','Constant Effect' = 'm_t') 
              ),
              h6(class='badge','View'),
              selectizeInput(ns('mfrd_view'), label = NULL, multiple = F,
                choices = c('Perspective' = 'custom','Top'='top','Front' = 'a1','Side' = 'a2') 
              ),
              conditionalPanel(condition= sprintf('input["%s"] == "custom"', ns('mfrd_view')),
                fluidRow(
                  column(6, h6(class='badge',icon('arrows-v')),
                    sliderInput(ns('mfrd_phi'), label = NULL, min = -180, 
                      max = 180, value = 30, ticks = F, post = '°')
                  ),
                  column(6, h6(class='badge',icon('arrows-h')),
                    sliderInput(ns('mfrd_theta'), label = NULL,min = -180, 
                      max = 180, value = -30, ticks = F, post = '°')
                  )
                )
              ),
              # h6(class='badge', 'Surface'),
              div(class='btn-group input-group input-group-sm', style='margin-bottom:5px;width:100%;',
                span(class='input-group-addon',style='width:25%;','panel'),
                actionButton(ns('mfrd_color_surface'), label = 'color', style='width:40%;',
                  `data-toggle`="button", `aria-pressed`="false", autocomplete="off"),
                actionButton(ns('mfrd_grid'), label = 'grid', style='width:30%;',
                  `data-toggle`="button", `aria-pressed`="false", autocomplete="off"),
                actionButton(ns('mfrd_shade'), label = 'shade', style='width:30%;',
                  `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
              ),
              # h6(class='badge', 'Data'),
              div(class='btn-group input-group input-group-sm', style='width:100%;',
                span(class='input-group-addon', style='width:25%;', 'data'),
                actionButton(ns('mfrd_raw_scale'), label = 'original scale', style='width:55%;',
                  `data-toggle`="button", `aria-pressed`="false", autocomplete="off"),
                actionButton(ns('mfrd_raw_data'), label = 'raw data', style='width:45%;',
                  `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
                # ,actionButton(ns('mfrd_local_data'), label = 'Local',
                # `data-toggle`="button", `aria-pressed`="false", autocomplete="off")
              )
            )
          )
        )
      )
    )
  )
}

model_estimate = function(input, output, session, dataframe, parameter, model_type){
  #### UPDATE MODEL TYPE SELECTIZERS ####
  
  # observe({
  #   updateSelectizeInput(session, 'est_addition',
  #     choices = c('additional estimate' = '',
  #       if(grepl('FUZZY', model_type()$type, fixed = T)) {
  #         c('Intention-to-treat' = 'itt',
  #           if (length(parameter$auxiliary())>0) c('Covariate' = 'aux'))
  #       } else {
  #         if (length(parameter$auxiliary())>0) c('Covariate' = 'aux')
  #       }),
  #     selected = isolate(input$est_addition))
  # })
  
  observe({
    available_choices = switch(model_type()$type,
      'FUZZY' = c('Complier Average' = 'regular', 'Intention-to-Treat' = 'itt'),
      'SHARP' = c('Complier Average' = 'regular'),
      'SHARP FRONTIER' = c('Centering' = 'center','Univariate (A1)' = 'univ1','Univariate (A2)' = 'univ2',   'Frontier' = 'frontier'),
      list(
        'complier average'   = c('Centering' = 'center','Univariate (A1)' = 'univ1','Univariate (A2)' = 'univ2',   'Frontier' = 'frontier'),
        'intention-to-treat' = c('Centering - ITT' = 'center_itt', 'Univariate (A1) - ITT' = 'univ1_itt','Univariate (A2) - ITT' = 'univ2_itt'
          # ,  'Frontier' = 'frontier_itt'
        )))
    
    current_plot_type = intersect(isolate(input$plot_type), available_choices)
    if(length(current_plot_type)==0){
      if(isolate(parameter$is_frontier())) 
        current_plot_type = 'center'
      else
        current_plot_type = 'regular'
    }
    
    updateSelectizeInput(session, 'plot_type', label = NULL, 
      choices =  available_choices,
      selected = current_plot_type
    )
  })
  
  #################################################
  #### GET ESTIMATES / SUMMARIES / PREDICTIONS ####
  #################################################
  
  result = reactiveValues()
  
  result$model = reactive({
    model = try(withProgress(message = 'Estimating...', value = NULL,
      expr = {
        if(!parameter$is_frontier()) {
          if(is.null(attr(dataframe(), 'mi_id'))) {
            rd_est(
              formula = parameter$formula(), 
              data = dataframe(), 
              cutpoint = parameter$cutoff1(),
              se.type = parameter$se_type(),
              cluster = if(parameter$se_type()=='cluster') get_columns(dataframe(), parameter$cluster_id()),
              kernel = parameter$kernel_type(),
              t.design = parameter$operator1(),
              est.cov = T
            )
          } else {
            rd_impute(
              formula = parameter$formula(), 
              data = dataframe(), 
              impute = dataframe()[, attr(dataframe(), 'mi_id')],
              cutpoint = parameter$cutoff1(),
              se.type = parameter$se_type(),
              cluster = if(parameter$se_type()=='cluster') get_columns(dataframe(), parameter$cluster_id()),
              kernel = parameter$kernel_type(),
              t.design = parameter$operator1(),
              est.cov = T
            )
          }
          
        } else {
          if(is.null(attr(dataframe(), 'mi_id'))) {
            eval(bquote(
              mrd_est(
                formula = parameter$formula(), 
                data = dataframe(), 
                cutpoint = c(.(parameter$cutoff1()), .(parameter$cutoff2())),
                t.design = c(parameter$operator1(), parameter$operator2()),
                se.type = parameter$se_type(),
                cluster = if(parameter$se_type()=='cluster')  get_columns(dataframe(), parameter$cluster_id()),
                kernel = parameter$kernel_type(),
                est.cov = T,
                boot = if(parameter$se_frontier_boot()) parameter$se_frontier_boot_n() else NULL
              )
            ))
            
            
            
            
          } else {
            eval(
              bquote(
                mrd_impute(
                  formula = parameter$formula(), 
                  data = dataframe(), 
                  impute = get_columns(dataframe(), attr(dataframe(), 'mi_id')),       
                  cutpoint = c(.(parameter$cutoff1()), .(parameter$cutoff2())),
                  t.design = c(parameter$operator1(), parameter$operator2()),
                  se.type = parameter$se_type(),
                  cluster = if(parameter$se_type()=='cluster')  get_columns(dataframe(), parameter$cluster_id()),
                  kernel = parameter$kernel_type(),
                  est.cov = T,
                  boot = if(parameter$se_frontier_boot()) parameter$se_frontier_boot_n() else NULL
                )
              )
            )
            
          }
          
        }
      })
      
    )
    validate(
      need(model, message = sprintf('Model estimation failed\n %sCheck model specification.', model))
    )
    return(model)
  })
  
  result$itt_model = reactive({
    model = withProgress(message = 'Estimating ITT models...', value = NULL,
      expr = try({
        if(!parameter$is_frontier()) {
          if(is.null(attr(dataframe(), 'mi_id'))) {
            rd_est(
              formula = parameter$formula(), 
              data = dataframe(), 
              cutpoint = parameter$cutoff1(),
              se.type = parameter$se_type(),
              cluster = if(parameter$se_type()=='cluster') get_columns(dataframe(), parameter$cluster_id()),
              kernel = parameter$kernel_type(),
              t.design = parameter$operator1(),
              est.itt = T
            )
          } else {
            rd_impute(
              formula = parameter$formula(), 
              data = dataframe(), 
              impute = get_columns(dataframe(), attr(dataframe(), 'mi_id')),       
              cutpoint = parameter$cutoff1(),
              se.type = parameter$se_type(),
              cluster = if(parameter$se_type()=='cluster') get_columns(dataframe(), parameter$cluster_id()),
              kernel = parameter$kernel_type(),
              t.design = parameter$operator1(),
              est.itt = T
            )
          }
          
        } else {
          if(is.null(attr(dataframe(), 'mi_id'))) {
            bquote(
              mrd_est(
                formula = parameter$formula(), 
                data = dataframe(), 
                cutpoint = c(.(parameter$cutoff1()),.(parameter$cutoff2())),
                t.design = c(parameter$operator1(), parameter$operator2()),
                se.type = parameter$se_type(),
                cluster = if(parameter$se_type()=='cluster')  get_columns(dataframe(), parameter$cluster_id()),
                kernel = parameter$kernel_type(),
                est.itt = T
              )
            )
          } else {
            bquote(
              mrd_impute(
                formula = parameter$formula(), 
                data = dataframe(), 
                impute = get_columns(dataframe(), attr(dataframe(), 'mi_id')),          
                cutpoint = .(c(parameter$cutoff1(), parameter$cutoff2())),
                t.design = c(parameter$operator1(), parameter$operator2()),
                se.type = parameter$se_type(),
                cluster = if(parameter$se_type()=='cluster')  get_columns(dataframe(), parameter$cluster_id()),
                kernel = parameter$kernel_type(),
                est.itt = T
              )
            )
            
          }
          
        }
      })
    )
    validate(
      need(model, message = sprintf('ITT model estimation failed\\n %s. Check model specification.', model))
    )
    return(model)
  })
  
  result$table = reactive({
    
    
    if(class(result$model()) == 'rd'){
      if(model_type()$type == 'FUZZY' & input$est_addition_itt %% 2 ==1 )
        tab_itt = format_summary_table(result$itt_model(),
          sections =  list('Parametric:regular:itt'=1:3, 'Nonparametric:regular:itt'=4:6),
          label_surfix = 'ITT') else tab_itt = NULL
          
          tab = format_summary_table(result$model(),
            sections =  list('Parametric:regular'=1:3, 'Nonparametric:regular'=4:6),
            tab_itt = tab_itt)
          return(tab)
    }
    
    if(class(result$model()) == 'mrd'){
      if(model_type()$type == 'FUZZY FRONTIER' & input$est_addition_itt %%2 == 1){
        tab_itt_center = format_summary_table(result$itt_model()$center$tau_MRD, 
          sections =  list('Parametric:centering:itt'=1:3, 'Nonparametric:centering:itt'=4:6),
          label_surfix = 'ITT')
        tab_itt_univ1 = format_summary_table(result$itt_model()$univ$tau_R, 
          sections =  list('Parametric:univariate:itt'=1:3, 'Nonparametric:univariate:itt'=4:6),
          label_surfix = 'ITT')
        tab_itt_univ2 = format_summary_table(result$itt_model()$univ$tau_M, 
          sections =  list('Parametric:univariate:itt'=1:3, 'Nonparametric:univariate:itt'=4:6),
          label_surfix = 'ITT')
      } else {
        tab_itt_center = NULL
        tab_itt_univ1 = NULL
        tab_itt_univ2 = NULL
      }
      
      tab_center = format_summary_table(result$model()$center$tau_MRD, 
        sections = list('Parametric:centering'=1:3, 'Nonparametric:centering'=4:6),
        tab_itt = tab_itt_center)
      tab_univ1 = format_summary_table(result$model()$univ$tau_R,  
        sections = list('Parametric:univariate'=1:3, 'Nonparametric:univariate'=4:6),
        tab_itt = tab_itt_univ1)
      tab_univ2 = format_summary_table(result$model()$univ$tau_M, 
        sections = list('Parametric:univariate'=1:3, 'Nonparametric:univariate'=4:6),
        tab_itt= tab_itt_univ2)

      ## Create labels for all model types based on the diff levels of
      ## organization
      label_lv1 <- c("Parametric (linear)", "Nonparametric (crossvalidated bandwidth)")
      label_lv2 <- c("- Unconstrained", "- Heterogeneous Effect", "- Constant Effect")
      label_lv3 <- c("-- Frontier 1", "-- Frontier 2", "-- Average")
      
      label_vec <- c()
      label_vec_temp <- c()
      for (i in 1:length(label_lv1)){
        label_vec <- c(label_vec, label_lv1[i])
        for (j in 1:length(label_lv2)){
          label_vec <- c(label_vec, label_lv2[j], label_lv3)
          label_vec_temp <- c(label_vec_temp, label_lv3)
        }
      }
      
      ## Get estimates for table
      n <- c(rep(result$model()$front$tau_MRD$obs$Param[1], 3),
             rep(result$model()$front$tau_MRD$obs$Param[2], 3),
             rep(result$model()$front$tau_MRD$obs$Param[3], 3),
             rep(result$model()$front$tau_MRD$obs$bw[1], 3),
             rep(result$model()$front$tau_MRD$obs$bw[2], 3),
             rep(result$model()$front$tau_MRD$obs$bw[3], 3))
      est <- c(t(result$model()$front$tau_MRD$est)[,1:2])
      se <- c(t(result$model()$front$tau_MRD$se)[,1:2])
      ci <- result$model()$front$tau_MRD$ci
      if (!is.null(ci)){ ## if we actually have confidence intervals
        ## (because bootstrapping has been performed)
        ci <- ci[1:4,] ## first four rows,
        ## since rows 1:2 correspond to 2.5% and 97.5% for parametric
        ## model, respsectively, while 3:4 is the same but for
        ## non-parametric w/ optimal bandwidth
        
        ## Transpose and stack rows so columns are 2.5% and 97.5% and
        ## rows are first parametric ones, then non-parametric
        ci <- data.frame(lower = c(t(ci[1,]), t(ci[3,])),
                         upper = c(t(ci[2,]), t(ci[4,])))
        
        ## Glue lower and upper bounds of ci into one nicely formatted
        ## String to display
        ci <- apply(ci, 1,
                    function(bounds) sprintf('[%.3f, %.3f]',
                                                    bounds[1],
                                                    bounds[2]))
      } else {
        ci <- NA
      }
      d <- c(t(result$model()$front$tau_MRD$d)[,1:2])
      
      ## Construct table for frontier approach
      tab_front = data.frame(
        label = label_vec_temp,
        bw = '-',
        n = n,
        # FIXME: using N from the center approach
        # get observations returned to
        # result$model()$center$tau_MRD$obs
        est = est,
        se = se,
        z = est/se,
        df = NA,
        p = pnorm(abs(est/se), lower.tail = F) * 2,
        ci = ci,
        d = d,
        stringsAsFactors = F
        )
      
      ## Replace numerical NA entries (uncomputed quantities) with dashes
      tab_front[is.na(tab_front)] <- '-'
      
      ## Add blank rows for labelling purposes
      ## (adding level 1 and level 2 label rows)
      tab_front = rbind(NA, NA, # for "Parametric (linear)" and
                        # "- Average" labels
                        tab_front[1:3,],
                        NA, # for "- Frontier 1" label
                        tab_front[4:6,],
                        NA, # for "- Frontier 2" label
                        tab_front[7:9,],
                        NA, NA,
                        tab_front[10:12,],
                        NA,
                        tab_front[13:15,],
                        NA,
                        tab_front[16:18,],
        stringsAsFactors = F)
      
      ## Add full set of model labels to table
      tab_front$label <- label_vec
      
      ## Add column to denote model type of each row for shiny UI to 
      ## know to display when need be
      tab_front$type <-  c(rep('parametric:frontier', 13),
                           rep('nonparametric:frontier', 13))
      
      ## Make sure names match up to other tables for display purposes
      ## (will be rbinding them together in a sec)
      names(tab_front) = names(tab_univ1)
      
      ## THIS IS WHERE THE ERROR OCCURS -- DOESN'T SEEM TO GET CALLED WHEN WE ONLY HAVE ONE ASST RULE
      tab = rbind(
        c('Centering', rep(NA, ncol(tab_center)-2),'centering'), tab_center, 
        c('Univariate (A1)', rep(NA, ncol(tab_univ1)-2),'univariate'), tab_univ1, 
        c('Univariate (A2)', rep(NA, ncol(tab_univ2)-2),'univariate'), tab_univ2, 
        c('Frontier', rep(NA, ncol(tab_front)-2),'frontier'), tab_front) 
      ## problem is in trying to rbind tab_front 
      ## (other three mini-tables bind just fine)
      ## centering and univerate mini tables have 11 variables,
      ## frontier mini-table has 43 variables...
      
      ## ISSUE: ncol of frontier mini table does not match up
      ## have 11 "real" variables (match up w/ the other mini tables),
      ## but for some reason, 32 NA cols also get created in code for
      ## tab_front...
      ## tab_front is initialized with 4 obs of 42 variables instead
      ## of ?? (8?) obvs of 11 variables
      
      ## note that in tab_center, univ1, univ2, the 8 "observations"
      ## are: "Parametric"       "- Linear"         "- Quadratic"      
      ## "- Cubic"          "Nonparametric"   "- Optimal"        
      ## "- Half-Optimal"   "- Double-Optimal"
      ## and the 11 "variables" are: "label" "bw"    "n"     "est"   
      ## "se"    "z"     "df"    "p"     "l95"   "es"    "type" 
      
      return(tab)
    }
  })
  
  prediction = reactiveValues()
  
  prediction$regular = reactive({
    req(class(result$model())=='rd')
    withProgress(message = 'Plotting...',detail = 'predict complier average models',value = NULL,
      expr = predict(result$model())
    )
  })
  prediction$itt = reactive({
    req(class(result$model())=='rd')
    withProgress(message = 'Plotting...', detail = 'predict intention-to-treat models',value = NULL,
      expr = {
        expr = predict(result$itt_model())
      }
    )
  })
  prediction$center = reactive({
    req(class(result$model())=='mrd')
    withProgress(message = 'Plotting...', detail='frontier models (centering)',value = NULL,
      expr = predict(result$model()$center$tau_MRD)
    )
  })
  prediction$center_itt = reactive(
    withProgress(message = 'Plotting...', detail='predict frontier models (centering - ITT)',value = NULL,
      expr = predict(result$itt_model()$center$tau_MRD)
    )
  )
  prediction$univ1 = reactive({
    req(class(result$model())=='mrd')
    withProgress(message = 'Plotting...', detail='predict frontier models (univariate A1)',value = NULL,
      expr = predict(result$model()$univ$tau_R)
    )
  })
  prediction$univ1_itt = reactive({
    req(class(result$model())=='mrd')
    withProgress(message = 'Plotting...', detail='predict frontier models (univariate A1 - ITT)',value = NULL,
      expr = predict(result$itt_model()$univ$tau_R)
    )
  })  
  prediction$univ2 = reactive({
    req(class(result$model())=='mrd')
    withProgress(message = 'Plotting...', detail='predict frontier models (univariate A2)',value = NULL,
      expr = predict(result$model()$univ$tau_M)
    )
  })
  prediction$univ2_itt = reactive({
    req(class(result$model())=='mrd')
    withProgress(message = 'predict...', detail='predict frontier models (univariate A2 - ITT)',
      expr = predict(result$itt_model()$univ$tau_M)
    )
  })    
  
  # GENERATE ESTIMATE TABLE
  output$estimate_table = DT::renderDataTable(server = F,
    expr = {
      # # CREATE A MULTI COLUMN TABLE HEAD
      # header = tags$table(class = 'display', 
      #   tags$thead(
      #     tags$tr(
      #       tags$th(),
      #       tags$th(em('N')),
      #       tags$th('Est.') ,
      #       tags$th(em('SE')),
      #       tags$th(em(if(parameter$se_type() != 'const') 'z' else 't')),
      #       tags$th(em('p')),
      #       tags$th(colspan = 2, '95% CI', style = 'text-align: right'),
      #       tags$th("Cohen's d")
      #     )
      #   )
      # )
      req(result$table())
      
      dt = DT::datatable(
        data = subset(
          result$table(), 
          subset =
            (input$est_family_par%%2==0 & grepl('^parametric', `type`, fixed = F) |
                input$est_family_npar%%2==0 & grepl('^nonparametric', `type`, fixed = F) |
                `type` %in% c('centering', 'univariate','frontier')  ) &
            (!grepl('itt', `type`, fixed = T) | input$est_addition_itt %% 2 == 1) &
            (!grepl('aux', `type`, fixed = T) | input$est_addition_aux %% 2 == 1) &
            (!grepl('centering', `type`, fixed = T) |  input$est_approach_centering%%2==0) &
            (!grepl('univariate', `type`, fixed = T) | input$est_approach_univariate%%2==1) &
            (!grepl('frontier', `type`, fixed = T) | input$est_approach_frontier%%2==1),
          select = -`type`
        ),
        extensions = 'Buttons',
        options = list(
          columnDefs = list(
            list(visible = parameter$se_type() == 'const'| !is.null(attr(dataframe(), 'mi_id')), 
              targets = 6),
            list(className = 'dt-right', targets = c(1:9))
          ),
          responsive = T,
          language.emptyTable = 'Estimation Failed.',
          scrollX = T,
          scrollY = T,
          pageLength = 999,
          searching = F,
          ordering = F,
          dom = 't',
          autoWidth = F
          
          # ,buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
        ),
        rownames = F,
        colnames = c('','Bandwidth','N','Est.', 'SE', 
          ifelse(parameter$se_type() == 'const'| !is.null(attr(dataframe(), 'mi_id')), 't', 'z'), 
          'df','p', '95% CI', "Cohen's d"),
        selection = 'none'
        # container = header
      )
      # print(result$table())
      
      all_labels = unique(result$table()$label)
      cov_labels = all_labels[grepl('+', all_labels, fixed = T)]
      secondary_labels = all_labels[grepl('-', all_labels, fixed = T)]
      super_labels = unique(result$table()$label[!grepl(':', result$table()$type, fixed=T)])
      primary_labels = setdiff(all_labels, c(cov_labels, secondary_labels))
      # 
      # print('COV_LABELS')
      # print(cov_labels)
      # print('SECONDARY LABELS')
      # print(secondary_labels)
      # print('PRIMARY LABELS')
      # print(primary_labels)
      # print('SUPER LABELS')
      # print(super_labels)
      
      # format column digits
      dt = DT::formatRound(dt, columns = c(2, 4:6, 8:10),digits = 3)
      # dt = DT::formatString(dt, columns = c(7,8), prefix = '[', suffix = ']')
      
      # format rows
      bold_labels = if(length(super_labels)>0) super_labels else primary_labels 
      dt = DT::formatStyle(dt, 'label' ,target = 'row',
        fontWeight = DT::styleEqual(bold_labels , rep('bold',length(bold_labels))),
        `background-color` = DT::styleEqual(all_labels , 
          ifelse(all_labels %in% super_labels, '#e7e7e7',
            ifelse(all_labels %in% primary_labels, '#f5f5f5',
              ifelse(all_labels %in% secondary_labels, '#ffffff', 
                '#f8f8f8')))),
        `color` = DT::styleEqual(all_labels , 
          ifelse(all_labels %in% super_labels, '#000000',
            ifelse(all_labels %in% primary_labels, '#000000',
              ifelse(all_labels %in% secondary_labels, '#000000', 
                '#888888'))))
      )
      
      # format cov rows and secondary rows
      if(length(cov_labels)>0){
        dt = DT::formatStyle(dt, 'label', target = 'cell',
          `text-indent` =  DT::styleEqual(all_labels , 
            ifelse(all_labels %in% super_labels, '0px',
              ifelse(all_labels %in% primary_labels, '0px',
                ifelse(all_labels %in% secondary_labels, '0px', 
                  '10px'))))
        )
      }
      
      
      return(dt)
      
    })  
  
  output$estimate_table_note = renderUI({
    req(result$table())
    h6(em('Note.'), 
      'Parametric = two-Stage least square (2SLS) polynomial regression,', 
      'Non-parametric = 2SLS local linear regression,',
      'using', parameter$kernel_type(), 'kernel',
      'with optimal bandwidth, half-optimal bandwidth, and double-optimal bandwidth (Imbens & Kalyanaraman, 2012).',
      if(length(parameter$auxiliary())>0) 
        sprintf('Covariates: %s.', paste(parameter$auxiliary(), collapse = ',')),
      sprintf('Type of SE is %s.',parameter$se_type()))
  })
  
  output$est_csv = downloadHandler( filename = 'rdd_estimate.csv',
    content = function(file) {
      write.csv(result$table(), file, row.names = F)
    })
  #### GENERATE PLOT ####
  
  rdd_plot_estimate = function(){
    if (input$plot_type!='frontier') {
      
      selected_model = switch(input$plot_type, 
        'regular' = result$model(), 
        'itt' = result$itt_model(),
        'center' = result$model()$center$tau_MRD,
        'center_itt' = result$itt_model()$center$tau_MRD,
        'univ1' = result$model()$univ$tau_R,
        'univ1_itt' = result$itt_model()$univ$tau_R,
        'univ2' = result$model()$univ$tau_M,
        'univ2_itt' = result$itt_model()$univ$tau_M
      )
      
      req(class(selected_model) == 'rd',
        input$rdd_prd_level >= 0, 
        input$rdd_prd_level < 100,
        input$rdd_bin_level >= 0, 
        input$rdd_bin_level < 100
      )
      
      plot(selected_model,
        preds = prediction[[input$plot_type]](),
        fit_line = tolower(input$rdd_which),
        fit_ci = ifelse(input$rdd_prd_level_area %% 2 == 0, 'area','dot'),
        fit_ci_level = input$rdd_prd_level/100,
        bin_n = switch(input$rdd_point, 'Evenly' = input$rdd_nbins, 'Quantile' = input$rdd_nbins, 'Raw' = 0, -1),
        bin_level = if(input$rdd_bin_level_hide %%2 == 1) 0 else input$rdd_bin_level/100,
        quant = input$rdd_point== 'Quantile',
        bin_size = if(input$rdd_point != 'Raw' & input$rdd_point_binsize %% 2 == 0) c('size') else c(),
        include_rugs = input$rdd_point != 'Raw'
      )
      
      xlab = if(input$plot_type %in% c('univ2_itt','univ2')){
        isolate(parameter$assignment2())
      } else {
        if(input$plot_type %in% c('center','center_itt'))
          'collapsed assignment'
        else
          isolate(parameter$assignment1())
      } 
      mtext(xlab,  1,  2)
      mtext(isolate(parameter$outcome()), 2, 2)
      grid(col = 'black')
    } else {
      plot(result$model()$front$tau_MRD, 
        model = input$mfrd_model, 
        phi = switch(input$mfrd_view, 'top' = 90, 'a1' = 0, 'a2' = 0,
          input$mfrd_phi),
        theta = switch(input$mfrd_view, 'top' = 0, 'a1' = 0, 'a2' = 90,
          input$mfrd_theta),
        d = ifelse(input$mfrd_view == 'custom', 1, 1e10),
        color_surface = input$mfrd_color_surface %% 2 == 1,
        raw_data = input$mfrd_raw_data %% 2 == 1,
        # local_data = input$mfrd_local_data %% 2 == 1,
        xlab = parameter$assignment1(),
        ylab = parameter$assignment2(),
        zlab = parameter$outcome(),
        gran = if(input$mfrd_grid %% 2 == 1) 16 else 2,
        shade = if(input$mfrd_shade %% 2 == 1) .4 else NA,
        scale = input$mfrd_raw_scale %% 2 == 0, 
        ticktype = 'detailed')
    }
  }
  
  ## PLOT
  output$rdd_plot = renderPlot(bg = 'transparent',
    expr = {
      par(mar = c(3,3,.5,0.2))
      rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = "white")
      par(new=T)
      rdd_plot_estimate()
    })

  output$rdd_plot_png = downloadHandler(
    filename = 'rdd_estimate.png',
    content = function(file) {
      png(file, width=700, height=600, res=150)
      rdd_plot_estimate()
      dev.off()
    }
  )    

  output$rdd_plot_svg= downloadHandler(
    filename = 'rdd_estimate.svg',
    content = function(file) {
      svg(file)
      rdd_plot_estimate()
      dev.off()
    }
  )  
    
  output$rdd_plot_pdf = downloadHandler(
    filename = 'rdd_estimate.pdf',
    content = function(file) {
      pdf(file)
      rdd_plot_estimate()
      dev.off()
    }
  )
  
  return(result)
}
