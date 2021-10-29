tagList(
  tags$head(
    tags$style(HTML("
      .panel-title {
        font-size: 13px!important;
      }
      .panel-title.pull-left {
        padding-top: 8px!important;
      }
      .badge {
        background-color: #428bca;
      }
      .input-group-addon.tail {
        padding:2.5px;
      }
      .panel-body .input-group {
        margin-bottom:5px;
      }
    "))
  ),
  
  navbarPage(title = "ShinyRDD", collapsible = T, 
    ## Model Page (i.e., do every major)
    tabPanel("Model", icon = icon('tachometer-alt'),
      source(file.path('ui_elements', 'page_model.R'), local = T)$value
    ),
    
    ## Power Analysis Page (i.e., do every major)
    tabPanel("Power", icon = icon('stethoscope'),
      source(file.path('ui_elements', 'page_power.R'), local = T)$value
    ),
    
    ## DROP REPORT PAGE, ALLOCATE IT TO MODEL PAGE
    ## Report Page
    # tabPanel("Report", icon = icon('file-text'),
    #   source(file.path('ui_elements', 'page_report.R'), local = T)$value
    # ),
    
    ## More Pages
    #navbarMenu("More", icon = icon('ellipsis-v'),
      # Help Page
      # tabPanel("Help", icon = icon('book'),
      #   source(file.path('ui_elements', 'page_help.R'), local = T)$value
      # ),
      tabPanel("Help", icon = icon('book'),
               source(file.path('ui_elements', 'page_help.R'), local = T)$value
      ),
      # About Page
      tabPanel("About", icon = icon('info-circle'), style="height:100%;",
        source(file.path('ui_elements', 'page_about.R'), local = T)$value
      )
    #)
  )
)
