sidebarLayout(
  
  # Sidebar ()
  sidebarPanel(width = 4,
    helpText("I receive inputs for formating a report."),
    shinyjs::disabled(
      checkboxInput('OK','OK')
    )
  ),
  # Main Panel
  mainPanel(width = 8,
    helpText("I show a rendered report in markdown.")
  )
)