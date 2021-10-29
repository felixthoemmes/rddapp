navlistPanel(widths = c(3,9),
  tabPanel("Author Information", 
    wellPanel(
      
      helpText("This software was developed by Felix Thoemmes, 
        Wang Liao, Ze Jin, Wenyu Zhang, Irena Papst, and Kim Hochstedler, all at Cornell University.", 
      "We are happy to receive feedback! Please send it to"),
      img(src='fjt_email.png'),
      
      hr(),
      helpText("The project was supported by the Institute of Education Sciences, U.S. Department of Education, through Grant R305D150029. The opinions expressed are those of the authors and do not represent
views of the Institute or the U.S. Department of Education")
    )
  ),
  tabPanel("References",
    helpText("Cite these."))
)
