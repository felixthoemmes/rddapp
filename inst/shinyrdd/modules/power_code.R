# Module UI
power_codeUI = function(id){
  ns = NS(id)
  tagList(
    p(),
    div(class='panel panel-default',
      div(class='panel-heading clearfix',
        h6(class = 'panel-title pull-left',
          'Script 2')
        # ,
        # downloadLink(ns('r_file'),label = NULL, class='btn btn-default btn-sm pull-right', 
        #   icon('file-text-o'),
        #   title = 'Download the script')
      ),
      div(class='panel-body', style='min-height: 480px;',
        verbatimTextOutput(ns('code'))
      )
    )
  )
}


# Module Server

power_code = function(input, output, session, parameter, result){
  output$code = renderText(
    expr = {
      lines = c(
        'library(rddapp)',
        sprintf('%s(num.rep = 100, sample.size = %s, eta.sq = %s, 
          %s, 
          coeff = %s)',
          parameter()$cmd,
          parameter()$pars$sample.size,
          parameter()$pars$eta.sq,
          switch(parameter()$cmd,
            'rd_power' = sprintf(
              'x.dist = "%s", x.para = %s, x.cut = %s, 
          x.fuzzy = %s, x.design = "%s"',
              parameter()$pars$x.dist,
              deparse(parameter()$pars$x.para),
              parameter()$pars$x.cut,
              deparse(parameter()$pars$x.fuzzy),
              parameter()$pars$x.design
            ),
            'mrd_power' = sprintf(
              'x1.dist = "%s", x1.para = %s, x1.cut = %s, 
          x1.fuzzy = %s, x1.design = "%s",
          x2.dist = "%s", x2.para = %s, x2.cut = %s, 
          x2.fuzzy = %s, x2.design = "%s"',
              parameter()$pars$x1.dist,
              deparse(parameter()$pars$x1.para),
              parameter()$pars$x1.cut,
              deparse(parameter()$pars$x1.fuzzy),
              parameter()$pars$x1.design,
              parameter()$pars$x2.dist,
              deparse(parameter()$pars$x2.para),
              parameter()$pars$x2.cut,
              deparse(parameter()$pars$x2.fuzzy),
              parameter()$pars$x2.design
            )
          ),
          deparse(parameter()$pars$coeff, width.cutoff = 500, nlines = 1)
        )
      )
      paste(lines, collapse = '\n')
    })
}