# Module UI
model_codeUI = function(id){
  ns = NS(id)
  tagList(
    p(),
    div(class='panel panel-default',
      div(class='panel-heading clearfix',
        h6(class = 'panel-title pull-left',
          'Script 1')
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

model_code = function(input, output, session, inputfile, parameter, result){
  output$code = renderText(
    expr = {
      
      lines = c(
        # library(rddapp)
        'library(rddapp)',
        '# read data',
        sprintf('dat <- %s("%s")', switch(
          tolower(substring(inputfile$filename(), nchar(inputfile$filename())-3)),
          '.csv' = 'read.csv',
          '.sav' = 'read.spss',
          'get'),
          inputfile$filename()
          ),
        '\n# sorting test',
        sprintf('dc_test(dat$%s, cutpoint = %s)',
          parameter$assignment1(),
          parameter$cutoff1()
        ),
        if(parameter$is_frontier()){
          sprintf('dc_test(dat$%s, cutpoint = %s)',
          parameter$assignment2(),
          parameter$cutoff2()
          )
        },
        '\n# estimate rdd',
        sprintf('m <- %s(%s, data = dat, cutpoint = %s, kernel = "%s", 
          se.type = "%s", %s t.design = %s, est.cov = TRUE)',
          ifelse(parameter$is_frontier(), 'mrd_est', 'rd_est'), 
          deparse(parameter$formula(), width.cutoff = 500),
          ifelse(parameter$is_frontier(), sprintf('c(%s, %s)',parameter$cutoff1(),parameter$cutoff2()), parameter$cutoff1()),
          parameter$kernel_type(),
          parameter$se_type(),
          ifelse(parameter$cluster_id()=='', '', sprintf('cluster = "%s",', parameter$cluster_id())),
          ifelse(parameter$is_frontier(), sprintf('c("%s", "%s")',parameter$operator1(),parameter$operator2()), sprintf('"%s"', parameter$operator1()))
        ),
        'summary(m)'
      )
    paste(lines, collapse = '\n')
    })
}