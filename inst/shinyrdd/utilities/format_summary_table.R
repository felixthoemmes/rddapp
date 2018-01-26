format_summary_table = function(x, se.type, sections = list('Parametric:regular'=1:3, 'Nonparametric:regular'=4:6), label_surfix=NULL, tab_itt=NULL) {
  # if (class(x)! %in% ('rd','')) stop('x is not a RD object.')
  # if (is.null(x$model)) stop ('x does not contain the original models')
  # if(is.null(x$model$iv)) {
  #   models = x$model
  # }  else {models = x$model$iv}
  
  tab = cbind(bw = NaN, n = NaN, est = x$est, se = x$se, z = x$z, df = NaN, p = x$p, l95=x$ci[,1], u95=x$ci[,2],
    es = if(is.null(x$d)) NA else x$d)
  
  rname = row.names(tab)
  
  tab[!duplicated(rname), 'n'] = x$obs
  tab[!duplicated(rname), 'bw'] = x$bw
  tab[!duplicated(rname), 'df'] = sapply(x$model$iv, function(m) m$df.residual)

  section_rows = data.frame(
    label = paste0(do.call(rbind, sapply(names(sections), strsplit, split = ':'))[,1], label_surfix),
    matrix(NaN, nrow = length(sections), ncol = ncol(tab), dimnames =  
        list(names(sections), colnames(tab))),
    rank = which(!duplicated(rname))[sapply(sections, function(v) v[1])] - .5,
    type = tolower(names(sections)),
    stringsAsFactors = F
  )
  
  row.names(tab) = ifelse(duplicated(rname),  paste('+', x$cov, sep=' '), paste('-', rname, sep = ' '))
  # row.names(tab)[!duplicated(rname) ] = ifelse(is.na(x$bw), row.names(tab)[!duplicated(rname)], 
  #   sprintf('%s (%4.3f)', row.names(tab)[!duplicated(rname)],x$bw))
  
  final_tab = rbind.data.frame(
    data.frame(label = row.names(tab), tab, rank = 1:nrow(tab), type = NA, row.names = NULL, stringsAsFactors = F), 
    section_rows,
    row.names = NULL,
    stringsAsFactors = F)
  
  final_tab = final_tab[order(final_tab$rank), ]
  final_tab$rank = NULL
  final_tab$type = na.omit(final_tab$type)[cumsum(!is.na(final_tab$type))]
  final_tab$type = ifelse(grepl('+', final_tab$label, fixed = T), 
    paste0(final_tab$type,':aux'), final_tab$type) 
  
  # final_tab$pstar = ifelse(final_tab$p < .001, "***",
  #   ifelse(final_tab$p < .01, '**', 
  #     ifelse(final_tab$p < .05, '*', NA)))
  final_tab$p = fmt_p(final_tab$p)
  final_tab$type = as.character(  final_tab$type)
  final_tab$label = as.character(  final_tab$label)
  final_tab$l95 = ifelse(is.na(final_tab$u95) | is.na(final_tab$l95),
    final_tab$u95, sprintf('[%.3f, %.3f]', final_tab$l95, final_tab$u95))
  
  final_tab$u95 = NULL
  final_tab= data.frame(lapply(final_tab, function(col) {
    col = ifelse(is.na(col) & !is.nan(col), '-', col)
    ifelse(col == 'NaN', '', col)
  }), stringsAsFactors = F)
  final_tab$label = gsub('Opt', 'Optimal', final_tab$label, fixed = T)
  
  # # add error as "-"
  # row_missing = apply(final_tab, 1, function(row) sum(is.na(row))) 
  # row_missing == (ncol(final_tab) - 1)
  
  
  if(!is.null(tab_itt)){
    tab_itt = tab_itt[grepl('-', tab_itt$label),]
    tab_itt$n = NA
    tab_itt$bw = NA
    final_tab$rn = 1:nrow(final_tab)
    
    tab_itt$rn = final_tab$rn[final_tab$label %in% tab_itt$label] + .5
    
    tab_itt$label ='+ ITT'
    final_tab_with_itt = rbind(final_tab, tab_itt)
    
    final_tab_with_itt = final_tab_with_itt[order(as.numeric(final_tab_with_itt$rn)),]
    final_tab_with_itt$rn = NULL
    
    final_tab_with_itt$type = as.character(final_tab_with_itt$type)
        final_tab_with_itt$label= as.character(final_tab_with_itt$label)
        
        
    return(final_tab_with_itt)
  }
  

  return(final_tab)
}