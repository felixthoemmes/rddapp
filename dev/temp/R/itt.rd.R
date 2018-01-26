#' Intention-to-treat estimation for fuzzy RDD.
#' 
#' \code{itt.rd} returns a \code{rd} object with intention-to-treat estimation.
#' 
#' @param object \code{rd} object, typically the result of \code{\link{rd_est}}

#' @import Formula
#' @export
#' @author Wang Liao
#' @examples 
#' x <- runif(1000, -1, 1)
#' cov <- rnorm(1000)
#' y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
#' tr = as.integer(x >= 0) + rbinom(100,1,.1)
#' rd = rd_est(y ~ x + tr | cov, cutpoint = 0) 
#' itt(rd)

itt.rd = function(object){
    if(class(object)!='rd') stop('object is not a RD object.')
    
    fml_str = as.character(object$call$formula)
    lhs_str = fml_str[2]
    rhs_str = strsplit(fml_str[3], '[+|]')[[1]]
    
    a_str = rhs_str[1]
    tr_str = rhs_str[2]
    cov_str = if(length(rhs_str)>=3) rhs_str[3:length(rhs_str)] else NULL
    
    t.design = object$call$t.design
    if(is.null(t.design)){
      warning('cannot find t.design in call')
      t.design = 'geq'
    }
      
    new_tr_str = sprintf('I(%s %s %s)',  a_str, 
      c('geq' = '>=', 'g' = '>', 'leq' = '<=', 'l' = '<')[t.design], 
      if(is.null(object$call$cutpoint)) 0 else object$call$cutpoint)
    new_fml_str = paste0(lhs_str, ' ~ ', a_str, ' + ', new_tr_str)
    
    if(!is.null(cov_str)) 
      new_fml_str = paste0(new_fml_str,  ' | ', cov_str)  
    
    object$call$formula = as.Formula(new_fml_str)
    object = eval.parent(object$call, n = 2)
    return(object)
}