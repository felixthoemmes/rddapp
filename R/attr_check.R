#' Attrition Checks
#'
#' \code{attr_check} reports missing data on treatment variable, assignment variable, and outcome.
#' This is an internal function and is typically not directly invoked by the user. 
#' It can be accessed using the triple colon, as in rddapp:::attr_check().
#' 
#' @param x1 A numeric object containing the assignment variable.
#' @param y A numeric object containing the outcome variable, with the same dimensionality 
#'   as \code{x1}.
#' @param t A numeric object containing the treatment variable (coded as 0 for untreated and 1 for treated), with the same dimensionality 
#'   as \code{x1} and \code{y}.
#' @param x2 A numeric object containing the secondary assignment variable.
#' 
#' @return \code{attr_check} returns a list containing the amount and percentage of missing data for all variables and subgroups, by treatment.
#'   
 
attr_check <- function(x1, y, t, x2 = NULL) {
  res = list(
    # get length of vector t to determine number of cases
    overallt = length(t),  
    # how many non-missing units coded 0 
    overallt0 = sum(t == 0, na.rm = TRUE),  
    # how many non-missung units coded 1
    overallt1 = sum(t == 1, na.rm = TRUE),   
    # how many missing units in t  
    overallmisst = sum(is.na(t)),  
    # how many missung units in y  
    overallmissy = sum(is.na(y)), 
    # how many missing units in y when t=0
    overallmissyt0 = sum(is.na(y[t == 0 & !is.na(t)])),
    # how many missing units in y when t=1  
    overallmissyt1 = sum(is.na(y[t == 1 & !is.na(t)])),
    # how many missung units in x1    
    overallmissx1 = sum(is.na(x1)),  
    # how many missing units in x1 when t=0
    overallmissx1t0 = sum(is.na(x1[t == 0 & !is.na(t)])),  
    # how many missing units in x1 when t=1
    overallmissx1t1 = sum(is.na(x1[t == 1 & !is.na(t)]))
  )  
  
  # if there is a second assignment x2, run the following
  if (is.null(x2)) 
    return(res)
  else
    return(c(res, 
      list(
        # how many missung units in x2 
        overallmissx2 = sum(is.na(x2)), 
        # how many missing units in x2 when t=0 
        overallmissx2t0 = sum(is.na(x2[t == 0 & !is.na(t)])), 
        # how many missing units in x2 when t=1 
        overallmissx2t1 = sum(is.na(x2[t == 1 & !is.na(t)]))  
      ) 
    ))
}
