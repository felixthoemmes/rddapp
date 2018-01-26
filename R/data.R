#' Carolina Abecedarian Project and the Carolina Approach to Responsive Education (CARE), 1972-1992
#'
#' A subset of children of the randomized controlled CARE trial on early childhood intervention. 
#' The randomized controlled trial was subsetted to mimic a regression-discontinuity design in which 
#' treatment was assigned only to mothers whose IQ was smaller than 85.
#'
#' @format A data frame with 81 rows and 5 variables:
#' \describe{
#'   \item{SUBJECT}{Unique ID variable}
#'   \item{DC_TRT}{Day Care (Preschool) Treatment Group, 1 = Treatment, 0 = Control}
#'   \item{APGAR5}{APGAR score at 5 minutes after birth}
#'   \item{MOMWAIS0}{Biological mother's WAIS (Wechsler Adult Intelligence Scale) 
#'     full-scale score at subject's birth}
#'   \item{SBIQ48}{Subject's Stanford Binet IQ score at 48 months}
#' }
#'
#' @source \url{http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4091}
#' @source \url{http://www.researchconnections.org/childcare/studies/4091/version/1}

"CARE"
