#' Descriptive Statistics for Model Data
#'
#' @param data A data.frame with each row representing an observation.
#' @param treatment The name of a numeric variable (treated = positive values).
#' @param assignment1 The variable name of the primary assignment.
#' @param assignment2 The variable name of the secondary assignment.
#' @param auxiliary A character vector of variable names of auxiliary covariates.
#'
#' @return A data.frame with descriptive statistics (mean, standard deviation, non-missing observation count)
#' @export
#'
#' @examples
summarize_model_data = function(data, treatment, outcome, assignment1, 
  assignment2 = NULL, auxiliary = NULL){
  df = sapply(data[, 
    c(treatment,
      outcome,
      assignment1,
      assignment2,
      auxiliary)
    ], function(col) 
      list(
        N = sum(!is.na(col)),
        M = mean(col, na.rm = T),
        SD = sd(col, na.rm = T)
      )
  )
  
  df_cor = cor(data[, 
    c(treatment,
      outcome,
      assignment1,
      assignment2,
      auxiliary)], use = 'pairwise.complete.obs'
  )
  is.na(df_cor[lower.tri(df_cor)]) = T
  is.na(diag(df_cor)) = T
  
  vf_dot = Vectorize(function(x) if(x<.01) '<.01' else gsub('0.','.',sprintf('%.2f', x), fixed = T))
  vf = Vectorize(function(x) if(x<.01) '<0.01' else sprintf('%.2f', x))
  
  # # reformat digits
  # df_cor[upper.tri(df_cor)] = vf_dot(df_cor[upper.tri(df_cor)])
  # df[c('M','SD'), ] = vf(df[c('M','SD'), ])  
  
  df = t(rbind(df, df_cor[1:nrow(df_cor)-1,]))
  
  rownames(df) = gsub(
    treatment, 
    paste(treatment, '(T)'),
    rownames(df), fixed = T
  )
  
  rownames(df) = gsub(
    outcome, 
    paste(outcome, '(O)'),
    rownames(df), fixed = T
  )
  
  rownames(df) = gsub(
    assignment1, 
    paste(assignment1, '(A1)'),
    rownames(df), fixed = T
  )
  
  try({rownames(df) = gsub(
    assignment2, 
    paste(assignment2, '(A2)'),
    rownames(df), fixed = T
  )}, silent = T)
  
  if (length(auxiliary)>0) {
    rownames(df)[(nrow(df)-length(auxiliary)+1):nrow(df)] = 
      sprintf('%s (C%g)', 
        rownames(df)[(nrow(df)-length(auxiliary)+1):nrow(df)],
        1:length(auxiliary))
  }
  
  # CHECK VARIABLE SCALES
  if(length(unique(na.omit(data[,treatment]))) != 2 | 
      any(!(unique(na.omit(data[,treatment])) %in% 0:1)))
    warning('Treatment (',treatment,') is not binary (i.e., 0/1), positive and non-positive 
      values will be considered as indicating Treated and Controlled cases, 
      respectively.')
  
  return(df)
}