library(rddapp)

results = sapply(min(s1nocov$x):max(s1nocov$x), function(c){
  m = try(RDestimate(y ~ x + tr, s1nocov, cutpoint = c), silent = T)
  if(class(m) == 'try-error') 
    return(list(cut = c, result = as.character(m)))
  else
    return(list(cut = c, result = 'success', model = m))
})

t(results)

# Is it possible to for RDestimate() to return partially successful result? 
# That is, if the the non-parametric model(s) failed, could it still return results
# of parametric models, and NA (or something else) in the coefficients table for the 
# failed model(s)?