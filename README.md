rddapp
==================================================
[![Travis-CI Build Status]](https://travis-ci.org/felixthoemmes/rddapp.svg?branch=master)](https://travis-ci.org/felixthoemmes/rddapp.svg?branch=master)

[![Downloads]](http://cranlogs.r-pkg.org/badges/rddapp)](http://cran.rstudio.com/web/packages/rddapp/index.html)

[![Cran Build]](https://www.r-pkg.org/badges/version/rddapp)

Overview
--------------------------------------------------

**rddapp** provides a set of functions for the analysis of the regression-discontinuity design (RDD). 

The three main parts are:
- estimation of effects of interest
- power analysis
- assumption checks


Estimation
--------------------------------------------------
The package estimates treatment effects from RDDs, for the following designs and approaches:
 - parametric RDD with single assigment variables, both sharp and fuzzy designs
 - non-parametric RDD with single assignment variables, both sharp and fuzzy designs
 - parametric RDDs with two assignment variables, both sharp and fuzzy designs, using univariate, centering, and frontier approaches


Power analysis
--------------------------------------------------
Given input from the user about desired Type I error rate, and assumptions about the population, 
the package allows estimation of power for the following designs: 
- single assigment RDDs (both sharp and fuzzy) using both parametric and non-parametric estimation
- multiple-assigment RDDs (both sharp and fuzzy) using various parametric models


Assumption checks
--------------------------------------------------
The package allows the user to perform a variety of assumption and sensitivity checks. 
- McCrary's sorting test on the assignment variable
- Sensitivty to the chosen bandwidth in non-parametric estimation
- Placebo tests to examine treatment effects at values away from the cut-off
- Discontinuities in the treatment probability at cut-off
- Discontinuities for baseline covariates

Installation
--------------------------------------------------

``` r
# Install the released version from CRAN
install.packages("rddapp")

# Or the development version from GitHub:
# install.packages("devtools")
devtools::install_github("felixthoemmes/rddapp")
```


