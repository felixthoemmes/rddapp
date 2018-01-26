@Felix: Package description

1. Update package description in
https://github.com/zejin/RDD/blob/master/R/rddapp-package.R
    - Use ?rddapp ?rdd ?rdrobust as reference
> Done

2. Update Title and Description in
https://github.com/zejin/RDD/blob/master/DESCRIPTION
    - Use
    https://github.com/zejin/RDD/blob/master/proj/reference/rdd_0.57/DESCRIPTION
    https://github.com/zejin/RDD/blob/master/proj/reference/rddtools_bquast_0.5.0/DESCRIPTION
    https://github.com/zejin/RDD/blob/master/proj/reference/rdrobust_0.93/DESCRIPTION
    as reference
> Done

3. Update title (citEntry) and paste (textVersion) in
    https://github.com/zejin/RDD/blob/master/inst/CITATION
    - Align with Title in 2
> Done

4. Add the R script generating the CARE dataset to
https://github.com/zejin/RDD/tree/master/data-raw
> Done

5. Fit the manual 
https://github.com/zejin/RDD/tree/master/proj/manual/draft1.Rmd
in the template
https://github.com/zejin/RDD/tree/master/vignettes/rddapp.Rmd
    - Continue to develop rddapp.Rmd
    - Use http://r-pkgs.had.co.nz/vignettes.html as reference

6. Update README file of package in
https://github.com/zejin/RDD/README.md
    - Use
    https://github.com/r-lib/testthat/blob/master/README.md
    https://github.com/zejin/RDD/blob/master/proj/reference/rddtools_bquast_0.5.0/README.md
    as reference

7. Update NEWS file of package in
https://github.com/zejin/RDD/NEWS.md
    - Use
    https://github.com/zejin/RDD/blob/master/proj/reference/rddtools_bquast_0.5.0/NEWS
    https://github.com/r-lib/testthat/blob/master/NEWS.md
    as reference

@Wang: Shiny GUI

1. Update library() and require() in
https://github.com/zejin/RDD/tree/master/inst/shinyrdd/global.R
    - Leave library(rddapp) only
        > Done
    - Use :: and ::: for functions from AER, sandwich, lmtest, Formula, shiny, DT
        > Done

2. Clean R scripts in 
https://github.com/zejin/RDD/tree/master/inst/shinyrdd
    - space: + - * / = <-
    - assign: replace = with <-
    - logic: replace F/T with FALSE/TRUE
    - remove unnecessary blank/line
    - use '' "" instead of \`\`

3. Manage plot color in
https://github.com/zejin/RDD/tree/master/R/sens_plot.R
https://github.com/zejin/RDD/tree/master/R/plot.rd.R

4. Discuss index use in
https://github.com/zejin/RDD/tree/master/R/mrd_sens_bw.R

5. Update description of plot.mfrd in 
https://github.com/zejin/RDD/tree/master/R/plot.mfrd.R

    > Done

6. Update description of sens_plot in 
https://github.com/zejin/RDD/tree/master/R/sens_plot.R

    > Done

7. Explain A1 and A2 in mrd_sens_cutoff, rd_sens_cutoff, sens_plot

    > Done

@Ze: R packages by Hadley Wickham

1. Introduction 
> Done

2. Package structure
> Done

3. R code
> Done

4. Package metadata
> Done

5. Object documentation
> Done

6. Vignettes: long-form documentation
> Done

7. Testing
> Done

8. Namespace
> Done

9. External data
> Done

10. Compiled code
> Done

11. Installed files
> Done

12. Other components
> Done

13. Git and GitHub
> Done

14. Automated checking
> Done

15. Releasing a package
> Done
