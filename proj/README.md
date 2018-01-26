# Web-based software to estimate power and causal effects of various regression discontinuity designs

## R packages
- [R packages (Hadley Wickham)](http://r-pkgs.had.co.nz/)
- [R package primer (Karl Broman)](http://kbroman.org/pkg_primer/)
- [Developing R packages (Jeff Leek)](https://github.com/jtleek/rpackages)
- [Writing R Extensions (CRAN)](https://cran.r-project.org/doc/manuals/R-exts.html#Creating-R-packages)
- [GitHub CRAN mirror](https://github.com/cran)

## Data sets
- [CARE](http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/4091)

## CRAN packages
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [Submit package to CRAN](https://xmpalantir.wu.ac.at/cransubmit/)
- [Submitting experience from R-bloggers](https://www.r-bloggers.com/submitting-your-first-package-to-cran-my-experience/)

## 'qpdf' is needed for checks on size reduction of PDFs
- [R-Forge](https://r-forge.r-project.org/scm/viewvc.php/*checkout*/pkg/DocumentationForDevelopers/first_time_setup.md?revision=3&root=wats)
- [GitHub](https://github.com/Tazinho/snakecase/issues/33)

## R Markdown
- [Rmarkdown](http://rmarkdown.rstudio.com/lesson-1.html)
- [Markdown syntax documentation](https://daringfireball.net/projects/markdown/syntax)
- [pandoc readme](http://pandoc.org/MANUAL.html)

## RDD packages
- [Packages Summary](reference/README.md)

### Estimation 

| package | `rdd` | `rdrobust` | `rddtools` | `rddapp` :octocat: |
|:----------:|:-------:|:-------------:|:-------------:|:------:|
| dependency | sandwich, lmtest, AER, Formula | no | sandwich, lmtest, AER, Formula, np | sandwich, lmtest, AER, Formula |
| model design | sharp, fuzzy | sharp, fuzzy | sharp, fuzzy | sharp, fuzzy |
| coefficient estimator | local linear regression | local polynomial regression | local polynomial regression | local linear regression |
| sharp estimation | call lm from stats | matrix computation | call lm from stats | call lm from stats |
| fuzzy estimation | call ivreg from AER | matrix computation | call ivreg from AER | call ivreg from AER |
| covariate | include | include | include, residual | include |
| bias correction | no | local polynomial regression | no | no |
| kernel function | tri, unif, epan, normal, etc | tri, unif, epan | tri, unif, normal | tri, unif, epan, etc |
| bandwidth selection | IK09 | CCT, IK12, CV, etc | IK12, RSW | IK09, IK12, RSW |
| covariance estimator | call vcovHC from sandwich | matrix computation | call vcov from stats | call vcovHC from stats |
| cluster correlation | call sandwich from sandwich | matrix computation | call sandwich from sandwich | call sandwich from sandwich |
| confidence interval | yes | yes | yes | yes |
| test | call DCdensity from rdd | no | call DCdensity from rdd, call ks.test, t.test from stats | call DCdensity from rdd | 

## Shiny apps
- [Shiny package from RStudio](https://github.com/rstudio/shiny)
- [Shiny tutorial](http://shiny.rstudio.com/tutorial/)
- [Supplementing your R package with a Shiny app](http://www.r-bloggers.com/supplementing-your-r-package-with-a-shiny-app-2/)

## Shiny servers
- [DigitalOcean](http://www.r-bloggers.com/how-to-get-your-very-own-rstudio-server-and-shiny-server-with-digitalocean/)
- [Shinyapps.io](http://www.shinyapps.io/)
- [Red Cloud (Cornell)](https://www.cac.cornell.edu/services/cloudservices.aspx)
- [rddapp Shinyapp](https://rddapp.shinyapps.io/shinyrdd/)

## install_github 
- [RDocumentation](https://www.rdocumentation.org/packages/devtools/versions/1.12.0/topics/install_github)
- [R help](http://finzi.psych.upenn.edu/library/devtools/html/install_github.html)
- [hadley/devtools](https://github.com/hadley/devtools)

```S
Sys.getenv("GITHUB_PATH")
Sys.setenv(GITHUB_PATH = "171931d137897fbef9350d7ea0b958cf093c5d4e")
Sys.getenv("GITHUB_PATH")

devtools::install_github("zejin/RDD", subdir = 'rdd/rddapp', auth_token = Sys.getenv("GITHUB_PATH"))
```

## Comments

- ignore `.gitignore` in `rddapp`
- ignore `.Rbuildignore` in `rddapp`
- order the [R packages (Hadley Wickham)](http://www.amazon.com/dp/1491910593/ref=cm_sw_su_dp?tag=r-pkgs-20)

## [Code style (Hadley Wickham)](http://r-pkgs.had.co.nz/r.html#style)

### File names
```S
# Good
fit_models.R
utility_functions.md

# Bad
foo.r
stuff.MD
```

### Object names
```S
# Good
day_one
day_1

# Bad
first_day_of_the_month
DayOne
dayone
djm1

# Bad
T <- FALSE
c <- 10
mean <- function(x) sum(x)
```

### Spacing
```S
# Good
average <- mean(feet / 12 + inches, na.rm = TRUE)

# Bad
average<-mean(feet/12+inches,na.rm=TRUE)

# Good
x <- 1:10
base::get

# Bad
x <- 1 : 10
base :: get

# Good
if (debug) do(x)
plot(x, y)

# Bad
if(debug)do(x)
plot (x, y)

# Good
list(
  total = a + b + c, 
  mean  = (a + b + c) / n
)

# Good
if (debug) do(x)
diamonds[5, ]

# Bad
if ( debug ) do(x)  # No spaces around debug
x[1,]   # Needs a space after the comma
x[1 ,]  # Space goes after comma not before
```

### Curly braces
```S
# Good
if (y < 0 && debug) {
  message("Y is negative")
}

if (y == 0) {
  log(x)
} else {
  y ^ x
}

# Bad
if (y < 0 && debug)
message("Y is negative")

if (y == 0) {
  log(x)
} 
else {
  y ^ x
}

# Good
if (y < 0 && debug) message("Y is negative")
```

### Indentation
```S
# Good
long_function_name <- function(a = "a long argument", 
                               b = "another argument",
                               c = "another long argument") {
  # As usual code is indented by two spaces.
}
```

### Assignment
```S
# Good
x <- 5
# Bad
x = 5
```
