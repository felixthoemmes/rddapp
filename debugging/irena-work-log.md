# Irena's Work Log

## Goal
debug rdd app (see `debugging-notes.md`)

## 2019-10-16 (2 hrs)
* reviewed bugs
* read r studio's [guide for debugging shiny apps](https://shiny.rstudio.com/articles/debugging.html)
  * made notes in `debugging-notes.md`
* reinstalled rddapp from github: `devtools::install_github("felixthoemmes/rddapp")`
  * took a little while because some dependencies were out of date...
* tried showcase mode out on bug 2
  * bug 2 doesn't manifest itself on my local copy of shinyrdd but it does show up at https://rddapp.shinyapps.io/shinyrdd_beta/
* **to do**: try publishing local copy of rddapp to site to see whether bug still manifests itself
  * if so, look at tracing on shinyapps.io in above guide

## 2019-10-18 (2 hrs)
* republished local version of shiny app to https://rddapp.shinyapps.io/shinyrdd_beta/ via R studio to see if it fixes bug 2
  * Felix downloaded and sent me the last version of `shinyrdd_beta` downloaded as a bundle from shinyapps.io as a safeguard in case we need to revert. bug 2 manifests itself when running this copy of the app locally
  * redployed app still has an error
* discovered `options(shiny.error = browser)`
* the app *doesn't* crash if you upload multivarRD.csv (the troublesome file) and input the parameters in the following order into shiny:
outcome: mt
assign1: vp >= 20
treatm: trt
assign2: mp <= 5
(so just setting the treatment variable *before* setting the second assignment rule)…
* the crash error is related from not finding a variable called "tr" in "data" found in the call to model.frame.default(formula =  tr ~ tstar_1 + tstar_2, data = data)
  * i think when the data gets loaded in the original way, EITHER:
    * the treatment column doesn't get renamed internally to "tr"
    * OR this line of code gets called and it should actually be "trt"?

## 2019-10-24 (2 hrs) -- recorded for last pay week
* investigating bug 2 to see whether the issue is how the treatment column is labelled in this particular dataset?
  [x] try "T" then "treatment" then something else entirely (Felix, 10/24)
  [x] try on both shiny and r (i.e. web and local?)
  * relabelling to "T" and "treatment" still provoked an error on both the web server and locally
* apparently changing the order of inputs, as documented above *doesn't* actually work... i guess when i thought it worked previously, it was really just that the app hadn't refreshed.
- - -
* it seems that it's not about how the treatment column is labelled in the original dataset
  * this is probably a weird edge-case for the model that wasn't implemented correctly (`tr` variable not created internally or typo in the formula `tr ~ tstar_1 + tstar_2`)
[x] make contingency table for this data based on the above assignment rules to figure out the number of data points in each quadrant (what kind of case are we dealing with here?)
  * see `debugging-sketchpad.R`: `freqs` table yields the following table where
  rows represent whether vp >= 20 and cols represent whether mp <= 5
        || FALSE || TRUE
  FALSE || 81    || 200
  TRUE  || 51    || 59
  * nothing looks immediately wrong with this assignment (all quadrants have at least 51 points, with a max of 200 points)
  * maybe the issue is then when we check against treatment status?
    * see `freqs_trt` table, where
    vp_pos = TRUE <=> vp >= 20
    mp_neg  = FALSE <=> mp <= 5
    ```
      vp_pos mp_neg   trt     n
      <lgl>  <lgl>   <dbl> <int>
    1 FALSE  FALSE      0    81
    2 FALSE  TRUE       1    51
    3 TRUE   FALSE      1   200
    4 TRUE   TRUE       1    59
    ```
    there are 2*2*2 = 8 possible combos of vp_pos, mp_neg, trt, but only 4 are returned here, missing 4 combinations:
    ```
    FALSE FALSE 1
    FALSE TRUE 0
    TRUE FALSE 0
    TRUE TRUE 0
    ```
    * all untreated cases were `FALSE FALSE` and treated cases satisfied at least one (if not both) rules...
    * since there are no cases where `TRUE TRUE 0`, all individuals assigned treatment by our assignment rules actually received treatment, so this should be a *sharp* RDD---actually NO should be also the case that no partially true cases got treated, so this is actually *fuzzy*; statement should be that ONLY individuals assigned treatment based on both rules simultaneously actually received treatment
    * nothing seems unusual here with the data, as far as i can tell
[x] traceback where `tr` is supposed to be created
  * `tr ~ tstar_1 + tstar_2` is invoked in `rd_type()`, an interal rddapp function
  * working through the function definition, it takes `treat` as an input, "The name of a numeric variable (treated = positive values)"
  * if either `assign_2` or `cutoff_2` are `NULL`, get creation of `trt = as.name(treat)` in list called `env` (environment?) in `fml` list (formula?) -- so in the case where there is only one assignment rule
  * else get `trt = as.name(treat)` in list called `env` in `fml` list
[x] change `tr` to `trt` in `tr ~ tstar_1 + tstar_2` of `rd_type()`
  * seems to fix bug!!!!!!!!!!!!!

## 2019-10-25 (1 hr)
* why is it that we were still able to perform other multivar RDDs sans error while developing the 3D plots last year...?
[x] check that changing `trt` column to `treatment` or `T` in `multivarRD.csv`
  * bug appears again!!
  * it looks like trt does not correspond to a general internal relabeling of the treatment variable within the scope of the whole `rd_type()` function
    * it's just used in some substitute statements within the function
* inserted browser() just before error is triggered to explore
  * the app crashes when checking for a *sharp frontier design* with the statement `if (all((xtabs(tr ~ tstar_1 + tstar_2, data) > 0) == matrix(c(0, 1, 1, 1), nrow = 2, byrow = TRUE)))`
    * the problem is that `tr` is supposed to refer to the treatment column, but it's hardcoded as `tr` for some reason
    * changing this line so that it refers to the treatment variable as specifed in the `rd_type()` function arguments actually does the trick
      * checked by changing the treatment col in `multivarRD.csv` to various names and it worked
      * also checked that this formula is compatible for non-binary treatment columns (`rd_type()` simply says that treated = positive values), so checked when treated = some positive number and untreated = 0; it works
  * i think we only worked with fuzzy frontier designs when developing the 3D plots last year, so that might explain why we were never able to provoke this error before
