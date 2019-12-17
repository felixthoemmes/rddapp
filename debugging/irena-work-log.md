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

## 2019-10-28 (1 hr)
[x] find a reproducible example of bug 1 (the power analysis breaking)
*  Felix: "In the beta version, it appears to crash every time I try to use the
power analysis that searches over a grid (the lower panel in the shiny
app)."
[x] read overview of monte carlo power analysis [here](https://deliveroo.engineering/2018/12/07/monte-carlo-power-analysis.html) to orient myself
* starting with the CARE package
    * outcome: MOMWAIS0
    * a1: APGAR5 >= 9
    * trt: DC_TRT
  * go to power tab, hit simulate button (two arrows chasing each other) with default settings and get crash
    ```
    Warning: Error in as.data.frame.default: cannot coerce class ""rdp"" to a data.frame
  59: stop
  58: as.data.frame.default
  44: <observer> [modules/simulate_power.R#301]
   1: shiny::runApp
   ```
   * in `modules/simulate_power.R`, only one call to `as.data.frame()`
   * the expression inside of `as.data.frame()` returns an object of class `rdp` that prints like this:
   ```
       success mean(est)   var(est) 0.001  0.01  0.05
    Linear     500  1.000083 0.05515612 0.786 0.938 0.988
    Opt        500  1.007370 0.08761318 0.608 0.828 0.930
    attr(,"class")
    [1] "rdp"
    ```
    * need to figure out how to convert `rdp` (or rather the output of its print method) to a data frame
    * i think `print.rdp` is an undocumented internal method (`?print.rdp` and `??print.rdp` don't yield anything)
    * need to figure out more about objects of class `rdp` as defined to figure out how to coorce into a data frame...(why do we need a data frame in the first place?)
    [x] try by simply removing `as.data.frame()` around result
    ```
        Warning: Error in $<-.data.frame: replacement has 0 rows, data has 2
    171: stop
    170: $<-.data.frame
    168: power_chart [modules/simulate_power.R#224]
    167: renderPlot [modules/simulate_power.R#281]
    165: func
    125: drawPlot
    111: <reactive:plotObj>
     95: drawReactive
     82: origRenderFunc
     81: output$simulate_power-power_chart
      1: runApp
  ```

## 2019-10-30 (2 hrs)
* bug is in bit that generates the power chart
  * noticed there is an earlier instance of calculating `res` like this (also in `simulate_power.R`), but right after the first calculation of `res`, there is a bit that updates `result$summary = res`... this wouldn't change the class of res tho (maybe that's ok?)
[x] remove `as.data.frame()` when assigning res and add `result$summary = res`
  * same bug as before (immediately above)
  * i think this is still a type error (the code relies on res being a data frame but its of class `rdp`)
  * type error comes up when `power_chart(df)` actually tries to use `df` for the first time (calls a column named `type`)
[x] try `res = as.data.frame(matrix(res, ncol = 6))` (since there should always be 6 columns and we can coerce rdp into a matrix, which can then be coerced into a df)
  * getting some type errors with the plot now... df provided for the `power_chart` should have a column called `type` so where is this supposed to be created?
 * why isn't the code set up to assign row names to res if there are fewer than 6 rows... seems arbitrarily hardcoded
[x] try instead `res = as.data.frame(unclass(res))`
  * this seems to work because it preserves the rownames generated by the call to `rd_power()` that creates `res` in the first place
  
## 2019-11-13 (2 hrs)
* added Felix's annotated list of bug reports to `debugging-notes.md`
  * referring to this list as "bug report 2"
* fixed first item (changed "IF" to "Assign treatment if" in Treatment Design panel)

## 2019-11-25 (2.5 hrs)
[x] program new UI elements for fig 4.2 as reactive values that do something server-side
* addressed bug 2.5 (added to help manual for it)
* finished responding to the rest of the bugs via email

## 2019-12-02 (1 hr)
* for bug 2.4, made y-lim values dynamic (min and max update depending on est plus minus standard error of model)
  * for one variable rd, just uses est from linear parametric model
  * for mrd, uses corresponding values for center, univ 1, and univ 2 models (see code diff for details)
  
## 2019-12-17 (3.5 hrs)
* looked at different multiples of the standard error for fig 4.2 y-lims
  * settled on 3*(standard error)
