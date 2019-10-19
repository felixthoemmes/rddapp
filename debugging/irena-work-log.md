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

## 2019-10-18
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
