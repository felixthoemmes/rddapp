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
