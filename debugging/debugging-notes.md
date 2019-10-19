# debugging notes

## bug reports

**bug 1)** Felix: "In terms of the power analysis, I don't have a reproducible example. In the beta version, it appears to crash every time I try to use the
power analysis that searches over a grid (the lower panel in the shiny
app)."

**bug 2)** dataset causes shiny GUI to crash

_file_
multivarRD.csv

_assignment/cutoffs**_
outcome: mt
assign1: vp >= 20
assign2: mp <= 5
treatm: trt

Felix: "The issue is with error reporting. In your example, the crashes are a result of your design probably not having any observations in some of the quadrants. The software should give you an error message in the design table, and then refuse to estimate anything. I guess it refuses, but doesn't really tell you why."

## debugging shiny with r studio

* can only use breakpoints in `shinyServer()`, but `browser()` works everywhere
  * not always helpful as shiny code doesn't execute linearly
  * need to rememeber to take `browser()` statements out after
* use **showcase mode** to ID which parts of code are executing after various actions (to help find where bugs trigger in code)
  * `shiny::runApp(display.mode="showcase")`
  * can also enable by default by setting `DisplayMode: Showcase` in DESCRIPTION file
* can also use the **reactive log**
  * more detailed than showcase mode
    * shows not only which reactives are executing in real time, but the dependencise between them
  * `options(shiny.reactlog=TRUE)`
  * more info [here](https://rstudio.github.io/reactlog
* enter debugger on error
  * `options(shiny.error = browser)`
