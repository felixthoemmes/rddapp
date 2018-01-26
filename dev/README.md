## Software licenses

* [software licenses summarized at-a-glance](https://tldrlegal.com/)
* [R Licenses](https://www.r-project.org/Licenses/)

### [Proper attribution of a function from a R package under Apache project](https://github.com/zejin/RDD/blob/master/reference/rddtools_bquast/R/dens_test.R)
```
#' Run the McCracy test for manipulation of the forcing variable
#' 
#' Calls the \code{\link[rdd]{DCdensity}} test from package \code{rdd} on a \code{rdd_object}.
#' 
#' @param rdd_object object of class rdd_data
#' @param bin Argument of the \code{\link{DCdensity}} function, the binwidth
#' @param bw Argument of the \code{\link{DCdensity}} function, the bandwidth
#' @param plot Whether to return a plot. Logical, default ot TRUE. 
#' @param \ldots Further arguments passed to \code{\link[rdd]{DCdensity}}. 
#' @export
#' @import rdd
#' @examples
#' data(house)
#' house_rdd <- rdd_data(y=house$y, x=house$x, cutpoint=0)
#' dens_test(house_rdd)
```

### [Proper attribution of derived work in a GPL project](https://programmers.stackexchange.com/questions/167935/proper-attribution-of-derived-work-in-a-gpl-project)
```
// HgSharp
// 
// Copyright 2005-2012 Matt Mackall <mpm@selenic.com> and Mercurial contributors
// Copyright 2011-2012 Anton Gogolev <anton.gogolev@hglabhq.com>
// 
// The following code is a derivative work of the code from the Mercurial project, 
// which is licensed GPLv2. This code therefore is also licensed under the terms 
// of the GNU Public License, verison 2.
```


## R package license

* `rdd` (Apache License 2.0)
* `rdrobust` (GPL-2)
* `rddtools` (GPL-2)

## [R packages by Hadley Wickham](http://www.amazon.com/dp/1491910593/ref=cm_sw_su_dp?tag=r-pkgs-20)

* [x] Getting started
  - [x] Introduction
  - [x] Package structure

* [ ] Package components 
  - [x] Code (`R/`)
  - [x] Package metadata `DESCRIPTION`
  - [x] Object documentation `man/`
  - [ ] Vignettes `vignettes/`
  - [x] Testing `tests/`
  - [ ] Namespaces `NAMESPACE`
  - [ ] Data `data/`
  - [ ] Compiled code `src/`
  - [x] Installed files `inst/`
  - [ ] Other components

* [ ] Best practices
  - [ ] Git and GitHub
  - [ ] Checking
  - [ ] Release
