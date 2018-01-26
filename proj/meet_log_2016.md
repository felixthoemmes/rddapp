## Feb 1, 2016 - Apr 19, 2016

### Ze
- which package to follow
> rdd is good to go; rdrobust has many self-defined estimators; rddtools has many derived classes

- which Imbens-Kalyanaraman bandwidth to implement
> not decided yet; could double check with matlab code on [Imbens Website](https://people.stanford.edu/imbens/software)

- check 2009 paper of Imbens-Kalyanaraman bandwidth
> triangular kernel is recommended with constant specified, gaussian kernel is not a reasonable choice due to Assumption 3.4; additional covar is considered; local linear regression is rate optimal and has good bias property at boundary; simply calculate one bandwidth for outcome and treatment in fuzzy model based on outcome

- check 2012 paper of Imbens-Kalyanaraman bandwidth
> different optimal rates for two cases; global/local choice; left & right/combined choice;

- whether fuzzy model degrades to sharp model smoothly or not
> only error within numerical tolerance is detected for the full model inference

- why use lm in the first stage of ivreg
> consistency argument from Economics 

- check kernel functions in rdd
> rectangular is not right due to the particular kernel function in 2009 paper, which is corrected by rddtools

- whether implement global polynomial regression or not
> model misspecification may happen

- check sorting test in rdd
> Felix

- whether implement local polynomial regression or not
> local linear regression is enough

- whether call vcovHC to estimate se
> vcovHC gives sandwich se, especially useful in nonparametric regression where we do not have any guidance and classical se from lm seems not appropriate

- whether use np for local linear regression or not
> np does not have inference

- why rdd and rddtools have different results using vcovHC
> different parameterization in rdd and rddtools does not have any impact, but rdd does take the subset data with positive weight while rddtools does not 

- whether add covariates or not
> Felix

- whether use bandwidth for global linear regression
> always use the whole data for global linear regression

- how to improve code style
> use [manage format](https://github.com/zejin/RDD/blob/master/rdd/manage_format.R) to clean code

- how to add testing code
> write testing cases in [test directory](https://github.com/zejin/RDD/tree/master/rdd/rddapp/tests/testthat) for any nontrivial function put in [R directory](https://github.com/zejin/RDD/tree/master/rdd/rddapp/R)

- write testing code for attribution reporter function
> Felix or Wang

- how to use functions in [R directory](https://github.com/zejin/RDD/tree/master/rdd/rddapp/R)
> first way is to use [load package](https://github.com/zejin/RDD/blob/master/rdd/load_package.R) to load source code to memory; second way is to use [install package](https://github.com/zejin/RDD/blob/master/rdd/install_package.R) to install rddapp package and then use library(rddapp) 

- where to put R code
> put in [R directory](https://github.com/zejin/RDD/tree/master/rdd/rddapp/R) with special comments as core functions in the package; put in [inst directory](https://github.com/zejin/RDD/tree/master/rdd/rddapp/inst) as independent scripts of the package

- refer to sorting test in rdd and write plot function for raw data display 
> Wang



### Wang

- how to label the table that summarizes attrition analysis
> current labeling is fine

- should the sorting test and linearity test be done individually for multiple assignment variables
> yes

- how should the bandwith in plotting linearity checks be determined
> Wang will check Imbens and Lemieux (2008)





## Apr 25, 2016

- Felix collects materials for frontier design from V. Wong

- Ze reported on Imbens 2009 bandwidth paper and checked the RDD package is doing things correctly; he noted several small errors
> should not use Gaussian kernel; Rectangular kernel has wrong "C" value

- Wang showed his Sorting test plot and will work on the linearity assumption plot (plot binned data with smoother)

- Ze will cross-check Imbens 2009 and 2012 paper to see how bandwidth estimatiors differ
  




## May 2, 2016

 - Ze reported on the difference between the IK 2009 and 2012 paper. He found out that the estimators differ in their convergence rates. The 2009 paper estimator has faster convergence when the assumption 3.6 of equal second deriviatives of the functions at the cut-off is violated. The 2012 estimator has more balanced convergence rates when this assumption is or is not violated. Importantly, we decided to use the 2012 estimator as the DEFAULT. Currently we are thinking that the user should not have the option to change this default, but Ze said it would be easy to do.
 
 - Wang had some questions about default choices for the plot of the forcing variable against the outcome. We decided to use a simple histogram-based default, e.g., Sturges's rule (1926) or some newer variant. Wang will update the RShiny app to generate this graph.
 
 - Felix will need to gather material on how to estimate the multi-variable RDD (frontier RDD). 
 
 - The next meeting wil be next semester. Have a great summer!


## Sep, 12 2016

- IK 2012 bandwidth is different between rddtools and rdrobust
> rddtools exactly follows the paper, while rdrobust takes local polynomical and bias correction into account

- error raised by the default behavior of rddtools::plotPlacebo()
> due to NA returned during the function’s internal recalculations of bandwidth using rddtools::rdd_bw_ik(), which further calls rddtools::rdd_bw_ik_low(). Because plotPlacebo() iteratively use each data point with a given range (25th-75th percentiles) as a new cut point and estimate a new bandwidth, the error is caused when a cut point yields in insufficient data points to calculate bw on one side of the cut point. If we call plotPlacebo(, same_bw = T), i.e., not recalculating bw when iterating through the new cut points, the error can be avoid. If we reduce the range of new cut points, e.g., from the default 25th/75th percentiles to 33rd/66th percentiles, the error can be avoid.


## Sep, 19 2016

- update rdd 0.56 to rdd 0.57
> RDestimate changes "Tr * covs" to "covs" in lm

- Discussed plotting options of rdd estimate and talked about the rdd plot and the rdrobust plot
> Ze will make sure that the RDplot function has necessary information passed from RDestimate
> Wang will implement the actual plot - the current idea is to display a plot that (a) shows the regression lines based on the ones that the user actually did (e.g., global linear, local linear), (b) a line for the cut-off, (c) a confidence interval around the line, (d) actual datapoints shaded by weight (see Code from Peter Steiner on visual non-parametrics)
> If time permits Wang will also look into placebo tests (tests of treatment effects at different cut-offs)
> Felix will create an R document that shows an analysis of a frontier RDD (multiple-assignment RDD) 
> Ze will look at this document and will try to think about how to implement this into our package

- We also discussed going to the PI meeting 
> Ze agreed to travel to DC to attend the meeting, Felix will register him, book his hotel, and will communicate with Ze on which flight to book


## Sep, 26 2016

- information from RDestimate to plot.RD 
> RDestimate()$frame gives the dataset, colnames(RDestimate()$frame) gives the variable names of the dataset, i.e. X, Y, Z, covs

- change o$frame[[ibw]] = dat.out to o$frame = dat.out in RDestimate
> data.frame is the same for different bandwidths

- discussed plots that Wang generated
> We decided that we do not need the plot that shows the fuzzy RDD relationship between assignment and Z. Wang will keep working on the plots (shading by weight, shading bins by weight, implementation of binned means for regular plot)

- further steps for Wang
> Wang will work on the placebo tests, sensitivity to bw plot, and he will modify the DCdensity function and document his changes and his functions

- power analysis
> Felix showed a very simple example of a power analysis and we discussed what the applied user could provide in order to do a power analysis, Ze will take the program (powerexample.R) and will adapt it so that given a particular user input, power is returned for the global RDD, and the local linear RDD (with default BW only)

- multiple frontier RDD
> Felix has the Stata do files from Vivian and will try to translate them into R (with help from Wang if needed)

## Oct, 3rd 2016

- Wang and Felix met without Ze because he is traveling
- Wang and Felix tried to run the R tests, but we failed to update the local repo. In the process we might accidentally submitted a commit to the master
- Wang and Felix checked out the RShiny app and tested some of its functions. We discussed that the first panel looks very good already, but that the plot in the second panel (Estimate) is wrong

- further steps for Wang
> Wang will redo the estimate plot (correct binning)
> Wang will also do the plot for the sensitivity to the bandwidth, the placebo tests (different cut-offs), and a table for the alternative outcomes (non-outcome covariates)
> After all this, Wang will put the power function that Ze wrote into the shiny app

- further steps for Felix
> Felix needs to write an R program for the analysis of the frontier design to give to Ze

- further steps for Ze
> Wang mentioned that it would be very helpful to have error messages from RDestimate (and maybe other functions?) that are more meaningful, so that the user of the RShiny app gets proper error messages. Ze, can you look into this and help us to capture error messages and make them more readable and user-friendly?

## Oct, 17th, 2016

- We discussed Wang's plotting function of the placebo test
- After lots of testing we realized that the problem of the current plot is that the treatment / control labels are not updated
- Wang will correct this plot
- This also lead to questions on how this is being performed on fuzzy RDDs.
- One possible solution would be the following:
  
> 1) Estimate treatment probability using probit model, predicting
observed treatment, from cut-off implied treatment, assignment
variable, and interaction.

> 2) Shift the cut-off.

> 3) Then form predicted treatment probabilities based on the equation
estimated in (1), using observed value on assignment variable and
assumed treatment based on cut-off as predictors,

> 4) Use predicted treatment prob to stochastically assign treatment and
control on new cut-off

> 5) Re-estimate fuzzy RDD

> 6) Repeat 2+.

- Felix already emailed the consultants and asked their opinon

- After Wang left, Felix and Ze started talking about the frontier RDD. Ze will implement a simple function that performs the "centering" approach to multiple assignment RDDs. This involves forming a univariate composite assignment score, based on the two assignment scores

## Oct, 24th, 2016

- Today we mostly talked about the feedback from Peter, especially the bug that quickly clicking on the cut-off results in an infinte loop. We discussed many different ways to circumvent this problem. One possible solution might be to to call DCdensity twice. Wang will investigate.
> Wang: on hold. I will figure out if after the Assumption Check page design is finalized.
- Wang will also work on the sensitivity plot. Work on the placebo test plot is held off until we come to a more final conclusion on how to conduct them with fuzz RDDs.

- Ze did some work on the frontier RDD (centering approach) but we did not have time to discuss this. Ze has some questions about the value of the assignment variable in the centering approach and Felix will investigate and ask Peter and Vivian. 

- Peter noted that the little check box "Treat peole at cut-off as treated" is confusing. We decided to instead have a hard-coded choice of treatment assignment is only if => than cut-off. Users will not be able to change this. Wang will change this in the RShiny app.
> Wang: done.

- Peter wanted to see the raw data - our Figure 3.1. already does this, but we will make this clearer by putting in a small sentence. 
> Wang: done. redesigned.

- Peter noted some issues with the sorting test plot (0 frequencies are displayed). Felix will read McCrary to think about an improved plotting display. 

- Peter wanted to have polynomial terms in both parametric and non-parametric. Felix does not think that this is a good idea. Vast majority of econ lit says the same (e.g., Gelman and Imbens)

- Auxiliary variables do not change plot. Should we plot residualized outcome. We will put that one on the back-burner, because it is currently not that critical.

- Peter noted that the app doesn't check whether an assignment variable is cont or discrete. Felix will check with Peter to clarify. 

- Felix will send Ze some information on the univariate approach to the frontier RDD. 

## Oct, 31st, 2016 - Happy Halloween!

- Today we discussed Vivian's comments and feedback. 

- We discussed the fact that app times out. One solution is to use th offline version which would not have this problem.

- Wang will also investigate if the timeout option can be changed on the RShiny server
> Wang: done. changed from 900 sec to 3600 sec.

- Wang will investigate how to save DataTable as PDF or CSV File.
> Wang: possible, ugly.

- It was unclear to Vivian whether auxiliary variables are included in the outcome model. However, we put that in the table footnote. We will also add this to the help file.

- When a non-numeric variable is used as an auxiliary variable, the output goes away. Wang will put in a small error message when a non-numeric variable is being used. The error message will appear on the input tab.
> Wang: done.

- Vivian noted that some folks might like raw data and others binned data in the plot of outcome and treatment assignment. We already have this option when binwidth is 0, but we need to make it clearer for the user.
> Wang: done (same as Peter's)

- Vivian noted that it was difficult for her to understand what global linear regression and other items on the estiamte tab meant. We will need to put some information in the help file. 

- Vivian had a good suggestion on organizing the plot output, especially separating assumptions of the design and assumptions of the analysis. 
> Wang: Linearity check dropped.
> Wang: McCrary Sorting Test now plot for any assignment / auxiliary variables per user's selection.

- Felix will try to think carefully about how best to arrange these and will let Wang know.

- Vivian made a good case that the parametric model should be extended and we should also report a global, quadratic, and a global, cubic model. This would mean there are two more lines in the output. Ze, this should be your first job to extend our RDestiamte function to also do these two parametric models. Make sure that terms are centered around the cut-off for proper interpretation of the lower-order terms. The model should allow for (1) linear with different slope on both sides of cut-off (the current model setup), but also (2) a quadratic fit in which both linear and quad component are allowed to differ on both sides of the cut-off, and (3) a cubic fit in which all three components (linear, quad, cube) are allowed to differ on both sides of the cut-off. In the output table, only the treatment effect should be reported. The other components are suppressed. 

- Wang should take these new estimates from RDestimate and put them in the table. After that, the plot that currently appears on the estimate tab needs to be updated. The user should be allowed to choose to plot any of the 6 models that we report. It might be an option to have a two-panel plot (one with parametric and the other with non-parametric soution). Alternatively a single panel with one plot could work as well. 
> Wang: redesigned UI, awaiting Ze on RDEstimate().

## November, 7th, 2016
We spend most time discussing Wang's changes to the RShiny interface, and also spend some time on thinking about how to best plot confidence intervals, and models with covariates. 

- We thought it would be better to filter variables in the input tab only by being numeric or non-numeric
> Wang: done.

- We were unsure at which cut-point the covariates should be inspected for the density test. Felix will need to email Vivian.

- We decided that it would be nice if the dot in the sensitivity plot that shows the optimal bw estimate should be in red. Wang will fix this. 
> Wang: done.

- The range of the sensitivity plot should be adjusted to include the CI of the bw-optimal estimate. Wang will fix this as well.
> Wang: done. (no UI provided, just always include the range of bw-optimal CI)

- We were unsure how to do placebo tests for fuzzy designs. Felix will bring this up to the consultants. 

- We did not know what Vivian meant with balance plots, and Felix will email her.

- After seeing Wang's changes to the RDD estimate plot, we decided to only show one panel with 6 options from the pull-down. Wang will change that in RShiny.
> Wang: done for UI design.

- Wang and Ze worked on the power analysis and presented the results. The Rshiny already looks very nice. The two will investigate whether it makes sense to transmit just 12 values from R to Shiny, or all 12,000 raw results. 

- Felix will look into how to also integrate the AIPE (accuracy in parameter estimate) approach to the power analysis. 

- We were unsure how to plot a fuzzy RDD with non-parametric estimation and covariates. We were also unsure how to generate predicted probabilities. Felix will investigate with the consultants. 
> Wang: experimented (newdata approach).
> For parametric model, predicts = X %\*% b, CI is based on http://stackoverflow.com/questions/38109501/how-does-predict-lm-compute-confidence-interval-and-prediction-interval, covariates are accounted.
> For non-parametric model, using the original approach from rdd::plot.RD(), covariates and fuzziness are not accounted.


## November, 14th, 2016
- Today we discussed mostly the issue of plotting estimates and confidence intervals, as we were still waiting to hear from the consultants

- We verified that passing a treatment assignment variable (and ALWAYS running the 2SLS) does NOT invalidate our results. If treatment is perfectly predicted (as in the case of a sharp RDD), then the first stage LS will yield predicted values of exactly 0 and 1 (without machine precision error), and the standard errors will also be identical.

- We discussed how to plot the predicted lines and CIs for non-parametric models that include covariates. Wang's code works for the parametric case (both fuzzy and sharp) with covariates. We currently always plot raw data points with overlaid lines (is that correct, Wang)? Ze had an idea about how to create the line, it involves taking predicted values of local linear for each datapoint, saving the weights - I am unsure about the last step, but Wang seems able to implement the idea.
> Wang: Done. Always hold covariates at their grand mean.

- Vivian: This is just to check that there is a discontinuity in treatment receipt at the cutoff (assumption 1 in Hahn, Todd, and van der Klauw). To do this, run an RD analysis with treatment receipt as at the outcome, and a flexible function of the assignment variable and treatment indicator. If the RD is sharp, the discontinuity in the probability of treatment will be 1, if it is fuzzy it is less than one. I think your software may automatically do this in order to determine whether it is a fuzzy or sharp RD, but many journals and researchers want to see the discontinuity in the probability of treatment in the case that it is fuzzy. 
> Wang: Done, included a new column in Table 1.2. However, the case for frontier RDD is unclear.

- Vivian: Yep, that's right. Just run the RD analysis  using baseline covariates as the outcome ... If the RD is valid, then there should be no discontinuous in baseline covariates at the cutoff. You could also provide effect size differences at the cutoff for baseline covariates (this is just the discontinuity at the cutoff on baseline covariates, divided by the standard deviation of the control group at/near the cutoff)... The balance test tells you percentage of african americans are different to the right and left of the cutoffs; the density tells you if there are actually fewer african americans to the left and right of the cutoffs. I don't see many other people do this, but I think its a nice flexibility to add. The balance test tells you if there is a problem on the observed covariates; the density test may be a useful diagnostic check for problems with the RD that may be an issue on unobserved covariates ... Attached is a draft of a little paper that Coady and I wrote for observational studies. On Figure 5 are the covariate balance plots in the RD. Note the first panel for African American shows that it is balanced at the cutoff; On Figure 7 is the density plots on covariates; there is a discontinuity in the density of african american students here. 
> Wang: It seems like there are two things: (a) just tabulate or plot estimates of RDD for all covariates. (b) plot the fitted lines of RDD for all covariates. (a) could be a mixed bar plot (effect size) + point(estimate ~ all covariates). Do we need (b)? (It depends on how we plot the fitted lines against the assignment variable)

- Vivian: When I've done the McCrary test on covariates, I just ran the test on the density of observations for different baseline groups. For example, I did the McCrary test to see if there was discontinuity in the density of african americans near the cutoff. 
> Wang: This is already done.

## November, 21st, 2016
During this meeting we spent most of our time thinking about how to plot confidence intervals for RDD estimates in the presence of covariates, and in the presence of fuzzy designs. Wang presented several ideas, with or without marginalizing covariates (setting them at their grand mean). Ze looked at the margins package from Thomas Leeper (and the lsmeans package), and we decided to marginalize covariates and construct CIs based on that. This appears to be also identical to the way that the margins package in stata does this. 
> Wang: Done (would like to discuss the case when the interpretation of the overlapness of two CIs at the cutoff does not agree with p-value of the RDD effect.)  
> Felix: They agree, the CIs can overlap and p van be < .05 (see e.g., Cummings et al. Inference by Eye)
Wang also showed us some of the power analyses. They look quite nice, but they take a long time to run, because of the repeated calls to rdestimate. We are unsure whether it is possible to speed up the code much. 
Due to Thanksgiving break, there are no additional tasks for next week, but we will meet again next Monday. 

## November, 28th, 2016
We discussed some new implementations from Wang in both the estimate and power section of the software. Wang made some changes in the interactivity and the layout of the app.
A couple of questions came up.

- We currently plot the estimate always using raw data. Peter suggested once that we might want to plot residualized data. But there are different options how to residualize. Felix will check with Peter on this.

- We felt it might be helpful to have some test datasets. Felix will create a bunch of them for a variety of conditions (fuzzy, frontier, with covs, etc.). 

- Wang felt that it would be better for the user to input partial eta square for the treatment effect (instead of the total R^2). Ze will look into whether this can be derived using closed-form formulas.
> The closed-form formulas with eta square is done.

- Wang's sensitivity to the bandwidth plot should be extended to 3 times the optimal, and 1/3 of the optimal. Also indicate hald and double in the plot.
> Done.

- Wang's plot of the sensivity to cutpoint needs to be reworked for user-friendliness. We decided to have the range slider not be interactive, as it confounded two functions. 
> Revised.

- We wanted to confirm with Vivian how to estiamte treatment probability jumps in frontier designs.

- Ze found a potential solution to plot non-parametric smoother lines with generated data. Wang will try to implement it.
> Revised. However, the predicted lines will not cover the full range of X. Re-estimation would fail for cutoffs that are too close to the limits of X.

- Ze needs to update RDEstimate to have an option about which models should be fitted. He also will change that in the power analysis as well. Wang will update the power analysis Shiny side of this.
> Ze's part is done. `RDestimate(..., simple = TRUE)` will give linear and optimal results only.


## December, 5th, 2016
- Wang and Felix met (Ze had a time conflict). We discussed feedback from consultant Joe Cappeleri. Joe suggested to also put in a correlation matrix of the variables involved in the model, especially the correlation coefficient of X and T (to gauge fuzzyness). Wang will implement this. 
> Wang: Done (as part of Table 1.1 Descriptive Statistics)

- Joe also suggested putting in references to the citations we have - we will do this in the help file and / or manual. 

- Another suggestion was that the sensitivty plot currently fails everytime any of the methods fails. What we need is a sensitivity plot that can partially fail and still display results. Wang mentioned that this would need to be implemented directly in the RDEstiamte function. Ze can look into this. E.g., there might be a situation in which the linear parametric RDD yields an estimate, but the non-parametric fails. Ideally, we should still be able to extract (and plot) the linear parametric estimate. Wang will update the plot as soon as Ze updates the function. 
> Ze: Done. If the bandwidth is not valid, then the model is omitted. If the model is not valid, then the estimate is NA.

- Wang and Felix looked at the analytically derived eta square. It looks like great work, but there were some questions about how some of the terms were derived. These will be discussed next meeting. 

- Wang and Felix realized that the created example datasets had some problems. This was due to rounding (that Felix did when writing out the data as csv). Felix has already fixed this and sent out an email. 
