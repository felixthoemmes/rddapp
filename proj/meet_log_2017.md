## Jan 24, 2017

Today we informally reviewed the work that we have done last year, and discussed the major goals for this second year of the grant. Our major goal is to implement the multiple-assignment variable RDD (MRDD). To make sure that all us have the same basic level of understanding of this design and the way that it is estimated, Felix, Wang, and Ze will read the paper on MRDDs by Vivian Wong et al. The paper is already in our repo folder. Felix has already added some rudimentary R code that is in the same folder as the PDF of the paper. 

## January, 31st

We went over Wong's MRDD paper and listed all the possible estimates. The plan is that Ze implements the univariate and centering approach, and that Wang tries to translate some Stata code from Vivian Wong. Ze also noted that it might be helpful to have some tests. He suggested that Felix goes through all of the R files in the R folder and (a) check whether additional comments are needed, and (b) whether there are some potential tests that could be included. 

## Feburary, 7th

Today we went over Felix's comments on the R files in the R folder. We covered all of them, except the main function, which we will do in our next meeting. The following tasks were assigned. 
- Felix will write a test for attr_check.R
- Wang will run a test for bw_ik09.R, calling both Stata and R on a fixed dataset and comparing results

  > Wang: Done, does not agree with rd_stata_09aug4.do (close in 2 digits). (inst/test_bw_ik09.R)
  
- Ze will run a test for bw_ik12.R, calling both rdrobust and rddapp on a fixed dataset and comparing results

  > Ze: no need to check with rdrobust. However, we haven't check with Imben's paper, because we don't have the Lee data.
  
- Ze will edit an error exception if bandwidth estimation fails

  > Ze: Done.
  
- Wang will add comments to dc_test.R

  > Wang: Done.

- Felix will download McCrary's data, his results, and write a test for the DC Density test

  > Felix: Does not agree exactly (5% off). Cannot guarrantee the data is the same. The raw data (both datasets) would need additional process to be used. 
  
- Wang will write a test for mrd_est.R that compares results to Vivian's stata package
  
  > Wang: Pending on parsing md_12.do to R. I'm waiting to learn how to calculate integrals in R.

- Ze will add comment in mrd.est on the assignment

  > Ze: Done.

- Ze will delete plot.rd.R, Wang will replace it with his version, and Felix will proof this version

  > Wang: Done.
  
- Wang will move the file that checks the fuzzy / sharp designs, and Felix will proof this version
  
  > Wang: Done. Also added special comments and example.
  
- Ze will look into using replicate() instead of for loops for the power analysis

  > Ze: Done. no significant efficiency improvement to for loop.
  
- Ze will test power_analy.R to see if Type I error rates are 5% when treatment effect is zero

  > Ze: Done. no problem. (most of the Type I error is 6%)

- Ze will look into line 76 of summary.rd.R to see what this two-sided F-test is

  > Ze: Confirmed, it's two-sided F-test.

##Feburary, 14th

Today, we went over all the tests and checks that we performed. Wang updated the meeting log of the previous meeting to list all the tasks that were already done. During the meeting the following additional tasks emerged. 

- Wang will fix attr_check.R that showed some errors because of the use of na.omit

  > Wang: Done.
  
- Wang will provide two more examples to test bw_IK09.R. Ze will take these examples and write a formal test.

  > Wang: Done.

- Wang and Ze will write a test for rd_predict to fix a bug in rd_est

  > Wang: fixed.
  
- Ze will email Wang a function to integrate in R
- Felix will proof the files that Wang uploaded (plot and summary)
- Felix will review power_analy.R one more time for general errors and comments
- Felix will email Drew Dimmery to ask about the two-sided F-test
- Wang will write a test to compare lm and ivreg for a SHARP RDD and check rd_predict on it

  > Wang: Done. (Agreed)

## Feburary, 28th

Today we spent a lot of time going over Wang's code to perform parametric MRDD. This code was translated from Vivian's Stata code. The code performs similar (but not identical to Vivian's), and some questions emerged. 

- Wang will remove the larger and equal sign from the GUI

  > Wang: Pending. It can be easily done after we decide how to design the input panel. 
  
- Felix will think more about how to check the design and design the GUI for one and two-variable RDD
- Felix will email Vivian and ask about the range number .15 and the .03 multiplier

  > Done. Vivan replied and said that both of these values were chosen after much deliberation, but essentialy ad-hoc. Vivian and I discussed data-adaptive procedures, and we came up with a simple one that instead of hard-coding .15 for the range around the cut-off, would just take a percentage, e.g., 10% of the data. The .03 should be OK being hard-coded because it just affects the grid for the estimates. 
  
- Wang should get rid of "with" statement and separate functions when possible

  > Wang: Done
  
- Ze will look into new points on kernel density estimate where Wang currently used approx() - he mentioned there is a more standard way
- Wang should save Stata workspace and give to Ze for more tests

  > Wang: Done, check inst/test_mv_rdd.R
  
- Wang should write modified test for rd.predict

  > Wang: Done. rd_est()'s ivreg and lm approaches agreed in effect estiamtes and predictions for sharp design.

- Felix will email Drew Dimmery again via linkedin to ask about the two-sided F-test

  >Done. Awaiting reply - not sure if anything will come from him. Seems busy at FB. 
  

## March, 7th

Today we spend most of our time going through the comments and corrections from our last meeting log. 

- Wang will remove the with, within, and by commands if they cause errors

  > Wang: Done. (the problem is actually not due to with(), within(), or by(), but due to referencing variables did not explicitly defined in the local environment, e.g.,the {} in with(df, {}). 
  
- Wang will try to reverse the binning so that the plot does not end up with a small bin right at the cut-off

  > Wang: Done.
  
- Ze will keep working on mrdest
- Felix will contact consultants to ask about design issue (smaller, smaller equal, etc.), and to inquire about fuzzy MRDDs
- Wang will try to create a 3d surface for the MRDD frontier approach

  > Wang: Half-baked... waiting for mfrd_est() so that I can start think more about the plot... 
  

## March, 21st

We went over Peter's comments.

- Felix will contact consultants and ask about design issue (smaller, smaller equal)

  > Done

- Ze will create wrapper for rd_est and mfred_est

  > Done

- Ze will create new object class
  > Done
  
- Felix will check with consultants whether F-test needed
 
  > Done
 
- Wang will look into generating ITT
 
  > Done

- Felix will email Peter and explain where certain things are in GUI

## March, 28th

Today we discussed some more of Peter's feedback and Wang showed us several improvements of the Shiny GUI, including the display filter that allows to pull lots of information (and hide it, if needed). Felix contacted both consultants and all agreed that a checkbox would be preferable. At this point Vivian also answered and agreed. Both Peter and Vivian feel a checkbox is better, and they both don't care much about the overall F-test of the regression. 
Please feel free to add comments such as "done" below. 
Most of the comments below are for Wang, but hopefully many of them are small, 

- Wang will extend range of sensitivity to bw plot

  > Done
  
- Wang will change "Treatment" to "Actual treatment" or "Received treatment" in GUI
  
  > Done.
  
- Wang will change "Original" to "Complier Average" in GUI

  > Done.
  
- Wang will experiment with putting in verbal if statements when selecting cut-off in GUI

  > Done.

- Wang will add checkbox to indicate boundary status ("Treated at boundary")

  > Done.
  
- Wang will add drop-down for kernel in GUI

  > Done. 
  
- Wang will hand-code row colors in Table 3.1

  > Done.
  
- Wang will change column headers in Table 3.1

  > Pending.

- Wang will display filters for MRDD as discussed (filter for univariate, centering, frontier)

  > Done.
  
- Wang will check digits displayed in CI in Table 3.1

  > Done.

- Ze will change summary function so that covariates and their inferential stats are pulled from coeftest and not lm
- Wang will change z/t dynamically in table depending on Type of SE used
  > Done. (only show "t" when the SE is "Regular" )

- Ze will test-run a fuzzy MRDD and look at the univariate and centering, using the following casese: MRDD with assignment 1 fuzzy, and assignment 2 sharp or MRDD with both fuzzy
- Ze will look into mfred_est.R and see if treatment quadrant is currently hand-coded as done by Vivian in her Stata code. If so, it needs to change to be dynamic


## April, 11th

We met after spring break and went over some changes that Wang and Ze made. We discussed some orginization of output, and the use of multiple imputation. 

- Ze will include error catching to report NA for situations in which MRDD has too few datapoints
- Wang will look into the bug that is caused by switching back and forth from MRDD and RDD

  > Done.

- Felix will confirm with consultants on how to generate SE, p, and CI for frontier average effect
 > Done. Peter says use delta method, or paper says use bootstrap. 
 
- Felix will check copyright of project CARE
 > Done. Awaiting response from them. 

- Felix will email Peter and explain where certain things are in GUI

- Felix will review power_analy.R one more time for general errors and comments

- Ze will look into handling multiple imputation

## April, 18th

- Wang will make predict.rd(), plot.rd(), plot.mfrd() to pass the tests.
  > Done.

- Ze will adjust rd_est for itt, tests for mfrd_est.

## April, 25th

- Ze will look into rd_impute and handling of bquoute
- Felix will fix s2cov, s2nocov, f2cov, f2nocov to include all parameters
  > Done.

- All will look into student feedback from Peter

- Felix will test run centered regression with quad terms
  > Done. Works because squaring centered terms leaves zero point unchanged. Mean of quad term not zero, but that's OK because we are estimating effect at cutoff and not average effect across all points. 
 
- Felix will look at Ze's mrd_power.R
- Wang will check plotting of local points
  > Done. elimiated the functionality because it is hard to visualize. 
- Wang will wait for Felix to generate new data for MRDD and test-run
 > Done.

## May, 2nd

- Felix will ask consultants about assignment issue with > or >= and centering approach
- Felix will ask consultants about bootstrap
- Felix will ask Vivian if frontier effect can be estimated for fuzzy designs
- Wang will put in N for frontier sample size
- Felix will think of how to implement placebo test for MRDD
- Ze will check if imputation =1 causes error
- Wang will remove header from Table 3.1
  > Done.
  
- Wang will look into quick deletion infinite loop bug
  > Done.

- Wang will look into download functions for tables
  > Done. The use can download CSV file. Table styling and higher-level header will not be preserved; the benefit of this CSV file is that it preserve full digits for numbers.

- Wang will allow scrolling in Table 1.1
  > Done. It will expand, instead of scrolling.

- If OK from Vivian, Ze will implement bootstrapped SE for frontier effect
- Wang will allow user to download tables from sensitivity tab
- Wang will get sensitivity analyses work.

## May, 9th

- We looked over the MRDD simulation for power
- Felix is still awaiting some reply from the consultants and will share it once it arrives
- Ze will look into writing a wrapper function for multiply imputed datasets
- Wang fixed and implemented a lot of new features in the UI based on the feedback from Peter's students
  > Done. Tables are downloadable now
- Wang will look into Vivian's Stata code to try to find out how one can generate true effects in an MRDD
  > Done.


## August, 25th

A new semester has started! Felix outlined general goals: 1) Publish paper in JSS, 2) Finish up MRDD, 3) Finish up power analysis. 
We also got feedback from Drew: the two-sided F-test is the smallest problem, other problems exist, use rdrobust instead, he is pulling rdd from CRAN.


  - Felix will finalize manual and send for proof reading
  - Felix will do quick lit search on effect sizes in RDDs
   > Done, use Cohen's d (estiamte at cut-off divided by standard deviation of outcome)
  - Retain F-tests for now, but we might delete them later
  - Ze will implement naive bootstrap, and cluster-bootstrapped as outlines in Wang's textbook (intact level 2 units)
  - Ze will draft email about centering indeterminante assigment
  > Done
  - Wang and Ze will discuss how to integrate multiple imputation in GUI
    > Done
  - Felix will check with Peter about feedback
    > Done


## September, 1st

- Felix will continue on manual and adjust wording on covariate balance checks (just use covariate as outcome, we won't implement it automatically)
- Ze and Wang will implement simple Cohen's d effect size

  > Done
  
- Wang will update GUI to include d and exclude F and display MI estiamtes

  > Done
  
- Wang will drop covs from sorting test

  > Done
  
- Wang will write small draft of how to use shinyapp offline

  > ```
  > library(rddapp)
  > library(shiny)
  > shiny::runApp(system.file("shinyrdd", package = 'rddapp'))
  > ```
  
- Wang will run check_package

  > ```
  > * checking tests ...
  >   Running ‘testthat.R’
  >  ERROR
  > Running the tests in ‘tests/testthat.R’ failed.
  > Complete output:
  >   > library(testthat)
  >   > library(rddapp)
  >   > 
  >   > test_check("rddapp")
  >   Error in library(rdd) : there is no package called 'rdd'
  >   Calls: test_check ... with_reporter -> force -> source_file -> eval -> eval -> library
  >   testthat results ================================================================
  >   OK: 27 SKIPPED: 0 FAILED: 0
  >   Execution halted
  > * DONE
  >```

- Ze will look into steps needed to publish on CRAN based on what we have so far
- Felix will create a simple multiply imputed dataframe
- Felix will look into the Lee and Caird adjustment for discrete variables

Felix got feedback from Peter and Vivian regarding the workshop. Participants of the workshop liked it a lot, and thought it was much simpler than doing it using code in Stata. They did not have time to collect any specific feedback, but they both feel that we should be able to release this soon. They will share it with another cohort of students. 

## September, 8th

- Ze and Wang will work on plotting the first imputation
- Wang will add button for bootstrapped SE for the frontier design
  > Done
- Ze will prepare template for CARE dataset for Felix to fill out
- Felix will continue to write draft
- We will get rid of the cutoff sensitivity analysis and bandwidth sensitivity for frontier in the GUI, because it would be too costly to do cutpoints with bootstraps, and we don't have non-parametric frontier RDD, and thus no bandwidth
- Wang and Ze will look into how to generate R code in the GUI
- Wang will implement sensitivites for the centering and univariate approach
- Felix will send Lee & Card paper on ordinal assigments
- Felix will generate multiple imputation testset

## September, 15th
- Felix will continue to write draft of manual
- Felix will finish presentation on RDD for Rochester
- Felix will ask Vivian for data on MRDD
- Wang will implement df for multiple imputation t-test
  > Done (if data is imputed OR the SE type = Constant, then the table of estimates will show an additional column 'df', and the name of test statistic will be 't' instead of 'z') 
- Ze will pass df to Wang's GUI
- Felix will finish CARE data
- Ze will give Felix template for CARE data
- Wang will modify bootstrap button (Make it clearer that the option refers to SE of the MFRD est)
  > Done (moved under Outcome Options)
- Wang will review code modifications that he pushed
  > Done
- Ze will read Lee and Card on specification error

THERE WILL BE NO MEETING NEXT WEEK BECAUSE FELIX IS TRAVELING

## October, 13th
Ze was still sick, so only Wang and Felix met.
 - Felix fixed the data.R file and uploaded CARE.rda. 
 - Felix will continue to write draft of manual
 - Because we were unable to obtain real MRDD data, we will just provide simulation code in the help file
 - Ze will pass df to Wang's GUI
 - Felix and Wang reviewed the GUI. The bandwidth estimates and sensitivity for MRDD looks good.
 
 A couple of minor additions for Wang:
 - Include true cut-off in sensitity plot 4.1
  > Done
 - Include actual bandwidth in plot 4.2
  > Done
 - Remove "model" from BW sensitivity CSV file
  > Done
 - Table 3.1. needs a download button
  > Done
 - Table 2.1. needs a download button
  > Done
 - Table 1.2 shapr MRDD design misspecification in design warning note
  > Done
 - Figure 4.2. update BW based on estimation
  > Done
 - Figure 4.1. always include cut-off in default range
  > Done


## October, 27th
There were some minor changes to the GUI for Wang to implement:

  - Fig. 4.2: add optimal bw as actual point
  - Fig. 4.2: y-axis scaling -> use 99.9% CI at optimal BW as the scaling of Y (or try other values 99%, 99.99% that seem to work)
  - Fig. 4.2: placement of the word "optimal"
  - Fig. 4.1: include actual estimate at cut-off
  - Fig. 4.1: change default cut-off range to +/- 1 SD
  - Fig. 4.1: wrong labeling for parameteric / non-parametric
 
    > All done.
 
  - Power analysis can proceed without graphs
  
    > Wang is working on it.
    
  - remove F-test in R package
  - DESCRIPTION file needs to be updated by Felix
  - Lee and Card S.E. adjustment is already implemented through cluster option in RDD, and thus in our package as well
  
  
  ## November, 3rd
  
  We met with the prospective new hires, and went over the things we did in the last two years. 
  
  - Wang will implement warning if pi_1 (probability of being treated) goes to 1.0 in the control group, thus extending the "undefined" designs
    
    > Done.
    
  
  - Ze gave us tasks for package development, they are posted on github
  - Wang should check libraries that he loads in shiny
  
    > Fixed per Ze's suggestion
    
  - Wang will compile list of NAMESPACE
  
    > Done.
    
  - Felix will write Vignette based on Ze's template
  - Wang will move plot_mfrd.R
  
    > Done.
  
  ## November, 10th
  
  Ze and Felix met and discussed some package development.
  
  - Ze will clean more code and check how to cite dependent packages.
  - Felix will write R script to get final CARE dataset. 
  - Felix will write vignette
  - Felix will circulate job ad among CS and IS departments and confirm with stats job candidates which part they would like to take over

  ## November, 17th
  - Felix will write R script to get final CARE dataset. 
  - Felix will write vignette
  - Felix will circulate job ad among CS and IS departments and confirm with stats job candidates which part they would like to take over
  -  Wang will put simulation data MRDD function as an internal function to be accessed via rddapp:::
  
    > Done.
  
