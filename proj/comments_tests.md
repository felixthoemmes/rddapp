#attr_check.R

This file was originally written by Felix, and then Wang took over. The special comments look fine to me. Also every single line has a regular comment, so I think this is good as well. 
Currently, the function accepts x2 (a second assignment variable) as an argument, but it does not actually compute anything. Now that we started working on the MRDD we should extend this function. 
- Potential tests: 
1) The special comments already have a super-simple example.

   x1 <- c(NA, 0, 1, NA, 2, NA)
   
   y <- c(NA, 0, NA, 2, 3, 4)
   
   t <- c(NA, NA, 0, 0, 1, NA)

Just using table we could count the number of missingns and compare it with the results of our function.


marginal missingness of all vars

   table(x1,useNA = "always")

   table(y,useNA = "always")

  table(t,useNA = "always")
 

conditional missingness of x1 and y, given that t is either 0 or 1

   table(t,x1,useNA = "always")

   table(t,y,useNA = "always")


Then we would need to compare some elements of this table with results from the function. 

We could make additional examples, by changing the data, but we could retain the tables function and count. 

#bw_ik09.R

This file was written by Drew Dimmery. It computes the IK optimal bandwidth from the 2009 paper. 
It appears to still has the original special comment.
I am not sure if we need the "verbose" argument, but see no harm in leaving it. 
It has some comments inside the code. The vast majority of the code are parts of the equations of IK-2009. 

I noted that there does not seem to be missing data handling, unless I overlooked it. 

A potential test could be to somehow obtain the data from the example in IK-2009. They used data from this paper:
Lee, D.
, (2008), “Randomized experiments from non-random selecti
on in U.S. House elections,”
Jour-
nal of Econometrics
, 142, 675-697.

Unfortunately, this paper does not provide the clean data, but only links to an online source, which has the raw data (which was cleaned in several steps by Lee!). 

Another option for a test would be to compare it to software from Imbens. Unfortunately, he only provides Stata and Matlab, so we can't do the test directly in R. 

https://people.stanford.edu/imbens/software

But we could construct a single dataset, then run the code in Stata, manually copy the result as a single value into R, and then check. I am unsure though whether the software uses the 2009 or 2012 version of his paper. 

Further, we could try to construct datasets that trigger any of the exceptions on purpose.
- Datasets in which assignment variable and outcome have different length due to missing data
- Datasets in which assignment variable and outcome are non-numeric (e.g., strings)
- Datasets that have sparse or no data at cutpoint

#bw_ik12.R

This file was originally written by Stigler and has presumambly still the old special comments. It has a decent amount of commenting, and most of the code is either checks, or doing parts of the equation of IK-2012. 
File has a lot of commented code at the end, and I am not sure why.

I could imagine a similar test as in the file bw_ik09.R, that is, either using data that IK used in the paper (difficult to get), or running the software that Imbens posted on his website. Again, I am not sure whether his posted software is IK09 or IK12.

Further, we could try to construct datasets that trigger any of the exceptions on purpose.
- Datasets in which assignment variable and outcome have different length due to missing data
- Datasets in which assignment variable and outcome are non-numeric (e.g., strings)
- Datasets that have sparse or no data at cutpoint


#dc_test.R

This file was written by Drew Dimmery. It computes the McCrary density test. 
It still has the original special comment. It has some comments within the file at various points. 
Here are some spots where some additional comments might be helpful.
- 72, when it computes cellval
- 128, clarify what function wt_kern is (which is our own function, correct?)
- 165, 175, contains several lines of commented-out code

Potential tests are to see if we can reproduce McCrary's results from his own paper. He used several datasets, but it appears that only one can be easily obtained. An alternative would be to compare our results to rdd package results. 

Further tests should probe the expections.
- Datasets that have sparse or no data at cutpoint should pose a problem
- Datasets with missing values
- Datasets in which assignment variable and outcome are non-numeric (e.g., strings)


#mrd.est
This is the function for the MRDD, written by Ze. Special comments look good to me.
Here are a couple of places where some additional comments might be helpful.
- Short comment at each "if" section that catches errors, etc. (e.g., 67, 74, etc.)
- line 139, what function is var_center? 
- line 166, in the univariate approach, what are g and l (in the stop comments and code)
- line 207, general coding question: is it normal that the actual estimation (here rd_est) is done within the return call. 
Perfectly OK with it, if that's common, just wanted to know, because intuetively I personally wouldn't do it. 

Some potential tests would be to compare it with Vivian's Stata files. Would have the disadvantage that it is not a pure R test, but we would have to create a dataset, run Stata, manually record the results and put into R, and then run R and compare. 

Further tests should probe the expections.
- Datasets that have sparse or no data at either or both cutpoints should pose a problem, especially for non-parametric estimation. 
- Datasets with missing values
- Datasets in which assignment variable and outcome are non-numeric (e.g., strings)
- Datasets in which the assignment variables have different signs (+/-) for the assignment. E.g., larger on X1 means treatment, but smaller on X2 means treatment
- Datasets in which some segments of combinations of being above or below the threshold on either x1 or x2 are empty
- Dataests in which some X1 is sharp, but X2 is fuzzy


#plot.rd.R

This function plots results of an RDD, and was originally written by Drew Dimmery, as the special comments suggest. If I recall correctly, then Wang updated this function. 
This function has little comments and should benefit from some extra comments, here:
- line 32, 38
- line 42, 45, 48
- line 53
- line 61
- line 67
- line 106-128 where the actual plot is constructed

Since this is a plot I don't think it makes sense to use actual data from someone else and recreate their plot. In fact, since this is a plot, I think other than not throwing an error, there is not much automatic testing that can be done. In the end, a human needs to look at the plot and judge whether it looks correct. At least I can't think of anything automated. 
Tests would include throwing some usual or unusual data to the function and checking whether the plot looks correct in terms of the effect that it shows. 


#power_analy.R
Good special comments that clearly lay out what the function does. 
Wasn't sure where the default values in the function came from, but I guess we need some to not have the function crash. 
I also noticed that the simulation uses a for loop (if I read the code correctly). Is this a potential bottleneck? Can we use replicate() or raply() instead, or somehow vectorize? 
Commenting in the function seems overall adequate, with line comments at most critical parts. 
Potential comments and tests below: 
- Line 74, shouldn't the two probs be 0 and 1, and not 1 and 1?
- Line 82, does this also account for situations in which the treatment prob is larger on the left hand side of the cut-off? 
- Line 92, do we still have an error exception for cut-points outside the range?
- Line 100, and Line 129, probably need to hear the logic behind this one more time from Ze
- Line 100, will this also work with sharp, since it requests fuzzy probs
- Line 104, what is the large chunk of commented-out code?
- Line 161, why use try() and should one of these probs be 0?
- Line 169, need some comments of what this part is doing

Potential tests: Goldberger has some analytic power solutions (for a very specific situation) and we should compare our results with his. Alternatively, we could also test whether Type I error rates are 5% if we set effect size to 0. 

#print.rd.R
This file was written by Drew Dimmery, as it states in the special comments. It has no comments in the code. I assume it is a function that prints some results from an rdd object. Do we still use this file?
I am unsure of possible tests that could be written. 

#rd_est.R
This file is the main estimation function. The special comments are still mostly from Drew Dimmery,
with some adjustments. Most of the code has also been adapted by Drew Dimmery with some adjustments on our part. 
The comments within the code are currently spare and we probably need a bit more. Below I suggest some places to comment, ask for clarification, and finally suggest some possible tests. 
- There is an argument "subset". It's clear what it does, but I am not sure why it even exists. 
- Is the argument "simple" from us, to make communication with shiny simpler?
- Maybe at the beginning of the code (103++) when several "if" checks are performed, it might be good to have a single line at the beginning of the if check to describe what it does. Most of them are straight-forward, e.g., one performs subsetting, one performs cluster-standard errors, etc. 
- I guess I am not entirely sure what the model.frame function does. I guess it parses formulas and subsets dataframes,but a comment, or an explanation here would be helpful. Here is an example: Z <- model.frame(formula, rhs = 1, lhs = 0, data = data, na.action = na.pass)[[2]]
- In the bandwidth selection, I am not sure why we use the try command here. 
    bw <- try(rdd_bw_ik(X = X, Y = Y, cutpoint = cutpoint, kernel = kernel, verbose = verbose), silent = T)

#rddapp-package.R
This file has a special comment, but no actual R code. I am not sure what it does. 

#shiny_run.R
This file has a short stub as special comments, and some comments in the code. 
This part of the code is the one that I am probably least familiar with, as it interacts with Shiny. I am not sure of any tests that could be written for this file. 

#summary.rd.R
This file was originally written by Drew Dimmery, as stated in the special comments. 
It provides a summary() function for rdd objects. It takes an object from an RDD analysis, extracts certain quantities, add some information (like stars for p-values). It could use some additional commenting in the code.
- Line 28, what does mod=false do
- Line 38, what does this piece of code do
- Line 76, what computation is this

Since this function extracts information out of an object, one test might be to feed it an object with known values, and then confirm that it extracts the correct ones. 

#var_center.R
This file was written by Ze Jin and computes the MRDD using the centering approach. The special comments seem well developed. There are currently no comments within the file. I would suggest to have a single line of comment on top of each if clause that checks for errors. 
-Line 39, unclear what l and g designs are, and how the checks are performed
Currently the file is still work in progress and no tests are suggested. 

#wt_kern.R
This file was written by Drew Dimmery. The special comments seem relatively complete. The function checks which kernel is used for the non-parametric bandwidth, and then returns a numerical value for each, based on the description of IK. 
I don't think any additional comments or tests are needed for this file. 
