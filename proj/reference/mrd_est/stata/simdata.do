cap program drop simdata
program simdata, rclass
set more off
set seed 1000

********************************************************
** SIMDATA *********************************************
********************************************************

** COMMAND OPTIONS *************************************

#delimit;
syntax [, 
rc(real 40) mc(real 60)				//cutoffs//
co(real .2) 					//correlation//
rmean(real 45) mmath(real 55) 			//means//
rsd(real 10) msd(real 10)			//sds//
mav(real 1) rav(real .5) rm(real .0)		//AV//
tr(real 4) rt(real .0) mt(real .0)		//treatment//
rmav(real .0) rrav(real .0) 			//reading tr x av//
mmav(real .0) mrav(real .0) 			// math tr x av//
mtint(real .0) rtint(real .0)			// tr x av1 x av2//
obs(real 5000)];				//Number of observations//
#delimit cr

** OPTION DEFINITIONS **********************************

* mav			math AV
* rav			reading AV
* rm			reading x math AV interaction
* tr			treatment indicator
* rt			reading treatment only
* mt			math treatment only
* rmav			reading tr x math AV
* rrav			reading tr x reading AV
* mmav			math tr x math AV
* mrav			math tr x reading AV
* mtint			math tr x reading x math AV
* rtint			reading tr x reading x math AV
* rc			reading cutoff
* mc			math cutoff
* co			correlation of reading and math AVs
* rmean			reading mean
* mmath			math mean
* rsd			standard deviation for reading AV
* msd			standard deviation for math AV

** TEMP VARIABLES **************************************

tempvar m_truey m_e1 m_prey m_e2

** CREATES SIMULATED DATA ******************************

qui drop _all
matrix cut = (`rc', `mc')		//Create a vector of reading and math cutoffs
matrix cor = (1, `co', 1)		//Create a vector that contains the equivalent of a lower traingular correlation matrix
matrix mean = (`rmean',`mmath')		//Create a vector that contains the means of the variables
matrix sd = (`rsd', `msd')		//Create a vector that contains the standard deviations

qui drawnorm readav mathav, n(`obs') corr(cor) cstorage(lower) means(mean) sds(sd)	
						*Draws a sample of 5000 cases from a normal distribution 
						*with specified correlation structure, means & SDs
						
label var mathav "Math school AV"
label var readav "Reading school AV"

** CREATES SCALAR VALUES ********************************

qui matrix list cut
qui matrix list cor
qui matrix list mean
qui matrix list sd

scalar readc = el(cut,1,1) 
scalar readmean = el(mean,1,1)
scalar readsd = el(sd,1,1)
scalar readv = readsd^2
scalar mathc = el(cut,1,2)
scalar mathmean = el(mean,1,2)
scalar mathsd = el(sd,1,2)
scalar mathv =  mathsd^2
scalar corr = el(cor,1,2)
scalar cov = corr*mathsd*readsd
scalar b1 = cov/mathv
scalar b2 = cov/readv

** CREATES TREATMENT VARIABLES ********************

qui gen treatr=. 
qui replace treatr=1 if mathav>=mathc & readav<readc
qui replace treatr=0 if mathav>=mathc & readav>=readc | mathav<mathc & readav<readc | mathav<mathc & readav>=readc

qui gen treatm=. 
qui replace treatm=1 if mathav<mathc & readav>=readc
qui replace treatm=0 if mathav<mathc & readav<readc | mathav>=mathc & readav<=readc | mathav>=mathc & readav>=readc

qui gen treatb=. 
qui replace treatb=1 if mathav<mathc & readav<readc
qui replace treatb=0 if mathav>=mathc & readav>=readc | mathav>=mathc & readav<readc | mathav<mathc & readav>=readc 

qui gen mcutoff=.
qui replace mcutoff=1 if mathav<mathc
qui replace mcutoff=0 if mathav>=mathc 

qui gen rcutoff=.
qui replace rcutoff=1 if readav<readc
qui replace rcutoff=0 if readav>=readc 

qui gen treat=1 if treatr==1 | treatm==1 | treatb==1
qui replace treat=0 if treat==.

** STANDARDIZE ASSIGNMENT VARIABLES*********************

egen zmathav = std(mathav)
label var zmathav "Standardized math school av"
egen zreadav = std(readav)
label var zreadav "Standardized reading school av"

qui sum mathav 
scalar mmean = r(mean)
scalar msd = r(sd)
qui sum readav
scalar rmean = r(mean)
scalar rsd = r(sd)

* New Standardized cutoff values
scalar zmathc = (mathc - mmean)/msd
scalar zreadc = (readc - rmean)/rsd

** CENTERS ASSIGNMENT VARIABLE UNSTANDARDIZED*********************

qui gen cmav = mathav - mathc
label var cmav "Centered math school av"
qui gen crav = readav - readc
label var crav  "Centered reading school av"

qui gen zcmav = zmathav - zmathc
label var zcmav "Standardized centered math school av"
qui gen zcrav = zreadav - zreadc
label var zcrav  "Standardized centered reading school av"

** CREATES MATH AND READING SAMPLES ******************************

qui gen mathsample = 1 if crav>=0
qui gen readsample = 1 if cmav>=0

** CREATES CONTROL COVARIATES STANDARDIZED****************

qui gen readmath = crav*cmav
qui gen trmrav = treatm*crav
qui gen trmmav = treatm*cmav
qui gen trrrav = treatr*crav
qui gen trrmav = treatr*cmav
qui gen trrmavrav = treatr*cmav*crav
qui gen trmmavrav = treatm*cmav*crav

** CREATES CONTROL COVARIATES UNSTANDARDIZED****************

qui gen zreadmath = zcrav*zcmav
qui gen ztrmrav = treatm*zcrav
qui gen ztrmmav = treatm*zcmav
qui gen ztrrrav = treatr*zcrav
qui gen ztrrmav = treatr*zcmav
qui gen ztrrmavrav = treatr*zcmav*zcrav
qui gen ztrmmavrav = treatm*zcmav*zcrav

** GENERATES OUTCOME VARIABLES **********************

** Math outcome**

* Pretest values
qui gen `m_truey' = 70 + 1*cmav + .5*crav
label var `m_truey' "True math pretest score"

qui gen `m_e1' = 0 + 2*invnorm(uniform())
qui gen `m_prey' = `m_truey' + `m_e1'
label var `m_prey' "Math pretest with error"

qui gen `m_e2' = 0 + 2*invnorm(uniform())

* Post-test values
#delimit;
qui gen m_posty = 70 + 
`mav'*cmav + `rav'*crav +
`rm'*readmath + 
`tr'*treat - `rt'*treatr - `mt'*treatm -
`mrav'*trmrav - `mmav'*trmmav - 
`rrav'*trrrav - `rmav'*`trrmav' - 
`rtint'*trrmavrav - `mtint'*trmmavrav + `m_e2';
#delimit cr

* Gain score
qui gen m_gainscore = m_posty - `m_prey' 
label var m_gainscore "Gain score for math outcome"

** Variable labels ********************************

label var treatr "Receives reading treatment"
label var treatm "Receives math treatment"
label var treatb "Receives both math and reading treatment"
label var mcutoff "Scored below math cutoff"
label var rcutoff "Scored below reading cutoff"
label var treat "Received any treatment"
label var mathsample "Above reading cutoff, math sample only"
label var readsample "Above math cutoff, reading sample only"

** Interaction terms**
label var readmath "crav x cmav"
label var trmrav "Math tr x crav"
label var trmmav "Math tr x cmav" 
label var trrrav "Read tr x crav"
label var trrmav "Read tr x cmav"
label var trrmavrav "Read tr x cmav x crav"
label var trmmavrav "Math tr x cmav x crav"

** Standardized variables **
label var zreadmath "std crav x cmav"
label var ztrmrav "std Math tr x crav"
label var ztrmmav "std Math tr x cmav"
label var ztrrrav "std Read tr x crav"
label var ztrrmav "std Read tr x cmav"
label var ztrrmavrav "std Read tr x cmav x crav"
label var ztrmmavrav "std Math tr x cmav x crav"

** Outcome **
label var m_posty "Simulated math outcome"

end
