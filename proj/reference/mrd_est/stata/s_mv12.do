cap program drop s_mv1
program s_mv1, rclass
set more off

********************************************************
** MULTIVARIATE APPROACH I *****************************
********************************************************

** COMMAND OPTIONS *************************************

syntax [varlist(min=1 max=1)] [, obs(real 5000)]
tokenize `varlist'

** LOCAL MACROS *****************************************

local J = `obs'			//Number of simulated observations. Must be even number//

** TEMP VARIABLES **************************************

#delimit;
tempvar 
readres mathres 
hrt1 hrt2 hmt1 
hr hm
treatment comp tmath tread
gx1 gx2 fx1 x1prod fx2 x2prod 
chm mtemp mtemp2 chr rtemp rtemp2 
htgx1 htgx2 htx1prod htx2prod 
tgx1 tgx2 tx1prod tx2prod;
#delimit cr

** ESTIMATIONS OF SURFACE ***********************************************

local av "zcrav zcmav"
local alltreat "treat treatr treatm" 						//All indicators for treatments//
local complete "ztrmrav ztrmmav ztrrrav ztrrmav ztrrmavrav ztrmmavrav" 	//Complete model with all interactions//

** Complete model**
qui reg `1' `av' zreadmath `alltreat' `complete' 
scalar streat     = _b[treat]
scalar streatr    = _b[treatr]
scalar streatm    = _b[treatm]
scalar strmrav    = _b[ztrmrav]
scalar strrmav    = _b[ztrrmav]

**Treatment indicators for only treatment planes**
qui reg `1' `av' `alltreat' 
scalar httreat  = _b[treat]
scalar httreatr = _b[treatr]
scalar httreatm = _b[treatm]

** Treatment indicator only**
qui reg `1' `av' treat 
scalar ttreat = _b[treat]

** SAMPLE RESTRICTIONS *******************************

qui sum zreadav
local w = r(sd)*.15

qui sum zmathav
local m = r(sd)*.15

* Sample restriction to obs "w" bw for reading frontier
qui gen `readres'=.
qui replace `readres'=0 if zreadav>zreadc+`w' | zreadav<zreadc-`w'
qui replace `readres'=1 if zreadav<=zreadc+`w' & zreadav>=zreadc-`w'

* Sample restriction to obs "m" bw for math frontier
qui gen `mathres'=.
qui replace `mathres'=0 if zmathav>zmathc+`m' | zmathav<zmathc-`m'
qui replace `mathres'=1 if zmathav<=zmathc+`m' & zmathav>=zmathc-`m'

** CREATES H VARIABLE *********************************

qui set obs `J'
local N = `J'/2
local O = `N' + 1

qui sum zcrav
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))

qui gen `hr' = 0 in 1/`N'
qui gen `hrt1' = _n-`N' in `O'/`J'
qui gen `hrt2' = `min' + `hrt1'/`N'*(`max' - `min') 
qui replace `hr' = `hrt2' in `O'/`J'

qui sum zcmav
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))

qui gen `hmt1' = _n-1 in 1/`N'
qui gen `hm' = `min' + `hmt1'/`N'*(`max' - `min') 
qui replace `hm' = 0 in `O'/`J'

*Generates treatment variables
qui gen `treatment' = 1 in 1/`J' 
qui gen `comp' = 0 in 1/`J'
qui gen `tmath' = 0 in 1/`N'
qui replace `tmath' = 1 in `O'/`J'
qui gen `tread' = 1 in 1/`N'
qui replace `tread' = 0 in `O'/`J'

**********************************************************************
** COMPLETE MODEL ****************************************************
**********************************************************************

** TREATMENT OUTCOME FUNCTIONS g(x)'s *******************************

* Function for treatment effect along MATH frontier
qui gen `gx1' = streat*`treatment' + streatm*`tmath' + strmrav*`tmath'*(`hr' - zreadc)

* Function for treatment effect along READING frontier
qui gen `gx2' = streat*`treatment' + streatr*`tread' + strrmav*`tread'*(`hm' - zmathc)

** DENSITY FUNCTIONS f(x)'s *****************************************

** Reading assignment variable / Math treatment frontier ***********

* Kernel density of reading assignment variable
qui kdensity zreadav if `mathres' == 1, gen(`fx1') at(`hr') tri nogr
*line `fx1' `hr' if `tmath' == 1, xline(zreadc)

* Product for numerator: g(x)*f(x)
qui gen `x1prod' = `gx1'*`fx1' if `hr'>=zreadc

* Numerator: Integral for numerator of MATH frontier (x1)
qui integ `x1prod' `hr' if `hr'>=zreadc 
scalar numintx1 = r(integral) 

* Denominator: Intergral of the conditional density function f1(x1) 
qui integ `fx1' `hr' if `hr'>=zreadc 
scalar denintx1 = r(integral) 

** Math assignment variable / Reading treatment frontier **************

* Kernel density of math assignment variable
qui kdensity zmathav if `readres' == 1, gen(`fx2') at(`hm') tri nogr
*line `fx2' `hm' if `tread' == 1, xline(zmathc)

* Product for numerator: g(x)*f(x)
qui gen `x2prod' = `gx2'*`fx2' if `hm'>=zmathc

* Numerator: Integral for numerator of READING frontier (x2)
qui integ `x2prod' `hm' if `hm'>=zmathc
scalar numintx2 = r(integral) 

* Denominator: Intergral of the conditional density function f2(x2)
qui integ `fx2' `hm' if `hm'>=zmathc
scalar denintx2 = r(integral) 

** Treatment weights *************************************************

* Marginal density at math cutoff
qui gen `chm' = `hm' - zmathc
qui gen `mtemp' = abs(`chm')
qui sum `mtemp'
qui gen `mtemp2' = 1 if `mtemp' == r(min)
qui sum `fx2' if `mtemp2' == 1
scalar mdmcw = r(mean)

* Marginal density at reading cutoff
qui gen `chr' = `hr' - zreadc
qui gen `rtemp' = abs(`chr')
qui sum `rtemp'
qui gen `rtemp2' = 1 if `rtemp' == r(min)
qui sum `fx1' if `rtemp2' == 1
scalar mdrcw = r(mean)

* Weight for Math frontier
return scalar mathw = mdmcw*denintx1

* Weight for Reading frontier
return scalar readw = mdrcw*denintx2

** Treatment estimates for complete model ******************************

* Math frontier expected value
local mathev = numintx1 / denintx1 
return scalar mathev = `mathev'

* Reading frontier expected value
local readev = numintx2 / denintx2 
return scalar readev = `readev'

* ATE across both frontiers
local ate = ((return(mathw)*`mathev') + (return(readw)*`readev')) / (return(mathw) + return(readw))
return scalar ate = `ate'

**********************************************************************
** HETEROGENEOUS TREATMENTS MODEL ***********************************
**********************************************************************

* Function for treatment effect along MATH frontier
qui gen `htgx1' = httreat*`treatment' + httreatm*`tmath' 

* Function for treatment effect along READING frontier
qui gen `htgx2' = httreat*`treatment' + httreatr*`tread' 

** DENSITY FUNCTIONS f(x)'s *****************************************

** Reading assignment variable / Math treatment frontier ***********

* Product for numerator: g(x)*f(x)
qui gen `htx1prod' = `htgx1'*`fx1' if `hr'>=zreadc

* Numerator: Integral for numerator of MATH frontier (x1)
qui integ `htx1prod' `hr' if `hr'>=zreadc 
scalar htnumintx1 = r(integral) 

** Math assignment variable / Reading treatment frontier **************

* Product for numerator: g(x)*f(x)
qui gen `htx2prod' = `htgx2'*`fx2' if `hm'>=zmathc

* Numerator: Integral for numerator of READING frontier (x2)
qui integ `htx2prod' `hm' if `hm'>=zmathc
scalar htnumintx2 = r(integral) 

** Treatment estimates for "heterogeneous treatments" (HT) model *******

* Math frontier expected value
local htmathev = htnumintx1 / denintx1 
return scalar htmathev = `htmathev'

* Reading frontier expected value
local htreadev = htnumintx2 / denintx2 
return scalar htreadev = `htreadev'

* ATE across both frontiers
local htate = ((return(mathw)*`htmathev') + (return(readw)*`htreadev')) / (return(mathw) + return(readw))
return scalar htate = `htate'

**********************************************************************
** TREATMENT ONLY MODEL **********************************************
**********************************************************************

* Function for treatment effect along MATH frontier
qui gen `tgx1' = ttreat*`treatment' 

* Function for treatment effect along READING frontier
qui gen `tgx2' = ttreat*`treatment' 

** DENSITY FUNCTIONS f(x)'s *****************************************

** Reading assignment variable / Math treatment frontier ***********

* Product for numerator: g(x)*f(x)
qui gen `tx1prod' = `tgx1'*`fx1' if `hr'>=zreadc 

* Numerator: Integral for numerator of MATH frontier (x1)
qui integ `tx1prod' `hr' if `hr'>=zreadc 
scalar tnumintx1 = r(integral) 

** Math assignment variable / Reading treatment frontier **************

* Product for numerator: g(x)*f(x)
qui gen `tx2prod' = `tgx2'*`fx2' if `hm'>=zmathc

* Numerator: Integral for numerator of READING frontier (x2)
qui integ `tx2prod' `hm' if `hm'>=zmathc
scalar tnumintx2 = r(integral) 

** Treatment estimates for "treatment only" (TO) model *******

* Math frontier expected value
local tmathev = tnumintx1 / denintx1 
return scalar tmathev = `tmathev'

* Reading frontier expected value
local treadev = tnumintx2 / denintx2 
return scalar treadev = `treadev'

* ATE across both frontiers
local tate = ((return(mathw)*`tmathev') + (return(readw)*`treadev')) / (return(mathw) + return(readw))
return scalar tate = `tate'

** DISPLAY ************************************************************

di _newline as txt as yellow "***EFFECT ESTIMATES FOR MULTIVARIATE I STANDARDIZED***" 
di _newline as txt "Math weight (st): " as red round(return(mathw), .00001)  
di as txt "Reading weight (st): " as red round(return(readw), .00001)

di _newline as txt "Math frontier for complete model (st): " as red round(`mathev', .0001)
di as txt "Reading frontier for complete model (st): " as red round(`readev', .0001)
di as txt "ATE for " as yellow "complete " as txt "model (st): " as red round(`ate', .0001)

di _newline as txt "Math frontier for heterogeneous treatment model (st): " as red round(`htmathev', .0001)
di as txt "Reading frontier for heterogeneous treatment model (st): " as red round(`htreadev', .0001)
di as txt "ATE for " as yellow "heterogeneous treatment " as txt "model (st): " as red round(`htate', .0001)

di _newline as txt "Math frontier for treatment only model (st): " as red round(`tmathev', .0001)
di as txt "Reading frontier for treatment only model (st): " as red round(`treadev', .0001)
di as txt "ATE for " as yellow "treatment only " as txt "model (st): " as red round(`tate', .0001)
************************************************************************

end

