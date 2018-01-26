cap program drop trueeffects
program trueeffects, rclass
set more off

********************************************************
** TRUE EFFECTS ****************************************
********************************************************

** COMMAND OPTIONS *************************************

syntax [, t(real 4) mt(real 0) rt(real 0) tmrav(real 0) trmav(real 0)]

** OPTION DEFINITIONS **********************************

* t 			size of treatment effect for all treatment observations
* mt 			size of treatment effect for MATH frontier
* rt 			size of treatment effect for READING frontier
* tmrav 		MATH treatment x reading AV interaction
* trmav 		READING treatment x math AV interaction

** LOCAL MACROS *****************************************

local J = _N			//Number of simulated observations. Must be even number//

** TEMP VARIABLES **************************************

tempvar hrt1 hrt2 hmt1 hr hm treat comp tmath tread gx1 gx2 fx1 fx2 x1prod x2prod 

** CREATES H VARIABLE *********************************

qui set obs `J'
local N = `J'/2
local O = `N' + 1

qui sum readav
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))

qui gen `hr' = 0 in 1/`N'
qui gen `hrt1' = _n-`N' in `O'/`J'
qui gen `hrt2' = `min' + `hrt1'/`N'*(`max' - `min') 
qui replace `hr' = `hrt2' in `O'/`J'

qui sum mathav
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))

qui gen `hmt1' = _n-1 in 1/`N'
qui gen `hm' = `min' + `hmt1'/`N'*(`max' - `min') 
qui replace `hm' = 0 in `O'/`J'

*Generates treatment variables
qui gen `treat' = 1 in 1/`J' 
qui gen `comp' = 0 in 1/`J'
qui gen `tmath' = 0 in 1/`N'
qui replace `tmath' = 1 in `O'/`J'
qui gen `tread' = 1 in 1/`N'
qui replace `tread' = 0 in `O'/`J'

** CONDITIONAL EXPECTATIONS & SDs ********************************

* |E for reading av, math tr
scalar cdex1 = (readmean + (b1*(mathc - mathmean)))
* |SD for reading av, math tr
scalar cdsd1 = sqrt((1-corr^2)*(readsd^2))

* |E for math av, reading tr
scalar cdex2 = (mathmean + (b2*(readc - readmean)))
* |SD for math av, read tr
scalar cdsd2 = sqrt((1-corr^2)*(mathsd^2))

**FRONTIER WEIGHTS ***********************************************

**Marginal probability at Math treatment UNSTANDARDIZED
scalar un_mdrav = normalden(mathc, mathmean, mathsd) 

**Marginal probability at Math treatment STANDARDIZED
scalar s_mdrav = normalden((mathc-mathmean)/mathsd, 0, 1) 

scalar z1 = ((readc - cdex1)/cdsd1)
scalar cdrav = (1-normal(z1))

*Weight for MATH frontier UNSTANDARDIZED
return scalar un_mathw = un_mdrav*cdrav

*Weight for MATH frontier STANDARDIZED
return scalar s_mathw = s_mdrav*cdrav

**Marginal probability at Reading treatment UNSTANDARDIZED
scalar un_mdmav = normalden(readc, readmean, readsd)

*Marginal probability of Reading treatment UNSTANDARDIZED
scalar s_mdmav = normalden((readc-readmean)/readsd, 0, 1)

scalar z2 = ((mathc - cdex2)/cdsd2)
scalar cdmav = (1-normal(z2))

*Weight for READING frontier UNSTANDARDIZED
return scalar un_readw = un_mdmav*cdmav

*Weight for READING frontier STANDARIZED 
return scalar s_readw = s_mdmav*cdmav

**********************************************************************
** EXPECTED VALUES  **************************************************
**********************************************************************

** FUNCTIONS G1(X1) & G2(X2) *****************************************

* Function for treatment effect along MATH frontier
qui gen `gx1' = `t'*`treat' - `mt'*`tmath' - `tmrav'*`tmath'*(`hr' - readc) if `tmath'==1

* Function for treatment effect along READING frontier
qui gen `gx2' = `t'*`treat' - `rt'*`tread' - `trmav'*`tread'*(`hm' - mathc) if `tread'==1

** MATH FRONTIER EV **************************************************

* Density function 
qui gen `fx1' = normalden(`hr', cdex1, cdsd1) if `tmath'==1 

* Product of outcome and density function
qui gen `x1prod' = `gx1'*`fx1' 

** Integral for numerator of MATH frontier
qui integ `x1prod' `hr' if `hr'>=readc
scalar intx1 = r(integral) 

**Expected value for MATH frontier
return scalar mathev = intx1 / cdrav

** READING FRONTIER EV ************************************************

* Density function along READING frontier
qui gen `fx2' = normalden(`hm', cdex2, cdsd2) if `tread'==1

* Product of outcome and density function
qui gen `x2prod' = `gx2'*`fx2' 

** Integral for numerator of READING frontier
qui integ `x2prod' `hm' if `hm'>=mathc
scalar intx2 = r(integral) 

**Expected value for READING frontier
return scalar readev = intx2 / cdmav

** AVERAGE TREATMENT EFFECT ACROSS BOTH FRONTIERS ************************

return scalar un_ate = ((return(un_mathw)*return(mathev)) + (return(un_readw)*return(readev))) / (return(un_readw) + return(un_mathw))
return scalar s_ate = ((return(s_mathw)*return(mathev)) + (return(s_readw)*return(readev))) / (return(s_readw) + return(s_mathw))

** DISPLAY **************************************************************

di _newline as txt as yellow "***TRUE EFFECTS***" 
di _newline as txt "Math weight (unstandardized): " as red round(return(un_mathw), .001)
di as txt "Reading weight (unstandardized): " as red round(return(un_readw), .001)
di _newline as txt "Math weight (standardized): " as red round(return(s_mathw), .001)
di as txt "Reading weight (standardized): " as red round(return(s_readw), .001)
di _newline as txt "Math frontier expected value: " as red round(return(mathev), .01)
di as txt "Reading frontier expected value: " as red round(return(readev), .01)
di as txt "Unstandardized ATE: " as red round(return(un_ate), .01)
di as txt "Standardized ATE: " as red round(return(s_ate), .01)

**************************************************************************
end
