cap program drop centering
program centering, rclass
set more off

********************************************************
** CENTERING APPROACH **********************************
********************************************************

** COMMAND OPTIONS *************************************

syntax [varlist(min=1 max=1)] [, obs(real 100) *]
tokenize `varlist'

** VARIABLE DEFINITIONS *******************************

* 1 is outcome variable

** OPTION DEFINITIONS **********************************

* bwidth		bandwidth for lpoly command

** LOCAL MACROS ***************************************

local J = `obs'					//Number of observations//
local opt "tri nogr deg(1) `options'" 		//Options for lpoly command//

** TEMP VARIABLES **************************************

#delimit;
tempvar 
z sdz rz
c_temp1 sd_temp1 r_temp1
abz absdz abrz
zind sdzind rzind
c_newav sd_newav r_newav
c_treat sd_treat r_treat
sdmav sdrav 
rmath rread
mtemp rtemp
rcmav rcrav
f0 f1 hf0 hf1 df0 df1
sdf0 sdf1 sdhf0 sdhf1 sddf0 sddf1
rf0 rf1 rhf0 rhf1 rdf0 rdf1;
#delimit cr

** CHOOSING MINIMUM CTRED SCORE ********************

qui egen `c_newav'= rowmin(cmav crav)
label var `c_newav' "Centered AV score"

** STANDARDIZED ASSIGNMENT VARIABLE **************** 

qui sum cmav
scalar msd = r(sd)
qui sum crav
scalar rsd = r(sd)

qui gen `sdmav' = cmav/msd
label var `sdmav' "Standardized math AV"

qui gen `sdrav' = crav/rsd
label var `sdrav' "Standardized reading AV"

** CHOOSING MINIMUM SD SCORE ********************

qui egen `sd_newav' = rowmin(`sdmav' `sdrav')
label var `sd_newav' "Standardized AV score"

** RANK ORDER ASSIGNMENT VARIABLE **************** 

sort mathav
qui egen `rmath' = rank(mathav)

sort readav
qui egen `rread' = rank(readav)

** CENTER RANK AV *******************************

qui gen `mtemp' = cmav if cmav>=0
sort `mtemp'
scalar rmathc = `rmath' in 1/1

qui gen `rtemp' = crav if crav>=0
sort `rtemp'
scalar rreadc = `rread' in 1/1

qui gen `rcmav' = `rmath' - rmathc
label var `rcmav' "Centered rank math av"

qui gen `rcrav' = `rread' - rreadc
label var `rcrav'  "Centered rank reading av"

** CHOOSING MINIMUM RANK CTRED SCORE *************

qui egen `r_newav' = rowmin(`rcmav' `rcrav')
label var `r_newav' "Rank centered AV score"

** CREATES Z VARIABLES **********************************

* For C_NEWAV

qui sum `c_newav'
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))
qui gen `c_temp1' = _n-1 in 1/`J'
qui gen `z' = `min' + `c_temp1'/`J'*(`max' - `min') 
qui sum `z'
cap set obs r(N)

qui g `abz' = abs(`z')
qui sum `abz'
qui g `zind' = 1 if `abz' == r(min)
qui sum `z' if `zind' == 1
if r(mean) < 0 {
qui replace `z' = `z' - r(mean)
}
else {
qui replace `z' = `z' - r(mean)
}
qui g `c_treat' = .
qui replace `c_treat' = 1 if `c_newav'<0
qui replace `c_treat' = 0 if `c_newav'>=0

* For SD_NEWAV 

qui sum `sd_newav'
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))
qui gen `sd_temp1' = _n-1 in 1/`J'
qui gen `sdz' = `min' + `sd_temp1'/`J'*(`max' - `min') 

qui g `absdz' = abs(`sdz')
qui sum `absdz'
qui g `sdzind' = 1 if `absdz' == r(min)
qui sum `sdz' if `sdzind' == 1
if r(mean) < 0 {
qui replace `sdz' = `sdz' - r(mean)
}
else {
qui replace `sdz' = `sdz' - r(mean)
}
qui g `sd_treat' = .
qui replace `sd_treat' = 1 if `sd_newav'<0
qui replace `sd_treat' = 0 if `sd_newav'>=0

* For R_NEWAV 

qui sum `r_newav'
local min = r(min) - (.03*(r(max)-r(min)))
local max = r(max) + (.03*(r(max)-r(min)))
qui gen `r_temp1' = _n-1 in 1/`J'
qui gen `rz' = `min' + `r_temp1'/`J'*(`max' - `min') 

qui g `abrz' = abs(`rz')
qui sum `abrz'
qui g `rzind' = 1 if `abrz' == r(min)
qui sum `rz' if `rzind' == 1
if r(mean) < 0 {
qui replace `rz' = `rz' - r(mean)
}
else {
qui replace `rz' = `rz' - r(mean)
}
qui g `r_treat' = .
qui replace `r_treat' = 1 if `r_newav'<0
qui replace `r_treat' = 0 if `r_newav'>=0

** NON-PARAMETRIC ESTIMATES ********************************************

** CENTERED VALUES *****************************************************

bwopt `1' `c_newav' `c_treat'
local bw_centered = r(hopt)
return scalar bw_centered = r(hopt)

qui lpoly `1' `c_newav' if `c_newav'<0, gen(`f0') bwidth(`bw_centered') at(`z') `opt'
qui replace `f0'=. if `z'>0
lpoly `1' `c_newav' if `c_newav'>=0, gen(`f1') at(`z') bwidth(`bw_centered') `opt'
qui replace `f1' = . if `z'<0

qui sum `f0' if `z' == 0 
scalar mf0 = r(mean)
qui sum `f1' if `z' == 0 
scalar mf1 = r(mean)
local te = mf0 - mf1
return scalar te = `te'


** STANDARDIZED AV VALUES *****************************************************

bwopt `1' `sd_newav' `sd_treat'
local bw_sd = r(hopt)
return scalar bw_sd = r(hopt)

qui lpoly `1' `sd_newav' if `sd_newav'<0, gen(`sdf0') at(`sdz') bwidth(`bw_sd') `opt'
qui replace `sdf0' = . if `sdz'>0
qui lpoly `1' `sd_newav' if `sd_newav'>=0, gen(`sdf1') at(`sdz') bwidth(`bw_sd') `opt'
qui replace `sdf1' = . if `sdz'<0

qui sum `sdf0' if `sdz' == 0 
scalar sdmf0 = r(mean)
qui sum `sdf1' if `sdz' == 0 
scalar sdmf1 = r(mean)
local sdte = sdmf0 - sdmf1
return scalar sdte = `sdte'

** RANK AV VALUES *****************************************************

qui lpoly `1' `r_newav' if `r_newav'<0, gen(`rf0') at(`rz') `opt'
qui replace `rf0' = . if `rz'>0
local bw_r = r(bwidth)					
return scalar bw_r = `bw_r'
qui lpoly `1' `r_newav' if `r_newav'>=0, gen(`rf1') at(`rz') bwidth(`bw_r') `opt'
qui replace `rf1' = . if `rz'<0

qui sum `rf0' if `rz' == 0 
scalar rmf0 = r(mean)
qui sum `rf1' if `rz' == 0 
scalar rmf1 = r(mean)
local rte = rmf0 - rmf1
return scalar rte = `rte'

** DISPLAY ************************************************************

di _newline as txt as yellow "***EFFECT ESTIMATES FOR CENTERING***" 
di _newline as txt "Treatment effect for " as red "Centering " as txt "(" round(`bw_centered', .01) as txt "): " as red round(`te', .01)  
di as txt "Treatment effect for " as red "Standardized Centering " as txt "(" round(`bw_sd', .01) as txt "): " as red round(`sdte', .01)
di as txt "Treatment effect for " as red "Rank Order Centering " as txt "(" round(`bw_r', .01) as txt "): " as red round(`rte', .01)

***************************************************************************

end
