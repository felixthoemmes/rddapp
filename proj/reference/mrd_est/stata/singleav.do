cap program drop singleav
program singleav, rclass

********************************************************
** SINGLE AV *******************************************
********************************************************

syntax [varlist(min=4 max=4)] [, obs(real 100) *]
tokenize `varlist'

** NOTES *********************************************

* 1 = Dependent variable
* 2 = Centered assignment variable
* 3 = Treatment variable
* 4 = mathsample / readsample

** LOCAL MACROS ***************************************

local J = `obs'					//Number of observations//
local opt "tri nogr deg(1) `options'" 		//Options for lpoly command//

** TEMP VARIABLES **************************************

#delimit;
tempvar 
c_temp1 
z abz zind
f0 f1;
#delimit cr

** Creates Z variable ***********************************

qui sum `2'
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

** NON-PARAMETRIC ESTIMATES ********************************************

bwopt `1' `2' `3' if `4' == 1
local bw = r(hopt)
return scalar hopt = r(hopt)

* Optimal bandwidth
qui lpoly `1' `2' if `2'<0 & `4' == 1, gen(`f0') at(`z') bwidth(`bw') `opt'
qui replace `f0'=. if `z'>0
qui lpoly `1' `2' if `2'>=0 & `4' == 1, gen(`f1') at(`z') bwidth(`bw') `opt'
qui replace `f1' = . if `z'<0

qui sum `f0' if `z' == 0 
scalar mf0 = r(mean)
qui sum `f1' if `z' == 0 
scalar mf1 = r(mean)
local te = mf0 - mf1
return scalar te = `te'

**************************************************************************

di as txt "Single AV estimate:  " as red round(return(te), .001)

end
