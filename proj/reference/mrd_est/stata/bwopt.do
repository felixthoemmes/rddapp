cap program drop bwopt
program bwopt, rclass
set more off

//Based on method proposed by Imbens & Kalyanaraman (2009) //

****************************************************
** OPTIMAL BANDWIDTH *******************************
****************************************************

syntax varlist(min=3 max=3) [if] [in] [, cut(real 0)]
marksample touse
tokenize `varlist'

* var1 = outcome var
* var2 = assignment var
* var3 = treatment variable

tempvar temp1 temp2 temp3 cavsq cavcu

** STEP 1 *************************************************
** Estimating the density and conditional variance at cutoff **

* Variance and N of AV ("forcing variable")
qui su `2' if `touse'
scalar av_var = r(Var)
scalar av_sd = r(sd)
scalar N = r(N)

* Pilot bandwidth
scalar h1 = 1.84*av_sd*N^(-1/5)	//Modified version of Silverman rule of H, based on uniform kernel on [-1,1]//

* Number of units on each side of cutoff, and outcomes to estimate conditional variance Y given AV, at cutoff
qui su `1' if `2'>=-h1 & `2'<`cut' & `touse'
scalar Nl = r(N)
scalar varl = r(Var)

qui su `1' if `2'<=h1 & `2'>=`cut' & `touse'
scalar Nr = r(N)
scalar varr = r(Var)

* Estimation of Density
scalar f0 = (Nl + Nr)/(2*N*h1)
* Estimation of conditional variance 
scalar var0 = (((Nl - 1)*varl) + ((Nr - 1)*varr))/(Nl + Nr)

** STEP 2 ***************************************************
** Estimating the curvature at the cutoff **

* Estimation of third derivative at cutoff (m30)
qui su `2' if `2'<`cut' & `touse', d
scalar medl = r(p50)

qui su `2' if `2'>=`cut' & `touse', d
scalar medr = r(p50)

* Limits of the second derivative at the cutoff from right and left
qui g `temp1' = 1 if `2'>=medl & `2'<=medr & `touse'
qui g `cavsq' = `2'*`2'
qui g `cavcu' = `2'*`2'*`2'
qui reg `1' `3' `2' `cavsq' `cavcu' if `temp1' == 1 & `touse'
scalar m30 = 6*_b[`cavcu']

* H2 bandwidths
scalar h2l = 3.56*((var0)/(f0*max(m30^2,.01)))^(1/7)*Nl^(-1/7)
scalar h2r = 3.56*((var0)/(f0*max(m30^2,.01)))^(1/7)*Nr^(-1/7)

* Estimation of second derivatives (m2l0 & m2r0) at zero using pilot bandwidths
qui g `temp2' = 1 if `2'>=-h2l & `2'<`cut' & `touse'
qui su `temp2'
scalar N2l = r(N)
qui reg `1' `2' `cavsq' if `temp2' == 1 & `touse'
scalar m2l0 = 2*_b[`cavsq']

qui g `temp3' = 1 if `2'<=h2r & `2'>=`cut' & `touse'
qui su `temp3'
scalar N2r = r(N)
qui reg `1' `2' `cavsq' if `temp3' == 1 & `touse'
scalar m2r0 = 2*_b[`cavsq']

** STEP 3 ****************************************************
** Calculating regularization terms and optimal bandwidth **

* Estimation of regularization terms
scalar rl = (720*var0)/(N2l*h2l^4)
scalar rr = (720*var0)/(N2r*h2r^4)

* Optimal bandwidth calculation
return scalar hopt = 3.4375*((2*var0)/(f0*((m2r0 - m2l0)^2 + (rr + rl))))^(1/5)*N^(-1/5) 	//Ck = 3.4375 for edge(tri) kernel//

display as txt "Optimal Bandwidth:  " as red return(hopt)
end
