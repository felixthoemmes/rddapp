/**** Regression Discontinuity Estimator with Optimal Bandwidth ****/



version 10.0



capture program drop rdob

program define rdob

syntax anything [if] [in] [,uniform detail c(real 0) fuzzy(varname)]

tokenize "`anything'"

local y `1'

local x `2'



/*Following are the temporary variables this program will use throughout*/ 



tempvar Y1 Y2 temp1 temp2 temp3 temp4 cons T1 T2 temp5 ind temp6 lambda xindl xindr xcl xcr xc

tempvar el er elx erx e







/*** Calculation of the Optimal Bandwidth ***/



/**Step 1: Estimation of density f(c) and conditional variance sigma-squared(c)**/



/*Calculating the pilot bandwidth h1*/



quietly summarize `x', detail

local Sx = r(sd)

local N = r(N)

local h1 = 1.84*`Sx'*(`N'^(-1/5))



/*Calculating the number of units on either side of the threshold

and the average outcomes*/



quietly count if (`c'-`h1')<=`x' & `x'<`c'

local Nh1n = r(N)

quietly count if `c'<=`x' & `x'<=(`c'+`h1')

local Nh1p = r(N)



quietly ameans `y' if (`c'-`h1')<=`x' & `x'<`c'

local Yh1n = r(mean)

quietly ameans `y' if `c'<=`x' & `x'<=(`c'+`h1')

local Yh1p = r(mean)



/*Estimating the density of X at c and the conditional variance of Y

given X_i=c at x=c */



local fxc = (`Nh1p'+`Nh1n')/(2*`N'*`h1')



generate `Y1' =(`y'-`Yh1n')^2 

quietly summarize `Y1' if (`c'-`h1')<=`x' & `x'<`c', detail

local Y1sum =r(sum)

generate `Y2' =(`y'-`Yh1p')^2 

quietly summarize `Y2' if `c'<=`x' & `x'<=(`c'+`h1'), detail

local Y2sum =r(sum)



local sigma2c = (1/(`Nh1p'+`Nh1n'))*(`Y1sum'+`Y2sum')





/**Step 2: Estimation of second derivatives**/



/*Calculating the pilot bandwidth h2*/



quietly summarize `x' if `x'>=`c', detail

scalar medXp = r(p50)

quietly summarize `x' if `x'<`c', detail

scalar medXn = r(p50)



generate `temp1'=(`x'>=`c')

generate `temp2'=`x'-`c'

generate `temp3'=`temp2'^2

generate `temp4'=`temp2'^3



quietly regress `y' `temp1' `temp2' `temp3' `temp4' if `x' >= medXn & `x' <= medXp



local m3c=6*_b[`temp4'] 



quietly count if `x'>=`c'

local Np=r(N)

quietly count if `x'<`c'

local Nn=r(N)



local h2p=3.56*((`sigma2c'/(`fxc'*max((`m3c')^2, 0.01)))^(1/7))*(`Np'^(-1/7))

local h2n=3.56*((`sigma2c'/(`fxc'*max((`m3c')^2, 0.01)))^(1/7))*(`Nn'^(-1/7))



/*Calculating the estimates of the second derivatives*/



quietly count if `c'<=`x' & `x'<=(`c'+`h2p')

local N2p=r(N)

quietly count if (`c'-`h2n')<=`x' & `x'<`c'

local N2n=r(N)



generate `cons'=1

generate `T1'=`x'-`c'

generate `T2'=`T1'^2



quietly regress `y' `cons' `T1' `T2' if `c'<=`x' & `x'<=(`c'+`h2p')

local m2pc=2*_b[`T2']

quietly regress `y' `cons' `T1' `T2' if (`c'-`h2n')<=`x' & `x'<`c'

local m2nc=2*_b[`T2'] 





/**Step 3: Calculation of regularization terms and calculation of the optimal bandwidth**/



/*Calculating the regularization terms*/



local rp=(720*`sigma2c')/(`N2p'*((`h2p')^4))

local rn=(720*`sigma2c')/(`N2n'*((`h2n')^4))



/*Calculating the optimal bandwidth*/



if "`uniform'"~="" {

local CK = 5.4

}

else {

local CK = 3.4375

}



local h_opt= `CK'*(((2*`sigma2c')/(`fxc'*(((`m2pc'-`m2nc')^2)+(`rp'+`rn'))))^(1/5))*`N'^(-1/5)











/*** Calculation of the Point Estimate and Standard Error for the RD ***/



/**Calculation of the point estimate**/



/*Calculating lambda*/



generate `temp5'= (`x'-`c')/`h_opt'

generate `ind'= abs(`temp5')<=1

generate `temp6'= 1-(abs(`temp5'))



if "`uniform'"~="" {

	generate `lambda'= `temp5'>(-0.5) & `temp5'<=0.5

	}

else {

	generate `lambda'= `temp6'*`ind'

	}



/*Estimating the alpha and beta*/



generate `xindl'=`x'<`c'

generate `xindr'=`x'>=`c'

generate `xcl'=(`c'-`x')*`xindl'

generate `xcr'=(`x'-`c')*`xindr' 

generate `xc'=`xcl'+`xcr'

local i=1

local nlistl beta_l

local nlistr beta_r



while "`3'" ~="" {

	quietly summarize `3' [iw=`lambda']

	local wz`i' = r(mean)

	tempvar z`i'

	generate `z`i'' = `3'-`wz`i''

	local nlistl `nlistl' gamma`i'_l

	local nlistr `nlistr' gamma`i'_r

	local i = `i'+1

	mac shift

	}



if `i'>2 {

	local u=`i'-1

	scalar unumco = `u'

	quietly regress `y' `xc' `z1'-`z`u'' [iw=`lambda'] if `x'<`c'

	local al=_b[_cons]

	local bl=_b[`xc']

	matrix Estimated_coefficients_Y_left=e(b)

	matrix rownames Estimated_coefficients_Y_left= "`y'"

	local nll `nlistl' alpha_l

	local nlr `nlistr' alpha_r

	matrix colnames Estimated_coefficients_Y_left= `nll'

	predict `el', r

	quietly regress `y' `xc' `z1'-`z`u'' [iw=`lambda'] if `x'>=`c'

	local ar=_b[_cons]

	local br=_b[`xc']

	matrix Estimated_coefficients_Y_right=e(b)

	matrix rownames Estimated_coefficients_Y_right= "`y'"

	matrix colnames Estimated_coefficients_Y_right= `nlr'

	predict `er', r

	if "`fuzzy'"~="" {

		tempvar ewl ewr

		quietly regress `fuzzy' `xc' `z1'-`z`u'' [iw=`lambda'] if `x'<`c'

		local awl=_b[_cons]

		local bwl=_b[`xc']

		matrix Estimated_coefficients_W_left=e(b)

		matrix rownames Estimated_coefficients_W_left= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_left= `nll'

		predict `ewl', r

		quietly regress `fuzzy' `xc' `z1'-`z`u'' [iw=`lambda'] if `x'>=`c'

		local awr=_b[_cons]

		local bwr=_b[`xc']

		matrix Estimated_coefficients_W_right=e(b)

		matrix rownames Estimated_coefficients_W_right= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_right= `nlr'

		predict `ewr', r

		}

	}

else if `i'==2 {

	quietly regress `y' `xc' `z1' [iw=`lambda'] if `x'<`c'

	local al=_b[_cons]

	local bl=_b[`xc']

	matrix Estimated_coefficients_Y_left=e(b)

	matrix rownames Estimated_coefficients_Y_left= "`y'"

	matrix colnames Estimated_coefficients_Y_left= beta_l gamma_l alpha_l

	predict `el', r

	quietly regress `y' `xc' `z1' [iw=`lambda'] if `x'>=`c'

	local ar=_b[_cons]

	local br=_b[`xc']

	matrix Estimated_coefficients_Y_right=e(b)

	matrix rownames Estimated_coefficients_Y_right= "`y'"

	matrix colnames Estimated_coefficients_Y_right= beta_r gamma_r alpha_r

	predict `er', r

	if "`fuzzy'"~="" {

		tempvar ewl ewr

		quietly regress `fuzzy' `xc' `z1' [iw=`lambda'] if `x'<`c'

		local awl=_b[_cons]

		local bwl=_b[`xc']

		matrix Estimated_coefficients_W_left=e(b)

		matrix rownames Estimated_coefficients_W_left= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_left= beta_l gamma_l alpha_l

		predict `ewl', r

		quietly regress `fuzzy' `xc' `z1' [iw=`lambda'] if `x'>=`c'

		local awr=_b[_cons]

		local bwr=_b[`xc']

		matrix Estimated_coefficients_W_right=e(b)

		matrix rownames Estimated_coefficients_W_right= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_right= beta_r gamma_r alpha_r

		predict `ewr', r

		}

	}

else {

	quietly regress `y' `xc' [iw=`lambda'] if `x'<`c'

	local al=_b[_cons]

	local bl=_b[`xc']

	matrix Estimated_coefficients_Y_left=e(b)

	matrix rownames Estimated_coefficients_Y_left= "`y'"

	matrix colnames Estimated_coefficients_Y_left= beta_l alpha_l

	predict `el', r

	quietly regress `y' `xc' [iw=`lambda'] if `x'>=`c'

	local ar=_b[_cons]

	local br=_b[`xc']

	matrix Estimated_coefficients_Y_right=e(b)

	matrix rownames Estimated_coefficients_Y_right= "`y'"

	matrix colnames Estimated_coefficients_Y_right= beta_r alpha_r

	predict `er', r

	if "`fuzzy'"~="" {

		tempvar ewl ewr

		quietly regress `fuzzy' `xc' [iw=`lambda'] if `x'<`c'

		local awl=_b[_cons]

		local bwl=_b[`xc']

		matrix Estimated_coefficients_W_left=e(b)

		matrix rownames Estimated_coefficients_W_left= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_left= beta_l alpha_l

		predict `ewl', r

		quietly regress `fuzzy' `xc' [iw=`lambda'] if `x'>=`c'

		local awr=_b[_cons]

		local bwr=_b[`xc']

		matrix Estimated_coefficients_W_right=e(b)

		matrix rownames Estimated_coefficients_W_right= "`fuzzy'"

		matrix colnames Estimated_coefficients_W_right= beta_r alpha_r

		predict `ewr', r

		}

	}



/* Calculating the standard errors */



generate `elx'=`el'*`xindl'

generate `erx'=`er'*`xindr'

generate `e'=`elx'+`erx'

if "`fuzzy'"~="" {

	tempvar ewlx ewrx ew

	generate `ewlx'=`ewl'*`xindl'

	generate `ewrx'=`ewr'*`xindr'

	generate `ew'=`ewlx'+`ewrx'

	}



if `i'>2 {

	if "`fuzzy'"~="" {

		local list3 `xc' `lambda' `e' `ew' `xindl' `xindr' `z1'-`z`u''

		mata: sef3("`list3'", "unumco")

		local t=2

		while `t'<=`N' {

			local s = `t'-1

			matrix DYL`t'=DYL`t'+DYL`s'

			matrix DYR`t'=DYR`t'+DYR`s'

			matrix DWL`t'=DWL`t'+DWL`s'

			matrix DWR`t'=DWR`t'+DWR`s'

			matrix DYWL`t'=DYWL`t'+DYWL`s'

			matrix DYWR`t'=DYWR`t'+DYWR`s'

			matrix GL`t'=GL`t'+GL`s'

			matrix GR`t'=GR`t'+GR`s'

			local t = `t'+1

			}	

		matrix DeltaYL=DYL`N'

		matrix DeltaYR=DYR`N'

		matrix DeltaWL=DWL`N'

		matrix DeltaWR=DWR`N'

		matrix DeltaYWL=DYWL`N'

		matrix DeltaYWR=DYWR`N'

		local dim = 2+`u'

		matrix vm=J(`dim', `dim', 0)	

		matrix Delta=(DeltaYL, DeltaYWL, vm, vm\DeltaYWL, DeltaWL, vm, vm\vm, vm, DeltaYR, DeltaYWR\vm, vm, DeltaYWR, DeltaWR)

		matrix GammaL=GL`N'

		matrix GammaR=GR`N'

		matrix Gamma=(GammaL, vm, vm, vm\vm, GammaL, vm, vm\vm, vm, GammaR, vm\vm, vm, vm, GammaR)

		

		matrix V=invsym(Gamma)*Delta*invsym(Gamma)

		matrix g=J((`dim'*4),1,0)

		matrix g[1,1]= -1/(`awr'-`awl')

		matrix g[(`u'+3),1]= (`ar'-`al')/((`awr'-`awl')^2)

		matrix g[((2*`u')+5),1]= 1/(`awr'-`awl')

		matrix g[((3*`u')+7),1]= -(`ar'-`al')/((`awr'-`awl')^2)

		matrix Vt=g'*V*g

		local rdvarf = Vt[1,1]

		local rdsef = sqrt(`rdvarf')

		}

	else {

		local list3 `xc' `lambda' `e' `xindl' `xindr' `z1'-`z`u''

		mata: se3("`list3'", "unumco")

		local t=2

		while `t'<=`N' {

			local s = `t'-1

			matrix DL`t'=DL`t'+DL`s'

			matrix DR`t'=DR`t'+DR`s'

			matrix GL`t'=GL`t'+GL`s'

			matrix GR`t'=GR`t'+GR`s'

			local t = `t'+1

			}	

		matrix DeltaL=DL`N'

		matrix DeltaR=DR`N'

		matrix GammaL=GL`N'

		matrix GammaR=GR`N'

		matrix VL=invsym(GammaL)*DeltaL*invsym(GammaL)

		matrix VR=invsym(GammaR)*DeltaR*invsym(GammaR)

		scalar val=VL[1,1]

		scalar var=VR[1,1]

		}

	}

else if `i'==2 {

	if "`fuzzy'"~="" {

		local list2 `xc' `lambda' `e' `ew' `xindl' `xindr' `z1'

		mata: sef2("`list2'")

		matrix g=J(12,1,0)

		matrix g[1,1]= -1/(`awr'-`awl')

		matrix g[4,1]= (`ar'-`al')/((`awr'-`awl')^2)

		matrix g[7,1]= 1/(`awr'-`awl')

		matrix g[10,1]= -(`ar'-`al')/((`awr'-`awl')^2)

		matrix Vt=g'*V*g

		local rdvarf = Vt[1,1]

		local rdsef = sqrt(`rdvarf')

		}

	else {

		local list2 `xc' `lambda' `e' `xindl' `xindr' `z1'

		mata: se2("`list2'")

		}

	}

else {

	if "`fuzzy'"~="" {

		local list1 `xc' `lambda' `e' `ew' `xindl' `xindr'

		mata: sef1("`list1'")

		matrix g=J(8,1,0)

		matrix g[1,1]= -1/(`awr'-`awl')

		matrix g[3,1]= (`ar'-`al')/((`awr'-`awl')^2)

		matrix g[5,1]= 1/(`awr'-`awl')

		matrix g[7,1]= -(`ar'-`al')/((`awr'-`awl')^2)

		matrix Vt=g'*V*g

		local rdvarf = Vt[1,1]

		local rdsef = sqrt(`rdvarf')

		}

	else {

		local list1 `xc' `lambda' `e' `xindl' `xindr'

		mata: se1("`list1'")

		}

	}





/*Calculating the point estimate and the standard error*/



if "`fuzzy'"~="" {

	local t_rd=(`ar'-`al')/(`awr'-`awl')

	}

else {

	local t_rd=`ar'-`al'

	}



if "`fuzzy'"~="" {

	local rdse=`rdsef'

	}

else {

	local rdse=sqrt(val+var)

	}







/*** Displaying the Final Results ***/



display in green "optimal bandwidth(h_opt) = " in yellow `h_opt'

display in green "RD point estimate = " in yellow `t_rd'

display in green "RD standard error = " in yellow `rdse'



if "`detail'"~="" {

display ""

display in blue "<Intermediate results for bandwidth calculation>"

display in green "h1 = " in yellow `h1'

display in green "N_h1- = " in yellow `Nh1n' _col(25) in green "N_h1+ = " in yellow `Nh1p'

display in green "Y_h1- = " in yellow `Yh1n' _col(25) in green "Y_h1+ = " in yellow `Yh1p'

display in green "N- = " in yellow `Nn' _col(25) in green "N+ = " in yellow `Np'

display in green "fx(c) = " in yellow `fxc' 

display in green "sigma^2(c) = " in yellow `sigma2c'

display in green "MedianX- = " in yellow medXn _col(25) in green "MedianX+ = " in yellow medXp

display in green "m^3(c) = " in yellow `m3c'

display in green "h2- = " in yellow `h2n' _col(25) in green "h2+ = " in yellow `h2p'

display in green "N2- = " in yellow `N2n' _col(25) in green "N2+ = " in yellow `N2p'

display in green "m^2-(c) = " in yellow `m2nc' _col(25) in green "m^2+(c) = " in yellow `m2pc'

display in green "r- = " in yellow `rn' _col(25) in green "r+ = " in yellow `rp'

display in green "C_K = " in yellow `CK'

display ""

display in blue "<Intermediate results for RD estimation>"

if "`fuzzy'"~="" {

	matrix list Estimated_coefficients_Y_left

	matrix list Estimated_coefficients_Y_right

	matrix list Estimated_coefficients_W_left

	matrix list Estimated_coefficients_W_right

	matrix list DeltaYL

	matrix list DeltaYR

	matrix list DeltaWL

	matrix list DeltaWR

	matrix list DeltaYWL

	matrix list DeltaYWR

	matrix list GammaL

	matrix list GammaR

	}

else {

	matrix list Estimated_coefficients_Y_left

	matrix list Estimated_coefficients_Y_right

	matrix list VL

	matrix list VR

	matrix list DeltaL

	matrix list DeltaR

	matrix list GammaL

	matrix list GammaR

	}

}



end







version 10.0

mata:

function se1(string matrix vlist)

{

	allv = st_data(1,.)

	nc = cols(allv)

	nv = nc-23

	xc = st_data(., (nv+18)); l = st_data(., (nv+13)); e = st_data(., nc)

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15))

	e2=e:^2; l2=l:^2; xc2=xc:^2

	el2=e2:*l2; el2l=el2:*xl; el2r=el2:*xr

	xcel2=xc:*e2:*l2; xcel2l=xcel2:*xl; xcel2r=xcel2:*xr

	xc2el2= xc2:*e2:*l2; xc2el2l=xc2el2:*xl; xc2el2r=xc2el2:*xr

	deltal=(colsum(el2l), colsum(xcel2l)\colsum(xcel2l), colsum(xc2el2l))

	deltar=(colsum(el2r), colsum(xcel2r)\colsum(xcel2r), colsum(xc2el2r))



	lxl = l:*xl; lxr = l:*xr

	lxcl = l:*xc:*xl ; lxcr = l:*xc:*xr 

	lxc2l = l:*(xc:^2):*xl; lxc2r = l:*(xc:^2):*xr

	gammal=(colsum(lxl), colsum(lxcl)\colsum(lxcl), colsum(lxc2l))

	gammar=(colsum(lxr), colsum(lxcr)\colsum(lxcr), colsum(lxc2r))

	

	igammal=invsym(gammal); igammar=invsym(gammar)

	V1l=igammal*deltal*igammal; V1r=igammar*deltar*igammar; 



	st_numscalar("val", V1l[1,1])

	st_numscalar("var", V1r[1,1])

	st_matrix("VL", V1l)

	st_matrix("VR", V1r)

	st_matrix("DeltaL", deltal)

	st_matrix("DeltaR", deltar)

	st_matrix("GammaL", gammal)

	st_matrix("GammaR", gammar)

}



function sef1(string matrix vlist)

{

	allv = st_data(1,.)

	nc = cols(allv)

	nv = nc-28

	xc = st_data(., (nv+18)); l = st_data(., (nv+13)) 

	e = st_data(., (nv+25)); ew = st_data(., (nv+28))

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15))



	e2=e:^2; l2=l:^2; xc2=xc:^2

	el2=e2:*l2; el2l=el2:*xl; el2r=el2:*xr

	xcel2=xc:*e2:*l2; xcel2l=xcel2:*xl; xcel2r=xcel2:*xr

	xc2el2= xc2:*e2:*l2; xc2el2l=xc2el2:*xl; xc2el2r=xc2el2:*xr

	deltayl=(colsum(el2l), colsum(xcel2l)\colsum(xcel2l), colsum(xc2el2l))

	deltayr=(colsum(el2r), colsum(xcel2r)\colsum(xcel2r), colsum(xc2el2r))



	ew2=ew:^2

	ewl2=ew2:*l2; ewl2l=ewl2:*xl; ewl2r=ewl2:*xr

	xcewl2=xc:*ew2:*l2; xcewl2l=xcewl2:*xl; xcewl2r=xcewl2:*xr

	xc2ewl2= xc2:*ew2:*l2; xc2ewl2l=xc2ewl2:*xl; xc2ewl2r=xc2ewl2:*xr

	deltawl=(colsum(ewl2l), colsum(xcewl2l)\colsum(xcewl2l), colsum(xc2ewl2l))

	deltawr=(colsum(ewl2r), colsum(xcewl2r)\colsum(xcewl2r), colsum(xc2ewl2r))



	eyw=e:*ew

	eywl2=eyw:*l2; eywl2l=eywl2:*xl; eywl2r=eywl2:*xr

	xceywl2=xc:*eyw:*l2; xceywl2l=xceywl2:*xl; xceywl2r=xceywl2:*xr

	xc2eywl2= xc2:*eyw:*l2; xc2eywl2l=xc2eywl2:*xl; xc2eywl2r=xc2eywl2:*xr

	deltaywl=(colsum(eywl2l), colsum(xceywl2l)\colsum(xceywl2l), colsum(xc2eywl2l))

	deltaywr=(colsum(eywl2r), colsum(xceywl2r)\colsum(xceywl2r), colsum(xc2eywl2r))



	lxl = l:*xl; lxr = l:*xr

	lxcl = l:*xc:*xl ; lxcr = l:*xc:*xr 

	lxc2l = l:*(xc:^2):*xl; lxc2r = l:*(xc:^2):*xr

	gammal=(colsum(lxl), colsum(lxcl)\colsum(lxcl), colsum(lxc2l))

	gammar=(colsum(lxr), colsum(lxcr)\colsum(lxcr), colsum(lxc2r))

	

	m=(0,0\0,0)

	r1=(deltayl, deltaywl, m, m); r2=(deltaywl, deltawl, m, m)

	r3=(m, m, deltayr, deltaywr); r4=(m, m, deltaywr, deltawr)

	delta=(r1\r2\r3\r4)



	s1=(gammal, m, m, m); s2=(m, gammal, m, m)

	s3=(m, m, gammar, m); s4=(m, m, m, gammar)

	gamma=(s1\s2\s3\s4)



	V=invsym(gamma)*delta*invsym(gamma)



	st_matrix("V", V)

	st_matrix("DeltaYL", deltayl)

	st_matrix("DeltaYR", deltayr)

	st_matrix("DeltaWL", deltawl)

	st_matrix("DeltaWR", deltawr)

	st_matrix("DeltaYWL", deltaywl)

	st_matrix("DeltaYWR", deltaywr)

	st_matrix("GammaL", gammal)

	st_matrix("GammaR", gammar)

}



function se2(string matrix vlist)

{

	allv = st_data(1,.)

	nc = cols(allv)

	nv = nc-24

	xc = st_data(., (nv+18)); l = st_data(., (nv+13)); e = st_data(., nc)

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15)); z1 = st_data(., (nv+19))

	

	xc2 = xc:^2; xcz=xc:*z1; zz=z1:*z1

	le2=(l:^2):*(e:^2)

	le2l = le2:*xl; le2r = le2:*xr

	ll = l:*xl; lr = l:*xr

	n=length(xc)

	A=J(n, 6, .)

		for (j=1; j<=n; j++) {

		A[j,1]=1; A[j,2]=xc[j]; A[j,3]=z1[j]

		A[j,4]=xc2[j]; A[j,5]=xcz[j]; A[j,6]=zz[j]

		}

	dl=le2l'*A; dr=le2r'*A

	gl=ll'*A; gr=lr'*A

	deltal=(dl[1], dl[2], dl[3]\dl[2], dl[4], dl[5]\dl[3], dl[5], dl[6])

	deltar=(dr[1], dr[2], dr[3]\dr[2], dr[4], dr[5]\dr[3], dr[5], dr[6])

	gammal=(gl[1], gl[2], gl[3]\gl[2], gl[4], gl[5]\gl[3], gl[5], gl[6])

	gammar=(gr[1], gr[2], gr[3]\gr[2], gr[4], gr[5]\gr[3], gr[5], gr[6])

	Vl=invsym(gammal)*deltal*invsym(gammal)

	Vr=invsym(gammar)*deltar*invsym(gammar)

	st_numscalar("val", Vl[1,1])

	st_numscalar("var", Vr[1,1])

	st_matrix("VL", Vl)

	st_matrix("VR", Vr)

	st_matrix("DeltaL", deltal)

	st_matrix("DeltaR", deltar)

	st_matrix("GammaL", gammal)

	st_matrix("GammaR", gammar)

}



function sef2(string matrix vlist)

{

	allv = st_data(1,.)

	nc = cols(allv)

	nv = nc-29

	xc = st_data(., (nv+18)); l = st_data(., (nv+13))

	e = st_data(., (nv+26)); ew = st_data(., (nv+29))

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15)); z1 = st_data(., (nv+19))

	

	xc2 = xc:^2; xcz=xc:*z1; zz=z1:*z1

	le2=(l:^2):*(e:^2)

	le2l = le2:*xl; le2r = le2:*xr

	ll = l:*xl; lr = l:*xr

	n=length(xc)

	A=J(n, 6, .)

		for (j=1; j<=n; j++) {

		A[j,1]=1; A[j,2]=xc[j]; A[j,3]=z1[j]

		A[j,4]=xc2[j]; A[j,5]=xcz[j]; A[j,6]=zz[j]

		}

	dyl=le2l'*A; dyr=le2r'*A

	gl=ll'*A; gr=lr'*A

	deltayl=(dyl[1], dyl[2], dyl[3]\dyl[2], dyl[4], dyl[5]\dyl[3], dyl[5], dyl[6])

	deltayr=(dyr[1], dyr[2], dyr[3]\dyr[2], dyr[4], dyr[5]\dyr[3], dyr[5], dyr[6])

	

	lew2=(l:^2):*(ew:^2)

	lew2l = lew2:*xl; lew2r = lew2:*xr

	dwl=lew2l'*A; dwr=lew2r'*A

	deltawl=(dwl[1], dwl[2], dwl[3]\dwl[2], dwl[4], dwl[5]\dwl[3], dwl[5], dwl[6])

	deltawr=(dwr[1], dwr[2], dwr[3]\dwr[2], dwr[4], dwr[5]\dwr[3], dwr[5], dwr[6])



	leyw=(l:^2):*e:*ew

	leywl = leyw:*xl; leywr = leyw:*xr

	dywl=leywl'*A; dywr=leywr'*A

	deltaywl=(dywl[1], dywl[2], dywl[3]\dywl[2], dywl[4], dywl[5]\dywl[3], dywl[5], dywl[6])

	deltaywr=(dywr[1], dywr[2], dywr[3]\dywr[2], dywr[4], dywr[5]\dywr[3], dywr[5], dywr[6])



	gammal=(gl[1], gl[2], gl[3]\gl[2], gl[4], gl[5]\gl[3], gl[5], gl[6])

	gammar=(gr[1], gr[2], gr[3]\gr[2], gr[4], gr[5]\gr[3], gr[5], gr[6])

	

	m=J(3,3,0)

	r1=(deltayl, deltaywl, m, m); r2=(deltaywl, deltawl, m, m)

	r3=(m, m, deltayr, deltaywr); r4=(m, m, deltaywr, deltawr)

	delta=(r1\r2\r3\r4)



	s1=(gammal, m, m, m); s2=(m, gammal, m, m)

	s3=(m, m, gammar, m); s4=(m, m, m, gammar)

	gamma=(s1\s2\s3\s4)



	V=invsym(gamma)*delta*invsym(gamma)



	st_matrix("V", V)

	st_matrix("DeltaYL", deltayl)

	st_matrix("DeltaYR", deltayr)

	st_matrix("DeltaWL", deltawl)

	st_matrix("DeltaWR", deltawr)

	st_matrix("DeltaYWL", deltaywl)

	st_matrix("DeltaYWR", deltaywr)

	st_matrix("GammaL", gammal)

	st_matrix("GammaR", gammar)

}



function se3(string matrix vlist, counter)

{

	allv = st_data(1,.)

	nc = cols(allv); u = st_numscalar(counter)

	nv = nc-23-u; q = nv+19; p = q+u-1 

	xc = st_data(., (nv+18)); l = st_data(., (nv+13)); e = st_data(., nc)

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15)); z = st_data(., (q..p))



	xc2=xc:^2; l2=l:^2; e2=e:^2

	n=length(xc)

		for (j=1; j<=n; j++) {

		oo=(1, xc[j]\xc[j], xc2[j])

		pp=(z[j,.]\xc[j]*z[j,.])

		qq=pp'

		rr=((z[j,.])')*z[j,.]

		op=(oo, pp); qr=(qq, rr)

		opqr=(op\qr)

		dl=l2[j]*e2[j]*xl[j]*opqr; dr=l2[j]*e2[j]*xr[j]*opqr

		gl=l[j]*xl[j]*opqr; gr=l[j]*xr[j]*opqr

		st_matrix("DL"+strofreal(j), dl)

		st_matrix("DR"+strofreal(j), dr)

		st_matrix("GL"+strofreal(j), gl)

		st_matrix("GR"+strofreal(j), gr)

		}

}



function sef3(string matrix vlist, counter)

{

	allv = st_data(1,.)

	nc = cols(allv); u = st_numscalar(counter)

	nv = nc-28-u; q = nv+19; p = q+u-1 

	xc = st_data(., (nv+18)); l = st_data(., (nv+13))

	e = st_data(., (p+7)); ew = st_data(., nc)

	xl = st_data(., (nv+14)); xr = st_data(., (nv+15)); z = st_data(., (q..p))



	xc2=xc:^2; l2=l:^2; e2=e:^2; ew2=ew:^2; eyw=e:*ew

	n=length(xc)

		for (j=1; j<=n; j++) {

		oo=(1, xc[j]\xc[j], xc2[j])

		pp=(z[j,.]\xc[j]*z[j,.])

		qq=pp'

		rr=((z[j,.])')*z[j,.]

		op=(oo, pp); qr=(qq, rr)

		opqr=(op\qr)

		

		dyl=l2[j]*e2[j]*xl[j]*opqr; dyr=l2[j]*e2[j]*xr[j]*opqr

		dwl=l2[j]*ew2[j]*xl[j]*opqr; dwr=l2[j]*ew2[j]*xr[j]*opqr

		dywl=l2[j]*eyw[j]*xl[j]*opqr; dywr=l2[j]*eyw[j]*xr[j]*opqr

		gl=l[j]*xl[j]*opqr; gr=l[j]*xr[j]*opqr



		st_matrix("DYL"+strofreal(j), dyl)

		st_matrix("DYR"+strofreal(j), dyr)

		st_matrix("DWL"+strofreal(j), dwl)

		st_matrix("DWR"+strofreal(j), dwr)

		st_matrix("DYWL"+strofreal(j), dywl)

		st_matrix("DYWR"+strofreal(j), dywr)

		st_matrix("GL"+strofreal(j), gl)

		st_matrix("GR"+strofreal(j), gr)

		}

}





end





/* end rdob.ado */