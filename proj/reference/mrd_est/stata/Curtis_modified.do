set seed 100

************************
** NOTES FOR CURTIS ****
************************

*** STEP 1: Run program files ***

* Set file directory *
cd "~/rdd/reference/mrd_est/stata" // set up your own file directory with program files //

do "simdata.do" // creates simulated data file //
do "trueeffects2.do" // produces "true" treatment effects // 
do "s_mv12.do" // frontier approach using marginal and conditional densities //
do "centering3.do" // centering approach //
do "singleav.do" // univariate approach //
do "bwopt.do" // bandwidth selection based on Imbens & Kalyanaraman (2009) //
do "IVRD.do" //IV approach using wald estimator //

*** STEP 2: Specification of simulated response surface & two assignment variables ***

* 
matrix rets = J(1,13,.) 
matrix coln rets = corr true true_std frnt cntr cntr_std cntr_rank true1 true2 frnt1 frnt2 univ1 univ2

forvalues corr = -.9(.1).9 {
	matrix ret = J(1,13,.) 
	
	simdata, ///
		obs(5000) rc(40) mc(60) ///
		co(`corr') rmean(45) mmath(55) rsd(10) msd(10) ///
		tr(4) rt(2) mt(-2) 

	
	trueeffects, mt(-2) rt(2) 	
	matrix ret[1,1] = (`corr', r(un_ate), r(s_ate), ., ., ., ., r(readev), r(mathev))
	
	s_mv1 m_posty
	matrix ret[1,4] = r(ate)
	matrix ret[1,10] = ( r(readev),  r(mathev) ) 
	
	centering m_posty 
	matrix ret[1,5] = ( r(te), r(sdte) , r(rte) )
	
	singleav m_posty cmav treatm mathsample 
	matrix ret[1,13] = r(te)
	
	singleav m_posty crav treatr readsample
	matrix ret[1,12] = r(te)
	
	
	matrix rets = rets \ ret
	
}

clear
svmat rets, names(matcol)
rename rets* *
drop in 1
// gen i = _n
// reshape long true true_s frnt  cntr cntr_s cntr_r univ, i(i)

twoway connected true true_std frnt cntr cntr_std  corr, ///
	mc(red blue green navy dknavy ) lc(red blue green navy dknavy ) ///
	legend(ring(1) position(3) cols(1)) name(ate, replace)
	
twoway connected true1 frnt1 univ1 corr, ///
	 mc(blue green navy) lc(blue green navy) ///
	legend(ring(1) position(3) cols(1)) name(a1, replace)
	
twoway connected true2 frnt2 univ2 corr, ///
	 mc(blue green navy)lc(blue green navy) ///
	legend(ring(1) position(3) cols(1)) name(a2, replace)
	
graph combine ate a1 a2 , ///
cols(1) xcomm note("n = 1000, c1 = 40 (<), c2 = 60 (<), mu1 = 45, sigma1 = 10, mu2 = 55, sigma2 = 100" "b_tr = 4, b_tr1 = 2, b_tr2 = -2" ) ///
imargin(0 0 0 0) graphregion(margin(l=22 r=22)) name(combine1, replace)

graph combine combine1 combine2
