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

* Choose scale and metric of assignment variables *
local sameav = "rc(40) mc(60) co(.2) rmean(45) mmath(55) rsd(10) msd(10)" // same scale and metric for both AVs //
local difdis = "rc(40) mc(60) co(.2) rmean(45) mmath(55) rsd(5) msd(20)" // different sds for both AVs //
*local   = "rc(40) mc(.60) co(.2) rmean(45) mmath(.55) rsd(10) msd(.10)" // different scale and metric for both AVs // 

* Choose response surface specification *
local model1 = "tr(4)" // constant treatment effects (model 1) //
local model2 = "tr(20) mt(-2) rt(2)" // heterogeneous treatment effects (model 2) //
local model3 = "tr(4) mt(-2) rt(2) mrav(-.25) rmav(.05) rtint(.025) mtint(.005)" // heterogeneous treatment effects, discontinuous potential outcomes (model 3) //

* Create simulated data *
simdata, `sameav' `model1'  // Constant effects / same scale & metric //
saveold Curtis_seed100_m1.dta, v(12) replace
li  m_posty readav mathav treatr treatm treatb in 1/5, clean
simdata, `sameav' `model2' // Heterogeneous effects / same scale & metric //
saveold Curtis_seed100_m2.dta, v(12) replace
li  m_posty readav mathav treatr treatm treatb in 1/5, clean
simdata, `sameav' `model3' //Heterogeneous effects with discontinuities / same scale & metric //
saveold Curtis_seed100_m3.dta, v(12) replace
li  m_posty readav mathav treatr treatm treatb in 1/5, clean
*** STEP 3: Estimate treatment effects using four approaches ***

* First, estimate true treatment effects *
trueeffects // Constant treatment effects // 
trueeffects, mt(-2) rt(2) // Heterogeneous treatment effects (model 2) //
trueeffects, mt(-2) rt(2) tmrav(-.25) trmav(-.05) // Hetero effects with discontinuity pot outcomes (model 3) //

* Second, estimate treatment effects using four approaches (produces results for one replication) *
s_mv1 m_posty  //frontier approach 1: estimates treatment effects using estimated conditional and marginal densities for each frontier separately//
centering m_posty //centering approach//
singleav m_posty cmav treatm mathsample //univariate approach for math frontier//
singleav m_posty crav treatr readsample //univariate approach for reading frontier//
ivrd m_posty cmav treat //IV approach for math frontier//
ivrd m_posty crav treat //IV approach for reading frontier//

