dataset = foreign::read.dta('~/rdd/reference/mrdd/stata/Curtis_seed100_m1.dta')
# dataset = foreign::read.dta('C:/Academia/Cornell/QML/RDD/RDD/reference/mrdd/stata/Curtis_seed100_m1.dta')

dat = dataset[c('readav', 'mathav', 'treat', 'm_posty')]

names(dat) = c('x1', 'x2', 'tr', 'y')
c1 = 40 
c2 = 60
# attach(dat)

mfrd_est_old <- function(dat, c1, c2) {
  # New Standardized cutoff values
  # stata: scalar zmathc = (mathc - mmean)/msd
  # stata: scalar zreadc = (readc - rmean)/rsd
  
  zc1 = c(scale(c1, center = mean(dat$x1), scale = sd(dat$x1)))
  zc2 = c(scale(c2, center = mean(dat$x2), scale = sd(dat$x2)))
  
  ### TRANSFORMAT VARIABLES ###
  
  dat = within(dat, 
    {
      # treatment for x1
      # stata: qui gen treatr=. 
      # stata: qui replace treatr=1 if mathav>=mathc & readav<readc
      # stata: qui replace treatr=0 if mathav>=mathc & readav>=readc | mathav<mathc & readav<readc | mathav<mathc & readav>=readc
      
      tr1 = 
        ifelse(x1 < c1 & x2 >= c2, 1,
          ifelse((x1 >= c1 & x2 >= c2) | 
              (x1 < c1 & x2 < c2) | 
              (x1 >= c1 & x2 < c2), 0, NA))
      
      # treatment for x2
      # stata: qui gen treatm=. 
      # stata: qui replace treatm=1 if mathav<mathc & readav>=readc
      # stata: qui replace treatm=0 if mathav<mathc & readav<readc | mathav>=mathc & readav<=readc | mathav>=mathc & readav>=readc
      
      tr2 =
        ifelse(x1 >= c1 & x2 < c2, 1,
          ifelse((x1 < c1 & x2 < c2) |
              (x1 <= c1 & x2 >= c2) |
              (x1 >= c1 & x2 >= c2), 0, NA))
  
      # treatment for both?
      # stata: code?
      
      trb = 
        ifelse(x1 < c1 & x2 < c2, 1,
          ifelse((x1 >= c1 & x2 >= c2) |
              (x1 < c1 & x2 >= c2) |
              (x1 >= c1 & x2 < c2), 0, NA))
      
      tr = ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
      
      # center/standardize assignments
      
      zx1 = c(scale(x1))
      zx2 = c(scale(x2))
      
      zcx1 = zx1 - zc1
      zcx2 = zx2 - zc2
      
      ### SAMPLE RESTRICTIONS ###
      # stata: egen zmathav = std(mathav)
      # stata: label var zmathav "Standardized math school av"
      # stata: egen zreadav = std(readav)
      # stata: label var zreadav "Standardized reading school av"
      
      # stata: qui sum mathav 
      # stata: scalar mmean = r(mean)
      # stata: scalar msd = r(sd)
      # stata: qui sum readav
      # stata: scalar rmean = r(mean)
      # stata: scalar rsd = r(sd)
      # stata: qui sum zreadav
      # stata: local w = r(sd)*.15
      # stata: qui sum zmathav
      # stata: local m = r(sd)*.15
      
      # ALERT: w = .15 & m = .15, because zreadav or zmathav were standardized. 
      
      # Sample restriction to obs "m" bw for math frontier
      # stata: qui gen `mathres'=.
      # stata: qui replace `mathres'=0 if zmathav>zmathc+`m' | zmathav<zmathc-`m'
      # stata: qui replace `mathres'=1 if zmathav<=zmathc+`m' & zmathav>=zmathc-`m'
      
      x2res = as.integer(zcx2 <= .15 & zcx2 >= -.15)
      
      # Sample restriction to obs "w" bw for reading frontier
      # stata: qui gen `readres'=.
      # stata: qui replace `readres'=0 if zreadav>zreadc+`w' | zreadav<zreadc-`w'
      # stata: qui replace `readres'=1 if zreadav<=zreadc+`w' & zreadav>=zreadc-`w'
      
      x1res = as.integer(zcx1 <= .15 & zcx1 >= -.15)
      
    })
  
  # TEST IF TRANSFORMATIONS ARE CORRECT: CLOSE

  all.equal(
    head(dat[c('x1','x2','tr','y','zcx1','zcx2','zx1','zx2','tr1','tr2')]),
    head(dataset[c('readav','mathav','treat','m_posty','zcrav','zcmav','zreadav','zmathav','treatr','treatm')]),
    check.names = F
  )
  # [1] "Component 5: Mean relative difference: 3.581497e-08"
  # [2] "Component 6: Mean relative difference: 2.173741e-08"
  # [3] "Component 7: Mean relative difference: 1.981248e-08"
  # [4] "Component 8: Mean relative difference: 3.115036e-08"
  
  ### FIT THE ORIGINAL DATASET ###
  
  # stata: local av "zcrav zcmav"
  # stata: local alltreat "treat treatr treatm" 						
  # stata: local complete "ztrmrav ztrmmav ztrrrav ztrrmav ztrrmavrav ztrmmavrav" 
    
  ## Complete model ##
  # stata: qui reg `1' `av' zreadmath `alltreat' `complete' 
  # 
  #       Source |       SS           df       MS      Number of obs   =     5,000
  # -------------+----------------------------------   F(12, 4987)     =  12898.45
  #        Model |  629548.548        12   52462.379   Prob > F        =    0.0000
  #     Residual |  20283.8234     4,987  4.06733977   R-squared       =    0.9688
  # -------------+----------------------------------   Adj R-squared   =    0.9687
  #        Total |  649832.372     4,999  129.992473   Root MSE        =    2.0168
  # 
  # ------------------------------------------------------------------------------
  #      m_posty |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #        zcrav |   5.004549   .0651618    76.80   0.000     4.876803    5.132295
  #        zcmav |   10.10207   .0664546   152.01   0.000     9.971787    10.23235
  #    zread math |  -.0904325   .0449877    -2.01   0.044    -.1786281   -.0022368
  #        treat |     4.1007   .1783127    23.00   0.000     3.751129    4.450271
  #       treatr |  -2.538275   .2824762    -8.99   0.000    -3.092053   -1.984497
  #       treatm |   1.728093   .1673674    10.33   0.000      1.39998    2.056207
  #      ztrm rav |   2.500085   .1252606    19.96   0.000     2.254519    2.745651
  #      ztrm mav |  -.2102382   .1262532    -1.67   0.096    -.4577499    .0372736
  #      ztrr rav |  -.6286196    .350292    -1.79   0.073    -1.315346    .0581068
  #      ztrr mav |   .8283463   .3795441     2.18   0.029     .0842729     1.57242
  #   ztrr mav rav |   1.457506   .5207995     2.80   0.005     .4365104    2.478503
  #   ztrm mav rav |   -.325843   .1031219    -3.16   0.002    -.5280073   -.1236786
  #        _cons |   70.00469   .1068757   655.01   0.000     69.79516    70.21421
  # ------------------------------------------------------------------------------
  
  m_s = lm(y ~  zcx1 * zcx2 * (tr1 + tr2) + tr, data = dat) 
  
  all.equal(sort(coef(m_s)),
    sort(c(5.004549,10.10207, -.0904325, 4.1007, -2.538275, 1.728093, 2.500085,
   -.2102382, -.6286196, .8283463, 1.457506, -.325843, 70.00469)),
    check.names = F
  )
  #  "Mean relative difference: 7.31848e-08"
  
  # stata: scalar streat     = _b[treat]
  # stata: scalar streatr    = _b[treatr]
  # stata: scalar streatm    = _b[treatm]
  # stata: scalar strmrav    = _b[ztrmrav]
  # stata: scalar strrmav    = _b[ztrrmav]
  
  ## Treatment indicators for only treatment planes ##
  # stata: qui reg `1' `av' `alltreat' 
  
  #       Source |       SS           df       MS      Number of obs   =     5,000
  # -------------+----------------------------------   F(5, 4994)      =  25243.30
  #        Model |  625099.117         5  125019.823   Prob > F        =    0.0000
  #     Residual |  24733.2547     4,994  4.95259406   R-squared       =    0.9619
  # -------------+----------------------------------   Adj R-squared   =    0.9619
  #        Total |  649832.372     4,999  129.992473   Root MSE        =    2.2254
  # 
  # ------------------------------------------------------------------------------
  #      m_posty |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #        zcrav |   6.422274   .0493335   130.18   0.000     6.325559     6.51899
  #        zcmav |   9.804979   .0490353   199.96   0.000     9.708848     9.90111
  #        treat |   6.129556   .1492441    41.07   0.000     5.836972     6.42214
  #       treatr |  -1.698267   .1582599   -10.73   0.000    -2.008526   -1.388008
  #       treatm |   2.513621   .1120099    22.44   0.000     2.294032    2.733209
  #        _cons |   68.50604   .0899533   761.57   0.000     68.32969    68.68239
  # ------------------------------------------------------------------------------
  
  m_h = lm(y ~  zcx1 + zcx2 + tr + tr1 + tr2, data = dat) 
  
  all.equal(sort(coef(m_h)),
    sort(c(6.422274, 9.804979, 6.129556, -1.698267, 2.513621, 68.50604)),
    check.names = F
  ) 
  # TRUE
  
  # stata: scalar httreat  = _b[treat]
  # stata: scalar httreatr = _b[treatr]
  # stata: scalar httreatm = _b[treatm]
  
  ## Treatment indicator only ##
  # stata: qui reg `1' `av' treat 
  #
  #       Source |       SS           df       MS      Number of obs   =     5,000
  # -------------+----------------------------------   F(3, 4996)      =  36133.76
  #        Model |  621202.383         3  207067.461   Prob > F        =    0.0000
  #     Residual |  28629.9886     4,996  5.73058219   R-squared       =    0.9559
  # -------------+----------------------------------   Adj R-squared   =    0.9559
  #        Total |  649832.372     4,999  129.992473   Root MSE        =    2.3939
  # 
  # ------------------------------------------------------------------------------
  #      m_posty |      Coef.   Std. Err.      t    P>|t|     [95% Conf. Interval]
  # -------------+----------------------------------------------------------------
  #        zcrav |   7.365164   .0362682   203.08   0.000     7.294063    7.436266
  #        zcmav |   9.366605   .0448743   208.73   0.000     9.278632    9.454579
  #        treat |   7.584776   .1101155    68.88   0.000     7.368901     7.80065
  #        _cons |   67.71802    .086656   781.46   0.000     67.54814    67.88791
  # ------------------------------------------------------------------------------
  
  m_t = lm(y ~ zcx1 + zcx2 + tr, data = dat) 
  
  all.equal(sort(coef(m_t)),
    sort(c(7.365164, 9.366605, 7.584776, 67.71802)),
    check.names = F
  ) 
  #  "Mean relative difference: 4.081632e-08"
  
  # stata: scalar ttreat = _b[treat]
  
  ### GENERATE A NEW DATASET (??H VARIABLE??)##
  
  J = nrow(dat) # OBS
  
  # stata: qui set obs `J'
  # stata: local N = `J'/2
  # stata: local O = `N' + 1
  
  N = J / 2
  
  # stata: qui sum zcrav
  # stata: local min = r(min) - (.03*(r(max)-r(min)))
  # stata: local max = r(max) + (.03*(r(max)-r(min)))
  
  # stata: qui gen `hr' = 0 in 1/`N'
  # stata: qui gen `hrt1' = _n-`N' in `O'/`J'
  # stata: qui gen `hrt2' = `min' + `hrt1'/`N'*(`max' - `min') 
  # stata: qui replace `hr' = `hrt2' in `O'/`J'
  
  # stata: qui sum zcmav
  # stata: local min = r(min) - (.03*(r(max)-r(min)))
  # stata: local max = r(max) + (.03*(r(max)-r(min)))
  
  # stata: qui gen `hmt1' = _n-1 in 1/`N'
  # stata: qui gen `hm' = `min' + `hmt1'/`N'*(`max' - `min') 
  # stata: qui replace `hm' = 0 in `O'/`J'
  
  # stata: *Generates treatment variables
  # stata: qui gen `treatment' = 1 in 1/`J' 
  # stata: qui gen `comp' = 0 in 1/`J'
  # stata: qui gen `tmath' = 0 in 1/`N'
  # stata: qui replace `tmath' = 1 in `O'/`J'
  # stata: qui gen `tread' = 1 in 1/`N'
  # stata: qui replace `tread' = 0 in `O'/`J'
  
  zcx1_min = min(dat$zcx1) - .03 * (max(dat$zcx1) - min(dat$zcx1))
  zcx1_max = max(dat$zcx1) + .03 * (max(dat$zcx1) - min(dat$zcx1))
  
  zcx2_min = min(dat$zcx2) - .03 * (max(dat$zcx2) - min(dat$zcx2))
  zcx2_max = max(dat$zcx2) + .03 * (max(dat$zcx2) - min(dat$zcx2))
  
  dat_h = data.frame(
    h1 = c(rep(0, N), zcx1_min + 1:N / N * (zcx1_max - zcx1_min)),
    h2 = c(zcx2_min + 0:(N - 1) / N * (zcx2_max - zcx2_min), rep(0, N)),
    tr = 1,
    comp = 0,
    tr1 = rep(c(1, 0), each = N),
    tr2 = rep(c(0, 1), each = N)
  )
  
  # .  li hr hm treatment comp tmath tread in 1/6, clean
  # 
  #        hr          hm   treatm~t   comp   tread   tmath     
  #   1.    0   -4.663815          1      0       1       0     
  #   2.    0   -4.660432          1      0       1       0     
  #   3.    0    -4.65705          1      0       1       0     
  #   4.    0   -4.653667          1      0       1       0     
  #   5.    0   -4.650284          1      0       1       0     
  #   6.    0   -4.646901          1      0       1       0     
  # 
  # .  li hr hm treatment comp tmath tread in 2501/2506, clean
  # 
  #                hr   hm   treatm~t   comp   tread   tmath     
  # 2501.   -3.618361    0          1      0       0       1     
  # 2502.   -3.615299    0          1      0       0       1     
  # 2503.   -3.612237    0          1      0       0       1     
  # 2504.   -3.609175    0          1      0       0       1     
  # 2505.   -3.606113    0          1      0       0       1     
  # 2506.    -3.60305    0          1      0       0       1     
  
  dat_h[c(1:6,2501:2506), c('h1','h2','tr','comp','tr1','tr2')]
  
  # **********************************************************************
  # ** COMPLETE MODEL ****************************************************
  # **********************************************************************
  
  # ** TREATMENT OUTCOME FUNCTIONS g(x)'s *******************************
  # 
  # * Function for treatment effect along MATH frontier
  # qui gen `gx1' = streat*`treatment' + streatm*`tmath' + strmrav*`tmath'*(`hr' - zreadc)
  
  dat_h$gx1 = m_s$coefficients['tr'] * dat_h$tr + 
    m_s$coefficients['tr2'] * dat_h$tr2 + 
    m_s$coefficients['zcx1:tr2'] * dat_h$tr2 * (dat_h$h1 - zc1)
  
  # . li gx1 in 1/6, clean
  # 
  #           gx1  
  #   1.   4.1007  
  #   2.   4.1007  
  #   3.   4.1007  
  #   4.   4.1007  
  #   5.   4.1007  
  #   6.   4.1007  
  # 
  # . li gx1 in 2501/2506, clean
  # 
  #               gx1  
  # 2501.   -1.940965  
  # 2502.   -1.933309  
  # 2503.   -1.925653  
  # 2504.   -1.917998  
  # 2505.   -1.910342  
  # 2506.   -1.902686  
  
  dat_h$gx1[c(1:6, 2501:2506)]
  
  # * Function for treatment effect along READING frontier
  # qui gen `gx2' = streat*`treatment' + streatr*`tread' + strrmav*`tread'*(`hm' - zmathc)
  
  dat_h$gx2 = m_s$coefficients['tr'] * dat_h$tr + 
    m_s$coefficients['tr1'] * dat_h$tr1 + 
    m_s$coefficients['zcx2:tr1'] * dat_h$tr1 * (dat_h$h2 - zc2)
  
  # . li gx2 in 1/6, clean
  # 
  #              gx2  
  #   1.   -2.715456  
  #   2.   -2.712653  
  #   3.   -2.709852  
  #   4.   -2.707049  
  #   5.   -2.704247  
  #   6.   -2.701445  
  # 
  # . li gx2 in 2501/2506, clean
  # 
  #            gx2  
  # 2501.   4.1007  
  # 2502.   4.1007  
  # 2503.   4.1007  
  # 2504.   4.1007  
  # 2505.   4.1007  
  # 2506.   4.1007  
  
  dat_h$gx2[c(1:6, 2501:2506)]
  
  # ** DENSITY FUNCTIONS f(x)'s *****************************************
  # 
  # ** Reading assignment variable / Math treatment frontier ***********
  # 
  # * Kernel density of reading assignment variable
  # qui kdensity zreadav if `mathres' == 1, gen(`fx1') at(`hr') tri nogr
  # . summ fx1
  # 
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #          fx1 |      5,000    .2398737    .1490146          0   .4138618
  
  # dat_h$fx1 = kdensity(dat$zx1[dat$x2res == 1], dat_h$h1)
  
  bw1 <- bw.nrd0(dat$zx1[dat$x2res == 1])
  dat_h$fx1 <- den_kern(x = dat$zx1[dat$x2res == 1], newx = dat_h$h1, 
                        bw = bw1, fun = tri_kern)
  
  # plot(dat_h$h1, dat_h$fx1)
  # points(stata_ws$h1, stata_ws$fx1, col = 'blue')
  
  # plot(fx1~h1, subset(dat_h, tr2 == 1), type  ='l')
  
  mean(dat_h$fx1)
  sd(dat_h$fx1)
  range(dat_h$fx1)
  
  # * Product for numerator: g(x)*f(x)
  # qui gen `x1prod' = `gx1'*`fx1' if `hr'>=zreadc
  
  dat_h$x1prod = with(dat_h, ifelse(h1 >= zc1, gx1 * fx1, NA))
  
  # * Numerator: Integral for numerator of MATH frontier (x1)
  # integ `x1prod' `hr' if `hr'>=zreadc 
  # number of points = 1486
  # integral         = 6.1323738
  # scalar numintx1 = r(integral) 
  
  numintx1 = with(subset(dat_h, h1 >= zc1), int_cubic(h1, x1prod)) # 6.133481
  
  # * Denominator: Intergral of the conditional density function f1(x1) 
  # integ `fx1' `hr' if `hr'>=zreadc 
  # number of points = 1486
  # integral         = .72130209
  # scalar denintx1 = r(integral) 

  denintx1 = with(subset(dat_h, h1 >= zc1), int_cubic(h1, fx1)) # 0.7165672
  
  # ** Math assignment variable / Reading treatment frontier **************
  # 
  # * Kernel density of math assignment variable
  # qui kdensity zmathav if `readres' == 1, gen(`fx2') at(`hm') tri nogr
  # *line `fx2' `hm' if `tread' == 1, xline(zmathc)
  
  # dat_h$fx2 = kdensity(dat$zx2[dat$x1res == 1], dat_h$h2)
  
  bw2 <- bw.nrd0(dat$zx2[dat$x1res == 1])
  dat_h$fx2 <- den_kern(x = dat$zx2[dat$x1res == 1], newx = dat_h$h2, 
                        bw = bw2, fun = tri_kern)
  
  # plot(dat_h$h2, dat_h$fx2)
  # points(stata_ws$h2, stata_ws$fx2, col = 'blue')
  
  dat_h$fx2[c(1:6, 2501:2506, 3480:3485)]
  
  # * Product for numerator: g(x)*f(x)
  # qui gen `x2prod' = `gx2'*`fx2' if `hm'>=zmathc
  
  dat_h$x2prod = with(dat_h, ifelse(h2 >= zc2, gx2 * fx2, NA))
  
  # * Numerator: Integral for numerator of READING frontier (x2)
  # integ `x2prod' `hm' if `hm'>=zmathc
  # number of points = 973
  # integral         = .52748626
  
  # scalar numintx2 = r(integral) 
  
  numintx2 = with(subset(dat_h, h2 >= zc2), int_cubic(h2, x2prod)) #  0.5406438
  
  # * Denominator: Intergral of the conditional density function f2(x2)
  # qui integ `fx2' `hm' if `hm'>=zmathc
  # scalar denintx2 = r(integral) 
  
  denintx2 = with(subset(dat_h, h2 >= zc2), int_cubic(h2, fx2)) #  0.264588
  
  # ** Treatment weights *************************************************
  # 
  # * Marginal density at math cutoff
  # qui gen `chm' = `hm' - zmathc
  # qui gen `mtemp' = abs(`chm')
  # qui sum `mtemp'
  # qui gen `mtemp2' = 1 if `mtemp' == r(min)
  # qui sum `fx2' if `mtemp2' == 1
  # scalar mdmcw = r(mean)
  
  md2cw = with(dat_h, {
    ch2 = abs(h2 - zc2)
    mean(fx2[ch2 == min(ch2)])
  })
  
  # * Marginal density at reading cutoff
  # qui gen `chr' = `hr' - zreadc
  # qui gen `rtemp' = abs(`chr')
  # qui sum `rtemp'
  # qui gen `rtemp2' = 1 if `rtemp' == r(min)
  # qui sum `fx1' if `rtemp2' == 1
  # scalar mdrcw = r(mean)
  
  md1cw = with(dat_h, {
    ch1 = abs(h1 - zc1)
    mean(fx1[ch1 == min(ch1)])
  })
  
  # * Weight for Math frontier
  # return scalar mathw = mdmcw*denintx1
  
  w2 = md2cw * denintx1
  
  # * Weight for Reading frontier
  # return scalar readw = mdrcw*denintx2
  
  w1 = md1cw * denintx2
  
  # ** Treatment estimates for complete model ******************************
  # 
  # * Math frontier expected value
  # local mathev = numintx1 / denintx1 
  # return scalar mathev = `mathev'
  
  ev2 = numintx1 / denintx1
  
  # * Reading frontier expected value
  # local readev = numintx2 / denintx2 
  # return scalar readev = `readev'
  
  ev1 = numintx2 / denintx2 
  
  # * ATE across both frontiers
  # local ate = ((return(mathw)*`mathev') + (return(readw)*`readev')) / (return(mathw) + return(readw))
  # return scalar ate = `ate'
  
  ate = ((w2*ev2) + (w1*ev1)) / (w2 + w1)
  
  # **********************************************************************
  # ** HETEROGENEOUS TREATMENTS MODEL ***********************************
  # **********************************************************************
  # 
  # * Function for treatment effect along MATH frontier
  # qui gen `htgx1' = httreat*`treatment' + httreatm*`tmath' 
  # summ htgx1
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #        htgx1 |      5,000    7.386366    1.256936   6.129556   8.643176
  # 
  
  dat_h$htgx1 = m_h$coefficients['tr'] * dat_h$tr + m_h$coefficients['tr2'] * dat_h$tr2
  
  mean(dat_h$htgx1)
  sd(dat_h$htgx1)
  
  # * Function for treatment effect along READING frontier
  # qui gen `htgx2' = httreat*`treatment' + httreatr*`tread' 
  # 
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #        htgx2 |      5,000    5.280422    .8492184   4.431289   6.129556
  
  dat_h$htgx2 = m_h$coefficients['tr'] * dat_h$tr + m_h$coefficients['tr1'] * dat_h$tr1
  
  mean(dat_h$htgx2)
  sd(dat_h$htgx2)
  
  # ** DENSITY FUNCTIONS f(x)'s *****************************************
  # 
  # ** Reading assignment variable / Math treatment frontier ***********
  # 
  # * Product for numerator: g(x)*f(x)
  # qui gen `htx1prod' = `htgx1'*`fx1' if `hr'>=zreadc
  # summ htx1prod
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #     htx1prod |      3,985     1.85381    .8916917          0    3.57708
  
  dat_h$htx1prod = with(dat_h, ifelse(h1 >= zc1, htgx1 * fx1, NA))
  
  sum(!is.na(dat_h$htx1prod))
  mean(dat_h$htx1prod, na.rm = T)
  sd(dat_h$htx1prod, na.rm = T)
  
  # * Numerator: Integral for numerator of MATH frontier (x1)
  # qui integ `htx1prod' `hr' if `hr'>=zreadc 
  # number of points = 1486
  # integral         = 6.2336946
  # scalar htnumintx1 = r(integral) 
  
  htnumintx1 = with(subset(dat_h, h1 >= zc1), int_cubic(h1, htx1prod)) # 6.192759
  
  # ** Math assignment variable / Reading treatment frontier **************
  # 
  # * Product for numerator: g(x)*f(x)
  # qui gen `htx2prod' = `htgx2'*`fx2' if `hm'>=zmathc
  
  dat_h$htx2prod = with(dat_h, ifelse(h2 >= zc2, htgx2 * fx2, NA))
  
  # * Numerator: Integral for numerator of READING frontier (x2)
  # qui integ `htx2prod' `hm' if `hm'>=zmathc
  # number of points = 973
  # integral         = 1.1583117
  # scalar htnumintx2 = r(integral) 
  
  htnumintx2 = with(subset(dat_h, h2 >= zc2), int_cubic(h2, htx2prod)) # 1.172715
   
  # ** Treatment estimates for "heterogeneous treatments" (HT) model *******
  # 
  # * Math frontier expected value
  # local htmathev = htnumintx1 / denintx1 
  # return scalar htmathev = `htmathev'
  
  htev2 = htnumintx1 / denintx1 
  
  # * Reading frontier expected value
  # local htreadev = htnumintx2 / denintx2 
  # return scalar htreadev = `htreadev'
  
  htev1 = htnumintx2 / denintx2
  
  # * ATE across both frontiers
  # local htate = ((return(mathw)*`htmathev') + (return(readw)*`htreadev')) / (return(mathw) + return(readw))
  # return scalar htate = `htate'
  
  htate = ((w2 * htev2) + (w1 * htev1)) / (w2 + w1)
  
  # **********************************************************************
  # ** TREATMENT ONLY MODEL **********************************************
  # **********************************************************************
  # 
  # * Function for treatment effect along MATH frontier
  # qui gen `tgx1' = ttreat*`treatment' 
  # summ tgx1
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #         tgx1 |      5,000    7.584775           0   7.584775   7.584775
  
  dat_h$tgx1 = m_t$coefficients['tr'] * dat_h$tr 
  
  mean(dat_h$tgx1)
  sd(dat_h$tgx1)
   
  # * Function for treatment effect along READING frontier
  # qui gen `tgx2' = ttreat*`treatment' 
  
  dat_h$tgx2 = m_t$coefficients['tr'] * dat_h$tr 
  
  mean(dat_h$tgx2)
  sd(dat_h$tgx2)
  
  # ** DENSITY FUNCTIONS f(x)'s *****************************************
  # 
  # ** Reading assignment variable / Math treatment frontier ***********
  # 
  # * Product for numerator: g(x)*f(x)
  # qui gen `tx1prod' = `tgx1'*`fx1' if `hr'>=zreadc 
  # . summ tx1prod
  # 
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #      tx1prod |      3,985    2.109927      .99684          0   3.139049
  # 
  
  dat_h$tx1prod = with(dat_h, ifelse(h1 >= zc1, tgx1 * fx1, NA))
  
  sum(!is.na(dat_h$tx1prod))
  mean(dat_h$tx1prod, na.rm = T)
  sd(dat_h$tx1prod, na.rm = T)
  range(dat_h$tx1prod, na.rm = T)
  
  # * Numerator: Integral for numerator of MATH frontier (x1)
  # . integ tx1prod hr if hr>=zreadc 
  # number of points = 1486
  # integral         = 5.4709144
  # scalar tnumintx1 = r(integral) 
  
  tnumintx1 = with(subset(dat_h, h1 >= zc1), int_cubic(h1, tx1prod)) # 5.435015
  
  # ** Math assignment variable / Reading treatment frontier **************
  # 
  # * Product for numerator: g(x)*f(x)
  # qui gen `tx2prod' = `tgx2'*`fx2' if `hm'>=zmathc
  
  #     Variable |        Obs        Mean    Std. Dev.       Min        Max
  # -------------+---------------------------------------------------------
  #      tx2prod |        973    .6039416    .8145213          0    3.13091
  
  dat_h$tx2prod = with(dat_h, ifelse(h2 >= zc2, tgx2 * fx2, NA))
  
  sum(!is.na(dat_h$tx2prod))
  mean(dat_h$tx2prod, na.rm = T)
  sd(dat_h$tx2prod, na.rm = T)
  range(dat_h$tx2prod, na.rm = T)
  
  # * Numerator: Integral for numerator of READING frontier (x2)
  # qui integ `tx2prod' `hm' if `hm'>=zmathc
  # number of points = 973
  # integral         = 1.9826138
  # scalar tnumintx2 = r(integral) 
  
  tnumintx2 = with(subset(dat_h, h2 >= zc2), int_cubic(h2, tx2prod)) # 2.007267
  
  # ** Treatment estimates for "treatment only" (TO) model *******
  # 
  # * Math frontier expected value
  # local tmathev = tnumintx1 / denintx1 
  # return scalar tmathev = `tmathev'
  
  tev2 = tnumintx1 / denintx1 
   
  # * Reading frontier expected value
  # local treadev = tnumintx2 / denintx2 
  # return scalar treadev = `treadev'
  
  tev1 = tnumintx2 / denintx2 
  
  # * ATE across both frontiers
  # local tate = ((return(mathw)*`tmathev') + (return(readw)*`treadev')) / (return(mathw) + return(readw))
  # return scalar tate = `tate'
  
  tate = ((w2 * tev2) + (w1 * tev1)) / (w2 + w1)
  
  # ** DISPLAY ************************************************************
  # 
  # di _newline as txt as yellow "***EFFECT ESTIMATES FOR MULTIVARIATE I STANDARDIZED***" 
  # di _newline as txt "Math weight (st): " as red round(return(mathw), .001)  
  # di as txt "Reading weight (st): " as red round(return(readw), .001)
  # 
  # di _newline as txt "Math frontier for complete model (st): " as red round(`mathev', .01)
  # di as txt "Reading frontier for complete model (st): " as red round(`readev', .01)
  # di as txt "ATE for " as yellow "complete " as txt "model (st): " as red round(`ate', .01)
  # 
  # di _newline as txt "Math frontier for heterogeneous treatment model (st): " as red round(`htmathev', .01)
  # di as txt "Reading frontier for heterogeneous treatment model (st): " as red round(`htreadev', .01)
  # di as txt "ATE for " as yellow "heterogeneous treatment " as txt "model (st): " as red round(`htate', .01)
  # 
  # di _newline as txt "Math frontier for treatment only model (st): " as red round(`tmathev', .01)
  # di as txt "Reading frontier for treatment only model (st): " as red round(`treadev', .01)
  # di as txt "ATE for " as yellow "treatment only " as txt "model (st): " as red round(`tate', .01)
  # ************************************************************************
  return(c(w1 = w1,
    w2 = w2, 
    ev1 = ev1,
    ev2 = ev2,
    ate = ate,
    htev1 = htev1,
    htev2 = htev2,
    htate = htate,
    tev1 = tev1,
    tev2 = tev2,
    tate = tate))
}
  
## mimic Stata's integ
int_cubic <- function(x, y){
  integrate(
    splinefun(x, y, method = "natural"), 
    lower = min(x), upper = max(x)
  )$value
}

## mimic Stata's kdensity
den_kern <- function(x, newx, bw, fun) {
  n <- length(x)
  newn <- length(newx)
  
  colMeans(fun((matrix(rep(newx, n), n, newn, byrow = T) - x) / bw)) / bw
}

tri_kern <- function(x) {
  ifelse(abs(x) > 1, 0, 1 - abs(x))
}

# kdensity = function(x, at){
#   den = density(x, kernel = 'triangular')
#   approx(den$x, den$y, xout = at, rule = 2)$y  
# }