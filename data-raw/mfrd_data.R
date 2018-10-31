# sim_data() adapted from Vivian's simdata.do
sim_data = function(
  n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
  b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2,
  b_tr1x1 = 0 , b_tr2x2 = 0, b_tr1x1x2 = 0, b_tr2x1x2 = 0, seed = 1000) {
  set.seed(seed)
  dat = MASS::mvrnorm(n, 
    mu = c(mu1, mu2), 
    Sigma = matrix(c(sigma1 ^ 2, rep(corr * sigma1 * sigma2, 2), sigma2 ^ 2), 2, 2))

  x1 = dat[,1]
  x2 = dat[,2]
  
  # treatment
  x1_tr <- treat_assign(x1, c1, t.design[1])
  x2_tr <- treat_assign(x2, c2, t.design[2])
  
  tr1 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & !x2_tr, 1, 0))
  tr2 <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(!x1_tr & x2_tr, 1, 0))
  trb <- ifelse(is.na(x1) | is.na(x2), NA, ifelse(x1_tr & x2_tr, 1, 0))
  
  tr = ifelse(tr1 == 1 | tr2 == 1 | trb == 1, 1, 0)
  
  # center assignments
  cx1 = x1 - c1
  cx2 = x2 - c2
  
  # outcome
  y = 
    const     * 1  + 
    b_x1      * cx1 + 
    b_x2      * cx2 + 
    b_x1x2    * cx1 * cx2 + 
    b_tr      * tr - 
    b_tr1     * tr1 - 
    b_tr2     * tr2 - 
    b_tr1x1	  * tr1 * cx1 - 
    b_tr1x2   * tr1 * cx2 - 
    b_tr2x1   * tr2 * cx1 - 
    b_tr2x2   * tr2 * cx2 - 
    b_tr1x1x2 * tr1 * cx1 * cx2 - 
    b_tr2x1x2 * tr2 * cx1 * cx2 + 
    rnorm(n,0,1)

  return(data.frame(x1, x2, y))
}

# true_effect() adapted from Vivian's trueffects.do
true_effect = function(
  mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
  x1_range, x2_range,
  b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2,
  local = 0.15, ngrid = 2500, margin = 0.03) {
  
  ### GENERATE A NEW DATASET ##
  N = ngrid
  
  x1_min = min(x1_range) - margin * (max(x1_range) - min(x1_range))
  x1_max = max(x1_range) + margin * (max(x1_range) - min(x1_range))
  
  x2_min = min(x2_range) - margin * (max(x2_range) - min(x2_range))
  x2_max = max(x2_range) + margin * (max(x2_range) - min(x2_range))
  
  dat_h = data.frame(
    h1 = c(rep(0, ngrid), x1_min + 1:ngrid / ngrid * (x1_max - x1_min)),
    h2 = c(x2_min + 0:(ngrid - 1) / ngrid * (x2_max - x2_min), rep(0, ngrid)),
    tr = 1,
    comp = 0,
    tr1 = rep(c(1, 0), each = ngrid),
    tr2 = rep(c(0, 1), each = ngrid)
  )
  
  ## CONDITIONAL EXPECTATIONS & SDs ##
  b1 = corr*sigma1*sigma2 / sigma1^2
  # |E for reading av, math tr
  cdex1 = (mu1 + (b1*(c2 - mu2)))
  # |SD for reading av, math tr
  cdsd1 = sqrt((1-corr^2)*(sigma1^2))
  
  b2 = corr*sigma1*sigma2 / sigma1^2
  # |E for math av, reading tr
  cdex2 = (mu2 + (b2*(c1 - mu1)))
  # |SD for math av, read tr
  cdsd2 = sqrt((1-corr^2)*(sigma2^2))
  
  ## FRONTIER WEIGHTS ***********************************************
  
  ## Marginal probability at Math treatment UNSTANDARDIZED
  un_mdx1 = dnorm(c2, mu2, sigma2) 
  
  ## Marginal probability at Math treatment STANDARDIZED
  std_mdx1 = dnorm((c2-mu2)/sigma2, 0, 1) 
  
  z1 = ((c1 - cdex1)/cdsd1)
  cdx1 = (1-pnorm(z1))
  
  # Weight for MATH frontier UNSTANDARDIZED
  un_w2 = un_mdx1*cdx1
  
  # Weight for MATH frontier STANDARDIZED
  std_w2 = std_mdx1*cdx1
  
  ## Marginal probability at Reading treatment UNSTANDARDIZED
  un_mdx2 = dnorm(c1, mu1, sigma1)
  
  # Marginal probability of Reading treatment UNSTANDARDIZED
  std_mdx2 = dnorm((c1-mu1)/sigma1, 0, 1)
  
  z2 = ((c2 - cdex2)/cdsd2)
  cdx2 = (1-pnorm(z2))
  
  #Weight for READING frontier UNSTANDARDIZED
  un_w1 = un_mdx2*cdx2
  
  #Weight for READING frontier STANDARIZED 
  std_w1 = std_mdx2*cdx2
  
  ###############
  ## EXPECTED VALUES  **************************************************
  ##********************************************************************
  
  ## FUNCTIONS G1(X1) & G2(X2) *****************************************
  
  # Function for treatment effect along MATH frontier
  dat_h$gx1 = with(dat_h,
    ifelse(tr2, b_tr*tr - b_tr2*tr2 - b_tr2x1*tr2*(h1 - c1), NA)
  )
  
  # Function for treatment effect along READING frontier
  dat_h$gx2 = with(dat_h,
    ifelse(tr1, b_tr*tr - b_tr1*tr1 - b_tr1x2*tr1*(h2 - c2), NA)
  )
  
  ## MATH FRONTIER EV **************************************************
  
  # Density function 
  dat_h$fx1 = with(dat_h, 
    ifelse(tr2, dnorm(h1, cdex1, cdsd1), NA)
  )
  
  # Product of outcome and density function
  dat_h$x1prod = dat_h$gx1 * dat_h$fx1
  
  # Integral for numerator of MATH frontier
  
  intx1 = with(subset(dat_h, !as.logical(treat_assign(h1,  c1, t.design[1]))),
    int_cubic(h1, x1prod))
  
  ## Expected value for MATH frontier
  ev2 = intx1 / cdx1
  
  ## READING FRONTIER EV ************************************************
  
  # Density function along READING frontier
  dat_h$fx2 = with(dat_h, 
    ifelse(tr1, dnorm(h2, cdex2, cdsd2), NA)
  )
  
  # Product of outcome and density function
  
  dat_h$x2prod = dat_h$gx2 * dat_h$fx2
  
  ## Integral for numerator of READING frontier
  
  intx2 = with(subset(dat_h, !as.logical(treat_assign(h2, c2, t.design[2]))),
    int_cubic(h2, x2prod))
  
  ## Expected value for READING frontier
  ev1 = intx2 / cdx2
  
  un_ate = (un_w2*ev2 +un_w1*ev1) / (un_w1 + un_w2)
  std_ate = (std_w2*ev2 + std_w1*ev1) / (std_w1 + std_w2)
  
  return(
    c(un_w1 = un_w1, un_w2 = un_w2, std_w1 = std_w1, std_w2 = std_w2, ev1 = ev1, ev2 = ev2, un_ate = un_ate, std_ate=std_ate)
  )
  
}

### MAIN ####
library(ggplot2)
library(rddapp)
# sample size
n = 1000

# model coefficient
const = 70
b_x1 = 2
b_x2 = 1
b_x1x2 = 0
b_tr = 8
b_tr1 = 2
b_tr2 = -2
b_tr1x1	= 0
b_tr1x2 = 0
b_tr2x1 = 0
b_tr2x2 = 0
b_tr1x1x2 = 0
b_tr2x1x2 = 0

# assignments
c1 = 40
c2 = 60
t.design = c('l','l')

mu1 = 45
sigma1 = 10
mu2 = 55
sigma2 = 10
corr = 0

dat = sim_data(n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
  b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2)

teff = true_effect(mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
  range(dat$x1), range(dat$x2),
  b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2)

mrd = mrd_est(y~x1+x2, data = dat, cutpoint = c(c1, c2), t.design = t.design)

# summarize results
rbind(
  true = teff,
  frontier_param = c(NA, NA, mrd$front$tau_MRD$w, mrd$front$tau_MRD$est['Param',][1:2], NA, mrd$front$tau_MRD$est['Param',][3]),
  frontier_bw = c(NA, NA, mrd$front$tau_MRD$w, mrd$front$tau_MRD$est['bw',][1:2], NA, mrd$front$tau_MRD$est['bw',][3]),
  centering_linear = c(rep(NA, 7), mrd$center$tau_MRD$est[1]),
  centering_opt = c(rep(NA, 7), mrd$center$tau_MRD$est[4]),
  univ_linear = c(rep(NA, 4), mrd$univ$tau_R$est[1],mrd$univ$tau_M$est[1], NA, NA),
  univ_opt = c(rep(NA, 4), mrd$univ$tau_R$est[4],mrd$univ$tau_M$est[4], NA, NA)
)

#### does corr affects estimates? (YES) ####
results_raw = mapply(corr = seq(-.7,.7,0.1), 
  MoreArgs = list(n = 1000, 
    mu1 = 45, mu2 = 55, sigma1 = 10, sigma2 = 10, c1 = 40, c2 = 60, 
    t.design = c('l','l'),
    b_tr = 4, b_tr1 = 2, b_tr2 = -2, b_tr2x1 = 0, b_tr1x2 = 0),
  SIMPLIFY = F,
  FUN = function(n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
    b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2, 
    b_tr1x1 = 0 , b_tr2x2 = 0, b_tr1x1x2 = 0, b_tr2x1x2 = 0){
    
    dat = sim_data(n, mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
      b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2)
    
    teff = true_effect(mu1, mu2, sigma1, sigma2, corr, c1, c2, t.design,
      range(dat$x1), range(dat$x2),
      b_tr, b_tr1, b_tr2, b_tr2x1, b_tr1x2)
    
    mrd = mrd_est(y~x1+x2, data = dat, cutpoint = c(c1, c2), t.design = t.design)

    est = rbind(
      true = teff,
      frontier = c(NA, NA, mrd$front$tau_MRD$w, mrd$front$tau_MRD$est['Param',][1:2], NA, mrd$front$tau_MRD$est['Param',][3]),
      frontier_bw = c(NA, NA, mrd$front$tau_MRD$w, mrd$front$tau_MRD$est['bw',][1:2], NA, mrd$front$tau_MRD$est['bw',][3]),
      centering_linear = c(rep(NA, 7), mrd$center$tau_MRD$est[1]),
      centering_opt = c(rep(NA, 7), mrd$center$tau_MRD$est[4]),
      univ_linear = c(rep(NA, 4), mrd$univ$tau_R$est[1],mrd$univ$tau_M$est[1], NA, NA),
      univ_opt = c(rep(NA, 4), mrd$univ$tau_R$est[4],mrd$univ$tau_M$est[4], NA, NA)
    )
    est = as.data.frame(est)
    est$approach = rownames(est)
    est$corr = corr
    rownames(est) = NULL
    return(est)
  })

results = do.call(rbind.data.frame, results_raw)

# plot ATE
ggplot(subset(results, subset = !grepl('univ', approach) )) +
  aes(x = corr, y = std_ate, color = approach) +
  geom_line()

# plot UNIV1
ggplot(subset(results, subset = grepl('univ|true', approach)  )) +
  aes(x = corr, y = ev1, color = approach) +
  geom_line()

# plot UNIV2
ggplot(subset(results, subset = grepl('univ|true', approach)  )) +
  aes(x = corr, y = ev2, color = approach) +
  geom_line()

#### SOME RESULTS FROM VIVIAN'S STATA CODE ####
# n = 1000, mu1 = 45, mu2 = 55, sigma1 = 10, sigma2 = 10, 
# c1 = 40, c2 = 60, t.design = c('l','l'),
# b_tr = 4, b_tr1 = 2, b_tr2 = -2, b_tr2x1 = 0, b_tr1x2 = 0

# corr = 0
# "true standardized" ATE 4.74
# "frontier"          ATE 4.63
# "centering"         ATE 4.96
# "centering std"     ATE 4.82

# corr = -.5
# "true standardized" ATE 4.43
# "frontier"          ATE 4.57
# "centering"         ATE 5.18
# "centering std"     ATE 5.15

# corr = -.4
# "true standardized" ATE 4.48
# "frontier"          ATE 4.51
# "centering"         ATE 4.52
# "centering std"     ATE 4.25

# corr = -.3
# "true standardized" ATE 4.55
# "frontier"          ATE 4.6699
# "centering"         ATE 5.79
# "centering std"     ATE 5.35

# corr = -.2
# "true standardized" ATE  4.61
# "frontier"          ATE  4.9166
# "centering"         ATE  6.73
# "centering std"     ATE  5.9

# corr = -.1
# "true standardized" ATE  4.67
# "frontier"          ATE  4.7754
# "centering"         ATE  3.8
# "centering std"     ATE  3.88



