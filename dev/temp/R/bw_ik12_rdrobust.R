#
library(rdrobust)

# based on rdbwselect_2014 in rdrobust 
bw_ik12_rdrobust = function(x, c=0, p=1, q=2, deriv=0, kernel="tri", scaleregul=1, delta=0.5) {
    X_l = x[x<c];    X_r = x[x>=c]
    Y_l = y[x<c];    Y_r = y[x>=c]
    N_l = length(X_l);   N_r = length(X_r)
    x_min=min(x);  x_max=max(x)
    N = N_r + N_l
    
    p1 = p+1;  p2 = p+2;  q1 = q+1;  q2 = q+2;  q3 = q+3  
    h_CCT=b_CCT=h_IK=b_IK=h_CV=NA
    N = length(x)
    
    ct1 = bwconst(p,deriv,kernel)
    C1_h = ct1[1];  C2_h = ct1[2]
    ct2 = bwconst(q,q,kernel)
    C1_b = ct2[1];  C2_b = ct2[2]
    ct3 = bwconst(q1,q1,kernel)
    C1_q = ct3[1];  C2_q = ct3[2]
    
    ct2 = bwconst(q,q,kernel)
    C1_b_uni = ct2[1];  C2_b_uni = ct2[2]
    ct3 = bwconst(q1,q1,kernel)
    C1_q_uni = ct3[1];  C2_q_uni = ct3[2]
    
    X_lq2 = matrix(c((X_l-c)^0, poly(X_l-c,degree=(q3-1),raw=T)),length(X_l),q3)
    X_rq2 = matrix(c((X_r-c)^0, poly(X_r-c,degree=(q3-1),raw=T)),length(X_r),q3)
    X_lq1 = X_lq2[,1:q2];  X_rq1 = X_rq2[,1:q2]
    X_lq  = X_lq2[,1:q1];  X_rq  = X_rq2[,1:q1]
    X_lp  = X_lq2[,1:p1];  X_rp  = X_rq2[,1:p1]
    
    #print("Computing IK Bandwidth Selector.")
    h_pilot_IK = 1.84*sd(x)*N^(-1/5)
    n_l_h1 = length(X_l[X_l>=c-h_pilot_IK])
    n_r_h1 = length(X_r[X_r<=c+h_pilot_IK])
    f0_pilot=(n_r_h1+n_l_h1)/(2*N*h_pilot_IK)
    
    s2_l_pilot = var(Y_l[X_l>=c-h_pilot_IK])
    s2_r_pilot = var(Y_r[X_r<=c+h_pilot_IK])
    if (s2_l_pilot==0){
      s2_l_pilot=var(Y_l[X_l>=c-2*h_pilot_IK])
    }
    if (s2_r_pilot==0){
      s2_r_pilot=var(Y_r[X_r<=c+2*h_pilot_IK])
    }
    
    V_IK_pilot = (s2_r_pilot+s2_l_pilot)/f0_pilot
    Vm0_pilot_IK = C2_h*V_IK_pilot
    Vm2_pilot_IK = C2_b*V_IK_pilot
    Vm3_pilot_IK = C2_q*V_IK_pilot
    
    x_IK_med_l = X_l[X_l>=median(X_l)]; y_IK_med_l = Y_l[X_l>=median(X_l)]
    x_IK_med_r = X_r[X_r<=median(X_r)]; y_IK_med_r = Y_r[X_r<=median(X_r)]
    x_IK_med = c(x_IK_med_r,x_IK_med_l); y_IK_med = c(y_IK_med_r,y_IK_med_l)
    sample_IK = length(x_IK_med)
    X_IK_med_q2 = matrix(c((x_IK_med-c)^0, poly(x_IK_med-c,degree=(q3-1),raw=T)),sample_IK,q3)
    X_IK_med_q1 = X_IK_med_q2[,1:q2]
    X_IK_med_q2=cbind(X_IK_med_q2,1*(x_IK_med>=c))
    X_IK_med_q1=cbind(X_IK_med_q1,1*(x_IK_med>=c))
    
    ### First Stage
    N_b_IK = (2*p+3)*Vm2_pilot_IK
    # Pilot Bandwidth
    N_q_r_pilot_IK = (2*q+3)*C2_q_uni*(s2_r_pilot/f0_pilot)
    N_q_l_pilot_IK = (2*q+3)*C2_q_uni*(s2_l_pilot/f0_pilot)
    m4_pilot_IK = qr.coef(qr(X_IK_med_q2, tol = 1e-10), y_IK_med)[q+3]
    D_q_pilot_IK = 2*(C1_q_uni*m4_pilot_IK)^2
    h3_r_pilot_IK = (N_q_r_pilot_IK / (N_r*D_q_pilot_IK))^(1/(2*q+5))
    h3_l_pilot_IK = (N_q_l_pilot_IK / (N_l*D_q_pilot_IK))^(1/(2*q+5))
    # Derivative
    X_lq_IK_h3=X_lq1[X_l>=c-h3_l_pilot_IK,]; Y_l_IK_h3 =Y_l[X_l>=c-h3_l_pilot_IK]
    X_rq_IK_h3=X_rq1[X_r<=c+h3_r_pilot_IK,]; Y_r_IK_h3 =Y_r[X_r<=c+h3_r_pilot_IK]
    m3_l_IK=qr.coef(qr(X_lq_IK_h3, tol = 1e-10), Y_l_IK_h3)[q2]
    m3_r_IK=qr.coef(qr(X_rq_IK_h3, tol = 1e-10), Y_r_IK_h3)[q2]
    D_b_IK = 2*(q-p)*(C1_b*(m3_r_IK - (-1)^(deriv+q+1)*m3_l_IK))^2
    # Regularization
    n_l_h3 = length(Y_l_IK_h3);n_r_h3 = length(Y_r_IK_h3)
    temp = regconst(q1,1);    con = temp[q2,q2]
    r_l_b = (con*s2_l_pilot)/(n_l_h3*h3_l_pilot_IK^(2*q1))
    r_r_b = (con*s2_r_pilot)/(n_r_h3*h3_r_pilot_IK^(2*q1))
    R_b_IK = scaleregul*2*(q-p)*(C1_b)^2*3*(r_l_b + r_r_b)
    # Final Bandwidth
    b_IK   = (N_b_IK / (N*(D_b_IK+R_b_IK)))^(1/(2*q+3))
    
    ### Second Stage
    N_h_IK = (2*deriv+1)*Vm0_pilot_IK
    # Pilot
    N_b_r_pilot_IK = (2*p1+1)*C2_b_uni*(s2_r_pilot/f0_pilot)
    N_b_l_pilot_IK = (2*p1+1)*C2_b_uni*(s2_l_pilot/f0_pilot)    
    m3_pilot_IK = qr.coef(qr(X_IK_med_q1, tol = 1e-10), y_IK_med)[q2]
    D_b_pilot_IK = 2*(q-p)*(C1_b_uni*m3_pilot_IK)^2
    h2_r_pilot_IK  = (N_b_r_pilot_IK / (N_r*D_b_pilot_IK))^(1/(2*q+3))
    h2_l_pilot_IK  = (N_b_l_pilot_IK / (N_l*D_b_pilot_IK))^(1/(2*q+3))
    # Derivative
    X_lq_IK_h2=X_lq[X_l>=c-h2_l_pilot_IK,]; Y_l_IK_h2 =Y_l[X_l>=c-h2_l_pilot_IK]
    X_rq_IK_h2=X_rq[X_r<=c+h2_r_pilot_IK,]; Y_r_IK_h2 =Y_r[X_r<=c+h2_r_pilot_IK]
    m2_l_IK=qr.coef(qr(X_lq_IK_h2, tol = 1e-10), Y_l_IK_h2)[p2]
    m2_r_IK=qr.coef(qr(X_rq_IK_h2, tol = 1e-10), Y_r_IK_h2)[p2]
    D_h_IK = 2*(p+1-deriv)*(C1_h*(m2_r_IK - (-1)^(deriv+p+1)*m2_l_IK))^2
    # Regularization
    n_l_h2 = length(Y_l_IK_h2);n_r_h2 = length(Y_r_IK_h2)
    temp = regconst(p1,1);  con = temp[p2,p2]
    r_l_h = (con*s2_l_pilot)/(n_l_h2*h2_l_pilot_IK^(2*p1))
    r_r_h = (con*s2_r_pilot)/(n_r_h2*h2_r_pilot_IK^(2*p1))
    R_h_IK = scaleregul*2*(p+1-deriv)*(C1_h)^2*3*(r_l_h + r_r_h)
    # Final Bandwidth
    h_IK  = (N_h_IK / (N*(D_h_IK+R_h_IK)))^(1/(2*p+3))
    
    return(c(h_IK,b_IK))
}

#
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)

#
bw_ik12_rdrobust(x)
