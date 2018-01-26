#generate covariates
set.seed(1234)
n <- 10000
a1 <- rnorm(n,0,1)
a2 <- rnorm(n,1,5)
t <- ifelse(a1 <0 | a2 <0,1,0)
table(a1<0,t)
table(a2<0,t)
table(a1<0,a2<0,t)
table(t)

#generate potential outcomes under homogenous treatment effect assumption
#this assumes that treatment effect is always 1.0 for every person and that relationship between
#covariates and potential outcomes is constant for both control and treated
y0 <- 0 + .2*a1 + .2*a2 + rnorm(n,0,.2)
y1 <- 1 + .2*a1 + .2*a2 + rnorm(n,0,.2)

#observed outcome
y <- ifelse(t==1,y1,y0)


#naive treatment estimator
lm(y~t)


#RDD estimation
#global parametric approach
#since cut-off is at zero, we do not need to center, otherwise we need to center variables at their cut-off
lm(y~t+a1+a2+t:a1+t:a2)


#frontier approach
mv_rdd(y = y, x1 = a1, x2 = a2, c1 = 0, c2 = 0, tr = t)

#centering approach
#first center variables at their cut-offs, here cut-off is zero, hence no centering needed
a12min <- apply(cbind(a1,a2),1,min)
#construct new treatment assignment which is identical to original treatment assignment (works only
#only certain treatment assignments, otherwise sign-reversals or max needed)
ta12min <- ifelse(a12min>0,0,1)
table(ta12min,t)

#then compute RDD based on new treatment assignment
#global parametric
lm(y~ta12min+a12min+ta12min:a12min)

#local using RDD
#note that RDD will flip signs again, because it always computes difference from left to right
RDestimate(y~a12min)


#univariate approach
#here we estimate effects for each assignment variable by excluding non-conforming units




#IV approach (not recommended by Wong et al.)
#here we also estimate effects for each assignment variable 


fhat <- kde(cbind(a1,a2,y))
plot(fhat,display="persp")
