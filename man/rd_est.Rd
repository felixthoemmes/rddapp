% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rd_est.R
\name{rd_est}
\alias{rd_est}
\title{Regression Discontinuity Estimation}
\usage{
rd_est(
  formula,
  data,
  subset = NULL,
  cutpoint = NULL,
  bw = NULL,
  kernel = "triangular",
  se.type = "HC1",
  cluster = NULL,
  verbose = FALSE,
  less = FALSE,
  est.cov = FALSE,
  est.itt = FALSE,
  t.design = NULL
)
}
\arguments{
\item{formula}{The formula of the RDD; a symbolic description of the model to be fitted. This is supplied in the
format of \code{y ~ x} for a simple sharp RDD or \code{y ~ x | c1 + c2}
for a sharp RDD with two covariates. A fuzzy RDD may be specified as
\code{y ~ x + z} where \code{x} is the running variable, and 
\code{z} is the endogenous treatment variable. Covariates are included in the 
same manner as in a sharp RDD.}

\item{data}{An optional data frame containing the variables in the model. If not found in \code{data},
the variables are taken from \code{environment(formula)}.}

\item{subset}{An optional vector specifying a subset of observations to be used in the fitting process.}

\item{cutpoint}{A numeric value containing the cutpoint at which assignment to the treatment is determined. The default is 0.}

\item{bw}{A vector specifying the bandwidths at which to estimate the RD. 
Possible values are \code{"IK09"}, \code{"IK12"}, and a user-specified non-negative numeric vector specifying the bandwidths at which to estimate the RD.
The default is \code{"IK12"}. If \code{bw} is \code{"IK12"}, the bandwidth is calculated using the Imbens-Kalyanaraman 
2012 method. If \code{bw}  is \code{"IK09"}, the bandwidth is calculated using 
the Imbens-Kalyanaraman 2009 method. Then the RD is estimated
with that bandwidth, half that bandwidth, and twice that bandwidth. 
If only a single value is passed into the function,
the RD will similarly be estimated at that bandwidth, half that bandwidth, 
and twice that bandwidth.}

\item{kernel}{A string indicating which kernel to use. Options are \code{"triangular"} 
(default and recommended), \code{"rectangular"}, \code{"epanechnikov"}, \code{"quartic"}, 
\code{"triweight"}, \code{"tricube"}, and \code{"cosine"}.}

\item{se.type}{This specifies the robust standard error calculation method to use,
from the "sandwich" package. Options are,
as in \code{\link[sandwich]{vcovHC}}, \code{"HC3"}, \code{"const"}, \code{"HC"}, \code{"HC0"}, 
\code{"HC1"}, \code{"HC2"}, \code{"HC4"}, \code{"HC4m"}, \code{"HC5"}. 
The default is \code{"HC1"}. This option is overridden by \code{cluster}.}

\item{cluster}{An optional vector of length n specifying clusters within which the errors are assumed
to be correlated. This will result in reporting cluster robust SEs. This option overrides
anything specified in \code{se.type}. It is suggested that data with a discrete running 
variable be clustered by each unique value of the running variable (Lee and Card, 2008).}

\item{verbose}{A logical value indicating whether to print additional information to 
the terminal. The default is \code{FALSE}.}

\item{less}{Logical. If \code{TRUE}, return the estimates of linear and optimal. If \code{FALSE} 
return the estimates of linear, quadratic, cubic, optimal, half and double. The default is \code{FALSE}.}

\item{est.cov}{Logical. If \code{TRUE}, the estimates of covariates will be included.
If \code{FALSE}, the estimates of covariates will not be included. The default is \code{FALSE}. This option is not
applicable if method is \code{"front"}.}

\item{est.itt}{Logical. If \code{TRUE}, the estimates of ITT will be returned.
The default is \code{FALSE}.}

\item{t.design}{A string specifying the treatment option according to design.
Options are \code{"g"} (treatment is assigned if \code{x} is greater than its cutoff),
\code{"geq"} (treatment is assigned if \code{x} is greater than or equal to its cutoff),
\code{"l"} (treatment is assigned if \code{x} is less than its cutoff),
and \code{"leq"} (treatment is assigned if \code{x} is less than or equal to its cutoff).}
}
\value{
\code{rd_est} returns an object of \link{class} "\code{rd}".
  The functions \code{summary} and \code{plot} are used to obtain and print a summary and 
  plot of the estimated regression discontinuity. The object of class \code{rd} is a list 
  containing the following components:
\item{type}{A string denoting either \code{"sharp"} or \code{"fuzzy"} RDD.}
\item{est}{Numeric vector of the estimate of the discontinuity in the outcome under 
  a sharp RDD or the Wald estimator in the fuzzy RDD, for each corresponding bandwidth.}
\item{se}{Numeric vector of the standard error for each corresponding bandwidth.}
\item{z}{Numeric vector of the z statistic for each corresponding bandwidth.}
\item{p}{Numeric vector of the p-value for each corresponding bandwidth.}
\item{ci}{The matrix of the 95% confidence interval, \code{c("CI Lower Bound", "CI Upper Bound")} 
  for each corresponding bandwidth.}
\item{d}{Numeric vector of the effect size (Cohen's d) for each estimate.}
\item{cov}{The names of covariates.}
\item{bw}{Numeric vector of each bandwidth used in estimation.}
\item{obs}{Vector of the number of observations within the corresponding bandwidth.}
\item{call}{The matched call.}
\item{na.action}{The number of observations removed from fitting due to missingness.}
\item{impute}{A logical value indicating whether multiple imputation is used or not.}
\item{model}{For a sharp design, a list of the \code{lm} objects is returned.
  For a fuzzy design, a list of lists is returned, each with two elements: 
  \code{firststage}, the first stage \code{lm} object, and \code{iv}, the \code{ivreg} object. 
  A model is returned for each corresponding bandwidth.}
\item{frame}{Returns the dataframe used in fitting the model.}
}
\description{
\code{rd_est} estimates both sharp and fuzzy RDDs using parametric and non-parametric 
(local linear) models. 
It is based on the \code{RDestimate} function in the "rdd" package.
Sharp RDDs (both parametric and non-parametric) are estimated using \code{lm} in the 
\pkg{stats} package.
Fuzzy RDDs (both parametric and non-parametric) are estimated using two-stage least-squares 
\code{ivreg} in the \pkg{AER} package. 
For non-parametric models, Imbens-Kalyanaraman optimal bandwidths can be used,
}
\examples{
set.seed(12345)
x <- runif(1000, -1, 1)
cov <- rnorm(1000)
y <- 3 + 2 * x + 3 * cov + 10 * (x >= 0) + rnorm(1000)
rd_est(y ~ x, t.design = "geq")
# Efficiency gains can be made by including covariates (review SEs in "summary" output).
rd_est(y ~ x | cov, t.design = "geq")
}
\references{
Lee, D. S., Lemieux, T. (2010).
  Regression Discontinuity Designs in Economics.
  Journal of Economic Literature, 48(2), 281-355. 
  \doi{10.1257/jel.48.2.281}.

Imbens, G., Lemieux, T. (2008).
  Regression discontinuity designs: A guide to practice.
  Journal of Econometrics, 142(2), 615-635. 
  \doi{10.1016/j.jeconom.2007.05.001}.

Lee, D. S., Card, D. (2010).
  Regression discontinuity inference with specification error. 
  Journal of Econometrics, 142(2), 655-674. 
  \doi{10.1016/j.jeconom.2007.05.003}.

Angrist, J. D., Pischke, J.-S. (2009). 
  Mostly harmless econometrics: An empiricist's companion. 
  Princeton, NJ: Princeton University Press.

Drew Dimmery (2016). rdd: Regression Discontinuity Estimation. R package
   version 0.57. https://CRAN.R-project.org/package=rdd

Imbens, G., Kalyanaraman, K. (2009). 
  Optimal bandwidth choice for the regression discontinuity estimator 
  (Working Paper No. 14726). National Bureau of Economic Research.
  \url{https://www.nber.org/papers/w14726}.

Imbens, G., Kalyanaraman, K. (2012). 
  Optimal bandwidth choice for the regression discontinuity estimator. 
  The Review of Economic Studies, 79(3), 933-959.
  \url{https://academic.oup.com/restud/article/79/3/933/1533189}.
}
