\name{Francemortality}
\alias{Francemortality}
\alias{Francesmoothmortality}
\docType{data}
\title{
French mortality data
}
\description{
Age-specific log mortality rates for France male, observed from ages 0 to 100 between years 1899 and 2005.
}
\usage{
data(Francemortality)
data(Francesmoothmortality)
}
\format{
An object of class \code{\link[rainbow]{fds}} and \code{fts}.
}
\details{
Francesmoothmortality is the smoothed version of Francemortality data. The smoothing technique is the penalized regression spline with monotonic constraint, described in Hyndman and Ullah (2007).  
}
\source{
The Human Mortality Database (\url{http://www.mortality.org}). 
}
\references{
H. Booth and R. J. Hyndman and L. Tickle and P. De Jong (2006) "Lee-Carter mortality forecasting: a multi-country comparison of variants and extensions", \emph{Demographic Research}, \bold{15}, 289-310.

R. J. Hyndman and M. S. Ullah (2006) "Robust forecasting of mortality and fertility rates: A functional data approach", \emph{Computational Statistics \& Data Analysis}, \bold{57}(10), 4942-4956.

R. J. Hyndman and H. Booth (2008) " Stochastic population forecasts using functional data models for mortality, fertility and migration", \emph{International Journal of Forecasting}, \bold{24}(3), 323-342.

R. J. Hyndman and H. Shang (2009) "Forecasting functional time series (with discussion)", \emph{Journal of the Korean Statistical Society}, \bold{38}(3), 199-221.
}
\examples{
plot(Francemortality)
plot(Francesmoothmortality)
}
\keyword{datasets}
