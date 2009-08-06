\name{Simulationdata}
\alias{Simulationdata}
\docType{data}
\title{
Simulated data
}
\description{
Simulated data used in Hyndman and Shang (2008).
}
\usage{data(Simulationdata)}
\format{
An object of class \code{\link[rainbow]{fds}}.
}
\references{
R. J. Hyndman and H. L. Shang. (2008) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Tech. rep. Department of Econometrics \& Business Statistics}, Monash University, November 2008, \url{http://www.buseco.monash.edu.au/depts/ebs/pubs/wpapers/2008/9-08.php}.
}
\examples{
plot(Simulationdata, col = rainbow(100))
lines(Simulationdata, index = 991:1000, col = "black")
}
\keyword{datasets}
