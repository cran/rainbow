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
R. J. Hyndman and H. L. Shang. (2009) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{in press}.
}
\examples{
plot(Simulationdata, col = rainbow(100))
lines(Simulationdata, index = 991:1000, col = "black")
}
\keyword{datasets}
