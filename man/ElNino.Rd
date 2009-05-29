\name{ElNino}
\alias{ElNino}
\docType{data}
\title{Sea surface temperature data set}
\description{
Monthly sea surface temperature from January 1951 to December 2007. 
}
\usage{data(ElNino)}
\format{
An object of class \code{\link[rainbow]{fd}}.
}
\details{
These temperatures are measured by the moored buoys in the "Nino region" defined by the coordinates 0-10 degree South and 90-80 degree West.
}
\source{
 The web link is \url{http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices}. The data is the third column with the title NINO1+2.
}
\references{
P. C. Besse and H. Cardot and D. B. Stephenson (2000) "Autoregressive forecasting of some functional climatic variations", \emph{Scandinavian Journal of Statistics}, \bold{27}(4), 673-687.

A. Antoniadis and T. Sapatinas (2003) "Wavelet methods for continuous-time prediction using Hilbert-valued autoregressive processes", \emph{Journal of Multivariate Analysis}, \bold{87}(1), 133-158.

F. Ferraty and A. Rabhi and P. Vieu (2005) "Conditional quantiles for dependent functional data with application to the climate El Nino phenomenon", \emph{Sankhya: The Indian Journal of Statistics}, \bold{67}(2), 378-398.
}
\examples{
plot(ElNino)
}
\keyword{datasets}
