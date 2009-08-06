\name{ElNino}
\alias{ElNino}
\docType{data}
\title{Sea surface temperature data set}
\description{
Monthly sea surface temperatures from January 1951 to December 2007. 
}
\usage{data(ElNino)}
\format{
An object of class \code{\link[rainbow]{sfts}}.
}
\details{
These averaged monthly sea surface temperatures are measured by the different moored buoys in the "Nino region" defined by the coordinates 0-10 degree South and 90-80 degree West.
}
\source{
National Weather Service Climate Prediction Center website at \url{http://www.cpc.ncep.noaa.gov/data/indices/sstoi.indices}. The data is the third column with the title NINO1+2.
}
\references{
A. Antoniadis and T. Sapatinas (2003) "Wavelet methods for continuous-time prediction using Hilbert-valued autoregressive processes", \emph{Journal of Multivariate Analysis}, \bold{87}(1), 133-158.

P. C. Besse, H. Cardot and D. B. Stephenson (2000) "Autoregressive forecasting of some functional climatic variations", \emph{Scandinavian Journal of Statistics}, \bold{27}(4), 673-687.

F. Ferraty, A. Rabhi and P. Vieu (2005) "Conditional quantiles for dependent functional data with application to the climate EL Nino Phenomenon", \emph{Sankhya: The Indian Journal of Statistics}, \bold{67}(2), 378-398.

F. Ferraty and P. Vieu (2007) \emph{Nonparametric functional data analysis}, New York: Springer.

E. Moran, R. Adams, B. Bakoyema, S. Fiorini and B. Boucek (2006) "Human strategies for coping with El Nino related drought in Amazonia", \emph{Climatic Change}, \bold{77}(3-4), 343-361.

A. Timmermann, J. Oberhuber, A. Bacher, M. Esch, M. Latif and E. Roeckner (1999) "Increased El Nino frequency in a climate model forced by future greenhouse warming", \emph{Nature}, \bold{398}(6729), 694-697.   
}
\examples{
plot(ElNino)
}
\keyword{datasets}
