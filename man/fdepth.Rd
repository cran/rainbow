\name{fdepth}
\alias{fdepth}
\title{Compute functional depth.}
\description{
Compute functional depth.
}
\usage{
fdepth(data, type = c("FM", "mode", "RP", "RPD"), trim = 0.25)
}
\arguments{
  \item{data}{An object of class \code{\link[rainbow]{fd}} or \code{fts}.}
  \item{type}{Type of functional depth.}
  \item{trim}{Percentage of trimming.}
}
\value{
A list containing the following components is returned.
  \item{median}{Median curve (highest depth).}
  \item{lmed}{Index of median curve.}
  \item{ltrim}{Indexes of the trimmed curves.}
  \item{prof}{Functional depth for each curve.}
  \item{mtrim}{Mean of trimmed curves.}
}
\details{
If \code{type="FM"}, it computes the functional depth of Fraiman and Muniz (2001), which is considered as the first functional depth.

If \code{type="mode"}, it computes the functional depth of Cuevas et al. (2006). A functional mode is defined as the curve most densely surrounded by the rest of curves of the dataset.

If \code{type="RP"} and \code{type="RPD"}, it computes random projection functional depth of Cuevas et al. (2007). 
Cuevas et al. (2007) considered the random projection depth based on measuring the depth of the functional data 
under projections and taking additional information of their derivatives. The basic idea is to project each functional curve, along a random direction, defining a 
point in \eqn{R^2}. A data depth in \eqn{R^2} provides an order of the projected points.

The argument trim=0.25 first order curves by depth, and then trim 25 percent curves that have comparably lower depth.
}
\references{
A. Cuevas and M. Febrero and R. Fraiman (2001) "Cluster Analysis:a further approach based on density estimation", \emph{Computational Statistics \& Data Analysis}, \bold{36}(4), 441-456.

A. Cuevas and M. Febrero and R. Fraiman (2006) "On the use of bootstrap for estimating functions with functional data", \emph{Computational Statistics \& Data Analysis}, \bold{51}(10), 1063-1074.

A. Cuevas and M. Febrero and R. Fraiman (2007) "Robust estimation and classification for functional data via projection-based depth notions", \emph{Computational Statistics}, \bold{22}(3), 481-496.

R. Fraiman and G. Muniz (2001) "Trimmed means for functional data", \emph{Test}, \bold{10}, 419-440.

M. Febrero and P. Galeano and W. Gonzalez-Manteiga (2008) "Outlier detection in functional data by depth measures, with application to identify abnormal NOx levels", \emph{Environmetrics}, \bold{19}(4), 331-345.
}
\author{Han Lin Shang}
\examples{
fdepth(data = ElNino, type = "FM")
fdepth(data = ElNino, type = "mode")
fdepth(data = ElNino, type = "RP")
fdepth(data = ElNino, type = "RPD")
}
\keyword{distribution}

