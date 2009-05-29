\name{plot.fdepth}
\alias{plot.fdepth}
\title{Plot functional depth}
\description{
Plot functional depth.
}
\usage{
\method{plot}{fdepth}(x, show.legend = TRUE, pos.legend = "bottomleft", ...)
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{fdepth}}.}
\item{show.legend}{Is legend required?}
\item{pos.legend}{When \code{show.legend = TRUE}, users can specify the position of the legend.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}
\value{Function produces a plot.}
\references{
R. J. Hyndman and H. L. Shang. (2008) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Tech. rep. 
Department of Econometrics \& Business Statistics}, Monash University, November 2008, 
\url{http://www.buseco.monash.edu.au/depts/ebs/pubs/wpapers/2008/9-08.php}.
}
\author{Rob J Hyndman, Han Lin Shang}
\seealso{\code{\link[rainbow]{fdepth}}}
\examples{
plot(fdepth(ElNino))
}
\keyword{hplot}

