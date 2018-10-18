\name{summaryfunction}
\alias{summaryfunction}
\title{
Summary statistics for functional data
}
\description{
Display summary statistics (minimum, 1st quantile, median, mean, 3rd quantile, maximum) and quantiles of functional data
}
\usage{
summaryfunction(ftsdata, plot.type = c("summarystats", "quantilestats"),
 quantilepercent = seq(0.1, 0.9, by = 0.1), plot.legend = FALSE, 
  legendpos = "topright", cex = 0.9, lwd = 1, lty = 1, ncol = 2)
}
\arguments{
  \item{ftsdata}{An object of class \code{\link{fds}}.}
  \item{plot.type}{Summary statistics or quantiles.}
  \item{quantilepercent}{Percentage of quantiles.}
  \item{plot.legend}{Is the legend required in the plot?}
  \item{legendpos}{When the \code{plot.legend = TRUE}, position of the legend.}
  \item{cex}{When the \code{plot.legend = TRUE}, point size.}
  \item{lwd}{When the \code{plot.legend = TRUE}, width of line.}
  \item{lty}{When the \code{plot.legend = TRUE}, line type.}
  \item{ncol}{When the \code{plot.legend = TRUE}, number of columns in the legend.}
}
\details{
A function for displaying summary statistics or quantiles of functional data.
}
\value{
Return a plot of summary statistics of functional data or a plot of quantiles of functional data.
}
\author{
Han Lin Shang.  Please, report bugs and suggestions to hanlin.shang@anu.edu.au
}
\seealso{
\code{\link{fds}}
}
\examples{
summaryfunction(Australiasmoothfertility, plot.type = "summarystats")
summaryfunction(Australiasmoothfertility, plot.type = "quantilestats", plot.legend = TRUE)
}
\keyword{methods}
