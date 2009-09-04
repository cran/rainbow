\name{plot.fds}
\alias{plot.fds}
\title{Plot functional objects}
\description{
Plot functional curves.
}
\usage{
\method{plot}{fds}(x, plot.type = c("functions", "time", "depth", "density"),
 labels = NULL, label.cex = 0.7, col = NULL, type = "l", lty = 1,
  xlab = x$xname, ylab = x$yname, pch = c(1:9,0, letters, LETTERS), ...)
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{fds}} or \code{fts}.}
\item{plot.type}{Type of plot. See details for more explanations.}
\item{labels}{Character vector of length \code{length(y$x)}. If \code{plot.type="time"} then the
	labels are printed beside each time plot.}
\item{label.cex}{Character size for labels.}
\item{col}{Colors to use in plot. Default is to use a rainbow color palette with the number of colors equal to the number of functions.}
\item{type}{1-character string giving the type of plot desired.}
\item{lty}{The line type.}
\item{xlab}{A title for x axis.}
\item{ylab}{A title for y axis.}
\item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting points.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}
\value{Function produces a plot.}
\details{
If \code{plot.type="functions"}, then functions are plotted using a rainbow color palette so the
first few functions are shown in red, followed by orange, yellow, green, blue
and indigo with the last few functions plotted in violet.

If \code{plot.type="time"}, then each value of x is shown as a separate time series in a time plot.

If \code{plot.type="depth"}, then functions are first ordered by depth and then plotted using a rainbow color palette.

If \code{plot.type="density"}, then functions are first ordered by density and then plotted using a rainbow color palette.
}
\references{
R. J. Hyndman and H. L. Shang. (2008) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Tech. rep. Department of Econometrics \& Business Statistics}, Monash University, November 2008, \url{http://www.buseco.monash.edu.au/depts/ebs/pubs/wpapers/2008/9-08.php}.
}
\author{Rob J Hyndman, Han Lin Shang}
\seealso{\code{\link[rainbow]{fds}}, \code{\link[rainbow]{lines.fds}}, \code{\link[rainbow]{points.fds}}}
\examples{
plot(x = Francesmoothmortality, plot.type = "time")
plot(x = Francesmoothmortality, plot.type = "depth")
plot(x = Francesmoothmortality, plot.type = "density")
plot(x = Francesmoothmortality, plot.type = "functions")
plot(x = ElNino, plot.type = "time")
plot(x = ElNino, plot.type = "depth")
plot(x = ElNino, plot.type = "density")
plot(x = ElNino, plot.type = "functions")
}
\keyword{hplot}

