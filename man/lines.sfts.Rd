\name{lines.sfts}
\alias{lines.sfts}
\title{Plot sliced functional objects}
\description{
Plot sliced functional curves.
}
\usage{
\method{lines}{sfts}(x, plot.type = c("functions", "time", "depth", "density"), index,
 labels = NULL, label.cex = 0.7, col = NULL, lty = 1, 
  pch = c(1:9,0, letters, LETTERS), ...)
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{sfts}.}}
\item{plot.type}{Type of plot. See details for more explanations.}
\item{index}{Indices of curves. When \code{lines.fd} and \code{points.fd} are called, index allows users to 
    specify
    which curve or curves are plotted. For instance, when index=2, the second curve ordered by time or depth or
    density is plotted.}
\item{labels}{Character vector of length \code{length(y$x)}. If \code{plot.type="time"} then the
	labels are printed beside each time plot.}
\item{label.cex}{Character size for labels.}
\item{col}{Colors to use in plot. Default is to use a rainbow color palette with the number of colors equal to the
    number of functions.}
\item{lty}{The line type.}
\item{pch}{Either an integer specifying a symbol or a single character to be used as the default in plotting
    points.}
\item{...}{Other plotting parameters passed to \code{\link[graphics]{par}}.}
}
\value{Function produces a plot.}
\details{
If \code{plot.type="functions"}, then functions are plotted using a rainbow color palette so the
first few functions are shown in red, followed by orange, yellow, green, blue
and indigo with the last few functions plotted in violet.

If \code{plot.type="time"}, then each value of x is shown as a separate time series in a time plot.

If \code{plot.type="depth"}, then functions are first ordered by depth and then plotted using a rainbow color
palette.

If \code{plot.type="density"}, then functions are first ordered by density and then plotted using a rainbow color
palette.
}
\references{
R. J. Hyndman and H. L. Shang. (2008) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Tech. 
rep.
Department of Econometrics \& Business Statistics}, Monash University, November 2008,
\url{http://www.buseco.monash.edu.au/depts/ebs/pubs/wpapers/2008/9-08.php}.
}
\author{Rob J Hyndman, Han Lin Shang}
\seealso{\code{\link[rainbow]{sfts}}, \code{\link[rainbow]{plot.sfts}}, \code{\link[rainbow]{points.sfts}}}
\examples{
plot(x = ElNino, plot.type = "functions")
lines(x = ElNino, plot.type = "functions", index = 3)
}
\keyword{hplot}

