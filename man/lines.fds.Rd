\name{lines.fds}
\alias{lines.fds}
\title{Plot functional objects}
\description{
Plot functional curves.
}
\usage{
\method{lines}{fds}(x, plot.type = c("functions", "time", "depth", "density"), index,
 labels = NULL, label.cex = 0.7, col = NULL, lty = 1, 
  pch = c(1:9,0, letters, LETTERS), ...)
}
\arguments{
\item{x}{An object of class \code{\link[rainbow]{fds}} or \code{fts}.}
\item{plot.type}{Type of plot. See details for more explanations.}
\item{index}{Indices of curves. When \code{lines.fd} and \code{points.fd} are called, index allows users to specify 
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
R. J. Hyndman and H. L. Shang. (2010) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Journal of Computational and Graphical Statistics}, \bold{19}(1), 29-45.
}
\author{Rob J Hyndman, Han Lin Shang}
\seealso{\code{\link[rainbow]{fds}}, \code{\link[rainbow]{plot.fds}}, \code{\link[rainbow]{points.fds}}}
\examples{
plot(x = Australiasmoothfertility, plot.type = "functions")
lines(x = Australiasmoothfertility, plot.type = "functions", index = 3)
plot(x = ElNino, plot.type = "functions")
lines(x = ElNino, plot.type = "functions", index = 3)
}
\keyword{hplot}

