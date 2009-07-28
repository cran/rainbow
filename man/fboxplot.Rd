\name{fboxplot}
\alias{fboxplot}
\title{Functional bagplot and functional HDR boxplot}
\description{
Compute bivariate bagplot, functional bagplot and bivariate HDR boxplot, functional HDR boxplot.
}
\usage{
fboxplot(data, plot.type = c("functional", "bivariate"),type = c("bag",
 "hdr"), alpha = c(0.01, 0.5), factor = 2.57, na.rm = TRUE, 
  xlab = data$xname, ylab = data$yname, ...) 
}
\arguments{
  \item{data}{An object of class \code{\link[rainbow]{fd}} or \code{fts}.}
  \item{plot.type}{Version of boxplot. When \code{plot.type="functional"}, a functional plot is provided. When \code{plot.type="bivariate"}, a square bivariate plot is provided.}
  \item{type}{Type of boxplot. When \code{type="bag"}, a bagplot is provided. When \code{type="hdr"}, a HDR boxplot is provided.}
  \item{alpha}{Coverage probability for the functional HDR boxplot. \eqn{\alpha}{alpha} are the coverage percentages of the outliers and the central region.}
  \item{factor}{When \code{type="bag"}, the outer region of a bagplot is the convex hull obtained by inflating the inner region by the bagplot factor.}
  \item{na.rm}{Remove missing values.}
  \item{xlab}{A title for the x axis.}
  \item{ylab}{A title for the y axis.}
  \item{...}{Other arguments.}
}
\details{
The functional curves are first projected into a finite dimensional subspace. 
For simiplicity, we choose the subspace as \eqn{R^2}. Based on Tukey (1974)'s halfspace bagplot and 
Hyndman (1996)'s HDR boxplot, we order each data point in \eqn{R^2} by data depth and data density. 
Outliers are those that have either lowest depth or lowest density.
}
\value{
NULL.
}
\references{
J. W. Tukey (1974) "Mathematics and the picturing of data", \emph{Proceedings of the International Congress of Mathematicians}, \emph{2}, 523-532, Canadian Mathematical Congress, Montreal.

R. Hyndman (1996) "Computing and graphing highest density regions", \emph{The American Statistician}, \emph{50}, 120-126.

R. J. Hyndman and H. L. Shang. (2008) "Rainbow plots, bagplots, and boxplots for functional data", \emph{Tech. rep. Department of Econometrics \& Business Statistics}, Monash University, November 2008, \url{http://www.buseco.monash.edu.au/depts/ebs/pubs/wpapers/2008/9-08.php}.
}
\author{Rob J Hyndman, Han Lin Shang}
\examples{
fboxplot(data = ElNino, plot.type = "functional", type = "bag")
fboxplot(data = ElNino, plot.type = "bivariate", type = "bag")
fboxplot(data = ElNino, plot.type = "functional", type = "hdr", alpha = c(0.07,0.5))
fboxplot(data = ElNino, plot.type = "bivariate", type = "hdr", alpha = c(0.07,0.5))
}
\keyword{multivariate}

