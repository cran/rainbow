\name{fd}
\alias{fd}
\alias{fts}
\title{Create functional objects}
\description{
\code{fd} is used to create functional data sets. 
\code{fts} is used to create functional time series. 
}
\usage{
fd(x, y, xname, yname)
fts(x, y, start = 1, frequency = 1, xname, yname)
}
\arguments{
\item{x}{Numeric vector of length \eqn{p}{p}.}
\item{y}{Matrix of size \eqn{p\times n}{p x n} representing \eqn{n}{n}
    functions of \eqn{x}{x} observed at points \eqn{1,\dots,p}{1,...,p}.}
\item{start}{The time of the first observation. Either a single number or a vector
    of two integers, which specify a natural time unit and a (1-based) number of
    samples into the time unit. See \code{\link[stats]{ts}} for details.}
\item{frequency}{The number of observations per unit of time.}
\item{xname}{Character string giving name of \code{x} vector. (optional)}
\item{yname}{Character string giving name of \code{y} vector. (optional)}
}
\value{An object of class \code{fd} or \code{fts}.}
\author{Rob J Hyndman}
\examples{
fts(x = 1:12, y = ElNino$y, xname = "Month", yname = "Sea surface temperature")
}
\keyword{ts}

