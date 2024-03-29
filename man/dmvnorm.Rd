% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{dmvnorm}
\alias{dmvnorm}
\title{Density of multivariate normal distribution}
\usage{
dmvnorm(x, mean, Sigma, log = FALSE)
}
\arguments{
\item{x}{A quantile vector of length \code{n}.}

\item{mean}{The mean vector of length \code{n}.}

\item{Sigma}{The covariance matrix of dimension \code{n} x \code{n}.}

\item{log}{A boolean, if \code{TRUE} the logarithm of the density value is returned.}
}
\value{
The density value.
}
\description{
This function computes the density of a multivariate normal distribution.
}
\examples{
x = c(0,0)
mean = c(0,0)
Sigma = diag(2)
dmvnorm(x = x, mean = mean, Sigma = Sigma)
dmvnorm(x = x, mean = mean, Sigma = Sigma, log = TRUE)
}
\keyword{distribution}
\keyword{internal}
