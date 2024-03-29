% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rtnorm}
\alias{rtnorm}
\title{Draw from one-sided truncated normal}
\usage{
rtnorm(mu, sig, trunpt, above)
}
\arguments{
\item{mu}{The mean.}

\item{sig}{The standard deviation.}

\item{trunpt}{The truncation point.}

\item{above}{A boolean, if \code{TRUE} truncate from above, otherwise from below.}
}
\value{
A numeric value.
}
\description{
This function draws from a one-sided truncated univariate normal
distribution.
}
\examples{
### samples from a standard normal truncated at 1 from above
R <- 1e4
draws <- replicate(R, rtnorm(0,1,1,TRUE))
plot(density(draws))
}
\keyword{distribution}
\keyword{internal}
