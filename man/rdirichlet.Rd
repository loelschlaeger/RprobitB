% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rdirichlet}
\alias{rdirichlet}
\title{Draw from Dirichlet distribution}
\usage{
rdirichlet(delta)
}
\arguments{
\item{delta}{A vector, the concentration parameter.}
}
\value{
A vector, the sample from the Dirichlet distribution of the same length as \code{delta}.
}
\description{
Function to draw from a Dirichlet distribution.
}
\examples{
rdirichlet(delta = 1:3)
}
\keyword{distribution}
\keyword{internal}
