% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_fitting.R
\name{transform_gibbs_samples}
\alias{transform_gibbs_samples}
\title{Transformation of Gibbs samples}
\usage{
transform_gibbs_samples(gibbs_samples, R, B, Q, normalization)
}
\arguments{
\item{gibbs_samples}{The output of \code{\link{gibbs_sampling}}, i.e. a list of Gibbs samples for
\itemize{
\item \code{Sigma},
\item \code{alpha} (if \code{P_f>0}),
\item \code{s}, \code{z}, \code{b}, \code{Omega} (if \code{P_r>0}).
}}

\item{R}{The number of iterations of the Gibbs sampler.}

\item{B}{The length of the burn-in period, i.e. a non-negative number of samples to
be discarded.}

\item{Q}{The thinning factor for the Gibbs samples, i.e. only every \code{Q}th
sample is kept.}

\item{normalization}{An object of class \code{RprobitB_normalization}, which can be created
via \code{\link{RprobitB_normalization}}.}
}
\value{
A list, the first element \code{gibbs_sampes_raw} is the input
\code{gibbs_samples}, the second element is the normalized, burned, and
thinned version of \code{gibbs_samples} called \code{gibbs_samples_nbt}.
The list gets the class \code{RprobitB_gibbs_samples}.
}
\description{
This function normalizes, burns and thins the Gibbs samples.
}
\keyword{internal}
