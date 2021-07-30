#' Construct object of class \code{RprobitB_model}
#' @description
#' Function that constructs an object of class \code{RprobitB_model}.
#' @param RprobitB_data
#'
#' @inheritParams fit
#' @param gibbs_samples
#'
#' @param statistics
#'
#' @return
#' An object of class \code{RprobitB_model}, i.e. a list with the arguments of
#' this function as elements.

RprobitB_model = function(RprobitB_data, scale, R, B, Q, lcus, prior,
                          gibbs_samples, statistics) {

  ### check inputs
  stopifnot(inherits(RprobitB_data,"RprobitB_data"))
  stopifnot(is.list(scale))
  stopifnot(Vectorize(is.natural.number)(c(R,B,Q)))
  stopifnot(inherits(lcus,"RprobitB_lcus"))
  stopifnot(inherits(prior,"RprobitB_prior"))
  stopifnot(inherits(gibbs_samples,"RprobitB_gibbs_samples"))
  stopifnot(inherits(statistics,"RprobitB_statistics"))

  ### create object of class "RprobitB_model"
  out = list("RprobitB_data" = RprobitB_data,
             "scale"         = scale,
             "R"             = R,
             "B"             = B,
             "Q"             = Q,
             "lcus"          = lcus,
             "prior"         = prior,
             "gibbs_samples" = gibbs_samples,
             "statistics"    = statistics)
  class(out) = "RprobitB_model"

  ### return object
  return(out)

}
