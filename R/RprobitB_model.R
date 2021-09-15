#' Construct object of class \code{RprobitB_model}
#' @description
#' Function that constructs an object of class \code{RprobitB_model}.
#' @param data
#' description
#' @inheritParams fit
#' @param gibbs_samples
#' escription
#' @param statistics
#' description
#' @return
#' An object of class \code{RprobitB_model}, i.e. a list with the arguments of
#' this function as elements.

RprobitB_model = function(data, scale, R, B, Q, latent_classes, prior,
                          gibbs_samples, statistics) {

  ### check inputs
  stopifnot(inherits(data,"RprobitB_data"))
  stopifnot(is.list(scale))
  stopifnot(is.numeric(R), R%%1 == 0, R>0)
  stopifnot(is.numeric(B), B%%1 == 0, B>0)
  stopifnot(is.numeric(Q), Q%%1 == 0, Q>0)
  stopifnot(inherits(latent_classes,"RprobitB_latent_classes"))
  stopifnot(inherits(prior,"RprobitB_prior"))
  stopifnot(inherits(gibbs_samples,"RprobitB_gibbs_samples"))
  stopifnot(inherits(statistics,"RprobitB_parameter_statistics"))

  ### create object of class "RprobitB_model"
  out = list("data"  = data,
             "scale"          = scale,
             "R"              = R,
             "B"              = B,
             "Q"              = Q,
             "latent_classes" = latent_classes,
             "prior"          = prior,
             "gibbs_samples"  = gibbs_samples,
             "statistics"     = statistics)
  class(out) = "RprobitB_model"

  ### return object
  return(out)
}
