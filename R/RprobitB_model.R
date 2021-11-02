#' Create object of class \code{RprobitB_model}.
#' @description
#' This function creates an object of class \code{RprobitB_model}.
#' @inheritParams mcmc
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @return
#' An object of class \code{RprobitB_model}, i.e. a list with the arguments of
#' this function as elements.

RprobitB_model = function(data, normalization, R, B, Q, latent_classes, prior,
                          gibbs_samples, classification) {

  ### check inputs
  stopifnot(inherits(data,"RprobitB_data"))
  stopifnot(inherits(normalization,"RprobitB_normalization"))
  stopifnot(is.numeric(R), R%%1 == 0, R>0)
  stopifnot(is.numeric(B), B%%1 == 0, B>0)
  stopifnot(is.numeric(Q), Q%%1 == 0, Q>0)
  stopifnot(inherits(latent_classes,"RprobitB_latent_classes"))
  stopifnot(inherits(prior,"RprobitB_prior"))
  stopifnot(inherits(gibbs_samples,"RprobitB_gibbs_samples"))

  ### create and return object of class "RprobitB_model"
  out = list("data"           = data,
             "normalization"  = normalization,
             "R"              = R,
             "B"              = B,
             "Q"              = Q,
             "latent_classes" = latent_classes,
             "prior"          = prior,
             "gibbs_samples"  = gibbs_samples,
             "classification" = classification)
  class(out) = "RprobitB_model"
  return(out)
}
