#' Check \code{distr}
#' @description
#' Function that checks the input \code{distr}. If \code{distr} is \code{NULL},
#' number generation functions of the standard normal distributions are added.
#' @param distr
#' A list of number generation functions from which the covariates
#' are drawn. Each element of \code{distr} must be of the form
#' \code{"name" = list(pars)}, where \code{"name"} is the name of the number
#' generation function and \code{pars} are required parameters.
#' Possible number generation functions are
#' \itemize{
#'   \item functions of the type \code{r*} from base R (e.g. \code{rnorm}) where
#'         all required parameters (except for \code{n}) must be specified,
#'   \item the function \code{sample}, where all required parameters
#'         (except for \code{size}) must be specified.
#' }
#' The order is the same as in \code{form}, i.e. the first element of
#' \code{distr} draws the first covariate of \code{form}.
#' If \code{distr} is not specified, all covariates are drawn from a
#' standard normal distribution.
#' If \code{distr} is specified, the length of \code{distr} must equal the
#' number of covariates.
#' @param no_cov
#' The non-negative number of covariates.
#' @examples
#' check_distr(distr = list("rgamma" = list(shape = 1),
#'                          "sample" = list(x = 1:10, replace = TRUE)),
#'             no_cov = 2)
#' @return
#' The checked input \code{distr}

check_distr = function(distr, no_cov) {

  ### if 'distr' is NULL, add rnorm
  if(is.null(distr))
    distr = rep(list("rnorm" = list("mean" = 0, "sd" = 1)), no_cov)

  ### check if 'distr' is a list
  stopifnot(is.list(distr))

  ### check if 'distr' has correct length
  if(length(distr) != no_cov)
    stop(paste0("The number '",length(distr),"' of elements in 'distr' does
                not match the number '",no_cov,"' of covariates"))

  ### check if 'distr' has proper elements
  for(i in 1:length(distr)){

    ### set 'size' and 'n' arguments to 1
    if(names(distr)[i] == "sample") distr[[i]]["size"] = 1
    if(grepl("^r",names(distr)[i])) distr[[i]]["n"] = 1

    ### check if element in 'distr' gives single numeric draw
    out = try(do.call(names(distr)[i], distr[[i]]), silent = TRUE)
    if(inherits(out,"try-error") || length(out) != 1 || !is.numeric(out))
      stop(paste("Could not interpret element number",i,"in 'distr'."))
  }

  ### add class to 'distr'
  class(lcus) = "RprobitB_distr"

  ### return checked 'distr'
  return(distr)

}
