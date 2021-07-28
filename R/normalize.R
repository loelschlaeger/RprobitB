#' Normalize choice data and true model parameters
#' @description
#' Function that normalizes choice data and (if available) the true model
#' parameters.
#' @details
#' Per default, utility differences are taken with respect to the last
#' alternative (\code{level=J}) and the first error-term variance is fixed to 1
#' (i.e. \code{scale = list("parameter" = "s", "index" = 1, "value" = 1)}).
#'
#' For more details see the vignette "How to normalize the choice model?":
#' \code{vignette("How to normalize the choice model?", package = "RprobitB")}
#' @param RprobitB_data
#' An object of class \code{RprobitB_data}
#' @param level
#' The number of choice alternative (or \code{"last"} for the last alternative)
#' chosen as the base alternative for utility differences to normalize for the
#' utility level.
#' @param scale
#' A list of three elements, determining the normalization with respect to
#' utility scale:
#' \itemize{
#'   \item \code{parameter}:
#'   either \code{"a"} (for a linear coefficient in \code{"alpha"}) or
#'   \code{"s"} (for a variance in the error-term covariance matrix
#'   \code{"Sigma"})
#'   \item \code{index}:
#'   the index of the parameter that gets fixed
#'   \item \code{value}:
#'   the value for the fixed parameter
#' }

normalize = function(RprobitB_data, level = "last",
                     scale = list("parameter" = "s", "index" = 1, "value" = 1)){

  ### check input
  if(!is.RprobitB_data(RprobitB_data))
    stop("Not an 'RprobitB_data' object.")
  if(level == "last")
    level = RprobitB_data[["J"]]
  if(!is.natural.number(level) || level > RprobitB_data[["J"]])
    stop(paste("'level' must be a non-negative number between 1 and",
               RprobitB_data[["J"]]))
  if(!is.list(scale))
    stop("'scale' must be a list")
  if(is.null(scale[["parameter"]]))
    scale[["parameter"]] = "s"
  if(!scale[["parameter"]] %in% c("s","a"))
    stop("'scale$parameter' must be one of 'a' or 's'.")
  if(is.null(scale[["index"]]))
    scale[["index"]] = 1
  if(scale[["parameter"]] == "a" &&
     !scale[["index"]] %in% seq_len(RprobitB_data[["P_f"]]))
    stop("'scale$index' is out of bound.")
  if(scale[["parameter"]] == "s" &&
     !scale[["index"]] %in% seq_len(RprobitB_data[["J"]]))
    stop("'scale$index' is out of bound.")
  if(is.null(scale[["value"]]))
    scale[["value"]] = 1
  if(!is.numeric(scale[["value"]]) || length(scale[["value"]])!=1 ||
     scale[["value"]] == 0)
    stop("'scale$value' must be a single numeric value not equal to zero.")
  if(scale[["parameter"]] == "s" && scale[["value"]] < 0)
    stop("'scale$value' must be non-negative.")

  ### define difference operator (computes differences wrt alternative i)
  J = RprobitB_data[["J"]]
  Delta = function(i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,i] = -1
    return(Delta)
  }

  ### normalize 'data'
  N = RprobitB_data[["N"]]
  T = RprobitB_data[["T"]]
  for(n in seq_len(N)){
    for(t in seq_len(T[n])){
      RprobitB_data[["data"]][[n]]$X[[t]] =
        Delta(level) %*% RprobitB_data[["data"]][[n]]$X[[t]]
    }
  }

  ### normalize 'U' in 'parm'
  RprobitB_data[["parm"]][["U"]] = Delta(level) %*%
    RprobitB_data[["parm"]][["U"]]

  ### normalize parameters in 'parm'
  if(scale[["parameter"]]=="a"){
    RprobitB_data[["parm"]][["alpha"]] =
      RprobitB_data[["parm"]][["alpha"]] * scale[["value"]] /
      RprobitB_data[["parm"]][["alpha"]][scale[["index"]]]


    b_draws_n     = if(P_r>0) norm$value * gibbs_loop_out$b_draws     / gibbs_loop_out$alpha_draws[,norm$index]
    Omega_draws_n = if(P_r>0) norm$value * norm$value * gibbs_loop_out$Omega_draws / (gibbs_loop_out$alpha_draws[,norm$index] * gibbs_loop_out$alpha_draws[,norm$index])
    Sigma_draws_n =           norm$value * norm$value * gibbs_loop_out$Sigma_draws / (gibbs_loop_out$alpha_draws[,norm$index] * gibbs_loop_out$alpha_draws[,norm$index])
    beta
  }
  if(scale[["parameter"]]=="s"){
    column = (model$J-1)*(norm$index-1)+norm$index
    alpha_draws_n = if(P_f>0) sqrt(norm$value) * gibbs_loop_out$alpha_draws / sqrt(gibbs_loop_out$Sigma_draws[,column])
    b_draws_n     = if(P_r>0) sqrt(norm$value) * gibbs_loop_out$b_draws     / sqrt(gibbs_loop_out$Sigma_draws[,column])
    Omega_draws_n = if(P_r>0) norm$value * gibbs_loop_out$Omega_draws / gibbs_loop_out$Sigma_draws[,column]
    Sigma_draws_n =           norm$value * gibbs_loop_out$Sigma_draws / gibbs_loop_out$Sigma_draws[,column]
    beta
  }

  ### return normalized RprobitB_data
  return(RprobitB_data)

}
