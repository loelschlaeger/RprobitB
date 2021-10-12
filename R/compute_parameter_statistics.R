#' Compute parameter statistics.
#' @description
#' This function computes parameter statistics from the Gibbs samples.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @return
#' An object of class \code{RprobitB_parameter_statistics}, which is a list of
#' statistics from the Gibbs samples for each model parameter \code{alpha},
#' \code{s}, \code{b}, \code{Omega}, \code{Sigma} with
#' \itemize{
#'   \item the parameter indices as rows and
#'   \item \code{mean}, standard deviation \code{sd}, Tukey's
#'         five number summary obtained from
#'         \code{\link[stats]{fivenum}} (i.e. minimum \code{min},
#'         lower-hinge \code{q.25}, \code{median},
#'         upper-hinge \code{q.75}, maximum \code{max}), and the
#'         Gelman-Rubin statistic \code{R_hat} as columns.
#' }

compute_parameter_statistics = function(gibbs_samples, P_f, P_r, J, C) {

  ### create coefficient labels
  labels = create_labels(P_f = P_f, P_r = P_r, J = J, C = C, symmetric = TRUE)

  ### function that computes statistic values
  compute_statistic_values = function(samples_nbt, samples_nb, labels){
    out = matrix(NA, nrow = length(labels), ncol=8)
    out[,1] = apply(samples_nbt,2,mean)
    out[,2] = apply(samples_nbt,2,sd)
    out[,3:7] = t(apply(samples_nbt,2,fivenum))
    out[,8] = apply(samples_nb,2,compute_R_hat)
    rownames(out) = labels
    colnames(out) = c("mean","sd","min","q.25","median","q.75","max","R^")
    return(out)
  }

  ### generate statistics for each parameter
  statistics = list()
  par_names = c("Sigma")
  if(P_f>0)
    par_names = c(par_names, "alpha")
  if(P_r>0)
    par_names = c(par_names, "s","b","Omega")
  for(par in par_names){
    statistics[[par]] = compute_statistic_values(
      samples_nbt = gibbs_samples$gibbs_samples_nbt[[par]],
      samples_nb = gibbs_samples$gibbs_samples_nb[[par]],
      labels = labels[[par]])
  }

  ### add class to 'statistics'
  class(statistics) = "RprobitB_parameter_statistics"

  ### return 'statistics'
  return(statistics)

}
