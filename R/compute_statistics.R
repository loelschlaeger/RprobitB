#' Compute statistics
#' @description
#' Function that computes statistics from the Gibbs samples.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}, i.e. the output of
#' \code{\link{transform_gibbs_samples}}.
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @return
#' A list of statistics from the Gibbs samples for each model parameter
#' \code{alpha}, \code{s}, \code{b}, \code{Omega}, \code{Sigma} with:
#' \itemize{
#'   \item the parameter indices as rows
#'   \item \code{mean}, standard deviation \code{sd}, Tukey's
#'         five number summary obtained from \code{\link[stats]{fivenum}} (i.e.
#'         minimum \code{min}, lower-hinge \code{q.25}, \code{median},
#'         upper-hinge \code{q.75}, maximum \code{max}), Gelman-Rubin
#'         statistic \code{R_hat} as columns
#' }
#' Appended is the element \code{C_est}, the estimated number of latent classes.

compute_statistics = function(gibbs_samples, P_f, P_r, J, C) {

  ### determine estimated number of latent classes
  last_s_draw =
    gibbs_samples$gibbs_samples_raw$
    s_draws[nrow(gibbs_samples$gibbs_samples_raw$s_draws),]
  C_est = length(last_s_draw[last_s_draw!=0])

  ### get coefficient labels
  labels = create_labels(P_f = P_f, P_r = P_r, J = J, C = C, C_est = C_est,
                         symmetric = TRUE)

  ### compute R_hat (Gelman-Rubin statistic)
  ### https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html
  ### samples_nb: normalized and burned but not thinned Gibbs samples
  ### parts: number of parts to divide the Gibbs samples
  compute_R_hat = function(samples_nb, parts=2){
    sub_chains = split(samples_nb,cut(seq_along(samples_nb),parts,labels=FALSE))
    L = length(samples_nb)/parts
    chain_means = sapply(sub_chains,mean)
    grand_mean = mean(chain_means)
    B = 1/(parts-1)*sum((chain_means-grand_mean)^2)
    chain_variances = sapply(sub_chains,var)
    W = sum(chain_variances)/parts
    R_hat = ((L-1)/L*W+B)/W
    return(R_hat)
  }

  ### function that computes statistic values
  compute_statistic_values = function(samples_nbt, samples_nb, labels){
    out = matrix(NA,nrow=length(labels),ncol=8)
    out[,1] = apply(samples_nbt,2,mean)
    out[,2] = apply(samples_nbt,2,sd)
    out[,3:7] = apply(samples_nbt,2,fivenum)
    out[,8] = apply(samples_nb,2,compute_R_hat)
    rownames(out) = labels
    colnames(out) = c("mean","sd","min","q.25","median","q.75","max","R^")
    return(out)
  }

  ### generate statistics for each parameter
  statistics = list()
  par_names = c("Sigma")
  if(P_f>0) par_names = c(par_names, "alpha")
  if(P_r>0) par_names = c(par_names, "s","b","Omega")
  for(par in par_names){
    statistics[[par]] = compute_statistic_values(
      samples_nbt = gibbs_samples$gibbs_samples_nbt[[par]],
      samples_nb = gibbs_samples$gibbs_samples_nb[[par]],
      labels = labels[[par]])
  }

  ### append 'C_est'
  statistics[["C_est"]] = C_est

  ### add class to 'statistics'
  class(statistics) = "RprobitB_statistics"

  ### return 'statistics'
  return(statistics)

}
