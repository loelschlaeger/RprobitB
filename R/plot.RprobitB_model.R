#' Plot method for \code{RprobitB_model}.
#' @description
#' This function is the plot method for an object of class \code{RprobitB_model}.
#' @param x
#' An object of class \code{\link{RprobitB_model}}.
#' @param type
#' The type of plot, which can be one of:
#' \itemize{
#'   \item \code{"effects"} (the default) for visualizing the effects,
#'   \item \code{"mixture"} for visualizing the mixture distributions,
#'   \item \code{"estimates"} for visualizing the parameter estimates,
#'   \item \code{"acf"} for autocorrelation plots of the Gibbs samples,
#'   \item \code{"trace"} for trace plots of the Gibbs samples.
#' }
#' @param restrict
#' A character (vector) of covariate or parameter names, which can be used to
#' restrict the visualizations to a subset of covariates and parameters,
#' respectively. Per default, a combined plot of all parameters is produced.
#' @param ...
#' Ignored.
#' @return
#' No return value. Draws a plot to the current device.
#' @export

plot.RprobitB_model = function(x, type = "effects", restrict = NULL, ...) {

  ### check inputs
  if(!inherits(x,"RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")
  if(!(length(type) == 1 &&
       type %in% c("effects", "mixture", "estimates", "acf", "trace")))
    stop("Unknown 'type'.")
  if(is.null(restrict))
    restrict = c("alpha", "s", "b", "Omega", "Sigma")
  if(!is.character(restrict))
    stop("'restrict' must be a character vector.")

  ### reset of 'par' settings
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  ### determine names of parameters to plot
  par_names = c()
  if(x$data$P_f > 0)
    par_names = c(par_names, "alpha")
  if(x$data$P_r > 0)
    par_names = c(par_names, "s", "b", "Omega")
  par_names = c(par_names, "Sigma")
  par_names = intersect(par_names, restrict)

  ### determine names of covariates to plot
  cov_names = intersect(x$data$covs$names, restrict)

  ### make plot type 'mixture'
  if(type == "mixture"){
    if(x$data$P_r == 0){
      warning("Plot type 'mixture' invalid because there are no random effects.")
    } else {
      est = compute_point_estimates(x, FUN = mean)
      true = x$data$true_parameter
      C_est = x$latent_classes$C
      C_true = x$data$true_parameter$C
      comb = expand.grid(1:x$data$P_r, 1:x$data$P_r)
      cov_names = x$data$covs$names[x$data$covs$random]
      par(mfrow = set_mfrow(nrow(comb)))
      for(i in 1:nrow(comb)){
        p1 = comb[i,1]
        p2 = comb[i,2]
        if(p1 == p2){
          plot_mixture_marginal(
            mean_est = as.list(est$b[paste0(1:C_est,".",p1)]),
            mean_true = as.list(true$b[paste0(1:C_true,".",p1)]),
            weight_est = est$s,
            weight_true = true$s,
            sd_est = as.list(sqrt(est$Omega[paste0(1:C_est,".",p1,",",p1)])),
            sd_true = as.list(sqrt(true$Omega[paste0(1:C_true,".",p1,",",p1)])),
            cov_name = cov_names[p1]
          )
        } else {
          mean_est = list()
          cov_est = list()
          for(c in 1:C_est){
            mean_est[[c]] = est$b[paste0(c,".",c(p1,p2))]
            cov_est[[c]] = matrix(est$Omega[paste0(c,".",as.vector(outer(c(p1,p2), c(p1,p2), paste, sep=",")))],2,2)
          }
          plot_mixture_contour(
            mean_est = mean_est,
            weight_est = est$s,
            cov_est = cov_est,
            beta_true = x$data$true_parameter$beta[c(p1,p2),],
            cov_names = cov_names[c(p1,p2)]
          )
        }
      }
    }
  }

  ### make plot type 'estimates'
  if(type == "estimates"){
    par(mfrow = c(1,1), mar = c(2,3,0,0) + 0.5)
    plot_estimates(x = x$gibbs_samples,
                   par_names = par_names)
  }

  ### make plot type 'effects'
  if(type == "effects"){
    if(x$data$P_f == 0 && x$data$P_r > 0){
      warning("Plot type 'effects' invalid because there are no effects.")
    } else {
      par(mfrow = c(1,1), mar = c(2,3,0,0) + 0.5)
      plot_effects(x = x$gibbs_samples,
                   par_names = intersect(par_names, c("alpha","b")),
                   cov_names = x$data$covs$names)
    }
  }

  ### make plot type 'trace'
  if(type == "trace"){
    gibbs_samples_nbt_filtered = filter_gibbs_samples(
      x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
      C = x$latent_classes$C, cov_sym = FALSE,
      keep_par = par_names)$gibbs_samples_nbt
    par(mfrow = set_mfrow(length(par_names)),
        mar = c(2,3,0,0) + 0.5)
    for(par_name in par_names){
      gibbs_samples = gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
      plot_trace(
        gibbs_samples = gibbs_samples,
        par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
      )
    }
  }

  ### make plot type 'acf'
  if(type == "acf"){
    gibbs_samples_nbt_filtered = filter_gibbs_samples(
      x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
      C = x$latent_classes$C, cov_sym = FALSE,
      keep_par = par_names)$gibbs_samples_nbt
    par(mfrow = set_mfrow(length(sapply(gibbs_samples_nbt_filtered, colnames))),
        mar = c(2,2,0,0) + 0.5)
    for(i in 1:length(gibbs_samples_nbt_filtered)){
      gibbs_samples = gibbs_samples_nbt_filtered[[i]]
      plot_acf(
        gibbs_samples = gibbs_samples,
        par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
      )
    }
  }
}
