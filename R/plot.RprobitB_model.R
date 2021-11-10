#' Plot method for \code{RprobitB_model}.
#' @description
#' This function is the plot method for an object of class
#' \code{RprobitB_model}.
#' @param x
#' An object of class \code{\link{RprobitB_model}}.
#' @param type
#' The type of plot, which can be one or more of:
#' \itemize{
#'   \item \code{"effects"} (the default) for visualizing the linear effects,
#'   \item \code{"mixture"} for visualizing the mixture distribution,
#'   \item \code{"acf"} for autocorrelation plots of the Gibbs samples,
#'   \item \code{"trace"} for trace plots of the Gibbs samples.
#' }
#' @param restrict
#' A character (vector) of covariate or parameter names, which can be used to
#' restrict the visualizations.
#' @param ...
#' Ignored.
#' @return
#' No return value. Draws a plot to the current device.
#' @export

plot.RprobitB_model = function(x, type = "effects", restrict = NULL, ...) {

  ### check inputs
  if(!inherits(x,"RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")
  if(!(length(type) == 1 && type %in% c("effects", "mixture", "acf", "trace")))
    stop("Unknown 'type'.")
  if(is.null(restrict))
    restrict = c("alpha","s","b","Omega","Sigma",x$data$linear_coeffs$name)
  if(!is.character(restrict))
    stop("'restrict' must be a character vector.")

  ### reset of 'par' settings
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  ### determine 'par_names' and 'coeff_names'
  par_names = c(if(x$data$P_f > 0) "alpha",
                if(x$data$P_r > 0) c("s", "b", "Omega"), "Sigma")
  par_names = intersect(par_names, restrict)
  coeff_names = x$data$linear_coeffs$name
  coeff_names = intersect(coeff_names, restrict)

  ### make plot type 'effects'
  if(type == "effects"){
    if(is.null(coeff_names) || all(!is.element(c("alpha","b"),par_names))){
      warning("Type 'effects' invalid because there are no effects.")
    } else {
      plot_effects(x = x$gibbs_samples, coeff_names = coeff_names)
    }
  }

  ### make plot type 'mixture'
  if(type == "mixture"){
    if(is.null(coeff_names) || !is.element("b",par_names)){
      warning("Type 'mixture' invalid because there are no random effects.")
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

  ### make plot type 'trace'
  if(type == "trace"){
    if(is.null(par_names)){
      warning("Type 'trace' invalid because there are no parameters.")
    } else {
      gibbs_samples_nbt_filtered = filter_gibbs_samples(
        x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
        C = x$latent_classes$C, cov_sym = FALSE,
        keep_par = par_names)$gibbs_samples_nbt
      par(mfrow = set_mfrow(length(par_names)))
      for(par_name in par_names){
        gibbs_samples = gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
        plot_trace(
          gibbs_samples = gibbs_samples,
          par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
        )
      }
    }
  }

  ### make plot type 'acf'
  if(type == "acf"){
    if(is.null(par_names)){
      warning("Type 'trace' invalid because there are no parameters.")
    } else {
      gibbs_samples_nbt_filtered = filter_gibbs_samples(
        x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
        C = x$latent_classes$C, cov_sym = FALSE,
        keep_par = par_names)$gibbs_samples_nbt
      par(mfrow = set_mfrow(sum(sapply(gibbs_samples_nbt_filtered, ncol))))
      for(par_name in par_names){
        gibbs_samples = gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
        plot_acf(
          gibbs_samples = gibbs_samples,
          par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
        )
      }
    }
  }
}
