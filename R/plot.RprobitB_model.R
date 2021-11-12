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
#' @param ignore
#' A character (vector) of covariate or parameter names that do not get
#' visualized.
#' @param ...
#' Ignored.
#' @return
#' No return value. Draws a plot to the current device.
#' @export

plot.RprobitB_model = function(x, type = "effects", ignore = NULL, ...) {

  ### check inputs
  if(!inherits(x,"RprobitB_model"))
    stop("Not of class 'RprobitB_model'.")
  if(!(length(type) == 1 && type %in% c("effects", "mixture", "acf", "trace")))
    stop("Unknown 'type'.")

  ### reset of 'par' settings
  oldpar = par(no.readonly = TRUE)
  on.exit(suppressWarnings(par(oldpar)))

  ### determine 'par_names' and 'linear_coeffs'
  par_names = c(if(x$data$P_f > 0) "alpha",
                if(x$data$P_r > 0) c("s", "b", "Omega"), "Sigma")
  par_names = setdiff(par_names, ignore)
  linear_coeffs = x$data$linear_coeffs[!x$data$linear_coeffs$name %in% ignore, ]
  linear_coeffs_fe = linear_coeffs[linear_coeffs$re == FALSE, ]
  linear_coeffs_re = linear_coeffs[linear_coeffs$re == TRUE, ]
  linear_coeffs_re_orig = linear_coeffs_re
  if(x$latent_classes$C > 1){
    for(i in 1:x$data$P_r){
      for(c in 1:x$latent_classes$C){
        linear_coeffs_re[nrow(linear_coeffs_re) + 1, ] =
          c(paste0(linear_coeffs_re[1,"name"],"_",c), linear_coeffs_re[1,"re"])
      }
      linear_coeffs_re = linear_coeffs_re[-1, ]
    }
  }
  linear_coeffs = rbind(linear_coeffs_fe, linear_coeffs_re)

  ### make plot type 'effects'
  if(type == "effects"){
    if(is.null(linear_coeffs$name) || all(!is.element(c("alpha","b"),par_names))){
      warning("Type 'effects' invalid because there are no effects.")
    } else {
      par(mfrow = c(1,1), oma = c(0,0,0,0),
          mar = c(3, 6, 1, 1), mgp = c(2, 1, 0), xpd = FALSE)
      plot_effects(gibbs_samples = x$gibbs_samples,
                   coeff_names = linear_coeffs$name)
    }
  }

  ### make plot type 'mixture'
  if(type == "mixture"){
    if(is.null(linear_coeffs_re_orig$name) || !is.element("b",par_names)){
      warning("Type 'mixture' invalid because there are no random effects.")
    } else {
      est = compute_point_estimates(x, FUN = mean)
      true = x$data$true_parameter
      comb = expand.grid(1:length(linear_coeffs_re_orig$name),
                         1:length(linear_coeffs_re_orig$name))
      par(mfrow = set_mfrow(nrow(comb)), oma = c(1, 1, 1, 1),
          mar = c(3, 3, 0, 0), mgp = c(2, 1, 0), xpd = NA)
      for(i in 1:nrow(comb)){
        p1 = comb[i,1]
        p2 = comb[i,2]

        if(p1 == p2){
          ### marginal plots
          mean_est = list()
          sd_est = list()
          weight_est = est$s
          for(c in 1:x$latent_classes$C){
            mean_est[[c]] = est$b[paste0(c,".",p1)]
            sd_est[[c]] = sqrt(est$Omega[paste0(c,".",p1,",",p1)])
          }
          if(is.null(true)){
            mean_true = NULL
            sd_true = NULL
            weight_true = TRUE
          } else {
            mean_true = list()
            sd_true = list()
            weight_true = true$s
            for(c in 1:x$data$true_parameter$C){
              mean_true[[c]] = est$b[paste0(c,".",p1)]
              sd_true[[c]] = sqrt(true$Omega[paste0(c,".",p1,",",p1)])
            }
          }
          plot_mixture_marginal(
            mean_est = mean_est,
            mean_true = mean_true,
            weight_est = weight_est,
            weight_true = weight_true,
            sd_est = sd_est,
            sd_true = sd_true,
            cov_name = linear_coeffs_re$name[p1]
          )
        } else {
          ### contour plots
          mean_est = list()
          cov_est = list()
          for(c in 1:x$latent_classes$C){
            mean_est[[c]] = est$b[paste0(c,".",c(p1,p2))]
            cov_est[[c]] = matrix(est$Omega[paste0(c,".",
                    as.vector(outer(c(p1,p2), c(p1,p2), paste, sep=",")))],2,2)
          }
          if(is.null(true)){
            beta_true = NULL
          } else {
            beta_true = x$data$true_parameter$beta[c(p1,p2),]
          }
          plot_mixture_contour(
            mean_est = mean_est,
            weight_est = est$s,
            cov_est = cov_est,
            beta_true = beta_true,
            cov_names = linear_coeffs_re_orig$name[c(p1,p2)]
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
      par(mfrow = set_mfrow(length(par_names)), oma = c(0,0,0,0),
          mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), xpd = FALSE, las = 1)
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
      ### remove parameters
      if(x$latent_classes$C == 1){
        keep_par = setdiff(par_names, "s")
      } else {
        keep_par = par_names
      }
      gibbs_samples_nbt_filtered = filter_gibbs_samples(
        x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
        C = x$latent_classes$C, cov_sym = FALSE,
        keep_par = keep_par)$gibbs_samples_nbt
      par(mfrow = set_mfrow(sum(sapply(gibbs_samples_nbt_filtered, ncol))),
          oma = c(0,0,0,0), mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), xpd = FALSE,
          las = 1)
      for(par_name in keep_par){
        gibbs_samples = gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
        plot_acf(
          gibbs_samples = gibbs_samples,
          par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
        )
      }
    }
  }
}
