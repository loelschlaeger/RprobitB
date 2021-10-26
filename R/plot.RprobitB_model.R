#' Plot method for \code{RprobitB_model}.
#' @description
#' This function is the plot method for an object of class \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
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
  if(!(length(type) == 1 && type %in% c("effects", "mixture", "estimates", "acf", "trace")))
    stop("Unknown 'type'.")
  if(is.null(restrict))
    restrict = c("alpha", "s", "b", "Omega", "Sigma")
  if(!is.character(restrict))
    stop("'restrict' must be a character vector.")

  ### reset of 'par' settings
  oldpar = par(no.readonly = TRUE)
  on.exit(par(oldpar))

  ### make plot type 'mixture'
  if(type == "mixture"){
    if(x$data$P_r == 0){
      stop("No random effects.")
    } else {
      #par(mfrow = c(length(cov_names), length(cov_names)))
      #pars_pairs = expand.grid(cov_names, cov_names, stringsAsFactors = FALSE)
      for(pars_pair in 1:nrow(pars_pairs)){
        par1 = pars_pairs[pars_pair,1]
        par2 = pars_pairs[pars_pair,2]
        if(par1 == par2){
          plot_marginal(x = x, par = par1)
        } else {
          plot_contour(x = x, par1 = pars_pairs[pars_pair,1],
                       par2 = pars_pairs[pars_pair,1])
        }
      }
    }
  }

  ### make plot type 'effects'
  if(type == "effects")
    plot_effects(x = x, restrict = restrict)

  ### determine names of parameters to plot
  par_names = c()
  if(x$data$P_f > 0)
    par_names = c(par_names, "alpha")
  if(x$data$P_r > 0)
    par_names = c(par_names, "s", "b", "Omega")
  par_names = c(par_names, "Sigma")
  par_names = intersect(par_names, restrict)

  ### make plot type 'estimates'
  if(type == "estimates")
    plot_estimates(x = x, restrict = restrict)

  ### make plot type 'trace'
  if(type == "trace"){
    par(mfrow = set_mfrow(length(par_names)))
    for(par_name in par_names)
      plot_trace(
        gibbs_samples = x$gibbs_samples$gibbs_samples_nbt[[par_name, drop = FALSE]],
        names = paste(par_name, labels[[par_name]], sep = "_")
      )
  }

  ### make plot type 'acf'
  if(type == "acf"){
    par_names_labels = sapply(par_names, function(x) paste(x, labels[[x]], sep = "_"))
    par(mfrow = set_mfrow(length(unlist(par_names_labels))))
    for(par_name in par_names)
      plot_acf(
        gibbs_samples = x$gibbs_samples$gibbs_samples_nbt[[par_name, drop = FALSE]],
        names = paste(par_name, labels[[par_name]], sep = "_")
      )
  }
}
