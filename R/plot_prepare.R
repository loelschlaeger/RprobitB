#' Prepare Gibbs samples for visualization.
#' @description
#'
#' @inheritParams plot.RprobitB_model
#' @return
#' A list. Each element is a list which has the name of one parameter group that
#' gets visualized (one of \code{c("alpha", "s", "b", "Omega", "Sigma")}) and
#' contains
#' \itemize{
#'   \item a matrix of Gibbs samples names \code{"gibbs_samples"}, where the
#'         columns correspond to the parameter labels,
#'   \item a point estimate \code{"estimate"},
#'   \item a character vector of covariate names (only for \code{"alpha"} and
#'         \code{"b"}).
#' }


plot_prepare = function(x, restrict, symmetric) {

  ### initialize output
  pp = list()

  ### determine names of parameters to plot
  par_names = c()
  if(x$data$P_f > 0)
    par_names = c(par_names, "alpha")
  if(x$data$P_r > 0)
    par_names = c(par_names, "s", "b", "Omega")
  par_names = c(par_names, "Sigma")
  par_names = intersect(par_names, restrict)
  for(par_name in par_names)
    pp[[par_name]] = list()

  ### create parameter labels
  labels_st = create_labels(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
                            C = x$data$true_parameter$C, symmetric = TRUE)
  labels_sf = create_labels(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
                            C = x$data$true_parameter$C, symmetric = FALSE)
  labels = if(symmetric) labels_st else labels_sf
  for(par_name in par_names)
    pp[[par_name]][["labels"]] = labels[[par_name]]

  ### add Gibbs samples
  for(par_name in par_names){
    pos = match(labels[[par_name]], labels_st[[par_name]])
    pp[[par_name]][["gibbs_samples"]] = x$gibbs_samples$gibbs_samples_nbt[[par_name]][,pos]
  }

  ### add point estimate
  point_estimates = compute_point_estimates(x)
  for(par_name in par_names)
    pp[[par_name]][["estimate"]] = point_estimates[[par_name]]

  ### return
  return(pp)

}
