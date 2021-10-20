#'
#' @description
#' Filter for symmetric parameters
#' @param x
#' An object of class \code{RprobitB_model}.

filter_gibbs_samples = function(x, drop_symmetric) {

  ### filter out symmetric Gibbs samples
  if(drop_symmetric){
    gibbs_samples = x$gibbs_samples$gibbs_samples
    labels_sf = create_labels(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
                              C = x$data$true_parameter$C, symmetric = FALSE)
    for(par in names(gibbs_samples)){
      gibbs_samples[[par]] = gibbs_samples[[par]][,labels_sf[[par]], drop = FALSE]
    }
    x$gibbs_samples = transform_gibbs_samples(
      gibbs_samples = gibbs_samples, R = x$R, B = x$B, Q = x$Q,
      normalization = x$normalization
    )
  }


}
