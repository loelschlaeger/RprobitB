#' Filter Gibbs samples.
#'
#' @description
#' This is a helper function that filters Gibbs samples.
#'
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @inheritParams create_parameter_labels
#'
#' @return
#' An object of class \code{RprobitB_gibbs_samples} filtered by the labels
#' \code{create_parameter_labels(P_f, P_r, J, C, cov_sym, keep_par, drop_par)}.
#'
#' @keywords
#' internal

filter_gibbs_samples <- function(x, P_f, P_r, J, C, cov_sym,
                                 keep_par = c("s", "alpha", "b", "Omega", "Sigma"),
                                 drop_par = NULL) {

  labels <- create_parameter_labels(P_f, P_r, J, C, cov_sym, keep_par, drop_par)
  for (gs in names(x)) {
    for (par in names(x[[gs]])) {
      if (!par %in% names(labels)) {
        x[[gs]][[par]] <- NULL
      } else {
        cols <- intersect(colnames(x[[gs]][[par]]), labels[[par]])
        x[[gs]][[par]] <- x[[gs]][[par]][, cols, drop = FALSE]
      }
      x[[gs]] <- x[[gs]][lengths(x[[gs]]) != 0]
    }
  }
  return(x)

}
