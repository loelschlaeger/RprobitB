#' Classify preferences of deciders.
#'
#' @description
#' This function classifies the preferences of deciders based on the estimated
#' latent classes.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param true
#' Set to \code{TRUE} to add true class memberships to output (if available).
#'
#' @return
#' A data frame with the deciders id and the latent class number.
#'
#' @export

preference_classification <- function(x, true = FALSE) {
  if (!inherits(x,"RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.")
  }
  if (x$data$P_r %in% c(0,1)){
    stop("No classification available.")
  } else {
    beta_samples <- x$gibbs_samples$gibbs_samples_nbt$beta
    if (is.null(beta_samples)){
      stop("No samples for 'beta' available.")
    }
    beta_samples_mean <- Reduce("+", beta_samples) / length(beta_samples)
    pe <- point_estimates(x)
    b_est <- apply(pe$b, 2, as.numeric, simplify = FALSE)
    Omega_est <- apply(pe$Omega, 2, matrix, nrow = x$data$P_r,
                       simplify = FALSE)
    class_prob <- matrix(NA, nrow = x$data$N, ncol = x$latent_classes$C)
    for(n in 1:x$data$N){
      for(c in 1:x$latent_classes$C){
        class_prob[n,c] <- dmvnorm(beta_samples_mean[,n], b_est[[c]],
                                   Omega_est[[c]])
      }
    }
    class_prob <- class_prob / rowSums(class_prob)
    classification <- apply(class_prob, 1, which.max)
    out <- data.frame(
      unique(x$data$choice_data$id),
      class_prob,
      classification
      )
    colnames(out) <- c(x$data$res_var_names$id,
                       paste("class", 1:x$latent_classes$C), "max")
    if(true){
      out <- cbind(out, true = x$data$true_parameter$z)
    }
    return(out)
  }
}
