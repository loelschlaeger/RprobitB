#' Print method for the summary of \code{RprobitB_model}.
#' @param x
#' An object of class \code{summary.RprobitB_model}.
#' @param statistics
#' ...
#' @param digits
#' ...
#' @param ...
#' Ignored.
#' @export

print.summary.RprobitB_model = function(x, statistics = c("mean", "sd", "R^"),
                                        digits = 2, ...) {

  cat(paste0("Summary of fitted probit model '",
             deparse1(x$form),"' via Bayesian estimation:\n\n"))

  ### summary of model
  cat("MCMC settings:\n")
  cat("- R:",x$R,"\n")
  cat("- B:",x$B,"\n")
  cat("- Q:",x$Q,"\n")
  cat("\n")

  ### summary of normalization
  print(x$normalization)
  cat("\n")

  ### legend of alternatives
  cat("Legend of alternatives:\n")
  for(i in seq_len(x$J))
    cat("-",paste0(i,":"),x$alternatives[i],"\n")
  cat("\n")

  ### legend of covariates with fixed coefficients
  if(x$P_f>0){
    cat("Covariates with fixed coefficients (alpha):\n")
    cov_fix = x$covs[!x$covs$random,"names"]
    for(i in seq_len(x$P_f))
      cat("-",paste0(i,":"),cov_fix[i],"\n")
    cat("\n")
  }

  ### legend of covariates with random coefficients
  if(x$P_r>0){
    cat("Random effects (b, Omega):\n")
    cov_random = x$covs[x$covs$random,"names"]
    for(i in seq_len(x$P_r))
      cat("-",paste0(i,":"),cov_random[i],"\n")
    cat("\n")
  }

  ### legend of latent classes
  if(x$P_r>0){
    print(x$latent_classes)
    cat("\n")
  }

  ### overview of estimates
  print(x$parameter_statistics, true = x$true_parameter, statistics = statistics,
        digits = digits)

  ### return 'x' invisibly
  return(invisible(x))
}
