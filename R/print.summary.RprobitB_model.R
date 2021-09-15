#' Print method for the summary of \code{RprobitB_model}
#' @param x
#' An object of class \code{summary.RprobitB_model}
#' @param ...
#' ignored
#' @export

print.summary.RprobitB_model = function(x, ...) {

  cat(paste0("Summary of fitted probit model '",
             deparse1(x$form),"' via Bayesian estimation:\n\n"))

  ### summary of model
  cat("MCMC settings:\n")
  cat("- R:",x$R,"\n")
  cat("- B:",x$B,"\n")
  cat("- Q:",x$Q,"\n")
  cat("\n")

  ### summary of normalization
  cat("Normalization:\n")
  norm_level = paste0(x$J," (",x$alternatives[x$J],")")
  cat("- Differenced with respect to alternative",norm_level,"\n")
  if(x$scale$parameter=="a")
    norm_scale = paste0(x$cov_fix[x$scale$index]," (alpha_",x$scale$index,")")
  if(x$scale$parameter=="s")
    norm_scale = paste0("the ",x$scale$index,". error term variance in Sigma")
  cat("- Coefficient of",norm_scale,"fixed to",x$scale$value,"\n")
  cat("\n")

  ### legend of alternatives
  cat("Legend of alternatives (Sigma):\n")
  for(i in seq_len(x$J)) cat("-",paste0(i,":"),x$alternatives[i],"\n")
  cat("\n")

  ### legend of covariates with fixed coefficients
  if(x$P_f>0){
    cat("Covariates with fixed coefficients (alpha):\n")
    for(i in seq_len(x$P_f)) cat("-",paste0(i,":"),x$cov_fix[i],"\n")
    cat("\n")
  }

  ### legend of covariates with random coefficients
  if(x$P_r>0){
    cat("Random effects (b, Omega):\n")
    for(i in seq_len(x$P_r)) cat("-",paste0(i,":"),x$cov_random[i],"\n")
    cat("\n")
  }

  ### legend latent classes
  if(x$P_r>0){
    cat("Latent classes (s):\n")
    cat("\n")
  }

  ### get coefficient labels
  labels = create_labels(P_f = x$P_f, P_r = x$P_r, J = x$J,
                         C = x$latent_classes$C, C_est = x$statistics$C_est,
                         symmetric = FALSE)

  ### function that creates table format for estimates
  print_tab = function(par_name){
    true = as.vector(x$parm[[par_name]])
    statistics = x$statistics[[par_name]]
    if(x$simulated) statistics = cbind(true,statistics)
    statistics = round(statistics, 2)
    statistics = statistics[labels[[par_name]],,drop=FALSE]
    colnames(statistics) = rep(sprintf("%6s"," "),ncol(statistics))
    rownames(statistics) = sprintf("%6s",labels[[par_name]])
    writeLines(paste("\n",par_name))
    print(statistics)
  }

  cat("Estimates:\n")
  cat(paste(sprintf("%6s"," "),
            if(x$simulated) sprintf("%6s","true"),
            sprintf("%6s","mean"),
            sprintf("%6s","sd"),
            sprintf("%6s","min"),
            sprintf("%6s","q.25"),
            sprintf("%6s","median"),
            sprintf("%6s","q.75"),
            sprintf("%6s","max"),
            sprintf("%6s","R^")))

  if(x$P_f>0)
    print_tab(par_name = "alpha")
  if(x$P_r>0){
    print_tab(par_name = "s")
    print_tab(par_name = "b")
    print_tab(par_name = "Omega")
  }
  print_tab(par_name = "Sigma")

  return(invisible(x))
}

