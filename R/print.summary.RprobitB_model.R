#' @export

print.summary.RprobitB_model = function(x, ...) {

  cat(paste0("Summary of model '",deparse1(x$form),"'\n\n"))

  ### summary of model
  cat("MCMC settings:\n")
  cat("- R:",x$R,"\n")
  cat("- B:",x$B,"\n")
  cat("- Q:",x$Q,"\n")
  cat("\n")

  ### legend of covariates
  cat("Legend of covariates:\n")
  cat("\n")

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

  cat("Posterior:\n")
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

