#' Print method for \code{RprobitB_data}.
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_data = function(x, ...){
  cat(ifelse(x$simulated,"Simulated","Empirical"),
      "data of",sum(x$T),"choices.\n")
  return(invisible(x))
}

#' Print method for \code{RprobitB_model}.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_model = function(x, ...){
  cat("Fitted probit model via Bayesian estimation.\n")
  return(invisible(x))
}

#' Print method for \code{RprobitB_normalization}.
#' @param x
#' An object of class \code{RprobitB_normalization}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_normalization = function(x,...) {
  cat("Normalization:\n")
  cat(paste0("- Level: Utility differences with respect to alternative ",
             x$level,".\n"))
  if(x$scale$parameter=="a")
    norm_scale = paste0(x$cov_fix[x$scale$index]," (alpha_",x$scale$index,")")
  if(x$scale$parameter=="s")
    norm_scale = paste0("the ",x$scale$index,". error term variance in Sigma")
  cat(paste0("- Scale: Coefficient of ",norm_scale," fixed to ",x$scale$value,
             ".\n"))
  return(invisible(x))
}

#' Print method for the summary of \code{RprobitB_data}.
#' @param x
#' An object of class \code{summary.RprobitB_data}.
#' @param ...
#' Ignored.
#' @export

print.summary.RprobitB_data = function(x, ...) {

  cat("Summary of",ifelse(x$simulated,"simulated","empirical"),
      "choice data\n\n")

  ### summary of decision makers
  cat(x$N, paste0("decision maker",ifelse(x$N==1,"","s")),"\n")
  if(length(unique(x$T))==1)
    cat(x$T[1], paste0("choice occasion",ifelse(unique(x$T)==1,"","s")),
        ifelse(x$N==1,"","each"),"\n")
  if(length(unique(x$T))>1)
    cat(min(x$T),"to",max(x$T),"choice occasions",
        ifelse(x$N==1,"","each"),"\n")
  cat(sum(x$T),"choices in total\n")
  cat("\n")

  ### summary of alternatives
  cat("Alternatives\n")
  print(x$alternatives)
  cat("\n")

  ### summary of covariates
  cat("Covariates\n")
  print(x$covariates)
  if(!is.null(x$asc_alt)){
    cat("\n")
    message = ifelse(length(x$asc_alt)==1,
                     "ASC added for alternative",
                     "ASCs added for alternatives")
    cat(message,paste(x$asc_alt,collapse=", "),"\n")
  }
  return(invisible(x))
}

#' Print method for the summary of \code{RprobitB_model}.
#' @param x
#' An object of class \code{summary.RprobitB_model}.
#' @param ...
#' Ignored.
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
  print(x$statistics, true = x$true_parameter)
  cat("\n")

  return(invisible(x))
}

#' Print method for \code{RprobitB_latent_classes}.
#' @param x
#' An object of class \code{RprobitB_latent_classes}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_latent_classes = function(x, ...) {
  cat("Latent classes:\n")
  if(!x$update){
    cat(paste("- Number:",x$C,"\n"))
  }
  cat(paste("- Update:",x$update,"\n"))
  if(x$update){
    cat(paste("- Initial number:",x$C,"\n"))
    cat(paste("- Maximum number:",x$Cmax,"\n"))
    cat(paste("- Buffer:",x$buffer,"\n"))
    cat(paste("- Minimum class weight:",x$epsmin,"\n"))
    cat(paste("- Maximum class weight:",x$epsmax,"\n"))
    cat(paste("- Mimumum class distance:",x$distmin,"\n"))
  }
  return(invisible(x))
}

#' Print method for \code{RprobitB_parameter_statistics}.
#' @param x
#' An object of class \code{RprobitB_parameter_statistics}.
#' @param true
#' Either \code{NULL} or an object of class \code{RprobitB_true_parameter}.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_parameter_statistics = function(x, true = NULL, ...) {

  cat("Print an overview of the parameter estimates here.\n")

  # ### get coefficient labels
  # labels = create_labels(P_f = x$P_f, P_r = x$P_r, J = x$J,
  #                        C = x$latent_classes$C, C_est = x$statistics$C_est,
  #                        symmetric = FALSE)
  #
  # ### function that creates table format for estimates
  # print_tab = function(par_name){
  #   true = as.vector(x$true_parameter[[par_name]])
  #   statistics = x$statistics[[par_name]]
  #   if(x$simulated) statistics = cbind(true,statistics)
  #   statistics = round(statistics, 2)
  #   statistics = statistics[labels[[par_name]],,drop=FALSE]
  #   colnames(statistics) = rep(sprintf("%6s"," "),ncol(statistics))
  #   rownames(statistics) = sprintf("%6s",labels[[par_name]])
  #   writeLines(paste("\n",par_name))
  #   print(statistics)
  # }
  #
  # cat("Estimates:\n")
  # cat(paste(sprintf("%6s"," "),
  #           if(x$simulated) sprintf("%6s","true"),
  #           sprintf("%6s","mean"),
  #           sprintf("%6s","sd"),
  #           sprintf("%6s","min"),
  #           sprintf("%6s","q.25"),
  #           sprintf("%6s","median"),
  #           sprintf("%6s","q.75"),
  #           sprintf("%6s","max"),
  #           sprintf("%6s","R^")))
  #
  # if(x$P_f>0)
  #   print_tab(par_name = "alpha")
  # if(x$P_r>0){
  #   print_tab(par_name = "s")
  #   print_tab(par_name = "b")
  #   print_tab(par_name = "Omega")
  # }
  # print_tab(par_name = "Sigma")

  return(invisible(x))
}
