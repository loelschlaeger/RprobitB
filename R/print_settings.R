#' Model setting
#' @description Function that prints the model settings.
#' @param model A list of model information.
#' @param lcus A list of latent class updating scheme parameters.
#' @param init A list of initial values.
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @param norm A list of normalization information.
#' @param out A list of output settings.
#' @return No return value.

print_settings = function(model,lcus,init,mcmc,norm,out){

  seperator = paste0(rep("-",42),collapse="")

  writeLines(seperator)
  writeLines("Bayes etimation of LCMMNP model    ")
  writeLines(paste("Model id:",out$id))
  writeLines(seperator)
  writeLines("Model settings:")
  writeLines(paste("N   =",model$N))
  if(length(model$T)==1){
    writeLines(paste("T   =",model$T))
  } else {
    writeLines(paste("T   =",min(model$T),"to",max(model$T)))
  }
  writeLines(paste("J   =",model$J))
  writeLines(paste("P_f =",model$P_f))
  writeLines(paste("P_r =",model$P_r))
  writeLines(paste("C   =",model$C))
  writeLines(seperator)
  writeLines("Normalization:")
  if(norm$parameter=="a") writeLines(paste0("parameter = alpha_",norm$index))
  if(norm$parameter=="s") writeLines(paste0("parameter = Sigma_",norm$index,norm$index))
  writeLines(paste("value     =",norm$value))
  writeLines(seperator)
  if(model$P_r>0 & lcus$do_lcus){
    writeLines("Latent class updating scheme:")
    writeLines(paste("C0      =",lcus$C0))
    writeLines(paste("Cmax    =",lcus$Cmax))
    writeLines(paste("epsmin  =",lcus$epsmin))
    writeLines(paste("epsmax  =",lcus$epsmax))
    writeLines(paste("distmin =",lcus$distmin))
    writeLines(paste("buffer  =",lcus$buffer))
    writeLines(seperator)
  }
  if(init$at_true){
    writeLines("Initialisation at true values.")
    writeLines(seperator)
  }
  writeLines("MCMC settings:")
  writeLines(paste("R           =",mcmc$R))
  writeLines(paste("B           =",mcmc$B))
  writeLines(paste("Q           =",mcmc$Q))
  writeLines(paste("sample size =",floor((mcmc$R-mcmc$B)/mcmc$Q)))
  writeLines(seperator)
}
