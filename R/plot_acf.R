#' Autocorrelation plots
#' @description Function that plots the autocorrelation of the Gibbs samples.
#' @details Function computes the [effective sample size](https://mc-stan.org/docs/2_18/reference-manual/effective-sample-size-section.html).
#' @param gibbs_samples A list of Gibbs samples.
#' @param model A list of model information.
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @param out A list of output settings.
#' @return No return value. Creates pdf-file "acf.pdf" in folder "\code{out[["rdir"]]/out[["id"]]}".

plot_acf = function(gibbs_samples,model,mcmc,out){

  ### get coefficient labels
  labels = create_labels(gibbs_samples,model,symmetric=FALSE)

  make_acf = function(name,draws_nb,draws_nbt,labels){
    for(c in seq_len(ncol(draws_nb))){
      if(any(draws_nb[,c]!=0) & length(unique(draws_nb[,c]))>1){
        rho = acf(draws_nb[,c],las=1,main=paste0(name,"_",labels[c],": normalized and burned samples"))
        ### compute effective sample size
        SS = mcmc$R-mcmc$B
        ESS = min(SS/(1+2*sum(rho$acf)),SS)
        legend("topright",x.intersp=-0.5,bg="white",legend=sprintf("%s %.2f",paste0(c("total sample size","effective sample size","factor"),":"),c(SS,ESS,SS/ESS)))
      }
    }
    for(c in seq_len(ncol(draws_nbt))){
      if(any(draws_nbt[,c]!=0) & length(unique(draws_nb[,1]))>1){
        acf(draws_nbt[,c],las=1,main=paste0(name,"_",labels[c],": normalized, burned and thinned samples"))
      }
    }
  }

  pdf(file=paste0(out$rdir,"/",out$id,"/acf.pdf"))
  if(model$P_f>0){
    make_acf(name      = "alpha",
             draws_nb  = gibbs_samples$gibbs_samples_nb$alpha_draws_nb,
             draws_nbt = gibbs_samples$gibbs_samples_nbt$alpha_draws_nbt,
             labels    = labels$alpha_label)
  }
  if(model$P_r>0){
    C_range = max(model$C,gibbs_samples$C_est,na.rm=TRUE)
    if(C_range>1){
      make_acf(name      = "s",
               draws_nb  = gibbs_samples$gibbs_samples_nb$s_draws_nb,
               draws_nbt = gibbs_samples$gibbs_samples_nbt$s_draws_nbt,
               labels    = labels$s_label)
    }
    make_acf(name      = "b",
             draws_nb  = gibbs_samples$gibbs_samples_nb$b_draws_nb,
             draws_nbt = gibbs_samples$gibbs_samples_nbt$b_draws_nbt,
             labels    = labels$b_label)
    id = rep(as.vector(lower.tri(matrix(NA,model$P_r,model$P_r),diag=TRUE)==TRUE),C_range)
    make_acf(name      = "Omega",
             draws_nb  = gibbs_samples$gibbs_samples_nb$Omega_draws_nb[,id,drop=FALSE],
             draws_nbt = gibbs_samples$gibbs_samples_nbt$Omega_draws_nbt[,id,drop=FALSE],
             labels    = labels$Omega_label)
  }
  if(model$J-1>1){
    id = as.vector(lower.tri(matrix(NA,model$J-1,model$J-1),diag=TRUE)==TRUE); id[1] = FALSE
    make_acf(name      = "Sigma",
             draws_nb  = gibbs_samples$gibbs_samples_nb$Sigma_draws_nb[,id,drop=FALSE],
             draws_nbt = gibbs_samples$gibbs_samples_nbt$Sigma_draws_nbt[,id,drop=FALSE],
             labels    = labels$Sigma_label[-1])
  }
  dev.off()
}
