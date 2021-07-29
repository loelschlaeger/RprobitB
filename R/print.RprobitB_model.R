#' Point estimates
#' @description Function that computes and prints point estimates, standard deviations and 5% and 95% quantiles.
#' @param gibbs_samples A list of Gibbs samples.
#' @param model A list of model information.
#' @param parm A list of true parameter values.
#' @return A list of model estimates.

print_estimates = function(gibbs_samples,model,parm){

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

  alpha_est = NA
  s_est     = NA
  b_est     = NA
  Omega_est = NA
  Sigma_est = NA

  ### get coefficient labels
  labels = make_labels(gibbs_samples,model,symm=TRUE)

  ### compute R_hat (Gelman-Rubin statistic)
  ### https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html
  comp_R_hat = function(samples,parts=2){
    sub_chains = split(samples,cut(seq_along(samples),parts,labels=FALSE))
    L = length(samples)/parts
    chain_means = sapply(sub_chains,mean)
    grand_mean = mean(chain_means)
    B = 1/(parts-1)*sum((chain_means-grand_mean)^2)
    chain_variances = sapply(sub_chains,var)
    W = sum(chain_variances)/parts
    R_hat = ((L-1)/L*W+B)/W
    return(R_hat)
  }

  ### function that creates table format for estimates
  create_tab = function(true,draws,chain,name,row_names){
    max.len        = max(length(true),dim(draws)[2])
    true           = c(true, rep(NA, max.len-length(true)))
    mean           = c(apply(draws,2,mean), rep(NA, max.len-dim(draws)[2]))
    sd             = c(apply(draws,2,sd), rep(NA, max.len-dim(draws)[2]))
    q.05           = c(apply(draws,2,quantile,prob=0.05), rep(NA, max.len-dim(draws)[2]))
    q.95           = c(apply(draws,2,quantile,prob=0.95), rep(NA, max.len-dim(draws)[2]))
    R_hat          = c(apply(chain,2,comp_R_hat), rep(NA, max.len-dim(chain)[2]))
    out            = round(cbind(true,mean,sd,q.05,q.95,R_hat),2)
    colnames(out)  = c(sprintf("%5s"," "),sprintf("%5s"," "),sprintf("%5s"," "),sprintf("%5s"," "),sprintf("%5s"," "),sprintf("%5s"," "))
    rownames(out)  = sprintf("%6s",row_names)
    writeLines(paste("\n",name))
    print(out)
    est = list(mean,sd,q.05,q.95)
    names(est) = c("mean","sd","q.05","q.95")
    return(est)
  }

  writeLines(seperator)
  writeLines("Posterior summary:")
  writeLines(paste(sprintf("%6s"," "),sprintf("%5s","true"),sprintf("%5s","mean"),sprintf("%5s","sd"),sprintf("%5s","5%"),sprintf("%5s","95%"),sprintf("%5s","R^")))

  if(model$P_f>0){
    alpha_est = create_tab(true = parm$alpha,
                           draws = gibbs_samples$gibbs_samples_nbt$alpha_draws_nbt,
                           chain = gibbs_samples$gibbs_samples_nb$alpha_draws_nb,
                           name = "alpha",
                           row_names = labels$alpha_label)
  }
  if(model$P_r>0){
    s_est = create_tab(true = parm$s,
                       draws = gibbs_samples$gibbs_samples_nbt$s_draws_nbt[,seq_len(max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                       chain = gibbs_samples$gibbs_samples_nb$s_draws_nb[,seq_len(max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                       name = "s",
                       row_names = labels$s_label)

    b_est = create_tab(true = parm$b,
                       draws = gibbs_samples$gibbs_samples_nbt$b_draws_nbt[,seq_len(model$P_r*max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                       chain = gibbs_samples$gibbs_samples_nb$b_draws_nb[,seq_len(model$P_r*max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                       name = "b",
                       row_names = labels$b_label)

    Omega_est = create_tab(true = parm$Omega,
                           draws = gibbs_samples$gibbs_samples_nbt$Omega_draws_nbt[,seq_len(model$P_r*model$P_r*max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                           chain = gibbs_samples$gibbs_samples_nb$Omega_draws_nb[,seq_len(model$P_r*model$P_r*max(model$C,gibbs_samples$C_est,na.rm=TRUE)),drop=FALSE],
                           name = "Omega",
                           row_names = labels$Omega_label)
  }
  Sigma_est = create_tab(true = parm$Sigma,
                         draws = gibbs_samples$gibbs_samples_nbt$Sigma_draws_nbt,
                         chain = gibbs_samples$gibbs_samples_nb$Sigma_draws_nb,
                         name = "Sigma",
                         row_names = paste0(rep(1:(model$J-1),each=model$J-1),",",rep(1:(model$J-1),times=model$J-1)))

  writeLines(seperator)
  estimates = list("alpha_est" = alpha_est,
                   "s_est"     = s_est,
                   "b_est"     = b_est,
                   "Omega_est" = Omega_est,
                   "Sigma_est" = Sigma_est)
  return(estimates)
}
