#' Trace plots
#' @description Function that plots traces of the Gibbs samples.
#' @param gibbs_samples A list of Gibbs samples.
#' @param model A list of model information.
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @param out A list of output settings.
#' @return No return value. Creates pdf-file "trace.pdf" in folder "\code{out[["rdir"]]/out[["id"]]}".

plot_trace = function(gibbs_samples,model,mcmc,out){
  labels = make_labels(gibbs_samples,model,symm=FALSE)
  make_trace = function(samples,thinned,name,legend){
    col = viridis::magma(n=ncol(samples),begin=0.1,end=0.9, alpha=0.6)
    plot.ts(samples,
            plot.type = "single",
            ylim = c(min(tail(samples,n=length(samples/2))),max(tail(samples,n=length(samples/2)))),
            col = col,
            xlab = "",
            ylab = "",
            xaxt = "n",
            las = 1,
            main = paste0(name,": ",ifelse(thinned,"normalized, burned and thinned samples","normalized samples")))
    if(thinned){
      at = c(1,(mcmc$R-mcmc$B)/mcmc$Q)
      labels = c("B+1","R")
    } else {
      at = c(1,mcmc$B/2,mcmc$B,mcmc$R)
      labels = c("1","B/2","B","R")
    }
    axis(side=1, at=at, labels=labels)
    abline(v=intersect(head(at,n=-1),tail(at,n=-1)),lty=2)
    legend("topright",legend=legend,lty=1,col=col,cex=0.75)
  }
  pdf(file=paste0(out$rdir,"/",out$id,"/trace.pdf"))
    if(model$P_f>0){
      make_trace(samples = gibbs_samples$gibbs_samples_n$alpha_draws_n,
                 thinned = FALSE,
                 name = "alpha",
                 legend = labels$alpha_label)
      make_trace(samples = gibbs_samples$gibbs_samples_nbt$alpha_draws_nbt,
                 thinned = TRUE,
                 name = "alpha",
                 legend = labels$alpha_label)
    }
    if(model$P_r>0){
      C_range = max(model$C,gibbs_samples$C_est,na.rm=TRUE)
      make_trace(samples = gibbs_samples$gibbs_samples_n$s_draws_n[,seq_len(C_range),drop=FALSE],
                 thinned = FALSE,
                 name = "s",
                 legend = labels$s_label)
      make_trace(samples = gibbs_samples$gibbs_samples_nbt$s_draws_nbt[,seq_len(C_range),drop=FALSE],
                 thinned = TRUE,
                 name = "s",
                 legend = labels$s_label)
      for(c in seq_len(C_range)){
        make_trace(samples = gibbs_samples$gibbs_samples_n$b_draws_n[,(model$P_r*(c-1)+1):(c*model$P_r),drop=FALSE],
                   thinned = FALSE,
                   name = "b",
                   legend = labels$b_label[(model$P_r*(c-1)+1):(c*model$P_r)])
        make_trace(samples = gibbs_samples$gibbs_samples_nbt$b_draws_nbt[,(model$P_r*(c-1)+1):(c*model$P_r),drop=FALSE],
                   thinned = TRUE,
                   name = "b",
                   legend = labels$b_label[(model$P_r*(c-1)+1):(c*model$P_r)])
      }
      id = which(lower.tri(matrix(NA,model$P_r,model$P_r),diag=TRUE)==TRUE)
      for(c in seq_len(C_range)){
        make_trace(samples = gibbs_samples$gibbs_samples_n$Omega_draws_n[,(model$P_r*model$P_r*(c-1)+1):(c*model$P_r*model$P_r),drop=FALSE][,id,drop=FALSE],
                   thinned = FALSE,
                   name = "Omega",
                   legend = labels$Omega_label[(length(id)*(c-1)+1):(c*length(id))])
        make_trace(samples = gibbs_samples$gibbs_samples_nbt$Omega_draws_nbt[,(model$P_r*model$P_r*(c-1)+1):(c*model$P_r*model$P_r),drop=FALSE][,id,drop=FALSE],
                   thinned = TRUE,
                   name = "Omega",
                   legend = labels$Omega_label[(length(id)*(c-1)+1):(c*length(id))])
      }
    }
    id = which(lower.tri(matrix(NA,model$J-1,model$J-1),diag=TRUE)==TRUE)
    make_trace(samples = gibbs_samples$gibbs_samples_n$Sigma_draws_n[,id,drop=FALSE],
               thinned = FALSE,
               name = "Sigma",
               legend = labels$Sigma_label)
    make_trace(samples = gibbs_samples$gibbs_samples_nbt$Sigma_draws_nbt[,id,drop=FALSE],
               thinned = TRUE,
               name = "Sigma",
               legend = labels$Sigma_label)
  dev.off()
}
