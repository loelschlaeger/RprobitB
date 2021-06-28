#' Marginal mixing distributions
#' @description Function that plots the estimated marginal mixing distributions.
#' @details Adds true mixing distribution if available.
#' @param gibbs_samples A list of Gibbs samples.
#' @param model A list of model information.
#' @param estimates A list of model estimates.
#' @param parm A list of true parameter values.
#' @param out A list of output settings.
#' @return No return value. Creates pdf-file "marginal.pdf" in folder "\code{out[["rdir"]]/out[["id"]]}".

plot_marginals = function(gibbs_samples,model,estimates,parm,out){
  if(model$P_r>0){

    s_est = estimates$s_est$mean
    C_est = gibbs_samples$C_est

    pdf(file=paste0(out$rdir,"/",out$id,"/marginal.pdf"))

      for(p in seq_len(model$P_r)){

        ### estimated parameter
        mu_est_p = estimates$b_est$mean[((p-1)*C_est+1):(p*C_est)]
        sd_est_p = numeric(C_est)
        for(c in seq_len(C_est)){
          Omega_est_c = matrix(estimates$Omega_est$mean[((c-1)*model$P_r*model$P_r+1):(c*model$P_r*model$P_r)],nrow=model$P_r,ncol=model$P_r)
          sd_est_p[c] = sqrt(diag(Omega_est_c)[p])
        }

        ### true parameter
        if(!is.null(parm$b) & !is.null(parm$Omega)){
          mu_true_p = parm$b[p,]
          sd_true_p = numeric(model$C)
          for(c in seq_len(model$C)){
            Omega_true_c = matrix(parm$Omega[,c],nrow=model$P_r,ncol=model$P_r)
            sd_true_p[c] = sqrt(diag(Omega_true_c)[p])
          }
        }

        ### specify x-range
        if(!is.null(parm$b) & !is.null(parm$Omega)){
          min = min( c(abs(mu_est_p) - 3*sd_est_p, abs(mu_true_p) - 3*sd_true_p) )
          max = max( c(abs(mu_est_p) + 3*sd_est_p, abs(mu_true_p) + 3*sd_true_p) )
        } else {
          min = min( abs(mu_est_p) - 3*sd_est_p )
          max = max( abs(mu_est_p) + 3*sd_est_p )
        }
        x = seq(min,max,0.01)

        ### compute density of true mixing components
        if(!is.null(parm$b) & !is.null(parm$Omega) &!is.null(parm$s)){
          true_mixture = matrix(0,nrow=length(x),ncol=model$C)
          for(c in seq_len(model$C)) true_mixture[,c] = parm$s[c] * dnorm(x,mean=mu_true_p[c],sd=sd_true_p[c])
        }

        ### compute density of estimated mixing components
        est_mixture = matrix(0,nrow=length(x),ncol=C_est)
        for(c in seq_len(C_est)) est_mixture[,c] = s_est[c] * dnorm(x,mean=mu_est_p[c],sd=sd_est_p[c])

        ### make empty plot
        if(!is.null(parm$b) & !is.null(parm$Omega) &!is.null(parm$s)){
          ymax = max(rowSums(est_mixture),rowSums(true_mixture))
        } else {
          ymax = max(rowSums(est_mixture))
        }
        oldpar = par(no.readonly = TRUE)
        on.exit(par(oldpar))
        par(las = 1)
        plot(0, xlim = c(min(x),max(x)), ylim = c(0,ymax),
             type = "n", main = "", xlab = "", ylab = "")
        title(main="density of marginal mixing distribution",xlab=bquote(paste(beta[.(p)])),ylab="")

        ### select colors for mixing components
        col = viridis::magma(n=max(C_est,model$C),begin=0.1,end=0.9, alpha=0.6)

        ### plot true mixing components
        # if(!is.null(parm$b) & !is.null(parm$Omega) &!is.null(parm$s)){
        #   for(c in seq_len(model$C)) lines(x,true_mixture[,c],col=col[c],lty=2,lwd=2)
        # }

        ### plot estimated mixing components
        for(c in seq_len(C_est)) lines(x,est_mixture[,c],col=col[c],lty=1,lwd=2)

        ### plot true mixture
        if(!is.null(parm$b) & !is.null(parm$Omega) &!is.null(parm$s)){
          lines(x,rowSums(true_mixture),lty=2,lwd=2,col="black")
        }

        ### plot estimated mixture
        lines(x,rowSums(est_mixture),lty=1,lwd=2,col="black")

        ### plot legend
        if(!is.null(parm$b) & !is.null(parm$Omega) &!is.null(parm$s)){
          legend("topright", cex=0.75,
                 legend = c(paste("class",seq_len(C_est)),"estimated mixture","true mixture"),
                 lty    = c(rep(1,max(C_est,model$C)),1,2),
                 lwd    = 2,
                 col    = c(col,"black","black"))
        } else {
          legend("topright", cex=0.75,
                 legend = c(paste("class",seq_len(C_est)),"mixture"),
                 lty    = 1,
                 lwd    = 2,
                 col    = c(col,"black"))
        }
      }

    dev.off()
  }
}
