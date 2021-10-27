#' Contour plots
#' @description
#' Function that creates contour plot and progress contour plots of the Gibbs
#' samples.
#' @param x
#' ...
#' @return
#' ...

plot_contour = function(x){
  if(model$P_r==2){
    make_contour = function(C,s,b,Omega,limits=NULL,main_add=NULL){
      if(is.null(parm$beta)){
        if(is.null(limits)){
          xmin = -5
          xmax = 5
          ymin = -5
          ymax = 5
        } else {
          xmin = limits$xlim[1]
          xmax = limits$xlim[2]
          ymin = limits$ylim[1]
          ymax = limits$ylim[2]
        }
      } else {
        xmin = floor(min(parm$beta[1,]))
        xmax = ceiling(max(parm$beta[1,]))
        ymin = floor(min(parm$beta[2,]))
        ymax = ceiling(max(parm$beta[2,]))
      }
      grid_x = seq(xmin,xmax,length.out=200)
      grid_y = seq(ymin,ymax,length.out=200)
      prob = matrix(0,nrow=length(grid_x),ncol=length(grid_y))
      for(i in seq_len(length(grid_x))){
        for(j in seq_len(length(grid_x))){
          for(c in 1:C){
            prob[i,j] = prob[i,j] + s[c]*dmvnrm_arma_mc(t(matrix(c(grid_x[i],grid_y[j]))),mean=b[((c-1)*model$P_r+1):(c*model$P_r)],sigma=matrix(Omega[((c-1)*model$P_r*model$P_r+1):(c*model$P_r*model$P_r)],2,2))
          }
        }
      }
      if(is.null(limits)){
        xlim = c(min(grid_x[which(rowSums(prob)>1e-2)]),max(grid_x[which(rowSums(prob)>1e-2)]))
        ylim = c(min(grid_x[which(colSums(prob)>1e-2)]),max(grid_x[which(colSums(prob)>1e-2)]))
        limits = list("xlim" = xlim, "ylim" = ylim)
      } else {
        xlim = limits$xlim
        ylim = limits$ylim
      }
      plot(1,type="n",xlim=xlim,ylim=ylim,xlab=expression(beta[1]),ylab=expression(beta[2]),main=paste("contour plot of mixing distribution",main_add))
      if(!is.null(parm$beta)) points(x=parm$beta[1,],y=parm$beta[2,],pch=16,col=rgb(0,0,0,0.1))
      contour(add=TRUE,grid_x,grid_y,prob,labcex=0.75)
      return(limits)
    }

    ### contour plot
    pdf(file=paste0(out$rdir,"/",out$id,"/contour.pdf"))
      limits = make_contour(C = gibbs_samples$C_est,
                            s = estimates$s_est$mean,
                            b = estimates$b_est$mean,
                            Omega = estimates$Omega_est$mean)
    dev.off()

    if(out$pp){
      ### progress contour plots
      steps = unique(sort(c(round(exp(seq(log(1),log(mcmc$B),length.out=5))),
                            round(exp(seq(log(mcmc$B),log(mcmc$R),length.out=5))))))
      if(lcus$do_lcus){
        update_steps = (which(gibbs_samples$gibbs_loop_out$update_steps==1)-1)*lcus$buffer+mcmc$B/2
        steps = unique(sort(c(steps,update_steps,update_steps-1)))
      }
      pdf(file=paste0(out$rdir,"/",out$id,"/contour_progress.pdf"))
        for(step in steps){
          C_step = sum(gibbs_samples$gibbs_samples_n$s_draws_n[step,]!=0)
          s_est_step = apply(gibbs_samples$gibbs_samples_n$s_draws_n[min(step,mcmc$B+1):step,1:C_step,drop=FALSE],2,mean)
          b_est_step = apply(gibbs_samples$gibbs_samples_n$b_draws_n[min(step,mcmc$B+1):step,1:(C_step*model$P_r),drop=FALSE],2,mean)
          Omega_est_step = apply(gibbs_samples$gibbs_samples_n$Omega_draws_n[min(step,mcmc$B+1):step,1:(C_step*model$P_r*model$P_r),drop=FALSE],2,mean)
          make_contour(C = C_step,
                       s = s_est_step,
                       b = b_est_step,
                       Omega = Omega_est_step,
                       limits = limits,
                       main_add = paste("in iteration",step))
          x_values = matrix(b_est_step,ncol=C_step)[1,]
          y_values = matrix(b_est_step,ncol=C_step)[2,]
          for(c in seq_len(C_step)){
            legend(x=x_values[c],y=y_values[c],legend=sprintf("%#.2f",s_est_step[c]),xjust=0.5,yjust=0.5,x.intersp=-0.75,y.intersp=-0.1,cex=0.75,bg=rgb(0,0,0,0.6),border="black")
          }
        }
      dev.off()
    }
  }
}
