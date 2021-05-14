#' WAIC
#' @description Function that computes the Widely Applicable Information Criterion (WAIC).
#' @param model A list of model information.
#' @param gibbs_samples A list of gibbs samples.
#' @param data A list of data information.
#' @return WAIC value (numeric).

compute_waic = function(model,gibbs_samples,data){

  N = model$N
  T = if(length(model$T)==model$N) model$T else rep(model$T,model$N)
  J = model$J
  P_f = model$P_f
  P_r = model$P_r
  C = ifelse(P_r>0,gibbs_samples$C_est,1)

  ### difference operator
  Delta = function(i){
    Delta = diag(J)[-J,,drop=FALSE]; Delta[,J] = -1
    return(Delta)
  }

  ### numer of samples
  no_samples = nrow(gibbs_samples$gibbs_samples_nbt$Sigma_draws_nbt)

  ### average likelihood of each choice over sets of parameters sampled from the posterior
  avg_likelihoods = numeric()

  ### variance of likelihoods of each choice over sets of parameters sampled from the posterior
  var_likelihoods = numeric()

  for(n in 1:N){
    cat(sprintf("Computing WAIC: %.0f%% \r",(n-1)/N*100,appendLF=FALSE))

    X_n = data[[n]]$X
    y_n = data[[n]]$y

    for(t in 1:T[n]){
      X_nt = X_n[[t]]
      j = y_n[t]
      P_nt_y_nt_all = numeric(no_samples)
      if(P_f>0) alpha = gibbs_samples$gibbs_samples_nbt$alpha_draws_nbt
      Sigma = gibbs_samples$gibbs_samples_nbt$Sigma_draws_nbt

      for(c in 1:C){
        if(P_r>0){
          s_c = gibbs_samples$gibbs_samples_nbt$s_draws_nbt[,c]
          b_c = gibbs_samples$gibbs_samples_nbt$b_draws_nbt[,((c-1)*P_r+1):(c*P_r),drop=FALSE]
          Omega_c = gibbs_samples$gibbs_samples_nbt$Omega_draws_nbt[,((c-1)*P_r*P_r+1):(c*P_r*P_r),drop=FALSE]
        } else {
          s_c = rep(1,no_samples)
        }

        for(sample in 1:no_samples){

          P_nt_y_nt = 0

          if(P_f>0){
            alpha_sample = alpha[sample,]
          }
          if(P_r>0){
            s_c_sample = s_c[sample]
            b_c_sample = b_c[sample,]
            Omega_c_sample = matrix(Omega_c[sample,],nrow=P_r,ncol=P_r)
          }
          Sigma_sample = matrix(Sigma[sample,],nrow=J-1,ncol=J-1)

          ### compute differences wrt reference alternative J
          if(model$P_f>0 | model$P_r>0) X_nt = Delta(J) %*% X_nt

          ### append 0's for reference alternative J
          if(model$P_f>0 | model$P_r>0) X_nt = rbind(X_nt,0)
          Sigma_sample = cbind(rbind(Sigma_sample,0),0)

          ### define parameters of multivariate normal distribution
          if(P_f>0){
            if(P_r>0){
              upper = as.vector(-Delta(j) %*% X_nt %*% c(alpha_sample,b_c_sample))
              sigma = Delta(j) %*% ( X_nt[,-(1:P_f)] %*% Omega_c_sample %*% t(X_nt[,-(1:P_f)]) + Sigma_sample ) %*% t(Delta(j))
            }
            if(P_r==0){
              upper = as.vector(-Delta(j) %*% X_nt %*% alpha_sample)
              sigma = Delta(j) %*%  Sigma_sample %*% t(Delta(j))
            }
          }
          if(P_f==0){
            if(P_r>0){
              upper = as.vector(-Delta(j) %*% X_nt %*% b_c_sample)
              sigma = Delta(j) %*% ( X_nt %*% Omega_c_sample %*% t(X_nt) + Sigma_sample ) %*% t(Delta(j))
            }
            if(P_r==0){
              upper = rep(0,J-1)
              sigma = Delta(j) %*% Sigma_sample %*% t(Delta(j))
            }
          }

          P_nt_y_nt = P_nt_y_nt + s_c[sample] * mvtnorm::pmvnorm(lower = rep(-Inf,J-1),
                                                                 upper = upper,
                                                                 mean  = rep(0,J-1),
                                                                 sigma = sigma)

          P_nt_y_nt_all[sample] = P_nt_y_nt
        }
      }

      avg_likelihoods = c(avg_likelihoods,mean(P_nt_y_nt_all))
      var_likelihoods = c(var_likelihoods,var(P_nt_y_nt_all))
    }
  }

  WAIC = -2 * ( sum(log(avg_likelihoods),na.rm=TRUE) + sum(var_likelihoods) )

  cat(sprintf("WAIC: %.4f",WAIC),"           \n")

  return(WAIC)
}
