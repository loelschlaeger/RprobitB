#' Transformation of Gibbs samples
#' @description Function that normalizes, burnes and thins the Gibbs samples. 
#' @param gibbs_loop_out Output of \code{gibbs_loop}.
#' @param model A list of model information.
#' @param mcmc A list of Markov chain Monte Carlo parameters.
#' @param norm A list of normalization information.
#' @return A list of normalized, burned and thinned Gibbs samples.

gibbs_transform = function(gibbs_loop_out,model,mcmc,norm) {
  P_f = model$P_f
  P_r = model$P_r
  R = mcmc$R
  B = mcmc$B
  Q = mcmc$Q
  
  ### determine estimated number of latent classes
  C_est = NA
  if(P_r>0){
    last_s_draw = gibbs_loop_out$s_draws[nrow(gibbs_loop_out$s_draws),]
    C_est       = length(last_s_draw[last_s_draw!=0])
  }
  
  ### normalization of samples
  if(norm$parameter=="a"){
    s_draws_n     = if(P_r>0) gibbs_loop_out$s_draws                                                                                                                     else NA
    alpha_draws_n =           norm$value * gibbs_loop_out$alpha_draws / gibbs_loop_out$alpha_draws[,norm$index]                                             
    b_draws_n     = if(P_r>0) norm$value * gibbs_loop_out$b_draws     / gibbs_loop_out$alpha_draws[,norm$index]                                                          else NA
    Omega_draws_n = if(P_r>0) norm$value * norm$value * gibbs_loop_out$Omega_draws / (gibbs_loop_out$alpha_draws[,norm$index] * gibbs_loop_out$alpha_draws[,norm$index]) else NA
    Sigma_draws_n =           norm$value * norm$value * gibbs_loop_out$Sigma_draws / (gibbs_loop_out$alpha_draws[,norm$index] * gibbs_loop_out$alpha_draws[,norm$index])
  }
  if(norm$parameter=="s"){
    column = (model$J-1)*(norm$index-1)+norm$index
    s_draws_n     = if(P_r>0) gibbs_loop_out$s_draws                                                                    else NA
    alpha_draws_n = if(P_f>0) sqrt(norm$value) * gibbs_loop_out$alpha_draws / sqrt(gibbs_loop_out$Sigma_draws[,column]) else NA
    b_draws_n     = if(P_r>0) sqrt(norm$value) * gibbs_loop_out$b_draws     / sqrt(gibbs_loop_out$Sigma_draws[,column]) else NA
    Omega_draws_n = if(P_r>0) norm$value * gibbs_loop_out$Omega_draws / gibbs_loop_out$Sigma_draws[,column]             else NA
    Sigma_draws_n =           norm$value * gibbs_loop_out$Sigma_draws / gibbs_loop_out$Sigma_draws[,column]
  }
  
  gibbs_samples_n = list("s_draws_n"     = s_draws_n,
                         "alpha_draws_n" = alpha_draws_n,
                         "b_draws_n"     = b_draws_n,
                         "Omega_draws_n" = Omega_draws_n,
                         "Sigma_draws_n" = Sigma_draws_n)
  
  ### burning of normalized samples
  s_draws_nb     = if(P_r>0) s_draws_n[(B+1):R,,drop=FALSE]     else NA
  alpha_draws_nb = if(P_f>0) alpha_draws_n[(B+1):R,,drop=FALSE] else NA
  b_draws_nb     = if(P_r>0) b_draws_n[(B+1):R,,drop=FALSE]     else NA
  Omega_draws_nb = if(P_r>0) Omega_draws_n[(B+1):R,,drop=FALSE] else NA
  Sigma_draws_nb =           Sigma_draws_n[(B+1):R,,drop=FALSE] 
  
  gibbs_samples_nb = list("s_draws_nb"     = s_draws_nb,
                          "alpha_draws_nb" = alpha_draws_nb,
                          "b_draws_nb"     = b_draws_nb,
                          "Omega_draws_nb" = Omega_draws_nb,
                          "Sigma_draws_nb" = Sigma_draws_nb)
  
  ### thinning of normalized samples
  s_draws_nt     = if(P_r>0) s_draws_n[seq(1,R,Q),,drop=FALSE]     else NA
  alpha_draws_nt = if(P_f>0) alpha_draws_n[seq(1,R,Q),,drop=FALSE] else NA
  b_draws_nt     = if(P_r>0) b_draws_n[seq(1,R,Q),,drop=FALSE]     else NA
  Omega_draws_nt = if(P_r>0) Omega_draws_n[seq(1,R,Q),,drop=FALSE] else NA
  Sigma_draws_nt =           Sigma_draws_n[seq(1,R,Q),,drop=FALSE] 
  
  gibbs_samples_nt = list("s_draws_nt"     = s_draws_nt,
                          "alpha_draws_nt" = alpha_draws_nt,
                          "b_draws_nt"     = b_draws_nt,
                          "Omega_draws_nt" = Omega_draws_nt,
                          "Sigma_draws_nt" = Sigma_draws_nt)
  
  ### thinning of normalized and burned samples
  s_draws_nbt     = if(P_r>0) s_draws_nb[seq(1,R-B,Q),,drop=FALSE]     else NA
  alpha_draws_nbt = if(P_f>0) alpha_draws_nb[seq(1,R-B,Q),,drop=FALSE] else NA
  b_draws_nbt     = if(P_r>0) b_draws_nb[seq(1,R-B,Q),,drop=FALSE]     else NA
  Omega_draws_nbt = if(P_r>0) Omega_draws_nb[seq(1,R-B,Q),,drop=FALSE] else NA
  Sigma_draws_nbt =           Sigma_draws_nb[seq(1,R-B,Q),,drop=FALSE] 
  
  gibbs_samples_nbt = list("s_draws_nbt"     = s_draws_nbt,
                           "alpha_draws_nbt" = alpha_draws_nbt,
                           "b_draws_nbt"     = b_draws_nbt,
                           "Omega_draws_nbt" = Omega_draws_nbt,
                           "Sigma_draws_nbt" = Sigma_draws_nbt)
  
  return(list("gibbs_samples_n"   = gibbs_samples_n,
              "gibbs_samples_nb"  = gibbs_samples_nb,
              "gibbs_samples_nt"  = gibbs_samples_nt,
              "gibbs_samples_nbt" = gibbs_samples_nbt,
              "C_est"             = C_est))
}
