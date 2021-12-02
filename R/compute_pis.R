#' Compute probability for each observation for different samples from the posterior.
#' @description
#' This function computes the probability for each observation for different samples
#' from the posterior.
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param no_samples
#' The number of different samples taken from the posterior.
#' @return
#' A matrix.
#' @export

# TODO: Add progress bar. Parallelize.
compute_pis <- function(x, no_samples = 2) {

  ### check input
  stopifnot(class(x) == "RprobitB_model")
  total_no_samples = (x$R - x$B) / x$Q
  no_samples = max(2, min(no_samples, total_no_samples))
  sample_indices = sample.int(n = total_no_samples, size = no_samples)

  ### extract meta parameters
  N = x$data$N
  T = x$data$T
  J = x$data$J
  P_f = x$data$P_f
  P_r = x$data$P_r

  ### extract Gibbs samples
  gibbs_samples_nbt = x$gibbs_samples$gibbs_samples_nbt
  Sigma_samples = gibbs_samples_nbt$Sigma[sample_indices, ,drop = FALSE]
  alpha_samples = gibbs_samples_nbt$alpha[sample_indices, ,drop = FALSE]
  s_samples = gibbs_samples_nbt$s[sample_indices, ,drop = FALSE]
  b_samples = gibbs_samples_nbt$b[sample_indices, ,drop = FALSE]
  Omega_samples = gibbs_samples_nbt$Omega[sample_indices, ,drop = FALSE]

  ### extract parameters of each sample
  pars = list()
  for(s in 1:no_samples){
    pars[[s]] <-  RprobitB_parameter(
      P_f = P_f, P_r = P_r, J = J, N = N,
      alpha = as.numeric(alpha_samples[s,]), C = C,
      s = as.numeric(s_samples[s,]),
      b = matrix(b_samples[s,], nrow = P_r, ncol = C),
      Omega = matrix(Omega_samples[s,], nrow = P_r^2, ncol = C),
      Sigma = matrix(Sigma_samples[s,], J-1, J-1),
      sample = FALSE)
  }

  ### compute probability for each observation i (rows) for each sample s (columns)
  pis = matrix(NA, nrow = sum(T), ncol = no_samples)
  for(n in 1:N) {
    X_n = x$data$data[[n]]$X
    y_n = x$data$data[[n]]$y
    for(t in 1:T[n]) {
      i <- sum(T[seq_len(n-1)]) + t
      X_nt = X_n[[t]]
      y_nt = y_n[t]
      for(s in 1:no_samples){
        pis[i,s] = compute_choice_probabilities(X = X_nt, alternatives = y_nt,
                                                parameter = pars[[s]])[y_nt]
      }
    }
  }

  ### return pis
  return(pis)
}
