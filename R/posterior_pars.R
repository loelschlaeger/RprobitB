#' Parameter sets from posterior samples
#'
#' @description
#' This function builds parameter sets from the normalized, burned and
#' thinned posterior samples.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @return
#' A list of \code{RprobitB_parameter} objects.
#'
#' @keywords
#' internal

posterior_pars <- function(x) {
  ### check input
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be an object of class 'RprobitB_fit'.",
         call. = FALSE
    )
  }

  ### extract meta parameters
  N <- x$data$N
  T <- x$data$T
  J <- x$data$J
  P_f <- x$data$P_f
  P_r <- x$data$P_r
  C <- x$latent_classes$C

  ### extract samples
  sample_size <- (x$R - x$B) / x$Q
  gibbs_samples_nbt <- x$gibbs_samples$gibbs_samples_nbt
  Sigma_samples <- gibbs_samples_nbt$Sigma
  alpha_samples <- gibbs_samples_nbt$alpha
  s_samples <- gibbs_samples_nbt$s
  b_samples <- gibbs_samples_nbt$b
  Omega_samples <- gibbs_samples_nbt$Omega

  ### extract parameters of each sample
  pars <- list()
  for (s in 1:sample_size) {
    pars[[s]] <- RprobitB_parameter(
      P_f = P_f,
      P_r = P_r,
      J = J,
      N = N,
      alpha = as.numeric(alpha_samples[s, ]),
      C = C,
      s = as.numeric(s_samples[s, ]),
      b = matrix(b_samples[s, ], nrow = P_r, ncol = C),
      Omega = matrix(Omega_samples[s, ], nrow = P_r^2, ncol = C),
      Sigma = matrix(Sigma_samples[s, ], J - 1, J - 1),
      sample = FALSE
    )
  }

  ### return 'pars'
  return(pars)
}
