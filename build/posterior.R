#' @inheritParams RprobitB_parameter
#' @inheritParams check_prior
#' @param plot
#' A boolean, set to \code{TRUE} to plot the class allocation of \code{beta}.
#' @inheritParams plot_class_allocation

update_z_dp <- function(beta, z, b, Omega, delta, xi, D, nu, Theta, plot = FALSE, ...) {

  ### class sizes
  m <- as.vector(table(z))
  C <- length(m)

  ### update Dirichlet process
  for(n in 1:length(z)){

    ### un-assign initial class membership
    m[z[n]] <- m[z[n]] - 1

    ### remove empty table
    if(m[z[n]] == 0) {
      m[z[n]] <- m[C]
      z[z == C] <- z[n]
      m <- m[-C]
      C <- C - 1
    }

    ### ensure that z[n] does not get counted
    z[n] <- -1

    ### storage for class log-probabilities
    logp <- rep(NA, C + 1)

    ### update class characteristics
    for(c in 1:C){

      ### extract beta points currently allocated to class c
      beta_c <- beta[, z == c, drop = FALSE]

      ### update Omega_c via mean of its posterior distribution
      Omega_c <- (Theta + crossprod(t(apply(beta_c, 2, function(x) x - b[,c,drop=FALSE])))) / (m[c]+nu-nrow(beta)-1)
      Omega[,c] <- as.vector(Omega_c)

      ### compute covariance (sig_b) and mean (mu_b) of posterior distribution of b_c
      sig_b <- solve(solve(D) + m[c] * solve(matrix(Omega_c, 2, 2)))
      mu_b <- sig_b %*% (solve(matrix(Omega_c, 2, 2)) %*% rowSums(beta_c) + solve(D) %*% xi)

      ### update b_c via mean of its posterior distribution
      b[,c] <- mu_b

      ### compute class assignment log-probabilities for existing classes from PPD
      logp[c] <- log(m[c]) + dmvnorm(beta[,n], mean = mu_b, Sigma = sig_b + Omega_c, log = TRUE)
    }

    ### compute log-probability for new class
    Omega_new <- matrix(Omega[,1:C, drop = FALSE] %*% (m/sum(m)), ncol = nrow(Omega)/2)
    b_new <- xi
    logp[C+1] <- log(delta) + dmvnorm(beta[,n], mean = b_new, Sigma = D + Omega_new, log = TRUE)

    ### transform log-probabilities to probabilities
    max_logp <- max(logp)
    logp <- logp - max_logp
    loc_probs <- exp(logp)
    loc_probs <- loc_probs / sum(loc_probs)

    ### draw new class membership
    newz <- sample(1:(C+1), 1, prob = loc_probs)
    if(newz == C + 1){
      ### spawn new class
      m <- c(m, 0)
      C <- C + 1
      ### add new class mean and covariance
      Omega <- cbind(Omega, as.vector(Omega_new))
      b <- cbind(b, as.vector(b_new))
    }
    z[n] <- newz
    m[newz] <- m[newz] + 1
  }

  ### draw a plot of current class allocation
  if(plot) plot_class_allocation(beta, z, b, Omega, m, ...)

  ### return updated parameters
  return(list("z" = z, "m" = m, "b" = b, "Omega" = Omega))
}
