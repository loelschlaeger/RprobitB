z <- sample.int(3, size = 500, replace = TRUE)
b <- matrix(c(0.1, 0.1, 2, 0, 0, -2), ncol = 3)
Omega <- matrix(
  c(1.3, -1, -1, 1.3, 0.2, 0, 0, 0.2, 0.3, 0.2, 0.2, 0.3), ncol = 3
)
beta <- sapply(
  z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], ncol = 2))
)
delta <- 1
mu_b_0 <- numeric(2)
Sigma_b_0 <- 10 * diag(2)
n_Omega_0 <- 4
V_Omega_0 <- diag(2)
identify_classes <- TRUE
Cmax <- 10

update_classes_dp <- function(
  beta, z, b, Omega,
  delta, mu_b_0, Sigma_b_0, n_Omega_0, V_Omega_0,
  identify_classes = TRUE, Cmax = 10
) {

  N <- length(z)
  C <- ncol(b)
  P_r <- nrow(b)
  Sigma_b_0_inv <- solve(Sigma_b_0)

  ## allocate space for class characteristics
  b_full <- matrix(0, nrow = P_r, ncol = Cmax)
  b_full[, 1:C] <- b
  Omega_full <- matrix(0, nrow = P_r * P_r, ncol = Cmax)
  Omega_full[, 1:C] <- Omega
  m_full <- numeric(Cmax)
  m_full[1:C] <- update_m(C, z, TRUE)


  tmp <- numeric(N)

  for (n in seq_len(N)) {

    ## unassign current class membership
    m_full[z[n]] <- m_full[z[n]] - 1

    ## check for empty class
    if (m_full[z[n]] == 0) {

      ### replace the empty class by the last class to prevent a hole
      m_full[z[n]] <- m_full[C]
      z[z == C] <- z[n]
      m_full[C] <- 0
      b_full[, z[n]] <- b_full[, C]
      b_full[, C] <- 0
      Omega_full[, z[n]] <- Omega_full[, C]
      Omega_full[, C] <- 0

      ## decrease class number
      C <- C - 1
    }

    z[n] <- -1

    logp <- numeric(C + 1)
    for (c in seq_len(C)) {

      # TODO: update this in doc

      logp[c] <- log(m_full[c]) + oeli::dmvnorm(
        beta[, n],
        mean = b_full[, c],
        Sigma = matrix(Omega_full[, c], nrow = P_r, ncol = P_r), log = TRUE
      )
    }

    ppd <- 0
    for (r in 1:10) {
      b_draw <- oeli::rmvnorm(n = 1, mean = mu_b_0, Sigma = Sigma_b_0)
      Omega_draw <- oeli::rwishart(df = n_Omega_0, scale = V_Omega_0, inv = TRUE)
      ppd <- ppd + 0.1 * oeli::dmvnorm(beta[, n], mean = b_draw, Sigma = Omega_draw)
    }

    logp[C + 1] <- log(delta) + log(ppd)

    p <- exp(logp - max(logp))
    p <- p / sum(p)


    tmp[n] <- which.max(p)

  }


}
