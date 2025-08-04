### parameter specification
set.seed(1)
N <- 200
s <- c(0.5, 0.3, 0.2)
z <- sample.int(3, size = N, replace = TRUE, prob = s)
b <- matrix(c(0, 0, 4, 0, 0, -4), ncol = 3)
Omega <- matrix(
  c(1.3, -1, -1, 1.3, 0.2, 0, 0, 0.2, 0.3, 0.2, 0.2, 0.3), ncol = 3
)

### sample data points
beta <- sapply(
  z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], ncol = 2))
)

### uninformed parameter values for conjugate prior distributions
delta <- 1
mu_b_0 <- numeric(2)
Sigma_b_0 <- 10 * diag(2)
n_Omega_0 <- 4
V_Omega_0 <- diag(2)

### uninformed initial parameter values
z_update <- rep(1, N)
b_update <- matrix(0, nrow = 2)
Omega_update <- matrix(c(1, 0, 0, 1), nrow = 4)

### run Dirichlet process
R <- 1000
C_seq <- numeric(R)
z_correct <- numeric(R)

for(r in 1:R) {
  cat(r, "\n")
  update <- update_classes_dp_new(
    beta = beta, z = z_update, b = b_update, Omega = Omega_update,
    delta = delta, mu_b_0 = mu_b_0, Sigma_b_0 = Sigma_b_0,
    n_Omega_0 = n_Omega_0, V_Omega_0 = V_Omega_0
  )
  z_update <- update$z
  b_update <- update$b
  Omega_update <- update$Omega
  C_seq[r] <- update$C
  z_correct[r] <- sum(z == z_update) / N
}

table(C_seq) / R * 100
z_correct |> plot(type = "l")

