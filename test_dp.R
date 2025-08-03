### parameter specification
set.seed(1)
N <- 500
s <- c(0.5, 0.3, 0.2)
z <- sample.int(3, size = N, replace = TRUE, prob = s)
b <- matrix(c(0, 0, 2, 0, 0, -2), ncol = 3)
Omega <- matrix(
  c(1.3, -1, -1, 1.3, 0.2, 0, 0, 0.2, 0.3, 0.2, 0.2, 0.3), ncol = 3
)

### sample data points
beta <- sapply(
  z, function(z) oeli::rmvnorm(n = 1, b[, z], matrix(Omega[, z], ncol = 2))
)

### uninformed parameter values for conjugate prior distributions
xi <- numeric(2); D <- 10 * diag(2) # mean and covariance for b
nu <- 4; Theta <- 10 * diag(2)      # df and scale for Omega
delta <- 1                         # concentration for s

### uninformed initial parameter values
z_update <- rep(1, N)
b_update <- matrix(0, nrow = 2)
Omega_update <- matrix(c(1, 0, 0, 1), nrow = 4)

### run Dirichlet process
R <- 1000
C_seq <- numeric(R)
for(r in 1:R) {
  update <- RprobitB:::update_classes_dp2(
    Cmax = 10, beta = beta, z = z_update, b = b_update, Omega = Omega_update,
    delta = delta, xi = xi, D = D, nu = nu, Theta = Theta
  )
  z_update <- update$z
  b_update <- update$b
  Omega_update <- update$Omega
  C_seq[r] <- update$C
}

table(C_seq) / R * 100
