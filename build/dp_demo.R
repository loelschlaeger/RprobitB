# Dirichlet Process with fixed beta ---------------------------------------
library(RprobitB)

### simulate beta
set.seed(1)
P_r <- 2
C_true <- 3
N <- sample(40:100, C_true)
(b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true))
(Omega_true <- matrix(replicate(C_true, rwishart(P_r + 1, 0.3*diag(P_r))$W, simplify = TRUE), nrow = P_r*P_r, ncol = C_true))
beta <- c()
for(c in 1:C_true) for(n in 1:N[c])
  beta <- cbind(beta, rmvnorm(mu = b_true[,c,drop=F], Sigma = matrix(Omega_true[,c,drop=F], ncol = P_r)))
z_true <- rep(1:3, times = N)
RprobitB:::plot_class_allocation(beta = beta, z = z_true, b = b_true, Omega = Omega_true, perc = 0.95)

### prior parameters
delta <- 0.3 # Adjust!
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)

### initial values
z <- rep(1, ncol(beta))
C <- length(unique(z))
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r*P_r, ncol = C)

### Dirichlet process
for(r in 1:1000){
  dp <- RprobitB:::update_classes_dp(Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta, s_desc = FALSE)
  z <- dp$z
  b <- dp$b
  Omega <- dp$Omega
  RprobitB:::plot_class_allocation(beta, z, b, Omega, r = r, perc = 0.95, sleep = 0.1)
}





