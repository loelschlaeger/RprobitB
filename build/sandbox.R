# Dirichlet Process with fixed beta ---------------------------------------

### simulate beta
set.seed(1)
P_r <- 2
C_true <- 3
N <- sample(40:100, C_true)
b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true)
Omega_true <- matrix(replicate(C_true, rwishart(P_r, 0.1*diag(P_r))$W, simplify = TRUE), nrow = P_r*P_r, ncol = C_true)
beta <- c()
for(c in 1:C_true) for(n in 1:N[c])
  beta <- cbind(beta, rmvnorm(mu = b_true[,c,drop=F], Sigma = matrix(Omega_true[,c,drop=F], ncol = P_r)))

### prior parameters
delta <- 1
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
pb <- txtProgressBar(min = 0, max = 100, style = 3)
for(r in 1:100){
  dp <- update_classes_dp(Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta)
  z <- dp$z
  b <- dp$b
  Omega <- dp$Omega
  plot_class_allocation(beta, z, b, Omega, r = r)
  setTxtProgressBar(pb, r)
}

# Simulate choices --------------------------------------------------------

data <- simulate_choices(
  form = choice ~ cost,
  N = 100,
  T = 10,
  J = 3,
  re = c("cost"),
  alternatives = c("train", "bus", "car"),
  seed = 1,
  C = 2
)
plot(data)

# Estimation --------------------------------------------------------------

model <- mcmc(
  data,
  R = 2000,
  B = 1998,
  print_progress = TRUE,
  seed = 1,
  latent_classes = list("C" = 8,
                        "weight_update" = TRUE,
                        "dp_update" = TRUE,
                        "Cmax" = 10,
                        "epsmin" = 0.1,
                        "epsmax" = 0.9,
                        "distmin" = 0.1)
)




### simulate beta
set.seed(1)
P_r <- 2
C_true <- 3
N <- c(5,10,15)
b_true <- matrix(replicate(C_true, rnorm(P_r)), nrow = P_r, ncol = C_true)
Omega_true <- matrix(replicate(C_true, rwishart(P_r, 0.1*diag(P_r))$W, simplify = TRUE), nrow = P_r*P_r, ncol = C_true)
beta <- c()
for(c in 1:C_true) for(n in 1:N[c])
  beta <- cbind(beta, rmvnorm(mu = b_true[,c,drop=F], Sigma = matrix(Omega_true[,c,drop=F], ncol = P_r)))

### prior parameters
delta <- 1
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)

### initial values
z <- rep(1, ncol(beta))
C <- length(unique(z))
b <- matrix(0, nrow = P_r, ncol = C)
Omega <- matrix(rep(diag(P_r), C), nrow = P_r*P_r, ncol = C)

dpr <- update_classes_dp_r(Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta)
dpc <- update_classes_dp(Cmax = 10, beta, z, b, Omega, delta, xi, D, nu, Theta)











