set.seed(1)

### draw beta
P_r <- 2
C_true <- 3
N <- sample(40:100, C_true)
b_true <- replicate(C_true, rnorm(P_r))
Omega_true <- replicate(C_true, rwishart(P_r, 0.1*diag(P_r))$W, simplify = TRUE)
beta <- c()
for(c in 1:C_true){
  for(n in 1:N[c]){
    beta <- cbind(beta, rmvnorm(mu = b_true[,c], Sigma = matrix(Omega_true[,c], ncol = P_r)))
  }
}

### prior settings
delta <- 1
xi <- numeric(P_r)
D <- diag(P_r)
nu <- P_r + 2
Theta <- diag(P_r)

### initial values
z0 <- rep(1, ncol(beta))
C0 <- length(unique(z0))
b0 <- matrix(0, nrow = P_r, ncol = C0)
Omega0 <- matrix(rep(diag(P_r), C0), nrow = P_r*P_r, ncol = C0)

### run Gibbs sampler
results <- crp_gibbs(beta = beta,
                     delta = delta, xi = xi, D = D, nu = nu, Theta = Theta,
                     z0, b0, Omega0,
                     R = 1000, Cmax = 10)

### assign classes that occur most often
tab <- apply(results$z_samples, 1, FUN = function(x) {
  tab <- table(x)
  ans <- names(tab[which.max(tab)])
  return(as.numeric(ans))
})
table(tab)

### Gibbs sampler definition
crp_gibbs <- function(beta,
                      delta, xi, D, nu, Theta,
                      z0, b0, Omega0,
                      R, Cmax, plot = TRUE, ...){

  ### initialization
  z <- z0
  m <- as.vector(table(z))
  b <- b0
  Omega <- Omega0
  pb <- txtProgressBar(min = 0, max = R, style = 3)

  ### storage of Gibbs samples
  z_samples <- matrix(NA, nrow = ncol(beta), ncol = R)

  ### Gibbs sampler
  for(r in 1:R){

    ### Dirichlet Process
    out_dp <- update_classes_dp(beta = beta, z = z, b = b, Omega = Omega,
                                delta = delta, xi = xi, D = D, nu = nu, Theta = Theta,
                                Cmax = Cmax)
    s <- out_dp$s
    C <- length(s)
    z <- out_dp$z
    m <- as.numeric(table(z))
    b <- out_dp$b
    Omega <- out_dp$Omega

    ### draw a plot of current class allocation
    if(plot) plot_class_allocation(beta, z, b, Omega, m, r = r, ...)

    ### save samples
    z_samples[,r] <- z

    setTxtProgressBar(pb, r)
  }

  close(pb)
  return(list("z_samples" = z_samples))
}
