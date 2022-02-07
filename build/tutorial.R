set.seed(1)

### draw data
b_true <- matrix(c(1.5,1.5,1.5,-1.5,-1.5,1.5), ncol = 3)
Omega_true <- matrix(c(0.3, 0.05, 0.05, 0.3, 0.5, -0.08, -0.08, 0.2, 0.8, 0.5, 0.5, 0.8), ncol = 3)
data <- c()
for(c in 1:3){
  data <- rbind(data, MASS::mvrnorm(n = 50, mu = b_true[,c], Sigma = matrix(Omega_true[,c], ncol = 2)))
}
plot(data)

### prior settings
delta <- 0.01 # concentration parameter of Dirichlet distribution
xi <- matrix(rep(0, 2), ncol = 2) # prior mean for each b_c
D <- diag(2) # prior covariance for each b_c
nu <- 4 # prior mean for each Omega_c
Theta <- diag(2) # prior covariance for each Omega_c

### initial values
z0 <- rep(1, nrow(data)) # initial data point assignment

### run Gibbs sampler
results <- crp_gibbs(data = data,
                     delta = delta, xi = xi, D = D, nu = nu, Theta = Theta,
                     z0 = z0,
                     R = 1000)

### assign classes that occur most often
tab <- apply(results$z_samples, 1, FUN = function(x) {
  tab <- table(x)
  ans <- names(tab[which.max(tab)])
  return(as.numeric(ans))
})
table(tab)

### Gibbs sampler definition
crp_gibbs <- function(data,
                      delta, xi, D, nu, Theta,
                      z0, R, Cmax = 100){

  ### data dimension
  N <- nrow(data)

  ### initialization
  z <- z0
  m <- as.vector(table(z))
  C <- length(m)
  b <- matrix(0, nrow = 2, ncol = Cmax)
  Omega <- matrix(rep(diag(2), C), nrow = 4, ncol = Cmax)
  pb <- txtProgressBar(min = 0, max = R, style = 3)

  ### storage of Gibbs samples
  z_samples <- matrix(NA, nrow = N, ncol = R)

  ### Gibbs sampler
  for(r in 1:R){

    ### Dirichlet Process
    for(n in 1:N){
      ### un-assign initial class membership
      m[z[n]] <- m[z[n]] - 1

      ### remove empty table
      if(m[z[n]] == 0) {
        m[z[n]] <- m[C]
        z[z == C] <- z[n]
        m <- m[-C]
        C <- C - 1
      }

      ### ensure that z[n] does not get counted as a cluster
      z[n] <- -1

      ### storage for cluster log-probabilities
      logp <- rep(NA, C + 1)

      ### update cluster characteristics
      for(c in 1:C){

        ### extract data points currently allocated to cluster c
        y_c <- data[z == c, , drop = FALSE]

        ### update Omega_c via mean of its posterior distribution
        Omega_c <- (Theta + crossprod(t(apply(y_c, 1, function(x) x - b[,c])))) / (m[c]+nu-ncol(data)-1)
        Omega[,c] <- as.vector(Omega_c)

        ### compute covariance (sig_b) and mean (mu_b) of posterior distribution of b_c
        sig_b <- solve(solve(D) + m[c] * solve(matrix(Omega_c, 2, 2)))
        mu_b <- sig_b %*% (solve(matrix(Omega_c, 2, 2)) %*% colSums(y_c) + solve(D) %*% t(xi))

        ### update b_c via mean of its posterior distribution
        b[,c] <- mu_b

        ### compute log-density of data point n from its posterior predictive distribution
        density_n <- dmvnorm(data[n,], mean = mu_b, Sigma = sig_b + Omega_c, log = TRUE)

        ### compute cluster assignment probabilities for existing cluster (conditioned on existing clusters)
        logp[c] <- log(m[c]) + density_n
      }

      ### compute probability for new cluster (conditioned on existing clusters)
      est_Omega <- matrix(Omega[,1:C, drop = FALSE] %*% (m/sum(m)), ncol = 2)
      logp[C+1] <- log(delta) + mvtnorm::dmvnorm(data[n,], mean = xi, sigma = D + est_Omega, log = TRUE)

      ### transform log-probabilities to probabilities
      max_logp <- max(logp)
      logp <- logp - max_logp
      loc_probs <- exp(logp)
      loc_probs <- loc_probs / sum(loc_probs)

      ### draw new cluster membership
      newz <- sample(1:(C+1), 1, prob = loc_probs)
      if(newz == C + 1){
        ### spawn new cluster
        m <- c(m, 0)
        C <- C + 1
      }
      z[n] <- newz
      m[newz] <- m[newz] + 1
    }

    setTxtProgressBar(pb, r)
    z_samples[, r] <- z
  }

  close(pb)
  return(list("z_samples" = z_samples))
}
