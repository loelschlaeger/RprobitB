set.seed(11)

### draw data
n <- 60
b <- matrix(c(1.5,1.5,1.5,-1.5,-1.5,1.5,-1.5,-1.5), ncol = 4)
Omega <- matrix(c(0.3, 0.05, 0.05, 0.3, 0.5, -0.08, -0.08, 0.2, 0.1, 0.03, 0.03, 0.1, 0.8, 0.5, 0.5, 0.8), ncol = 4)
data <- c()
for(c in 1:4){
  data <- rbind(data, MASS::mvrnorm(n = 60, mu = b[,c], Sigma = matrix(Omega[,c], ncol = 2)))
}
plot(data)

### prior settings
delta <- 0.01 # concentration parameter of Dirichlet distribution
xi <- matrix(rep(0, 2), ncol = 2) # prior mean for class means
D <- diag(2) * 9 # prior variance for class means
sigma_y <- diag(2) # data variance (assumed known and same for all classes)

### initial values
z0 <- rep(1, nrow(data)) # initial data point assignment

### run Gibbs sampler
results <- crp_gibbs(data = data, delta = delta, xi = xi, D = D,
                     sigma_y = sigma_y, z0 = z0, R = 1000)

### assign classes that occur most often
tab <- apply(results$z_samples, 1, FUN = function(x) {
  tab <- table(x)
  ans <- names(tab[which.max(tab)])
  return(as.numeric(ans))
})
table(tab)

### Gibbs sampler definition
crp_gibbs <- function(data, delta, xi, D, sigma_y, z0, R){

  ### data dimension
  N <- nrow(data)

  ### initialization
  z <- z0
  m <- as.vector(table(z))
  C <- length(m)
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

        ### extract data points currently present in this cluster
        y <- data[z == c, , drop = FALSE]

        ### compute cluster precision and variance
        tau_p <- solve(D) + m[c] * solve(sigma_y)
        sig_p <- solve(tau_p)

        ### update cluster covariance
        Omega_c <- (D + crossprod(t(apply(y, 1, function(x) x - mean_p)))) / (m[c]+2-ncol(data)-1)

        ### update cluster mean
        mean_p <- sig_p %*% (solve(sigma_y) %*% colSums(y) + solve(D) %*% t(xi))

        ### compute cluster assignment probabilities for existing cluster (conditioned on existing clusters)
        logp[c] <- log(m[c]) + mvtnorm::dmvnorm(data[n,], mean = mean_p, sigma = sig_p + sigma_y, log = TRUE)
      }

      ### compute probability for new cluster (conditioned on existing clusters)
      logp[C+1] <- log(delta) + mvtnorm::dmvnorm(data[n,], mean = xi, sigma = D + sigma_y, log = TRUE)

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
