set.seed(11)

### draw data
n <- 60
means <- list(c(1.5,1.5),
              c(1.5,-1.5),
              c(-1.5,1.5),
              c(-1.5,-1.5))
Sigmas <- list(matrix(c(0.3, 0.05, 0.05, 0.3), ncol = 2),
               matrix(c(0.5, -0.08, -0.08, 0.2), ncol = 2),
               matrix(c(0.1, 0.03, 0.03, 0.1), ncol = 2),
               matrix(c(0.8, 0.5, 0.5, 0.8), ncol = 2))
clus1 <- MASS::mvrnorm(n = n, mu = means[[1]], Sigma = Sigmas[[1]])
clus2 <- MASS::mvrnorm(n = n, mu = means[[2]], Sigma = Sigmas[[2]])
clus3 <- MASS::mvrnorm(n = n, mu = means[[3]], Sigma = Sigmas[[3]])
clus4 <- MASS::mvrnorm(n = n, mu = means[[4]], Sigma = Sigmas[[4]])
data <- rbind(clus1, clus2, clus3, clus4)
plot(data)

### run Gibbs sampler
alpha <- 0.01 # concentration parameter of Dirichlet distribution
mu0 <- matrix(rep(0, 2), ncol = 2) # prior mean for class means
sigma0 <- diag(2) * 9 # prior variance for class means
sigma_y <- diag(2) # data variance (assumed known and same for all classes)
c_init <- rep(1, nrow(data)) # initial data point assignment
maxIters <- 1000 # number of Gibbs sampler iterations
results <- crp_gibbs(data = data, alpha = alpha, mu0 = mu0, sigma0 = sigma0,
                     sigma_y = sigma_y, c_init = c_init, maxIters = maxIters)

### assign classes that occur most often
tab <- apply(results, 1, FUN = function(x) {
  tab <- table(x)
  ans <- names(tab[which.max(tab)])
  return(as.numeric(ans))
})
table(tab)

### Gibbs sampler definition
crp_gibbs <- function(data, alpha, mu0, sigma0, sigma_y, c_init, maxIters){

  ### data dimension
  D <- ncol(data)
  N <- nrow(data)

  ### precision priors
  tau0 <- solve(sigma0)
  tau_y <- solve(sigma_y)

  ### initialization
  z <- c_init
  n_k <- as.vector(table(z))
  Nclust <- length(n_k)
  pb <- txtProgressBar(min = 0, max = maxIters, style = 3)

  ### storage of Gibbs samples
  cms <- matrix(NA, nrow = N, ncol = maxIters)
  cmeans <- list()
  cvariances <- list()

  ### Gibbs sampler
  for(iter in 1:maxIters){

    ### add storage space
    cmeans[[iter]] <- list()
    cvariances[[iter]] <- list()

    ### Dirichlet Process
    for(n in 1:N){
      ### un-assign initial class membership
      c_i <- z[n]
      n_k[c_i] <- n_k[c_i] - 1

      ### remove empty table
      if(n_k[c_i] == 0) {
        n_k[c_i] <- n_k[Nclust]
        z[z == Nclust] <- c_i
        n_k <- n_k[-Nclust]
        Nclust <- Nclust - 1
      }

      ### ensure that z[n] does not get counted as a cluster
      z[n] <- -1

      ### storage for cluster log-probabilities
      logp <- rep(NA, Nclust + 1)

      ### update cluster characteristics
      for(c_i in 1:Nclust){

        ### extract data points currently present in this cluster
        y <- data[z == c_i, , drop = FALSE]

        ### compute cluster precision and variance
        tau_p <- tau0 + n_k[c_i] * tau_y
        sig_p <- solve(tau_p)

        ### compute / update cluster mean
        mean_p <- sig_p %*% (tau_y %*% colSums(y) + tau0 %*% t(mu0))

        ### update cluster covariance
        sigma_p <- (sigma0 + crossprod(t(apply(y, 1, function(x) x - mean_p)))) / (n_k[c_i]+2-D-1)

        ### save cluster means and variances
        cmeans[[iter]][[c_i]] <- mean_p
        cvariances[[iter]][[c_i]] <- sigma_p

        ### compute cluster assignment probabilities for existing cluster (conditioned on existing clusters)
        logp[c_i] <- log(n_k[c_i]) + mvtnorm::dmvnorm(data[n,], mean = mean_p, sigma = sig_p + sigma_y, log = TRUE)
      }

      ### compute probability for new cluster (conditioned on existing clusters)
      logp[Nclust+1] <- log(alpha) + mvtnorm::dmvnorm(data[n,], mean = mu0, sigma = sigma0 + sigma_y, log = TRUE)

      ### transform log-probabilities to probabilities
      max_logp <- max(logp)
      logp <- logp - max_logp
      loc_probs <- exp(logp)
      loc_probs <- loc_probs / sum(loc_probs)

      ### draw new cluster membership
      newz <- sample(1:(Nclust+1), 1, prob = loc_probs)
      if(newz == Nclust + 1){
        ### spawn new cluster
        n_k <- c(n_k, 0)
        Nclust <- Nclust + 1
      }
      z[n] <- newz
      n_k[newz] <- n_k[newz] + 1
    }

    setTxtProgressBar(pb, iter)
    cms[, iter] <- z
  }

  close(pb)
  return(list("cms" = cms, "cmeans" = cmeans, "cvariances" = cvariances))
}
