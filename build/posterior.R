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

#' Plot class allocation (for \code{P_r = 2} only)
#' @description
#' This function plots the allocation of decision-maker specific coefficient vectors
#' \code{\beta} given the allocation vector \code{z}, the class means \code{b},
#' and the class covariance matrices \code{Omega}.
#' @details
#' Only in the two-dimensional case, i.e. only if \code{P_r = 2}.
#' @inheritParams RprobitB_parameter
#' @param ...
#' Optional visualization parameters:
#' \itemize{
#'   \item \code{colors}, a character vector of color specifications,
#'   \item \code{perc}, a numeric between 0 and 1 to draw the \code{perc} percentile
#'         ellipsoids for the underlying Gaussian distributions (\code{perc = 0.95} per default),
#'   \item \code{r}, the current iteration number of the Gibbs sampler to be displayed in the legend,
#'   \item \code{sleep}, the number of seconds to pause after plotting.
#' }
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal
#' @examples
#' b <- matrix(c(-1,1,1,1), ncol = 2)
#' Omega <- matrix(c(0.8,0.5,0.5,1,0.5,-0.2,-0.2,0.3), ncol = 2)
#' z <- rep(1:2, each = 10)
#' beta <- sapply(z, function(z) rmvnorm(mu = b[,z], Sigma = matrix(Omega[,z], ncol = 2)))
#' RprobitB:::plot_class_allocation(beta = beta, z = z, b = b, Omega = Omega,
#'                                  colors = c("red","blue"), perc = 0.5, r = 1)
#' @importFrom mixtools ellipse
#' @importFrom graphics legend

plot_class_allocation <- function(beta, z, b, Omega, ...) {
  m <- as.vector(table(z))
  graphic_pars <- list(...)
  if(!is.null(graphic_pars[["colors"]])){
    colors <- graphic_pars[["colors"]]
  } else {
    colors <- c('black','forestgreen', 'red2', 'orange', 'cornflowerblue',
                'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue',
                'mediumorchid1', 'firebrick4', 'yellowgreen', 'lightsalmon', 'tan3',
                'tan1', 'darkgray', 'wheat4', '#DDAD4B', 'chartreuse',
                'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                'darkolivegreen1' , 'tan2', 'tomato3', '#7CE3D8', 'gainsboro')
  }
  plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]))
  points(t(beta), col = colors[z], pch = 19)
  if(!is.null(graphic_pars[["perc"]])){
    perc <- graphic_pars[["perc"]]
  } else {
    perc <- 0.95
  }
  for(c in 1:length(m)){
    mixtools::ellipse(mu = b[,c], sigma = matrix(Omega[,c], ncol = nrow(Omega)/2),
                      alpha = 1 - perc, npoints = 250, col = colors[c])
  }
  if(!is.null(graphic_pars[["r"]])){
    title = paste("Iteration", graphic_pars[["r"]])
  } else {
    title = NULL
  }
  graphics::legend("topleft", legend = paste0("class ", 1:length(m), " (", round(m / sum(m) * 100), "%)"),
                   pch = 19, col = colors[1:length(m)], title = title)
  if(!is.null(graphic_pars[["sleep"]])){
    Sys.sleep(graphic_pars[["sleep"]])
  }
}
