#' Compute WAIC value.
#'
#' @description
#' This function computes the WAIC value of an \code{RprobitB_model}.
#'
#' @param x
#' An object of class \code{RprobitB_model}.
#' @param S
#' The number of posterior samples used for the calculation of WAIC. Must be
#' greater or equal two for variance computation.
#' @param progress
#' A boolean whether to print computation progress.
#' @param check_conv
#' A boolean, determining whether to plot the convergence behavior of the WAIC
#' calculation.
#' @param ncores
#' Computation of \code{p_si} is parallized, set the number of cores.
#'
#' @return
#' Invisibly returns a list of
#' \itemize{
#'   \item \code{waic}, the WAIC value,
#'   \item \code{se_waic}, the standard error of \code{waic}
#'   \item \code{p_si}, the matrix of probabilities of each observation for the
#'         \code{S} different posterior samples,
#'   \item \code{S}, the number of posterior samples,
#'   \item \code{lppd}, the log pointwise predictive density,
#'   \item \code{p_waic}, the effective number of parameters,
#'   \item \code{p_waic_i}, the vector of summands of \code{p_waic}.
#' }
#'
#' @keywords
#' internal
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cov | 0,
#'   N = 10,
#'   T = 10,
#'   J = 2,
#'   seed = 1
#' )
#' x <- mcmc(data)
#' RprobitB:::waic(x = x, S = 10, progress = TRUE, check_conv = TRUE, ncores = 2)
#'
#' @importFrom progress progress_bar
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal

waic <- function(x, S = 1000, progress = TRUE, check_conv = FALSE,
                 ncores = parallel::detectCores() - 1) {

  ### check input
  stopifnot(class(x) == "RprobitB_fit")
  stopifnot(is.numeric(S), length(S) == 1, S%%1==0)
  S <- max(2, S)

  ### calculate p_si and log(p_si)
  p_si <- compute_p_si(x = x, S = S, progress = progress, ncores = ncores)
  log_p_si <- log(p_si)

  ### calculate WAIC
  lppd <- sum(log(rowSums(p_si)) - log(S))
  p_waic_i <- apply(log_p_si, 1, var)
  p_waic <- sum(p_waic_i)
  waic <- -2*(lppd - p_waic)
  se_waic <- sqrt(nrow(p_si) * var(p_waic_i))

  if(check_conv){
    ### print progress
    if(progress){
      pb <- progress::progress_bar$new(format = "Preparing WAIC convergence graphic: :percent",
                                       total = S-1, clear = FALSE)
    }

    ### compute sequence of waic value for progressive sets of posterior samples
    waic_seq <- numeric(S)
    se_waic_seq <- numeric(S)
    for(s in 2:S){
      if(progress){
        pb$tick()
      }
      lppd_temp <- sum(log(rowSums(p_si[,1:s,drop=FALSE])) - log(s))
      p_waic_i_temp <- apply(log_p_si[,1:s,drop=FALSE], 1, var)
      p_waic_temp <- sum(p_waic_i_temp)
      waic_seq[s] <- -2*(lppd_temp - p_waic_temp)
      se_waic_seq[s] <- sqrt(nrow(p_si) * var(p_waic_i_temp))
    }
    seq <- data.frame(waic_seq = waic_seq[-1],
                           se_waic_seq = se_waic_seq[-1])
    ### plot sequence
    p <- ggplot2::ggplot(data = seq, ggplot2::aes(x = 1:nrow(seq), y = waic_seq)) +
      ggplot2::geom_line() +
      ggplot2::geom_ribbon(ggplot2::aes(ymin = waic_seq - se_waic_seq,
                           ymax = waic_seq + se_waic_seq), alpha=0.2) +
      ggplot2::labs(x = "Number of posterior samples",
                    y = "WAIC",
                    title = "The WAIC value for different sizes of posterior samples") +
      ggplot2::theme_minimal()
    print(p)
  }

  ### prepare and return output
  out <- list("waic" = waic,
              "se_waic" = se_waic,
              "p_si" = p_si,
              "S" = S,
              "lppd" = lppd,
              "p_waic" = p_waic,
              "p_waic_i" = p_waic_i)
  return(invisible(out))
}

#' Compute probability for each observation for different samples from the posterior.
#'
#' @description
#' This function computes the probability for each observation for different samples
#' from the posterior.
#'
#' @inheritParams waic
#'
#' @return
#' A matrix, observations in rows and posterior samples in columns.
#'
#' @keywords
#' internal
#'
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW
#' @importFrom progress progress_bar

compute_p_si <- function(x, S, progress = TRUE, ncores = parallel::detectCores() - 1) {

  ### check input
  stopifnot(class(x) == "RprobitB_fit")
  stopifnot(is.numeric(S), length(S) == 1, S%%1==0)
  stopifnot(class(progress) == "logical", length(progress) == 1)
  S_max <- (x$R - x$B) / x$Q
  S <- min(S, S_max)
  s_vec <- sort(sample.int(n = S_max, size = S))

  ### extract meta parameters
  N = x$data$N
  T = x$data$T
  J = x$data$J
  P_f = x$data$P_f
  P_r = x$data$P_r
  C = x$latent_classes$C

  ### extract Gibbs samples
  gibbs_samples_nbt = x$gibbs_samples$gibbs_samples_nbt
  Sigma_samples = gibbs_samples_nbt$Sigma[s_vec, , drop = FALSE]
  alpha_samples = gibbs_samples_nbt$alpha[s_vec, , drop = FALSE]
  s_samples = gibbs_samples_nbt$s[s_vec,  ,drop = FALSE]
  b_samples = gibbs_samples_nbt$b[s_vec, , drop = FALSE]
  Omega_samples = gibbs_samples_nbt$Omega[s_vec, , drop = FALSE]

  ### extract parameters of each sample
  pars = list()
  for(s in 1:S){
    pars[[s]] <-  RprobitB_parameter(
      P_f = P_f, P_r = P_r, J = J, N = N,
      alpha = as.numeric(alpha_samples[s,]), C = C,
      s = as.numeric(s_samples[s,]),
      b = matrix(b_samples[s,], nrow = P_r, ncol = C),
      Omega = matrix(Omega_samples[s,], nrow = P_r^2, ncol = C),
      Sigma = matrix(Sigma_samples[s,], J-1, J-1),
      sample = FALSE)
  }

  ### register parallel backend
  cluster <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cluster)

  ### register progress bar
  if(progress){
    pb <- progress::progress_bar$new(format = "Computing p_si: :percent", total = S, clear = FALSE)
    opts <- list(progress = function(n) pb$tick())
  } else {
    opts <- list()
  }

  ### compute probability for each observation i (rows) for each sample s (columns)
  p_si <- foreach::foreach(s = 1:S, .packages = "RprobitB", .combine = "cbind",
                           .options.snow = opts) %dopar% {
                             out <- c()
                             for(n in 1:N){
                               X_n = x$data$data[[n]]$X
                               y_n = x$data$data[[n]]$y
                               for(t in 1:T[n]) {
                                 X_nt = X_n[[t]]
                                 y_nt = y_n[t]
                                 alt_index <- which(x$data$alternatives == y_nt)
                                 out <- c(out, compute_choice_probabilities(
                                   X = X_nt, alternatives = alt_index, parameter = pars[[s]])[alt_index])
                               }
                             }
                             out
                           }

  ### stop parallel backend
  parallel::stopCluster(cluster)

  ### return p_si
  return(p_si)
}
