#' Compare fitted models
#'
#' @description
#' This function returns a table with several criteria for model comparison.
#'
#' @details
#' See the vignette on model selection for more details.
#'
#' @param ...
#' One or more objects of class \code{RprobitB_fit}.
#' @param criteria
#' A vector of one or more of the following characters:
#' \itemize{
#'   \item \code{"npar"} for the number of model parameters (see \code{\link{npar}}),
#'   \item \code{"LL"} for the log-likelihood value (see \code{\link{logLik}}),
#'   \item \code{"AIC"} for the AIC value (see \code{\link{AIC}}),
#'   \item \code{"BIC"} for the BIC value (see \code{\link{BIC}}),
#'   \item \code{"WAIC"} for the WAIC value (also shows its standard error
#'         `sd(WAIC)` and the number `pWAIC` of effective model parameters,
#'         see \code{\link{WAIC}}),
#'   \item \code{"MMLL"} for the marginal model log-likelihood,
#'   \item \code{"BF"} for the Bayes factor,
#'   \item \code{"pred_acc"} for the prediction accuracy (see \code{\link{pred_acc}}).
#' }
#' @param add_form
#' Set to \code{TRUE} to add the model formulas.
#'
#' @return
#' A data frame, criteria in columns, models in rows.
#'
#' @export

model_selection <- function(..., criteria = c("npar", "LL", "AIC", "BIC"),
                            add_form = FALSE) {

  ### check inputs
  models <- as.list(list(...))
  model_names <- unlist(lapply(sys.call()[-1], as.character))[1:length(models)]
  for (i in seq_len(length(models))) {
    if (!inherits(models[[i]], "RprobitB_fit")) {
      stop(paste0("Input '", model_names[i],
                  "' is not of class 'RprobitB_fit'."),
           call. = FALSE)
    }
  }
  if (!is.character(criteria)) {
    stop("'criteria' must be a character vector.",
         call. = FALSE)
  }

  ### create output matrix
  output <- matrix(NA_real_, nrow = 0, ncol = length(models))
  colnames(output) <- model_names
  if(add_form){
    output <- rbind(output,
                    "form" = sapply(models, function(x) deparse1(x$data$form)))
  }

  ### fill output
  for (crit in unique(criteria)) {
    if (crit == "npar") {
      output <- rbind(output, "npar" = sapply(models, npar))
    }
    if (crit == "LL") {
      output <- rbind(output, "LL" = sapply(models, logLik))
    }
    if (crit == "AIC") {
      output <- rbind(output, "AIC" = sapply(models, AIC))
    }
    if (crit == "BIC") {
      output <- rbind(output, "BIC" = sapply(models, BIC))
    }
    if (crit == "WAIC") {
      waic_out <- lapply(models, WAIC)
      output <- rbind(output, "WAIC" = sapply(waic_out, function(x) x))
      output <- rbind(output, "se(WAIC)" = sapply(waic_out, function(x) attr(x, "se_waic")))
      output <- rbind(output, "pWAIC" = sapply(waic_out, function(x) attr(x, "p_waic")))
    }
    if (crit == "MMLL"){
      models <- lapply(models, mml)
      output <- rbind(output, "MMLL" = sapply(models, function(x) attr(x[["mml"]], "mmll")))
    }
    if (crit == "BF" && length(models) >= 2) {
      models <- lapply(models, mml)
      mmll_out <- sapply(models, function(x) attr(x[["mml"]], "mmll"))
      for (nmod in seq_len(length(models))) {
        rownames_old <- rownames(output)
        output <- rbind(output, exp(mmll_out - mmll_out[nmod]))
        rownames(output) <- c(rownames_old, paste0("BF(*,", model_names[nmod],")"))
      }
    }
    if (crit == "pred_acc") {
      output <- rbind(output, "pred_acc" = sapply(models, pred_acc))
    }
  }

  ### transform output to data frame
  output <- as.data.frame(output)

  class(output) <- c("RprobitB_model_selection", "data.frame")
  return(output)
}

#' @noRd
#' @export

print.RprobitB_model_selection <- function(x, digits = 2, ...) {
  for (row in rownames(x)) {
    if (row == "form") {
      x["form",] <- sprintf("%s", x["form",])
    }
    if (row == "LL") {
      x["LL",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["LL",]))
    }
    if (row == "AIC") {
      x["AIC",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["AIC",]))
    }
    if (row == "BIC") {
      x["BIC",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["BIC",]))
    }
    if (row == "WAIC") {
      x["WAIC",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["WAIC",]))
    }
    if (row == "se(WAIC)") {
      x["se(WAIC)",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["se(WAIC)",]))
    }
    if (row == "pWAIC") {
      x["pWAIC",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["pWAIC",]))
    }
    if (row == "MMLL") {
      x["MMLL",] <- sprintf(paste0("%.", digits, "f"), as.numeric(x["MMLL",]))
    }
    if (startsWith(row, "BF(")) {
      x[row,] <- as.numeric(sprintf(paste0("%.", digits, "f"), as.numeric(x[row,])))
      for (col in 1:ncol(x)) {
        if (is.na(x[row, col])) {
          x[row, col] <- "NA"
        } else if (as.numeric(x[row, col]) < 1 / 100) {
          x[row, col] <- "< 0.01"
        } else if (as.numeric(x[row, col]) > 100) {
          x[row, col] <- "> 100"
        }
      }
    }
    if (row == "pred_acc") {
      x["pred_acc",] <- sprintf(paste0("%.", digits, "f%%"), as.numeric(x["pred_acc",]) * 100)
    }
  }
  class(x) <- "data.frame"
  print(x)
}

#' @exportS3Method
#' @importFrom stats AIC

AIC.RprobitB_fit <- function(object, ..., k = 2) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik.RprobitB_fit)
  npar <- sapply(models, npar)
  aic <- mapply(function(ll, npar) -2 * ll + 2 * npar, ll, npar)
  return(aic)
}

#' @exportS3Method
#' @importFrom stats BIC

BIC.RprobitB_fit <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  ll <- sapply(models, logLik)
  npar <- sapply(models, npar)
  nobs <- sapply(models, nobs)
  bic <- mapply(function(ll, npar, nobs) -2 * ll + npar * log(nobs), ll, npar, nobs)
  return(bic)
}

#' Compute WAIC value
#'
#' @description
#' This function computes the WAIC value of an \code{RprobitB_fit} object.
#'
#' @details
#' WAIC is short for Widely Applicable (or Watanabe-Akaike) Information
#' Criterion. As for AIC and BIC, the smaller the WAIC value the better the
#' model. Its definition is
#' \deqn{WAIC = -2 \cdot lppd + 2 \cdot  p_{WAIC},}
#' where \eqn{lppd} stands for log pointwise predictive density and
#' \eqn{p_{WAIC}} is a penalty term proportional to the variance in the
#' posterior distribution that is sometimes called effective number of
#' parameters.
#' The \eqn{lppd} is approximated as follows. Let
#' \deqn{p_{is} = \Pr(y_i\mid \theta_s)} be the probability of observation
#' \eqn{y_i} given the \eqn{s}th set \eqn{\theta_s} of parameter samples from
#' the posterior. Then
#' \deqn{lppd = \sum_i \log S^{-1} \sum_s p_{si}.}
#' The penalty term is computed as the sum over the variances in log-probability
#' for each observation:
#' \deqn{p_{WAIC} = \sum_i V_{\theta} \left[ \log p_{si} \right].}
#' The \eqn{WAIC} has a standard error \eqn{SE} of
#' \deqn{SE = \sqrt{n \cdot V_i \left[-2 \left(lppd -
#' V_{\theta} \left[ \log p_{si} \right] \right)\right]},}
#' where \eqn{n} is the number of choices.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @return
#' A numeric, the WAIC value, with the following attributes:
#' \itemize{
#'   \item \code{se_waic}, the standard error of the WAIC value,
#'   \item \code{lppd}, the log pointwise predictive density,
#'   \item \code{p_waic}, the effective number of parameters,
#'   \item \code{p_waic_vec}, the vector of summands of \code{p_waic},
#'   \item \code{p_si}, the output of \code{\link{compute_p_si}}.
#' }
#'
#' @keywords
#' internal
#'
#' @export

WAIC <- function(x) {

  ### check input
  if(!inherits(x,"RprobitB_fit")){
    stop("'x' must be an object of class 'RprobitB_fit'.",
         call. = FALSE)
  }

  ### check if 'x' contains 'p_si'
  if(is.null(x[["p_si"]])){
    stop("Cannot compute WAIC.\n",
         "Please compute the probability for each observed choice at posterior samples first.\n",
         "For that, use the function 'compute_p_si()'.",
         call. = FALSE)
  }

  ### calculate p_si and log(p_si)
  p_si <- x[["p_si"]]
  log_p_si <- log(p_si)

  ### calculate WAIC
  lppd <- sum(log(rowSums(p_si)) - log(ncol(p_si)))
  p_waic_vec <- apply(log_p_si, 1, var)
  p_waic <- sum(p_waic_vec)
  waic <- -2*(lppd - p_waic)
  se_waic <- sqrt(nrow(p_si) * var(p_waic_vec))

  ### prepare and return output
  out <- waic
  attr(out, "se_waic") <- se_waic
  attr(out, "lppd") <- lppd
  attr(out, "p_waic") <- p_waic
  attr(out, "p_waic_vec") <- p_waic_vec
  attr(out, "p_si") <- p_si
  class(out) <- c("RprobitB_waic", "numeric")
  return(out)
}

#' @noRd
#' @export

print.RprobitB_waic <- function(x, digits = 2, ...) {
  cat(sprintf(paste0("%.", digits, "f", " (%.", digits, "f)"), x,
              attr(x, "se_waic")))
}

#' @noRd
#' @export
#' @importFrom ggplot2 ggplot aes geom_line geom_ribbon labs theme_minimal

plot.RprobitB_waic <- function(x, ...) {

  ### extract 'p_si' from 'x'
  p_si <- attr(x, "p_si")
  S <- ncol(p_si)
  log_p_si <- log(p_si)

  ### compute sequence of waic value for progressive sets of posterior samples
  pb <- RprobitB_pb(title = "Preparing WAIC convergence plot",
                    total = S,
                    tail = "Gibbs samples")
  waic_seq <- numeric(S)
  se_waic_seq <- numeric(S)
  RprobitB_pb_tick(pb)
  for(s in 2:S){
    RprobitB_pb_tick(pb)
    lppd_temp <- sum(log(rowSums(p_si[,1:s,drop=FALSE])) - log(s))
    p_waic_i_temp <- apply(log_p_si[,1:s,drop=FALSE], 1, var)
    p_waic_temp <- sum(p_waic_i_temp)
    waic_seq[s] <- -2*(lppd_temp - p_waic_temp)
    se_waic_seq[s] <- sqrt(nrow(p_si) * var(p_waic_i_temp))
  }
  seq <- data.frame(waic_seq = waic_seq[-1], se_waic_seq = se_waic_seq[-1])

  ### plot sequence
  p <- ggplot2::ggplot(data = seq, ggplot2::aes(x = 1:nrow(seq), y = waic_seq)) +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(
      ggplot2::aes(ymin = waic_seq - se_waic_seq,
                   ymax = waic_seq + se_waic_seq), alpha = 0.2) +
    ggplot2::labs(
      x = "Number of posterior samples",
      y = "WAIC",
      title = "The WAIC value for different sizes of posterior samples") +
    ggplot2::theme_minimal()
  print(p)
}

#' @exportS3Method
#' @importFrom stats nobs

nobs.RprobitB_fit <- function(object, ...) {
  return(sum(object$data$T))
}

#' @exportS3Method
#' @importFrom stats logLik

logLik.RprobitB_fit <- function(object, par_set = mean, recompute = FALSE, ...) {
  if(!is.null(object[["ll"]]) && !recompute){
    ll <- object[["ll"]]
  } else {
    probs <- choice_probabilities(x = object, par_set = par_set)
    choices <- as.character(unlist(sapply(object$data$data, `[[`, "y")))
    ll <- 0
    for (row in 1:nrow(probs)){
      if (object$data$ranked) {
        y_seq <- strsplit(choices[row], ",")[[1]][1]
        ll <- ll + log(probs[row, y_seq])
      } else {
        ll <- ll + log(probs[row, choices[row]])
      }
    }
  }
  return(as.numeric(ll))
}

#' Extract number of model parameters
#'
#' @description
#' This function extracts the number of model parameters of an
#' \code{RprobitB_fit} object.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#'
#' @param ...
#' Optionally more objects of class \code{RprobitB_fit}.
#'
#' @return
#' Either a numeric value (if just one object is provided) or a numeric vector.
#'
#' @export

npar <- function(object, ...) {
  UseMethod("npar")
}

#' @exportS3Method
#' @rdname npar

npar.RprobitB_fit <- function(object, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(object)
  } else {
    models <- c(list(object), models)
  }
  npar <- sapply(models, function(mod) {
    mod$data$P_f + (mod$data$P_r + mod$data$P_r^2) * mod$latent_classes$C +
      mod$data$J * (mod$data$J - 1) / 2 - 1
  })
  return(npar)
}

#' Compute choice probabilities at posterior samples
#'
#' @description
#' This function computes the probability for each observed choice at the
#' (normalized, burned and thinned) samples from the posterior. These
#' probabilities are required to compute the \code{\link{WAIC}} and the
#' marginal model likelihood \code{\link{mml}}.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param ncores
#' This function is parallelized, set the number of cores here.
#' @param recompute
#' Set to \code{TRUE} to recompute the probabilities.
#'
#' @return
#' The object \code{x}, including the object \code{p_si}, which is a matrix of
#' probabilities, observations in rows and posterior samples in columns.
#'
#' @export
#'
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster stopCluster
#' @importFrom doSNOW registerDoSNOW

compute_p_si <- function(x, ncores = parallel::detectCores() - 1, recompute = FALSE) {

  ### check input
  if(!inherits(x,"RprobitB_fit")){
    stop("'x' must be an object of class 'RprobitB_fit'.",
         call. = FALSE)
  }
  if(!(is.numeric(ncores) && length(ncores) == 1 && ncores > 0 && ncores%%1==0)){
    stop("'ncores' must be a positive integer.",
         call. = FALSE)
  }

  ### check if 'p_si' in 'x' already exists if 'recompute = FALSE'
  if(!recompute && !is.null(x$p_si)) {
    return(x)
  }

  ### extract pars from Gibbs samples
  pars <- posterior_pars(x)

  ### register parallel backend
  cluster <- parallel::makeCluster(ncores)
  doSNOW::registerDoSNOW(cluster)

  ### register progress bar
  if(getOption("RprobitB_progress")){
    pb <- RprobitB_pb(title = "Computing p_si",
                      total = length(pars),
                      tail = "parameter sets")
    opts <- list(progress = function(n) pb$tick())
  } else {
    opts <- list()
  }

  ### compute probability for each observation i (rows) for each sample s (columns)
  s <- NULL
  p_si <- foreach::foreach(s = 1:length(pars), .packages = "RprobitB",
                           .combine = "cbind", .options.snow = opts) %dopar% {
    out <- c()
    for(n in 1:x$data$N){
      X_n = x$data$data[[n]]$X
      y_n = x$data$data[[n]]$y
      for(t in 1:x$data$T[n]) {
        X_nt = X_n[[t]]
        y_nt = y_n[t]
        alt_index <- which(x$data$alternatives == y_nt)
        out <- c(out, compute_choice_probabilities(
          X = X_nt, alternatives = alt_index, parameter = pars[[s]])[alt_index]
        )
      }
    }
    out
  }

  ### stop parallel backend
  parallel::stopCluster(cluster)

  ### save 'p_si' in 'x'
  x[["p_si"]] <- p_si

  ### return 'x'
  return(x)
}

#' Approximate marginal model likelihood
#'
#' @description
#' This function approximates the model's marginal likelihood.
#'
#' @details
#' The model's marginal likelihood \eqn{p(y\mid M)} for a model \eqn{M} and data
#' \eqn{y} is required for the computation of Bayes factors. In general, the
#' term has no closed form and must be approximated numerically.
#'
#' This function uses the posterior Gibbs samples to approximate the model's
#' marginal likelihood via the posterior harmonic mean estimator.
#' To check the convergence, call `plot(x$mml)`, where `x` is the output
#' of this function. If the estimation does not seem to have
#' converged, you can improve the approximation by combining the value
#' with the prior arithmetic mean estimator. The final approximation of the
#' model's marginal likelihood than is a weighted sum of the posterior harmonic
#' mean estimate and the prior arithmetic mean estimate,
#' where the weights are determined by the sample sizes.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param S
#' The number of prior samples for the prior arithmetic mean estimate. Per
#' default, \code{S = 0}. In this case, only the posterior samples are used
#' for the approximation via the posterior harmonic mean estimator, see the
#' details section.
#' @param ncores
#' Computation of the prior arithmetic mean estimate is parallelized, set the
#' number of cores.
#' @param recompute
#' Set to \code{TRUE} to recompute the likelihood.
#'
#' @return
#' The object \code{x}, including the object \code{mml}, which is the model's
#' approximated marginal likelihood value.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom foreach %dopar%
#' @importFrom parallel makeCluster stopCluster detectCores
#' @importFrom doSNOW registerDoSNOW

mml <- function(x, S = 0, ncores = parallel::detectCores() - 1, recompute = FALSE) {

  ### input checks
  if(!inherits(x,"RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit.",
         call. = FALSE)
  }
  if(is.null(x[["p_si"]])){
    stop("Please compute the probability for each observed choice at posterior samples first.\n",
         "For that, use the function 'compute_p_si()'.",
         call. = FALSE)
  }
  if(!(is.numeric(S) && length(S)==1 && S>=0 && S%%1==0)){
    stop("'S' must be an integer.",
         call. = FALSE)
  }
  if(!(is.numeric(ncores) && length(ncores) == 1 && ncores > 0 && ncores%%1==0)){
    stop("'ncores' must be a positive integer.",
         call. = FALSE)
  }

  ### check if 'mml' in 'x' already exists if 'recompute = FALSE'
  if(!recompute && !is.null(x[["mml"]])) {
    return(x)
  }

  ### helper variables
  add_args <- list(P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
                   N = x$data$N, C = x$latent_classes$C, sample = FALSE)

  ### compute posterior harmonic mean estimate
  p_si <- x[["p_si"]]
  N <- nrow(p_si)
  S_post <- ncol(p_si)
  cont_post <- numeric(S_post)
  const <- round(0.5 * N)
  for(s in 1:S_post) {
    cont_post[s] <- 1/exp(sum(log(p_si[,s]) + const/N))
  }
  mml_value <- S_post/sum(cont_post)
  approx_seq <- seq_along(cont_post)/cumsum(cont_post)

  if(S > 0){
    ### register parallel backend
    cluster <- parallel::makeCluster(ncores)
    doSNOW::registerDoSNOW(cluster)

    ### register progress bar
    if(getOption("RprobitB_progress")){
      pb <- RprobitB_pb(title = "Computing prior arithmetic mean estimate",
                        total = S,
                        tail = "parameter sets")
      opts <- list(progress = function(n) pb$tick())
    } else {
      opts <- list()
    }

    ### compute prior arithmetic mean estimate
    s <- NULL
    cont_prior <- foreach::foreach(s = 1:S, .packages = "RprobitB", .combine = "cbind", .options.snow = opts) %dopar% {
      prior_sample <- draw_from_prior(x$prior, C = x$latent_classes$C)
      par <- do.call(what = RprobitB_parameter, args = c(prior_sample, add_args))
      probs <- choice_probabilities(x = x, par_set = par)
      choices <- as.character(unlist(sapply(x$data$data, `[[`, "y")))
      ll <- 0
      for (row in 1:nrow(probs)){
        ll <- ll + log(probs[row, choices[row]]) + const/N
      }
      exp(ll)
    }

    ### stop parallel backend
    parallel::stopCluster(cluster)

    ### merge posterior harmonic mean estimate with prior arithmetic mean estimate
    cont_prior <- cont_prior[cont_prior != 0]
    S_new <- length(cont_prior)
    if(S_new == 0){
      warning("Could not use any prior sample.", call. = FALSE)
    } else {
      if(S_new < S){
        warning("Could only use ", S_new, " of ", S,
                " prior samples that led to a positive probability.", call. = FALSE)
      }
      mml_value_prior <- sum(cont_prior)/S_new
      S_total <- S_post + S_new

      mml_value <- mml_value * S_post/S_total + mml_value_prior * S_new/S_total
      approx_seq <- c(approx_seq, mml_value * S_post/(S_post + seq_along(cont_prior)) + mml_value_prior * seq_along(cont_prior)/(S_post + seq_along(cont_prior)))
    }
  }

  ### save 'mml_value' in 'x'
  out <- mml_value
  attr(out, "mmll") <- log(mml_value) - const
  attr(out, "mml_vec") <- approx_seq
  attr(out, "factor") <- const
  class(out) <- c("RprobitB_mml", "numeric")
  x[["mml"]] <- out

  ### return 'x'
  return(x)
}

#' @noRd
#' @param log
#' Set to \code{TRUE} to print the logarithm of the marginal model likelihood.
#' @export

print.RprobitB_mml <- function(x, log = FALSE, ...) {
  if(!log){
    cat(sprintf(paste0("%.2e * exp(-%.f)"), x, attr(x, "factor")))
  } else {
    cat(attr(x, "mmll"))
  }
}

#' @noRd
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom rlang .data

plot.RprobitB_mml <- function(x, log = FALSE, ...) {
  if(log){
    mml_vec <- log(attr(x, "mml_vec")) - attr(x, "factor")
  } else {
    mml_vec <- attr(x, "mml_vec")
  }
  p <- ggplot2::ggplot(data = data.frame("S" = seq_along(mml_vec),
                                         "mml_vec" = mml_vec),
                       ggplot2::aes(x = .data$S, y = .data$mml_vec)) +
    ggplot2::geom_line() +
    ggplot2::theme_minimal()
  if(log){
    p <- p + ggplot2::labs(
      x = "Number of samples",
      y = "Marginal log-likelihood",
      title = "The marginal log-likelihood value for different sample sizes")
  } else {
    p <- p + ggplot2::labs(
      x = "Number of samples",
      y = paste("Marginal likelihood *", sprintf("exp(-%.f)", attr(x, "factor"))),
      title = "The marginal likelihood value for different sample sizes") +
      ggplot2::scale_y_log10()
  }
  print(p)
}

#' Parameter sets from posterior samples
#'
#' @description
#' This function builds parameter sets from the normalized, burned and
#' thinned posterior samples.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @return
#' A list of \code{RprobitB_parameter} objects.
#'
#' @keywords
#' internal

posterior_pars <- function(x){

  ### check input
  if(!inherits(x, "RprobitB_fit")){
    stop("'x' must be an object of class 'RprobitB_fit'.",
         call. = FALSE)
  }

  ### extract meta parameters
  N = x$data$N
  T = x$data$T
  J = x$data$J
  P_f = x$data$P_f
  P_r = x$data$P_r
  C = x$latent_classes$C

  ### extract samples
  sample_size <- (x$R - x$B) / x$Q
  gibbs_samples_nbt = x$gibbs_samples$gibbs_samples_nbt
  Sigma_samples = gibbs_samples_nbt$Sigma
  alpha_samples = gibbs_samples_nbt$alpha
  s_samples = gibbs_samples_nbt$s
  b_samples = gibbs_samples_nbt$b
  Omega_samples = gibbs_samples_nbt$Omega

  ### extract parameters of each sample
  pars = list()
  for(s in 1:sample_size){
    pars[[s]] <-  RprobitB_parameter(
      P_f = P_f,
      P_r = P_r,
      J = J,
      N = N,
      alpha = as.numeric(alpha_samples[s,]),
      C = C,
      s = as.numeric(s_samples[s,]),
      b = matrix(b_samples[s,], nrow = P_r, ncol = C),
      Omega = matrix(Omega_samples[s,], nrow = P_r^2, ncol = C),
      Sigma = matrix(Sigma_samples[s,], J-1, J-1),
      sample = FALSE)
  }

  ### return 'pars'
  return(pars)
}

#' Sample from prior distributions
#'
#' @description
#' This function returns a sample from each parameter's prior distribution.
#'
#' @param prior
#' An object of class \code{RprobitB_prior}, which is the output of
#' \code{\link{check_prior}}.
#' @param C
#' The number of latent classes.
#'
#' @return
#' A list of draws for \code{alpha}, \code{s}, \code{b}, \code{Omega}, and
#' \code{Sigma} (if specified for the model).
#'
#' @keywords
#' internal
#'
#' @examples
#' prior <- check_prior(P_f = 1, P_r = 2, J = 3)
#' RprobitB:::draw_from_prior(prior, C = 2)

draw_from_prior <- function(prior, C = 1) {

  ### input checks
  if(!inherits(prior,"RprobitB_prior")){
    stop("'prior' must be of class 'RprobitB_prior.", call. = FALSE)
  }

  ### alpha ~ MVN(eta,Psi)
  if(identical(prior$eta,NA) || identical(prior$Psi,NA)){
    alpha <- NULL
  } else {
    alpha <- rmvnorm(mu = prior$eta, Sigma = prior$Psi)
  }

  ### s ~ D(delta)
  if(identical(prior$delta,NA)){
    s <- NULL
  } else {
    s <- sort(rdirichlet(rep(prior$delta,C)), decreasing = TRUE)
  }

  ### b_c ~ MVN(xi,D) for all c
  if(identical(prior$xi,NA) || identical(prior$D,NA)){
    b <- NULL
  } else {
    b <- matrix(replicate(C, rmvnorm(mu = prior$xi, Sigma = prior$D)), ncol = C)
  }

  ### Omega_c ~ IW(nu,Theta) for all c
  if(identical(prior$nu,NA) || identical(prior$Theta,NA)){
    Omega <- NULL
  } else {
    Omega <- matrix(replicate(C, rwishart(nu = prior$nu, V = prior$Theta)$IW), ncol = C)
  }

  ### Sigma ~ IW(kappa,E)
  if(identical(prior$kappa,NA) || identical(prior$E,NA)){
    Sigma <- NULL
  } else {
    Sigma <-  rwishart(nu = prior$kappa, V = prior$E)$IW
  }

  ### return draws
  draws <- list("alpha" = alpha,
                "s" = s,
                "b" = b,
                "Omega" = Omega,
                "Sigma" = Sigma)
  return(draws)
}

#' Compute prediction accuracy
#'
#' @description
#' This function computes the prediction accuracy of an \code{RprobitB_fit}
#' object. Prediction accuracy means the share of choices that are correctly
#' predicted by the model, where prediction is based on the maximum choice
#' probability.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param ...
#' Optionally specify more \code{RprobitB_fit} objects.
#'
#' @return
#' A numeric.
#'
#' @export

pred_acc <- function(x, ...) {
  models <- list(...)
  if(length(models) == 0){
    models <- list(x)
  } else {
    models <- c(list(x), models)
  }
  pa <- sapply(models, function(x){
    conf <- predict.RprobitB_fit(x, data = NULL, overview = TRUE)
    sum(diag(conf)) / sum(conf)
  })
  return(pa)
}

