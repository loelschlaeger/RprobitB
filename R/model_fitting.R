#' Check prior parameters
#'
#' @description
#' This function checks the compatibility of submitted parameters for the prior
#' distributions and sets missing values to default values.
#'
#' @inheritParams RprobitB_data
#' @param eta
#' The mean vector of length `P_f` of the normal prior for `alpha`.
#' Per default, `eta = numeric(P_f)`.
#' @param Psi
#' The covariance matrix of dimension `P_f` x `P_f` of the normal prior for
#' `alpha`.
#' Per default, `Psi = diag(P_f)`.
#' @param delta
#' A numeric for the concentration parameter vector `rep(delta,C)` of the
#' Dirichlet prior for `s`.
#' Per default, `delta = 1`. In case of Dirichlet process-based updates of the
#' latent classes, `delta = 0.1` per default.
#' @param xi
#' The mean vector of length `P_r` of the normal prior for each `b_c`.
#' Per default, `xi = numeric(P_r)`.
#' @param D
#' The covariance matrix of dimension `P_r` x `P_r` of the normal prior for
#' each `b_c`.
#' Per default, `D = diag(P_r)`.
#' @param nu
#' The degrees of freedom (a natural number greater than `P_r`) of the Inverse
#' Wishart prior for each `Omega_c`.
#' Per default, `nu = P_r + 2`.
#' @param Theta
#' The scale matrix of dimension `P_r` x `P_r` of the Inverse Wishart prior for
#' each `Omega_c`.
#' Per default, `Theta = diag(P_r)`.
#' @param kappa
#' The degrees of freedom (a natural number greater than `J-1`) of the Inverse
#' Wishart prior for `Sigma`.
#' Per default, `kappa = J + 1`.
#' @param E
#' The scale matrix of dimension `J-1` x `J-1` of the Inverse Wishart
#' prior for `Sigma`.
#' Per default, `E = diag(J - 1)`.
#' @param zeta
#' The mean vector of length `J - 2` of the normal prior for the logarithmic
#' increments `d` of the utility thresholds in the ordered probit model.
#' Per default, `zeta = numeric(J - 2)`.
#' @param Z
#' The covariance matrix of dimension `J-2` x `J-2` of the normal prior for the
#' logarithmic increments `d` of the utility thresholds in the ordered probit
#' model. Per default, `Z = diag(J - 2)`.
#'
#' @details
#' A priori, we assume that the model parameters follow these distributions:
#' \itemize{
#'   \item \eqn{\alpha \sim N(\eta, \Psi)}
#'   \item \eqn{s \sim Dir(\delta)}
#'   \item \eqn{b_c \sim N(\xi, D)} for all classes \eqn{c}
#'   \item \eqn{\Omega_c \sim IW(\nu,\Theta)} for all classes \eqn{c}
#'   \item \eqn{\Sigma \sim IW(\kappa,E)}
#'   \item \eqn{d \sim N(\zeta, Z)}
#' }
#' where \eqn{N} denotes the normal, \eqn{Dir} the Dirichlet, and \eqn{IW}
#' the Inverted Wishart distribution.
#'
#' @return
#' An object of class `RprobitB_prior`, which is a list containing all
#' prior parameters. Parameters that are not relevant for the model
#' configuration are set to `NA`.
#'
#' @export
#'
#' @examples
#' check_prior(P_f = 1, P_r = 2, J = 3, ordered = TRUE)

check_prior <- function(
    P_f, P_r, J, ordered = FALSE, eta = numeric(P_f), Psi = diag(P_f),
    delta = 1, xi = numeric(P_r), D = diag(P_r), nu = P_r + 2,
    Theta = diag(P_r), kappa = if(ordered) 4 else (J + 1),
    E = if(ordered) diag(1) else diag(J - 1), zeta = numeric(J - 2),
    Z = diag(J - 2)
    ) {

  ### initialize prior list
  prior <- list()

  ### check supplied values and set missing prior parameters to default values
  if (P_f > 0) {

    ### alpha ~ MVN(eta,Psi)
    if (!is.numeric(eta) || length(eta) != P_f) {
      stop("'eta' must be a numeric vector of length 'P_f'.",
           call. = FALSE)
    }
    if (!is.numeric(Psi) || !is.matrix(Psi) || any(dim(Psi) != c(P_f, P_f))) {
      stop("'Psi' must be a numeric matrix of dimension 'P_f' x 'P_f'.",
           call. = FALSE)
    }
  } else {
    eta <- NA
    Psi <- NA
  }
  if (P_r > 0) {

    ### s ~ D(delta)
    if (!is.numeric(delta) || length(delta) != 1) {
      stop("'delta' must be a single numeric value.",
           call. = FALSE)
    }

    ### b_c ~ MVN(xi,D)
    if (!is.numeric(xi) || length(xi) != P_r) {
      stop("'xi' must be a numeric vector of length 'P_r'.",
           call. = FALSE)
    }
    if (!is.numeric(D) || !is.matrix(D) ||
        any(dim(D) != c(P_r, P_r))) {
      stop("'D' must be a numeric matrix of dimension 'P_r' x 'P_r'.",
           call. = FALSE)
    }

    ### Omega_c ~ IW(nu,Theta)
    if (!is.numeric(nu) || length(nu) != 1 || nu <= P_r) {
      stop("'nu' must be a single numeric value greater or equal 'P_r'.",
           call. = FALSE)
    }
    if (!is.numeric(Theta) || !is.matrix(Theta) ||
        any(dim(Theta) != c(P_r, P_r))) {
      stop("'Theta' must be a numeric matrix of dimension 'P_r' x 'P_r'.",
           call. = FALSE)
    }
  } else {
    delta <- NA
    xi <- NA
    D <- NA
    nu <- NA
    Theta <- NA
  }

  ### Sigma ~ IW(kappa,E)
  if (ordered) {
    if (!is.numeric(kappa) || length(kappa) != 1 || kappa <= 3) {
      stop("'kappa' must be a single numeric value greater or equal '3'.",
           call. = FALSE)
    }
    if (!is.numeric(E) || !is.matrix(E) || any(dim(E) != c(1,1))) {
      stop("'E' must be a numeric matrix of dimension '1' x '1'.",
           call. = FALSE)
    }
  } else {
    if (!is.numeric(kappa) || length(kappa) != 1 || kappa <= J - 1) {
      stop("'kappa' must be a single numeric value greater or equal 'J-1'.",
           call. = FALSE)
    }
    if (!is.numeric(E) || !is.matrix(E) || any(dim(E) != c(J - 1, J - 1))) {
      stop("'E' must be a numeric matrix of dimension 'J-1' x 'J-1'.",
           call. = FALSE)
    }
  }

  ### d ~ N(zeta,Z)
  if (ordered) {
    if (!is.numeric(zeta) || length(zeta) != J-2) {
      stop("'zeta' must be a numeric vector of length 'J - 2'.",
           call. = FALSE)
    }
    if (!is.numeric(Z) || !is.matrix(Z) || any(dim(Z) != c(J - 2, J - 2))) {
      stop("'Z' must be a numeric matrix of dimension 'J-2' x 'J-2'.",
           call. = FALSE)
    }
  } else {
    zeta <- NA
    Z <- NA
  }

  ### build and return prior parameters
  prior <- list(
    "eta" = eta, "Psi" = Psi, "delta" = delta, "xi" = xi, "D" = D, "nu" = nu,
    "Theta" = Theta, "kappa" = kappa, "E" = E, "zeta" = zeta, "Z" = Z)
  class(prior) <- c("RprobitB_prior", "list")
  return(prior)
}

#' Set initial values for the Gibbs sampler
#'
#' @description
#' This function sets initial values for the Gibbs sampler.
#'
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes.
#' @param ss
#' Optionally the output of \code{\link{sufficient_statistics}}.
#'
#' @return
#' A list of initial values for the Gibbs sampler.
#'
#' @keywords
#' internal
#'
#' @examples
#' RprobitB:::set_initial_gibbs_values(
#'   N = 2, T = 3, J = 3, P_f = 1, P_r = 2, C = 2
#' )

set_initial_gibbs_values <- function(
    N, T, J, P_f, P_r, C, ordered = FALSE, ranked = FALSE, ss = NULL
    ) {

  ### check inputs
  stopifnot(is.numeric(N), N %% 1 == 0, N > 0)
  stopifnot(is.numeric(T), T %% 1 == 0, T > 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0, P_f >= 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0, P_r >= 0)
  stopifnot(is.numeric(C), C %% 1 == 0, C > 0)
  stopifnot(is.logical(ordered))
  stopifnot(is.logical(ranked))

  ### define initial values
  alpha0 <- if (P_f > 0) numeric(P_f) else NA
  z0 <- if (P_r > 0) rep(1, N) else NA
  m0 <- if (P_r > 0) round(rep(N, C) * 2^(C:1 - 1) / sum(2^(C:1 - 1))) else NA
  b0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = C) else NA
  Omega0 <- if (P_r > 0) matrix(rep(as.vector(diag(P_r)), C), nrow = P_r * P_r,
                                ncol = C) else NA
  beta0 <- if (P_r > 0) matrix(0, nrow = P_r, ncol = N) else NA
  U0 <- matrix(0, nrow = J - 1, ncol = N * max(T))
  Sigma0 <- diag(J - 1)

  ### special case of ordered probit
  if (ordered) {
    d0 <- rep(0,J-2)
    U0 <- matrix(0, nrow = 1, ncol = N * max(T))
    Sigma0 <- diag(1)
    if (!is.null(ss)) {
      if (P_f > 0) {
        W_mat <- Reduce(rbind, ss$W)
        alpha0 <- as.numeric(solve(t(W_mat) %*% W_mat) %*% t(W_mat) %*%
                               na.omit(as.numeric(t(ss$y))))
      }
      if (P_r > 0) {
        X_mat <- Reduce(rbind, ss$X)
        b0 <- as.numeric(solve(t(X_mat) %*% X_mat) %*% t(X_mat) %*%
                               na.omit(as.numeric(t(ss$y))))
        b0 <- matrix(rep(b0, times = C), nrow = P_r, ncol = C)
      }
    }
  } else {
    d0 <- NA
  }

  if (ranked) {

  }

  ### define 'init'
  init <- list(
    "alpha0" = alpha0,
    "z0" = z0,
    "m0" = m0,
    "b0" = b0,
    "Omega0" = Omega0,
    "beta0" = beta0,
    "U0" = U0,
    "Sigma0" = Sigma0,
    "d0" = d0
  )

  ### return 'init'
  return(init)
}

#' Create object of class `RprobitB_latent_classes`
#'
#' @description
#' This function creates an object of class `RprobitB_latent_classes` which
#' defines the number of latent classes and their updating scheme.
#' The `RprobitB_latent_classes` object generated
#' by this function is only of relevance if the model possesses at least one
#' random coefficient, i.e. if `P_r>0`.
#'
#' @details
#' ## Why update latent classes?
#' In order not to have to specify the number of latent classes before
#' estimation.
#'
#' ## What options to update latent classes exist?
#' Currently two updating schemes are implemented, weight-based and
#' via a Dirichlet process, see
#' [the vignette on modeling heterogeneity](https://loelschlaeger.de/RprobitB/articles/v04_modeling_heterogeneity.html).
#'
#' ## What is the default behavior?
#' One latent class without updates is specified per default. Print an
#' \code{RprobitB_latent_classes}-object to see a summary of all relevant
#' (default) parameter settings.
#'
#' ## Why is \code{Cmax} required?
#' The implementation requires an upper bound on the number of latent classes
#' for saving the Gibbs samples. However, this is not a restriction since the
#' number of latent classes is bounded by the number of deciders in any case.
#' A plot method for visualizing the sequence of class numbers after estimation
#' and can be used to check if \code{Cmax} was reached, see
#' \code{\link{plot.RprobitB_fit}}.
#'
#' @param latent_classes
#' Either \code{NULL} (for no latent classes) or a list of parameters specifying
#' the number of latent classes and their updating scheme:
#' \itemize{
#'   \item \code{C}: The fixed number (greater or equal 1) of latent classes,
#'         which is set to 1 per default. If either \code{weight_update = TRUE}
#'         or \code{dp_update = TRUE} (i.e. if classes are updated), \code{C}
#'         equals the initial number of latent classes.
#'   \item \code{weight_update}: A boolean, set to \code{TRUE} to weight-based
#'         update the latent classes. See ... for details.
#'   \item \code{dp_update}: A boolean, set to \code{TRUE} to update the latent
#'         classes based on a Dirichlet process. See ... for details.
#'   \item \code{Cmax}: The maximum number of latent classes.
#'   \item \code{buffer}: The number of iterations to wait before a next
#'         weight-based update of the latent classes.
#'   \item \code{epsmin}: The threshold weight (between 0 and 1) for removing
#'         a latent class in the weight-based updating scheme.
#'   \item \code{epsmax}: The threshold weight (between 0 and 1) for splitting
#'         a latent class in the weight-based updating scheme.
#'   \item \code{distmin}: The (non-negative) threshold in class mean difference
#'         for joining two latent classes in the weight-based updating scheme.
#' }
#'
#' @return
#' An object of class \code{RprobitB_latent_classes}.
#'
#' @examples
#' ### default setting
#' RprobitB:::RprobitB_latent_classes()
#'
#' ### setting for a fixed number of two latent classes
#' RprobitB:::RprobitB_latent_classes(list(C = 2))
#'
#' ### setting for weight-based on Dirichlet process-based updates
#' RprobitB:::RprobitB_latent_classes(
#'   list("weight_update" = TRUE, "dp_update" = TRUE)
#' )
#'
#' @keywords
#' internal

RprobitB_latent_classes <- function(latent_classes = NULL) {

  ### check if 'latent_classes' is 'NULL' or a list
  if (!is.null(latent_classes)) {
    if (!is.list(latent_classes)) {
      stop("'latent_classes' must be either 'NULL' or a list.", call. = FALSE)
    }
  } else {
    latent_classes <- list()
  }

  ### set default number of latent classes
  if (is.null(latent_classes[["C"]])) {
    latent_classes[["C"]] <- 1
  }

  ### determine whether latent classes should be weight-based updated
  latent_classes[["weight_update"]] <-
    ifelse(!isTRUE(latent_classes[["weight_update"]]) &&
             !isFALSE(latent_classes[["weight_update"]]),
           FALSE, latent_classes[["weight_update"]]
    )

  ### determine whether latent classes should be DP-based updated
  latent_classes[["dp_update"]] <-
    ifelse(!isTRUE(latent_classes[["dp_update"]]) ||
             !isFALSE(latent_classes[["dp_update"]]),
           FALSE, latent_classes[["dp_update"]]
    )

  if (!latent_classes[["weight_update"]] && !latent_classes[["dp_update"]]) {
    ### remove other parameters in case of no updates
    latent_classes <- list("C" = latent_classes[["C"]],
                           "weight_update" = FALSE,
                           "dp_update" = FALSE)
  } else {
    ### specify updating parameters
    if (is.null(latent_classes[["Cmax"]])){
      latent_classes[["Cmax"]] <- max(10, latent_classes[["C"]])
    }

    ### set missing parameters to default values
    if (is.null(latent_classes[["buffer"]]))
      latent_classes[["buffer"]] <- 100
    if (is.null(latent_classes[["epsmin"]]))
      latent_classes[["epsmin"]] <- 0.01
    if (is.null(latent_classes[["epsmax"]]))
      latent_classes[["epsmax"]] <- 0.99
    if (is.null(latent_classes[["distmin"]]))
      latent_classes[["distmin"]] <- 0.1

    ### remove redundant parameters
    req_names <- c("C", "weight_update", "dp_update", "Cmax")
    if(latent_classes[["weight_update"]]){
      req_names <- c(req_names, "buffer", "epsmin", "epsmax", "distmin")
    }
    latent_classes[!names(latent_classes) %in% req_names] <- NULL
  }

  ### check 'latent_classes'
  if (!is.numeric(latent_classes$C) || !latent_classes$C %% 1 == 0 ||
      !latent_classes$C > 0) {
    stop("'latent_classes$C' must be a positive integer.", call. = FALSE)
  }
  if (latent_classes[["weight_update"]] || latent_classes[["dp_update"]]) {
    if (!is.numeric(latent_classes$Cmax) || !latent_classes$Cmax %% 1 == 0 ||
        !latent_classes$Cmax > 0) {
      stop("'latent_classes$Cmax' must be a positive integer.", call. = FALSE)
    }
  }
  if (latent_classes[["weight_update"]]) {
    if (!is.numeric(latent_classes$buffer) ||
        !latent_classes$buffer %% 1 == 0 ||
        !latent_classes$buffer > 0) {
      stop("'latent_classes$buffer' must be a positive integer.", call. = FALSE)
    }
    if (!is.numeric(latent_classes$epsmin) || !latent_classes$epsmin <= 1 ||
        !latent_classes$epsmin >= 0) {
      stop("'latent_classes$epsmin' must be a numeric between 0 and 1.",
           call. = FALSE)
    }
    if (!is.numeric(latent_classes$epsmax) || !latent_classes$epsmax <= 1 ||
        !latent_classes$epsmax >= 0 ||
        !latent_classes$epsmin < latent_classes$epsmax) {
      stop(
        "'latent_classes$epsmax' must be a numeric between 0 and 1 and",
        "greater than 'latent_classes$epsmin'.", call. = FALSE
      )
    }
    if (!is.numeric(latent_classes$distmin) || !0 <= latent_classes$distmin) {
      stop("'latent_classes$distmin' must be a non-negative numeric value.",
           call. = FALSE)
    }
  }

  ### add boolean for class update
  if (latent_classes[["weight_update"]] || latent_classes[["dp_update"]]) {
    latent_classes[["class_update"]] <- TRUE
  } else {
    latent_classes[["class_update"]] <- FALSE
  }

  ### add class to 'latent_classes'
  class(latent_classes) <- "RprobitB_latent_classes"

  ### return latent_classes
  return(latent_classes)
}

#' @noRd
#' @export
#' @importFrom crayon underline

print.RprobitB_latent_classes <- function(x, ...) {
  cat(crayon::underline("Latent classes\n"))
  if(!x[["class_update"]]){
    cat("C =", x$C, "\n")
  } else {
    cat("DP-based update:", x[["dp_update"]], "\n")
    cat("Weight-based update:", x[["weight_update"]], "\n")
    cat("Initial classes:", x$C, "\n")
    cat("Maximum classes:", x$Cmax, "\n")
    if(x[["weight_update"]]){
      cat("Updating buffer:", x$buffer, "\n")
      cat("Minimum class weight:", x$epsmin, "\n")
      cat("Maximum class weight:", x$epsmax, "\n")
      cat("Mimumum class distance:", x$distmin, "\n")
    }
  }
  return(invisible(x))
}

#' Create object of class `RprobitB_normalization`
#'
#' @description
#' This function creates an object of class `RprobitB_normalization`,
#' which determines the utility scale and level.
#'
#' @details
#' Any choice model has to be normalized with respect to the utility level and
#' scale.
#' \itemize{
#'   \item For level normalization, {RprobitB} takes utility differences with
#'         respect to one alternative.
#'         For the ordered model where only one utility is modeled, {RprobitB}
#'         fixes the first utility threshold to 0.
#'   \item For scale normalization, RprobitB fixes one model parameter. Per
#'         default, the first error-term variance is fixed to `1`.
#'         This is specified via `scale = "Sigma_1,1 := 1"`.
#'         Alternatively, any error-term variance or any non-random coefficient
#'         can be fixed.
#' }
#'
#' @param level
#' The alternative name with respect to which utility differences are computed.
#' Currently, only differences with respect to the last alternative can be
#' computed.
#' @param scale
#' A character which determines the utility scale. It is of the form
#' `<parameter> := <value>`, where `<parameter>` is either the name of a fixed
#' effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of `Sigma`, and
#' `<value>` is the value of the fixed parameter.
#' @inheritParams overview_effects
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class `RprobitB_normalization`, which is a list of
#' \itemize{
#'   \item `level`, a list with the elements `level` (the number of the
#'         alternative specified by the input `level`) and `name` (the name of
#'         the alternative, i.e. the input `level`), or alternatively
#'         \code{NA} in the ordered probit case,
#'   \item and `scale`, a list with the elements `parameter` (either `"s"` for
#'         an element of `Sigma` or `"a"`for an element of `alpha`), the
#'         parameter `index`, and the fixed `value`. If `parameter = "a"`, also
#'         the `name` of the fixed effect.
#' }
#'
#' @examples
#' RprobitB:::RprobitB_normalization(
#'   level = "B",
#'   scale = "price := -1",
#'   form = choice ~ price + time + comfort + change | 1,
#'   re = "time",
#'   alternatives = c("A", "B"),
#'   base = "A"
#' )
#'
#' @keywords
#' internal

RprobitB_normalization <- function(
    level, scale = "Sigma_1,1 := 1", form, re = NULL, alternatives, base,
    ordered = FALSE
    ) {

  ### check inputs
  if(missing(alternatives)){
    stop("Please specify 'alternatives'.",
         call. = FALSE)
  }
  if(!is.character(alternatives)) {
    stop("'alternatives' must be a character vector",
         call. = FALSE)
  }
  if(missing(level) || is.null(level)){
    level <- tail(alternatives, n = 1)
  }
  if(level != tail(alternatives, n = 1)){
    level <- tail(alternatives, n = 1)
    warning(paste0("Currently, only alternatives with respect to the last ",
                   "alternative can be computed.\nTherefore, 'level' = ",
                   tail(alternatives, n = 1), "' is set."),
            immediate. = TRUE, call. = FALSE)
  }
  if(missing(form)){
    stop("Please specify 'form'.",
         call. = FALSE)
  }
  if(!(is.character(level) && length(level) == 1 && level %in% alternatives)){
    stop("'level' must be one element of 'alternatives'.",
         call. = FALSE)
  }
  if(!(is.character(scale) && length(scale) == 1)){
    stop("'scale' must be a single character.",
         call. = FALSE)
  }
  scale <- gsub(" ", "", scale, fixed = TRUE)
  if(!grepl(":=", scale, fixed = TRUE)) {
    stop("'scale' is not in format '<parameter> := <value>'.",
         call. = FALSE)
  }
  if(missing(base)){
    stop("Please specify 'base'.",
         call. = FALSE)
  }
  if(!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean.",
         call. = FALSE)
  }

  ### set 'level'
  if(ordered) {
    level <- NA
  } else {
    alt_name <- level
    level <- which(alternatives == level)
    level <- list("level" = level, "name" = alt_name)
  }

  ### set 'scale'
  effects <- overview_effects(
    form = form, re = re, alternatives = alternatives,
    base = base
  )
  parameter <- strsplit(scale, ":=", fixed = TRUE)[[1]][1]
  par_name <- NA
  if(parameter %in% effects[["effect"]]){
    index <- which(parameter == effects[["effect"]])
    if(effects[index, "random"]){
      stop(paste0("'", parameter, "' is a random effect and cannot be used ",
                  "for scale normalization."),
           call. = FALSE)
    }
    par_name <- parameter
    parameter <- "a"
  } else {
    parameter_split <- strsplit(parameter, split = "_")[[1]]
    if(parameter_split[1] %in% c("Sigma","sigma")) {
      parameter <- "s"
      index <- suppressWarnings(
        as.numeric(strsplit(parameter_split[2], split = ",")[[1]][1])
      )
      if(is.na(index) || index %% 1 != 0 || index <= 0){
        stop(paste("'<parameter>' in 'scale = <parameter> := <value>' is not",
                   "in the form 'Sigma_<j>,<j>' for an integer <j>."),
             call. = FALSE)
      }
      if(index > length(alternatives)) {
        stop(paste("'<j>' in 'Sigma_<j>,<j>' for '<parameter>' in",
                   "'scale = <parameter> := <value>' must not be greater than",
                   "the length of 'alternatives'."),
             call. = FALSE)
      }
    } else {
      stop("Please check the specification of 'scale'.",
           call. = FALSE)
    }
  }
  value <- suppressWarnings(
    as.numeric(strsplit(scale, ":=", fixed = TRUE)[[1]][2])
    )
  if(is.na(value)) {
    stop(paste("'<value>' in 'scale = <parameter> := <value>' is not",
               "a numeric value."),
         call. = FALSE)
  }
  if(value == 0){
    stop("'<value>' in 'scale = <parameter> := <value>' must be non-zero.",
         call. = FALSE)
  }
  if(value < 0 && parameter == "s"){
    stop("'<value>' in 'scale = <parameter> := <value>' must be non-zero ",
         "when fixing an error term variance.",
         call. = FALSE)
  }
  scale <- list("parameter" = parameter, "index" = index, "value" = value,
                "name" = par_name)

  ### create and return object of class 'RprobitB_normalization'
  out <- list("level" = level, "scale" = scale)
  class(out) <- "RprobitB_normalization"
  return(out)
}

#' @noRd
#' @export
#' @importFrom crayon underline

print.RprobitB_normalization <- function(x, ...) {
  if(identical(NA,x$level)) {
    cat(paste0(
      "Level: Fixed first utility threshold to 0.\n"
    ))
    cat(paste0(
      "Scale: Error term variance fixed to ", x$scale$value, ".\n"
    ))
  } else {
    cat(paste0(
      "Level: Utility differences with respect to alternative '",
      x$level$name, "'.\n"
    ))
    if (x$scale$parameter == "a") {
      cat(paste0(
        "Scale: Coefficient of effect '", x$scale$name, "' (alpha_", x$scale$index,
        ") fixed to ", x$scale$value, ".\n"
      ))
    }
    if (x$scale$parameter == "s") {
      cat(paste0(
        "Scale: Coefficient of the ", x$scale$index, ". error term variance ",
        "fixed to ", x$scale$value, ".\n"
      ))
    }
  }
  return(invisible(x))
}


#' Fitting probit models via Markov chain Monte Carlo simulation
#'
#' @description
#' This function performs Markov chain Monte Carlo simulation for fitting
#' different types of probit models (binary, multivariate, mixed, latent class,
#' ordered, ranked) to discrete choice data.
#'
#' @details
#' See [the vignette on model fitting](https://loelschlaeger.de/RprobitB/articles/v03_model_fitting.html)
#' for more details.
#'
#' @param data
#' An object of class \code{RprobitB_data}.
#' @inheritParams RprobitB_normalization
#' @param R
#' The number of iterations of the Gibbs sampler.
#' @param B
#' The length of the burn-in period, i.e. a non-negative number of samples to
#' be discarded.
#' @param Q
#' The thinning factor for the Gibbs samples, i.e. only every \code{Q}th
#' sample is kept.
#' @param print_progress
#' A boolean, determining whether to print the Gibbs sampler progress and the
#' estimated remaining computation time.
#' @param prior
#' A named list of parameters for the prior distributions. See the documentation
#' of \code{\link{check_prior}} for details about which parameters can be
#' specified.
#' @inheritParams RprobitB_latent_classes
#' @param seed
#' Set a seed for the Gibbs sampling.
#' @param fixed_parameter
#' Optionally specify a named list with fixed parameter values for \code{alpha},
#' \code{C}, \code{s}, \code{b}, \code{Omega}, \code{Sigma}, \code{Sigma_full},
#' \code{beta}, \code{z}, or \code{d} for the simulation.
#' See [the vignette on model definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
#' for definitions of these variables.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1
#' )
#' model <- fit_model(data = data, R = 1000, seed = 1)
#' summary(model)
#'
#' @export
#'
#' @importFrom utils tail
#' @import Rcpp
#'
#' @seealso
#' \itemize{
#'   \item [prepare_data()] and [simulate_choices()] for building an
#'         \code{RprobitB_data} object
#'   \item [update()] for estimating nested models
#'   \item [transform()] for transforming a fitted model
#' }

fit_model <- function(
    data, scale = "Sigma_1,1 := 1", R = 1000, B = R / 2, Q = 1,
    print_progress = getOption("RprobitB_progress"), prior = NULL,
    latent_classes = NULL, seed = NULL, fixed_parameter = list()
    ) {

  ### check inputs
  if (!inherits(data, "RprobitB_data")) {
    stop(
      "'data' must an object of class 'RprobitB_data', i.e. the output of",
      " 'RprobitB::prepare()' or 'RprobitB::simulate()'.",
      call. = FALSE
    )
  }
  if (!data[["choice_available"]]) {
    stop(
      "Cannot use 'data' for model fitting because information on choices",
      " is not available.",
      call. = FALSE
    )
  }
  if (!is.numeric(R) || !R %% 1 == 0 || !R > 0) {
    stop("'R' must be a positive integer.",
         call. = FALSE)
  }
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.",
         call. = FALSE)
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.",
         call. = FALSE)
  }
  if (!isTRUE(print_progress) && !isFALSE(print_progress)) {
    stop("'print_progress' must be a boolean.",
         call. = FALSE)
  }

  ### set normalization
  normalization <- RprobitB_normalization(
    level = NULL, scale = scale, form = data$form, re = data$re,
    alternatives = data$alternatives, base = data$base, ordered = data$ordered
  )

  ### set latent classes
  latent_classes <- RprobitB_latent_classes(latent_classes = latent_classes)
  if(latent_classes$dp_update && is.null(prior[["delta"]])) {
    prior[["delta"]] <- 0.1
  }

  ### set fixed parameter
  fixed_parameter <- unclass(do.call(
    what = RprobitB_parameter,
    args = c(
      list("P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J, "N" = data$N,
           "C" = latent_classes$C, "ordered" = data$ordered, sample = FALSE),
      fixed_parameter
    )
  ))[names(fixed_parameter)]
  if(latent_classes[["class_update"]]) {
    no_fix <- c("s","z","b","Omega")
    if(any(names(fixed_parameter) %in% no_fix)) {
      stop("You cannot fix parameter ",
           paste(intersect(no_fix, names(fixed_parameter)), collapse = ", "),
           " when updating C.", call. = FALSE)
    }
  }

  ### set prior parameters
  prior <- do.call(
    what = check_prior,
    args = c(list("P_f" = data$P_f, "P_r" = data$P_r, "J" = data$J,
                  "ordered" = data$ordered), prior)
  )

  ### compute sufficient statistics
  ss <- sufficient_statistics(data = data, normalization = normalization)

  ### set initial values for the Gibbs sampler
  init <- set_initial_gibbs_values(
    N = data[["N"]], T = data[["T"]], J = data[["J"]], P_f = data[["P_f"]],
    P_r = data[["P_r"]], C = latent_classes[["C"]], ordered = data[["ordered"]],
    ss = ss
  )

  ### Gibbs sampling
  if (!is.null(seed))
    set.seed(seed)
  timer_start <- Sys.time()
  gibbs_samples <- gibbs_sampling(
    sufficient_statistics = ss, prior = prior,
    latent_classes = unclass(latent_classes), fixed_parameter = fixed_parameter,
    init = init, R = R, B = B, print_progress = print_progress,
    ordered = data[["ordered"]], ranked = data[["ranked"]]
  )
  timer_end <- Sys.time()

  ### filter Gibbs samples
  if (data$P_f == 0)
    gibbs_samples["alpha"] <- NULL
  if (data$P_r == 0)
    gibbs_samples[c("s","z","b","Omega","class_sequence")] <- NULL
  if (!data$ordered)
    gibbs_samples["d"] <- NULL

  if (latent_classes[["class_update"]]) {
    ### update number of latent classes
    latent_classes[["C"]] <- sum(utils::tail(gibbs_samples[["s"]], 1) != 0)

    ### remove zeros for unoccupied classes
    gibbs_samples[["s"]] <- gibbs_samples[["s"]][,
                                          1:latent_classes[["C"]], drop = FALSE]
    gibbs_samples[["b"]] <- gibbs_samples[["b"]][,
                        1:(data[["P_r"]] * latent_classes[["C"]]), drop = FALSE]
    gibbs_samples[["Omega"]] <- gibbs_samples[["Omega"]][,
                      1:(data[["P_r"]]^2 * latent_classes[["C"]]), drop = FALSE]
  }

  ### save class sequence
  if (!is.null(gibbs_samples[["class_sequence"]])) {
    class_sequence <- as.vector(gibbs_samples[["class_sequence"]])
    gibbs_samples <- within(gibbs_samples, rm(class_sequence))
  } else {
    class_sequence <- NULL
  }

  ### label Gibbs samples
  labels <- parameter_labels(
    P_f = data$P_f, P_r = data$P_r, J = data$J, C = latent_classes[["C"]],
    ordered = data$ordered, cov_sym = TRUE, drop_par = NULL
  )
  for (par in names(labels)) {
    colnames(gibbs_samples[[par]]) <- labels[[par]]
  }

  ### normalize, burn and thin 'gibbs_samples'
  gibbs_samples <- transform_gibbs_samples(
    gibbs_samples = gibbs_samples, R = R, B = B, Q = Q,
    normalization = normalization
  )

  ### normalize true model parameters based on 'normalization'
  if (data$simulated) {
    data$true_parameter <- transform_parameter(
      parameter = data$true_parameter, normalization = normalization,
      ordered = data$ordered
    )
  }

  ### build 'RprobitB_fit' object
  out <- RprobitB_fit(
    data = data,
    scale = scale,
    level = NULL,
    normalization = normalization,
    R = R,
    B = B,
    Q = Q,
    latent_classes = latent_classes,
    prior = prior,
    gibbs_samples = gibbs_samples,
    class_sequence = class_sequence,
    comp_time = difftime(timer_end, timer_start)
  )

  ### calculate log-likelihood
  RprobitB_pp("Computing log-likelihood")
  out[["ll"]] <- suppressMessages(logLik.RprobitB_fit(out))

  ### return 'RprobitB_fit' object
  return(out)
}

#' Compute sufficient statistics
#'
#' @description
#' This function computes sufficient statistics from an `RprobitB_data`
#' object for the Gibbs sampler to save computation time.
#'
#' @param data
#' An object of class `RprobitB_data`.
#' @param normalization
#' An object of class `RprobitB_normalization`, which can be created
#' via \code{\link{RprobitB_normalization}}.
#'
#' @return
#' A list of sufficient statistics on the data for Gibbs sampling, containing
#' \itemize{
#'   \item the elements \code{N}, \code{T}, \code{J}, \code{P_f} and \code{P_r}
#'         from \code{data},
#'   \item \code{Tvec}, the vector of choice occasions for each decider of
#'         length \code{N},
#'   \item \code{csTvec}, a vector of length \code{N} with the cumulated sums of
#'         \code{Tvec} starting from \code{0},
#'   \item \code{W}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a fixed coefficient (or \code{NA} if \code{P_f = 0}),
#'   \item \code{X}, a list of design matrices differenced with respect to
#'         alternative number \code{normalization$level$level}
#'         for each decider in each choice occasion with covariates that
#'         are linked to a random coefficient (or \code{NA} if \code{P_r = 0}),
#'   \item \code{y}, a matrix of dimension \code{N} x \code{max(Tvec)} with the
#'         observed choices of deciders in rows and choice occasions in columns,
#'         decoded to numeric values with respect to their appearance in
#'         \code{data$alternatives}, where rows are filled with \code{NA} in
#'         case of an unbalanced panel,
#'   \item \code{WkW}, a matrix of dimension \code{P_f^2} x \code{(J-1)^2}, the
#'         sum over Kronecker products of each transposed element in \code{W}
#'         with itself,
#'   \item \code{XkX}, a list of length \code{N}, each element is constructed in
#'         the same way as \code{WkW} but with the elements in \code{X} and
#'         separately for each decider,
#'   \item \code{rdiff} (for the ranked case only), a list of matrices that
#'         reverse the base differencing and instead difference in such a way
#'         that the resulting utility vector is negative.
#' }
#'
#' @importFrom MASS ginv
#'
#' @keywords
#' internal

sufficient_statistics <- function(data, normalization) {

  ### check input
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' must be of class 'RprobitB_data'.", call. = FALSE)
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.",
         call. = FALSE)
  }

  ### make a copy of 'data'
  data_copy <- data

  ### extract parameters
  N <- data_copy$N
  T <- data_copy$T
  Tvec <- if (length(T) == 1) rep(T, N) else T
  J <- data_copy$J
  P_f <- data_copy$P_f
  P_r <- data_copy$P_r

  ### compute utility differences with respect to 'normalization$level$level'
  ### (not for an ordered probit model)
  RprobitB_pp("Computing sufficient statistics", 0, 4)
  if (!data$ordered) {
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        data_copy$data[[n]]$X[[t]] <- delta(J, normalization$level$level) %*%
          data_copy$data[[n]]$X[[t]]
      }
    }
  }

  ### decode choice to numeric with respect to appearance
  RprobitB_pp("Computing sufficient statistics", 1, 4)
  y <- matrix(0, nrow = N, ncol = max(Tvec))
  if (data$ranked) {
    choice_set <- sapply(permutations(data$alternatives), paste, collapse = ",")
  } else {
    choice_set <- data$alternatives
  }
  for (n in 1:N) {
    y_n <- match(data_copy$data[[n]][[2]], choice_set)
    y[n, ] <- c(y_n, rep(NA, max(Tvec) - length(y_n)))
  }

  ### extract covariates linked to fixed ('W') and to random coefficients ('X')
  RprobitB_pp("Computing sufficient statistics", 2, 4)
  W <- list()
  X <- list()
  if (P_f > 0 & P_r > 0) {
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <-
          data_copy$data[[n]][[1]][[t]][,seq_len(P_f), drop = FALSE]
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <-
          data_copy$data[[n]][[1]][[t]][,-seq_len(P_f), drop = FALSE]
      }
    }
  }
  if (P_f > 0 & P_r == 0) {
    X <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        W[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }
  if (P_f == 0 & P_r > 0) {
    W <- NA
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        X[[sum(Tvec[seq_len(n - 1)]) + t]] <- data_copy$data[[n]][[1]][[t]]
      }
    }
  }

  ### compute \sum kronecker(t(W_nt),t(W_nt)) for each W_nt in W
  RprobitB_pp("Computing sufficient statistics", 3, 4)
  WkW <- NA
  if (P_f > 0) {
    WkW <- if(data$ordered) {
      matrix(0, nrow = P_f^2, ncol = 1)
    } else {
      matrix(0, nrow = P_f^2, ncol = (J - 1)^2)
    }
    for (n in seq_len(N)) {
      for (t in seq_len(Tvec[n])) {
        WkW <- WkW + kronecker(t(W[[sum(Tvec[seq_len(n - 1)]) + t]]),
                               t(W[[sum(Tvec[seq_len(n - 1)]) + t]]))
      }
    }
  }

  ### for each n, compute \sum kronecker(t(X_nt),t(X_nt))
  RprobitB_pp("Computing sufficient statistics", 4, 4)
  XkX <- NA
  if (P_r > 0) {
    XkX <- list()
    for (n in seq_len(N)) {
      XnkXn <- if(data$ordered) {
        matrix(0, nrow = P_r^2, ncol = 1)
      } else {
        matrix(0, nrow = P_r^2, ncol = (J - 1)^2)
      }
      for (t in seq_len(Tvec[n])) {
        XnkXn <- XnkXn + kronecker(t(X[[sum(Tvec[seq_len(n - 1)]) + t]]),
                                   t(X[[sum(Tvec[seq_len(n - 1)]) + t]]))
      }
      XkX[[n]] <- XnkXn
    }
  }

  ### compute 'rdiff' (only in the ranked case)
  if (data$ranked) {
    rdiff <- list()
    perm <- permutations(data$alternatives)
    Dinv <- round(MASS::ginv(delta(J, normalization$level$level)))
    for(p in 1:length(perm)) {
      rdiff[[p]] <- M(ranking = match(perm[[p]], data$alternatives)) %*% Dinv
    }
  } else {
    rdiff <- NA
  }

  ### build and return 'suff_statistics'
  suff_statistics <- list(
    "N" = N,
    "T" = T,
    "J" = J,
    "P_f" = P_f,
    "P_r" = P_r,
    "Tvec" = Tvec,
    "csTvec" = cumsum(Tvec) - Tvec,
    "W" = W,
    "X" = X,
    "y" = y,
    "WkW" = WkW,
    "XkX" = XkX,
    "rdiff" = rdiff
  )
  return(suff_statistics)
}

#' Update and re-fit a probit model
#'
#' @description
#' This function estimates a nested probit model based on a given
#' \code{RprobitB_fit} object.
#'
#' @details
#' All parameters (except for \code{object}) are optional and if not specified
#' retrieved from the specification for \code{object}.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#' @param ...
#' Ignored.
#' @inheritParams prepare_data
#' @inheritParams fit_model
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @export

update.RprobitB_fit <- function(
    object, form, re, alternatives, id, idc, standardize, impute, scale, R, B,
    Q, print_progress, prior, latent_classes, seed, ...
    ) {
  data <- prepare_data(
    form = if(missing(form)) object$data$form else form,
    choice_data = object$data$choice_data,
    re = if(missing(re)) object$data$re else re,
    alternatives = if(missing(alternatives)) object$data$alternatives else alternatives,
    id = if(missing(id)) object$data$res_var_names$id else id,
    idc = if(missing(idc)) object$data$res_var_names$idc else idc,
    standardize = if(missing(standardize)) object$data$standardize else standardize,
    impute = if(missing(impute)) "complete_cases" else impute
    )
  model <- fit_model(
    data = data,
    scale = if(missing(scale)) object$scale else scale,
    R = if(missing(R)) object$R else R,
    B = if(missing(B)) object$B else B,
    Q = if(missing(Q)) object$Q else Q,
    print_progress = if(missing(print_progress)) getOption("RprobitB_progress") else print_progress,
    prior = if(missing(prior)) NULL else prior,
    latent_classes = if(missing(latent_classes)) object$latent_classes else latent_classes,
    seed = if(missing(seed)) NULL else seed
    )
  return(model)
}

#' Create object of class \code{RprobitB_fit}.
#'
#' @description
#' This function creates an object of class \code{RprobitB_fit}.
#'
#' @inheritParams fit_model
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param class_sequence
#' The sequence of class numbers during Gibbs sampling of length \code{R}.
#' @param comp_time
#' The time spent for Gibbs sampling.
#'
#' @return
#' An object of class \code{RprobitB_fit}.
#'
#' @keywords
#' internal

RprobitB_fit <- function(
    data, scale, level, normalization, R, B, Q, latent_classes, prior,
    gibbs_samples, class_sequence, comp_time
    ) {

  ### check inputs
  stopifnot(inherits(data, "RprobitB_data"))
  stopifnot(is.character("scale"))
  stopifnot(is.character("level"))
  stopifnot(inherits(normalization, "RprobitB_normalization"))
  stopifnot(is.numeric(R), R %% 1 == 0, R > 0)
  stopifnot(is.numeric(B), B %% 1 == 0, B > 0)
  stopifnot(is.numeric(Q), Q %% 1 == 0, Q > 0)
  stopifnot(inherits(latent_classes, "RprobitB_latent_classes"))
  stopifnot(is.list(prior))
  stopifnot(inherits(gibbs_samples, "RprobitB_gibbs_samples"))
  stopifnot(inherits(comp_time, "difftime"))

  ### create and return object of class "RprobitB_fit"
  out <- list(
    "data" = data,
    "scale" = scale,
    "level" = level,
    "normalization" = normalization,
    "R" = R,
    "B" = B,
    "Q" = Q,
    "latent_classes" = latent_classes,
    "prior" = prior,
    "gibbs_samples" = gibbs_samples,
    "class_sequence" = class_sequence,
    "comp_time" = comp_time
  )
  class(out) <- "RprobitB_fit"
  return(out)
}

#' @noRd
#' @export

print.RprobitB_fit <- function(x, ...) {
  cat("Probit model '", deparse1(x$data$form), "'.\n", sep = "")
  return(invisible(x))
}

#' @noRd
#' @importFrom stats sd
#' @export

summary.RprobitB_fit <- function(object, FUN = c(
  "mean" = mean, "sd" = stats::sd,
  "R^" = R_hat
), ...) {

  ### check class of 'object'
  if (!inherits(object, "RprobitB_fit")) {
    stop("Not of class 'RprobitB_fit'.")
  }

  ### compute statistics from 'gibbs_samples'
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      x = object$gibbs_samples,
      P_f = object$data$P_f,
      P_r = object$data$P_r,
      J = object$data$J,
      C = ifelse(
        object$data$simulated,
        max(object$latent_classes$C, object$data$true_parameter$C),
        object$latent_classes$C
      ),
      ordered = object$data$ordered,
      cov_sym = FALSE,
      drop_par = NULL,
    ),
    FUN = FUN
  )

  ### build 'summary.RprobitB_fit' object
  out <- list(
    "form" = object$data$form,
    "R" = object$R,
    "B" = object$B,
    "Q" = object$Q,
    "P_f" = object$data$P_f,
    "P_r" = object$data$P_r,
    "linear_coefs" = object$data$linear_coefs,
    "J" = object$data$J,
    "alternatives" = object$data$alternatives,
    "normalization" = object$normalization,
    "latent_classes" = object$latent_classes,
    "prior" = object$prior,
    "statistics" = statistics,
    "simulated" = object$data$simulated,
    "true_parameter" = object$data$true_parameter
  )
  class(out) <- "summary.RprobitB_fit"

  ### return 'summary.RprobitB_fit' object
  return(out)
}

#' @param digits
#' The number of printed decimal places.
#' @noRd
#' @export
#' @importFrom crayon underline

print.summary.RprobitB_fit <- function(x, digits = 2, ...) {
  cat(crayon::underline("Probit model\n"))
  cat("Formula:", deparse1(x$form), "\n")
  cat(paste0("R: ", x$R, ", B: ", x$B, ", Q: ", x$Q, "\n"))
  print(x$normalization)
  cat("\n")
  if (x$P_r > 0) {
    print(x$latent_classes)
    cat("\n")
  }

  ### overview of estimates
  print(x = x$statistics, true = x$true_parameter, digits = digits)

  ### return 'x' invisibly
  return(invisible(x))
}

#' Transform an \code{RprobitB_fit} object
#'
#' @description
#' Given an object of class \code{RprobitB_fit}, this function can:
#' \itemize{
#'   \item change the length \code{B} of the burn-in period,
#'   \item change the the thinning factor \code{Q} of the Gibbs samples,
#'   \item change the utility \code{scale}.
#' }
#'
#' @details
#' See the vignette "Model fitting" for more details:
#' \code{vignette("v03_model_fitting", package = "RprobitB")}.
#'
#' @param _data
#' An object of class \code{\link{RprobitB_fit}}.
#' @inheritParams fit_model
#' @param check_preference_flip
#' Set to \code{TRUE} to check for flip in preferences after new \code{scale}.
#' @param ...
#' Ignored.
#'
#' @return
#' The transformed \code{RprobitB_fit} object.
#'
#' @examples
#' data(model_train)
#'
#' ### change the length B of the burn-in period
#' transform(model_train, B = 1)
#'
#' ### change the thinning factor Q
#' transform(model_train, Q = 1)
#'
#' ### change the scale
#' transform(model_train, scale = "Sigma_1,1 := 1")
#'
#' @export
#'
#' @rdname transform

transform.RprobitB_fit <- function(`_data`, B = NULL, Q = NULL, scale = NULL,
                                   check_preference_flip = TRUE, ...) {

  ### check inputs
  x <- `_data`
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.")
  }
  if (is.null(B)) {
    B <- x[["B"]]
  } else {
    x[["B"]] <- B
  }
  if (is.null(Q)) {
    Q <- x[["Q"]]
  } else {
    x[["Q"]] <- Q
  }
  R <- x[["R"]]
  P_f <- x[["data"]][["P_f"]]
  J <- x[["data"]][["J"]]
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.")
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.")
  }
  if (is.null(scale)) {
    normalization <- x[["normalization"]]
  } else {
    ### check if new scale flips preferences
    if (check_preference_flip) {
      preference_flip(
        model_old = x,
        model_new = transform.RprobitB_fit(
          x, scale = scale, check_preference_flip = FALSE)
        )
    }
    normalization <- RprobitB_normalization(
      level = x$level,
      scale = scale,
      form = x$data$form,
      re = x$data$re,
      alternatives = x$data$alternatives,
      base = x$data$base)
    x[["normalization"]] <- normalization
  }

  ### scale, burn and thin Gibbs samples
  x[["gibbs_samples"]] <- transform_gibbs_samples(
    x[["gibbs_samples"]][["gibbs_samples_raw"]],
    R = R,
    B = B,
    Q = Q,
    normalization = normalization
  )

  ### scale true model parameters
  if (x[["data"]][["simulated"]]) {
    x[["data"]][["true_parameter"]] <- transform_parameter(
      parameter = x[["data"]][["true_parameter"]],
      normalization = normalization
    )
  }

  ### re-calculate likelihood
  x[["ll"]] <- logLik(x, recompute = TRUE)

  ### return 'RprobitB_fit'
  return(x)
}

#' Transformation of Gibbs samples
#'
#' @description
#' This function normalizes, burns and thins the Gibbs samples.
#'
#' @param gibbs_samples
#' The output of \code{\link{gibbs_sampling}}, i.e. a list of Gibbs samples for
#' \itemize{
#'   \item \code{Sigma},
#'   \item \code{alpha} (if \code{P_f>0}),
#'   \item \code{s}, \code{z}, \code{b}, \code{Omega} (if \code{P_r>0}).
#' }
#' @inheritParams RprobitB_data
#' @inheritParams fit_model
#' @inheritParams sufficient_statistics
#'
#' @return
#' A list, the first element \code{gibbs_sampes_raw} is the input
#' \code{gibbs_samples}, the second element is the normalized, burned, and
#' thinned version of \code{gibbs_samples} called \code{gibbs_samples_nbt}.
#' The list gets the class \code{RprobitB_gibbs_samples}.
#'
#' @keywords
#' internal

transform_gibbs_samples <- function(gibbs_samples, R, B, Q, normalization) {

  ### check inputs
  if (!is.list(gibbs_samples)){
    stop("'gibbs_samples' must be a list of Gibbs samples.", call. = FALSE)
  }
  if (!is.numeric(R) || !R %% 1 == 0 || !R > 0) {
    stop("'R' must be a positive integer.", call. = FALSE)
  }
  if (!is.numeric(B) || !B %% 1 == 0 || !B > 0 || !B < R) {
    stop("'B' must be a positive integer smaller than 'R'.", call. = FALSE)
  }
  if (!is.numeric(Q) || !Q %% 1 == 0 || !Q > 0 || !Q < R) {
    stop("'Q' must be a positive integer smaller than 'R'.", call. = FALSE)
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.",
         call. = FALSE)
  }

  ### function to scale the samples
  scaling <- function(samples, factor) {
    if (is.null(samples)) NULL else samples * factor
  }

  ### normalization (scaling) of samples
  scale <- normalization[["scale"]]
  s_n <- scaling(gibbs_samples[["s"]], 1)
  z_n <- scaling(gibbs_samples[["z"]], 1)
  d_n <- scaling(gibbs_samples[["d"]], 1)
  if (scale[["parameter"]] == "a") {
    factor <- scale[["value"]] / gibbs_samples[["alpha"]][, scale[["index"]]]
    alpha_n <- scaling(gibbs_samples[["alpha"]], factor)
    b_n <- scaling(gibbs_samples[["b"]], factor)
    Omega_n <- scaling(gibbs_samples[["Omega"]], factor^2)
    Sigma_n <- scaling(gibbs_samples[["Sigma"]], factor^2)
    beta_n <- gibbs_samples[["beta"]]
    for (i in 1:length(beta_n)) beta_n[[i]] <- scaling(beta_n[[i]], factor[i])
  }
  if (scale[["parameter"]] == "s") {
    factor <- scale[["value"]] / gibbs_samples[["Sigma"]][, paste0(scale[["index"]], ",", scale[["index"]])]
    alpha_n <- scaling(gibbs_samples[["alpha"]], sqrt(factor))
    b_n <- scaling(gibbs_samples[["b"]], sqrt(factor))
    Omega_n <- scaling(gibbs_samples[["Omega"]], factor)
    Sigma_n <- scaling(gibbs_samples[["Sigma"]], factor)
    beta_n <- gibbs_samples[["beta"]]
    for (i in 1:length(beta_n)) beta_n[[i]] <- scaling(beta_n[[i]], factor[i])
  }
  gibbs_samples_n <- list(
    "s" = s_n,
    "z" = z_n,
    "alpha" = alpha_n,
    "beta" = beta_n,
    "b" = b_n,
    "Omega" = Omega_n,
    "Sigma" = Sigma_n,
    "d" = d_n
  )
  gibbs_samples_n <- gibbs_samples_n[lengths(gibbs_samples_n) != 0]

  ### function to burn samples
  burn <- function(samples) {
    if (is.null(samples)) {
      return(NULL)
    } else {
      if (!is.list(samples)) {
        return(samples[(B + 1):R, , drop = FALSE])
      }
      if (is.list(samples)) {
        return(samples[(B + 1):R])
      }
    }
  }

  ### burning of normalized samples
  s_nb <- burn(s_n)
  z_nb <- burn(z_n)
  alpha_nb <- burn(alpha_n)
  b_nb <- burn(b_n)
  Omega_nb <- burn(Omega_n)
  Sigma_nb <- burn(Sigma_n)
  beta_nb <- burn(beta_n)
  d_nb <- burn(d_n)
  gibbs_samples_nb <- list(
    "s" = s_nb,
    "z" = z_nb,
    "alpha" = alpha_nb,
    "beta" = beta_nb,
    "b" = b_nb,
    "Omega" = Omega_nb,
    "Sigma" = Sigma_nb,
    "d" = d_nb
  )
  gibbs_samples_nb <- gibbs_samples_nb[lengths(gibbs_samples_nb) != 0]

  ### function to thin samples
  thin <- function(samples, end) {
    if (identical(samples, NULL)) {
      return(NULL)
    } else {
      if (!is.list(samples)) {
        return(samples[seq(1, end, Q), , drop = FALSE])
      }
      if (is.list(samples)) {
        return(samples[seq(1, end, Q)])
      }
    }
  }

  ### thinning of normalized samples
  s_nt <- thin(s_n, R)
  z_nt <- thin(z_n, R)
  alpha_nt <- thin(alpha_n, R)
  b_nt <- thin(b_n, R)
  Omega_nt <- thin(Omega_n, R)
  Sigma_nt <- thin(Sigma_n, R)
  beta_nt <- thin(beta_n, R)
  d_nt <- thin(d_n, R)
  gibbs_samples_nt <- list(
    "s" = s_nt,
    "z" = z_nt,
    "alpha" = alpha_nt,
    "beta" = beta_nt,
    "b" = b_nt,
    "Omega" = Omega_nt,
    "Sigma" = Sigma_nt,
    "d" = d_nt
  )
  gibbs_samples_nt <- gibbs_samples_nt[lengths(gibbs_samples_nt) != 0]

  ### thinning of normalized and burned samples
  s_nbt <- thin(s_nb, R - B)
  z_nbt <- thin(z_nb, R - B)
  alpha_nbt <- thin(alpha_nb, R - B)
  b_nbt <- thin(b_nb, R - B)
  Omega_nbt <- thin(Omega_nb, R - B)
  Sigma_nbt <- thin(Sigma_nb, R - B)
  beta_nbt <- thin(beta_nb, R - B)
  d_nbt = thin(d_nb, R - B)
  gibbs_samples_nbt <- list(
    "s" = s_nbt,
    "z" = z_nbt,
    "alpha" = alpha_nbt,
    "beta" = beta_nbt,
    "b" = b_nbt,
    "Omega" = Omega_nbt,
    "Sigma" = Sigma_nbt,
    "d" = d_nbt
  )
  gibbs_samples_nbt <- gibbs_samples_nbt[lengths(gibbs_samples_nbt) != 0]

  ### return list of transformed Gibbs samples
  gibbs_samples <- list(
    "gibbs_samples_raw" = gibbs_samples,
    "gibbs_samples_nbt" = gibbs_samples_nbt
  )
  class(gibbs_samples) <- "RprobitB_gibbs_samples"
  return(gibbs_samples)
}

#' Transformation of parameter values
#'
#' @description
#' This function transforms parameter values based on \code{normalization}.
#'
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @param normalization
#' An object of class \code{RprobitB_normalization}.
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class \code{RprobitB_parameter}.
#'
#' @keywords
#' internal

transform_parameter <- function(parameter, normalization, ordered = FALSE) {

  ### check inputs
  if (!inherits(parameter, "RprobitB_parameter")) {
    stop("'parameter' must be of class 'RprobitB_parameter'.", stop = FALSE)
  }
  if (!inherits(normalization, "RprobitB_normalization")) {
    stop("'normalization' must be of class 'RprobitB_normalization'.",
         stop = FALSE)
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean.", stop = FALSE)
  }

  ### function to scale the parameters
  scaling <- function(par, factor) {
    if (anyNA(par)) {
      NA
    } else {
      out <- par * factor
      ### preserve names
      names(out) <- names(par)
      return(out)
    }
  }

  ### scale elements of 'parameter'
  scale <- normalization[["scale"]]
  if (scale[["parameter"]] == "a") {
    factor <- scale[["value"]] / parameter[["alpha"]][scale[["index"]]]
    parameter[["alpha"]] <- scaling(parameter[["alpha"]], factor)
    parameter[["b"]] <- scaling(parameter[["b"]], factor)
    parameter[["Omega"]] <- scaling(parameter[["Omega"]], factor^2)
    parameter[["Sigma"]] <- scaling(parameter[["Sigma"]], factor^2)
    parameter[["beta"]] <- scaling(parameter[["beta"]], factor)
  }
  if (scale[["parameter"]] == "s") {
    factor <- if(ordered) {
      scale[["value"]] / parameter[["Sigma"]]
    } else {
      scale[["value"]] / parameter[["Sigma"]][scale[["index"]], scale[["index"]]]
    }
    parameter[["alpha"]] <- scaling(parameter[["alpha"]], sqrt(factor))
    parameter[["b"]] <- scaling(parameter[["b"]], sqrt(factor))
    parameter[["Omega"]] <- scaling(parameter[["Omega"]], factor)
    parameter[["Sigma"]] <- scaling(parameter[["Sigma"]], factor)
    parameter[["beta"]] <- scaling(parameter[["beta"]], sqrt(factor))
    if (!ordered) {
      parameter[["Sigma_full"]] <- undiff_Sigma(
        parameter[["Sigma"]], normalization[["level"]][["level"]])
    }
  }

  ### return 'parameter'
  return(parameter)
}

#' Check for flip in preferences after change in model scale.
#'
#' @description
#' This function checks if a change in the model scale flipped the preferences.
#'
#' @param model_old
#' An object of class \code{RprobitB_fit}, the model before the scale change.
#' @param model_new
#' An object of class \code{RprobitB_fit}, the model after the scale change.
#'
#' @return
#' No return value, called for side-effects.
#'
#' @keywords
#' internal
#'
#' @importFrom stats ecdf

preference_flip <- function(model_old, model_new) {
  stopifnot(inherits(model_old, "RprobitB_fit"))
  stopifnot(inherits(model_new, "RprobitB_fit"))
  stopifnot(model_old[["data"]][["P_f"]] == model_new[["data"]][["P_f"]])
  stopifnot(model_old[["data"]][["P_r"]] == model_new[["data"]][["P_r"]])
  flag <- FALSE
  for (p in seq_len(model_old[["data"]][["P_f"]])) {
    P1 <- stats::ecdf(model_old[["gibbs_samples"]][["gibbs_samples_nbt"]][["alpha"]][, p])
    P2 <- stats::ecdf(model_new[["gibbs_samples"]][["gibbs_samples_nbt"]][["alpha"]][, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  for (p in seq_len(model_old[["data"]][["P_r"]])) {
    P1 <- stats::ecdf(model_old[["gibbs_samples"]][["gibbs_samples_nbt"]][["b"]][, p])
    P2 <- stats::ecdf(model_new[["gibbs_samples"]][["gibbs_samples_nbt"]][["b"]][, p])
    if (P1(0) != P2(0)) {
      flag <- TRUE
    }
  }
  if (flag) {
    stop("This transformation seems to flip the preferences. ",
         "Set 'check_preference_flip = FALSE' to transform anyway.",
         call. = FALSE)
  }
}

#' Transform differenced to non-differenced error term covariance matrix
#'
#' @description
#' This function transforms the differenced error term covariance matrix
#' \code{Sigma} back to a non-differenced error term covariance matrix.
#'
#' @param Sigma
#' An error term covariance matrix of dimension \code{J-1} x \code{J-1} which
#' was differenced with respect to alternative \code{i}.
#' @param i
#' An integer, the alternative number with respect to which \code{Sigma}
#' was differenced.
#' @param checks
#' If \code{TRUE} the function runs additional input and transformation checks.
#' @param pos
#' If \code{TRUE} the function returns a positive matrix.
#' @param labels
#' If \code{TRUE} the function adds labels to the output matrix.
#'
#' @return
#' A covariance matrix of dimension \code{J} x \code{J}. If this covariance
#' matrix gets differenced with respect to alternative \code{i}, the results is
#' again \code{Sigma}.
#'
#' @keywords
#' internal
#'
#' @examples
#' J <- 3
#' i <- 2
#' Sigma_full <- rwishart(3, diag(3))$W
#' Sigma <- delta(J, 2) %*% Sigma_full %*% t(delta(J, 2))
#' Sigma_back <- RprobitB:::undiff_Sigma(Sigma = Sigma, i = 2)

undiff_Sigma <- function(Sigma, i, checks = TRUE, pos = TRUE, labels = TRUE) {
  J <- nrow(Sigma) + 1
  if (checks) {
    ### check inputs
    Sigma <- as.matrix(Sigma)
    if (!is_covariance_matrix(Sigma)) {
      stop("'Sigma' is no covariance matrix.", call. = FALSE)
    }
    if (!(length(i) == 1 && is.numeric(i) && i %% 1 == 0 && i <= J && i >= 1)) {
      stop("'i' must be an alternative number.", call. = FALSE)
    }
  }

  ### add zero row and column to Sigma at row and column i
  if (i == 1) {
    Sigma_full <- rbind(0, cbind(0, Sigma))
  } else if (i == J) {
    Sigma_full <- rbind(cbind(Sigma, 0), 0)
  } else {
    Sigma_full <- cbind(Sigma[, 1:(i - 1)], 0, Sigma[, i:(J - 1)])
    Sigma_full <- rbind(Sigma_full[1:(i - 1), ], 0, Sigma_full[i:(J - 1), ])
  }

  ### add kernel element to make all elements non-zero
  if (pos) {
    Sigma_full <- Sigma_full + 1
  }

  if (checks) {
    ### check if 'Sigma_full' is a covariance matrix
    if (!is_covariance_matrix(Sigma_full)) {
      stop("Back-transformed matrix is no covariance matrix.", call. = FALSE)
    }

    ### check if back-differencing yields differenced matrix
    Sigma_back <- delta(J, i) %*% Sigma_full %*% t(delta(J, i))
    if (any(abs(Sigma_back - Sigma) > sqrt(.Machine$double.eps))) {
      stop("Back-differencing failed.", call. = FALSE)
    }
  }

  ### return undifferenced covariance matrix
  if (labels) {
    names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
  }
  return(Sigma_full)
}

