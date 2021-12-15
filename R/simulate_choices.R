#' Simulate choice data.
#' @description
#' This function simulates choice data for the RprobitB package.
#' @details
#' See the vignette "Choice data" for more details:
#' \code{vignette("choice_data", package = "RprobitB")}.
#' @inheritParams RprobitB_data
#' @inheritParams check_form
#' @param covariates
#' A named list of covariate values. Each element must be a vector of length
#' equal to the number of choice occasions and named according to a covariate.
#' Covariates for which no values are supplied are drawn from a standard normal
#' distribution.
#' @param seed
#' Set a seed for the simulation.
#' @param ...
#' Optionally specify \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_full}, \code{beta}, or \code{z} for the simulation.
#' @inheritParams prepare_data
#' @return
#' An object of class \code{RprobitB_data}.
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cost | income | time,
#'   N = 100,
#'   T = 10,
#'   J = 2,
#'   re = c("cost", "time"),
#'   alternatives = c("car", "bus"),
#'   seed = 1,
#'   alpha = c(-1,1),
#'   C = 2
#' )
#' @export

simulate_choices <- function(form, N, T, J, re = NULL, alternatives = NULL,
                             covariates = NULL, seed = NULL, ...) {

  ### check 'form'
  check_form_out <- check_form(form = form, re = re)
  form <- check_form_out$form
  choice <- check_form_out$choice
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### check other inputs
  if (!is.numeric(N) || N %% 1 != 0) {
    stop("'N' must be a non-negative number.")
  }
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  if (any(!is.numeric(T)) || any(T %% 1 != 0)) {
    stop("'T' must be non-negative or a vector of non-negative numbers.")
  }
  if (!is.numeric(J) || J %% 1 != 0 || !J >= 2) {
    stop("'J' must be a number greater or equal 2.")
  }
  if (is.null(alternatives)) {
    if(J > 26){
      stop("Please specify 'alternatives'.")
    } else {
      alternatives <- LETTERS[1:J]
    }
  }
  if (length(alternatives) != J || !is.character(alternatives)) {
    stop("'alternatives' must be a character (vector) of length 'J'.")
  }
  if (!is.null(covariates)) {
    for(i in 1:length(covariates)) {
      if (length(covariates[[i]]) != sum(T)) {
        stop(paste0("In 'covariates', there must be ", sum(T), " values for '", names(covariates)[i], "'."))
      }
    }
  }

  ### sort alternatives
  alternatives <- sort(alternatives)

  ### draw covariates
  if (!is.null(seed)) {
    set.seed(seed)
  }
  choice_data <- data.frame("id" = rep(1:N, times = T),
                            "idc" = unlist(sapply(T, seq_len, simplify = FALSE)))
  for (var in vars[[1]]) {
    for(alt in alternatives) {
      var_alt <- paste0(var,"_",alt)
      if(var_alt %in% names(covariates)) {
        cov <- matrix(covariates[[var_alt]], ncol = 1)
        covariates[[var_alt]] <- NULL
      } else {
        cov <- matrix(rnorm(n = sum(T)), ncol = 1)
      }
      colnames(cov) <- var_alt
      choice_data <- cbind(choice_data, cov)
    }
  }
  for (var in vars[[2]]) {
    if(var %in% names(covariates)) {
      cov <- matrix(covariates[[var]], ncol = 1)
      covariates[[var]] <- NULL
    } else {
      cov <- matrix(rnorm(n = sum(T)), ncol = 1)
    }
    colnames(cov) <- var
    choice_data <- cbind(choice_data, cov)
  }
  for (var in vars[[3]]) {
    for(alt in alternatives) {
      var_alt <- paste0(var,"_",alt)
      if(var_alt %in% names(covariates)) {
        cov <- matrix(covariates[[var_alt]], ncol = 1)
        covariates[[var_alt]] <- NULL
      } else {
        cov <- matrix(rnorm(n = sum(T)), ncol = 1)
      }
      colnames(cov) <- var_alt
      choice_data <- cbind(choice_data, cov)
    }
  }

  ### report unsed elements in 'covariates'
  if (length(names(covariates)) > 0) {
    warning(paste("The column(s)",paste(paste0("'",names(covariates),"'", collapse = ", ")), "in 'covariates' are ignored."))
  }

  ### add ASCs (for all but the last alternative)
  if (ASC) {
    choice_data$ASC <- 1
  }

  ### determine number and names of linear coefficients
  linear_coeffs <- overview_effects(form, re, alternatives)
  P_f <- sum(linear_coeffs$re == FALSE)
  P_r <- sum(linear_coeffs$re == TRUE)
  linear_coeffs_names <- linear_coeffs$name

  ### check supplied and draw missing model parameters
  true_parameter <- do.call(
    what = RprobitB_parameter,
    args = c(
      list(
        "P_f" = P_f, "P_r" = P_r,
        "J" = J, "N" = N, "seed" = seed
      ),
      list(...)
    )
  )

  ### compute lower-triangular Choleski root of 'Sigma_full'
  L <- suppressWarnings(t(chol(true_parameter$Sigma_full, pivot = TRUE)))

  ### transform 'choice_data' in list format 'data'
  data <- list()
  ids <- unique(choice_data[, "id"])
  N <- length(ids)
  T <- as.numeric(table(choice_data[, "id"]))

  ### simulate choices
  for (n in seq_len(N)) {

    ### extract data for each decision maker
    data[[n]] <- list()
    data[[n]][["X"]] <- list()
    data_n <- choice_data[choice_data[, "id"] == ids[n], ]
    y_n <- numeric(T[n])

    for (t in seq_len(T[n])) {

      ### extract data for each choice occasion
      data_nt <- data_n[t, ]
      X_nt <- matrix(NA, nrow = J, ncol = 0)

      ### type-1 covariates
      for (var in vars[[1]]) {
        old_names <- colnames(X_nt)
        col <- numeric(J)
        for (j in 1:J) {
          col[j] <- data_nt[, paste0(var, "_", alternatives[j])]
        }
        X_nt <- cbind(X_nt, col)
        colnames(X_nt) <- c(old_names, var)
      }

      ### type-2 covariates
      for (var in c(vars[[2]], if (ASC) "ASC")) {
        old_names <- colnames(X_nt)
        mat <- matrix(0, J, J)[, -J, drop = FALSE]
        for (j in 1:(J - 1)) {
          mat[j, j] <- data_nt[, var]
        }
        X_nt <- cbind(X_nt, mat)
        colnames(X_nt) <- c(old_names, paste0(var, "_", alternatives[1:(J - 1)]))
      }

      ### type-3 covariates
      for (var in vars[[3]]) {
        old_names <- colnames(X_nt)
        mat <- matrix(0, J, J)
        for (j in 1:J) {
          mat[j, j] <- data_nt[, paste0(var, "_", alternatives[j])]
        }
        X_nt <- cbind(X_nt, mat)
        colnames(X_nt) <- c(old_names, paste0(var, "_", alternatives))
      }

      ### sort covariates
      X_nt <- X_nt[, linear_coeffs_names, drop = FALSE]

      ### save in list
      data[[n]][["X"]][[t]] <- X_nt

      ### build coefficient vector
      if (P_f > 0 & P_r > 0) {
        coeff <- c(true_parameter$alpha, true_parameter$beta[, n])
      }
      if (P_f > 0 & P_r == 0) {
        coeff <- true_parameter$alpha
      }
      if (P_f == 0 & P_r > 0) {
        coeff <- true_parameter$beta[, n]
      }
      if (P_f == 0 & P_r == 0) {
        coeff <- NA
      }

      ### compute utility and choice decision
      eps <- L %*% rnorm(J)
      if (P_f == 0 & P_r == 0) {
        U_nt <- eps
      } else {
        V_nt <- X_nt %*% coeff
        U_nt <- V_nt + eps
      }
      y_n[t] <- which.max(U_nt)
    }

    data[[n]][["y"]] <- y_n
  }

  ### save choices in 'choice_data'
  choice_data["choice"] <- unlist(lapply(data, function(x) x[["y"]]))

  ### create output
  out <- RprobitB_data(
    data = data,
    choice_data = choice_data,
    N = N,
    T = T,
    J = J,
    P_f = P_f,
    P_r = P_r,
    alternatives = alternatives,
    form = form,
    re = re,
    ASC = ASC,
    linear_coeffs = linear_coeffs,
    standardize = NULL,
    simulated = TRUE,
    choice_available = TRUE,
    true_parameter = true_parameter
  )

  ### return 'RprobitB_data' object
  return(out)
}
