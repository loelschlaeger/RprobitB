#' Simulate choice data
#'
#' @description
#' This function simulates choice data from a probit model.
#'
#' @details
#' See [the vignette on choice data](https://loelschlaeger.de/RprobitB/articles/v02_choice_data.html)
#' for more details.
#'
#' @inheritParams RprobitB_data
#' @inheritParams check_form
#' @param covariates
#' A named list of covariate values. Each element must be a vector of length
#' equal to the number of choice occasions and named according to a covariate.
#' Covariates for which no values are supplied are drawn from a standard normal
#' distribution.
#' @param seed
#' Set a seed for the simulation.
#' @param true_parameter
#' Optionally specify a named list with true parameter values for \code{alpha},
#' \code{C}, \code{s}, \code{b}, \code{Omega}, \code{Sigma}, \code{Sigma_full},
#' \code{beta}, \code{z}, or \code{d} for the simulation.
#' See [the vignette on model definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
#' for definitions of these variables.
#' @inheritParams prepare_data
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @examples
#' ### simulate data from a binary probit model with two latent classes
#' data <- simulate_choices(
#'   form = choice ~ cost | income | time,
#'   N = 100,
#'   T = 10,
#'   J = 2,
#'   re = c("cost", "time"),
#'   alternatives = c("car", "bus"),
#'   seed = 1,
#'   true_parameter = list(
#'     "alpha" = c(-1, 1),
#'     "b" = matrix(c(-1, -1, -0.5, -1.5, 0, -1), ncol = 2),
#'     "C" = 2
#'   )
#' )
#'
#' ### simulate data from an ordered probit model
#' data <- simulate_choices(
#'   form = opinion ~ age + gender,
#'   N = 10,
#'   T = 1:10,
#'   J = 5,
#'   alternatives = c("very bad", "bad", "indifferent", "good", "very good"),
#'   ordered = TRUE,
#'   covariates = list(
#'     "gender" = rep(sample(c(0, 1), 10, replace = TRUE), times = 1:10)
#'   ),
#'   seed = 1
#' )
#'
#' ### simulate data from a ranked probit model
#' data <- simulate_choices(
#'   form = product ~ price,
#'   N = 10,
#'   T = 1:10,
#'   J = 3,
#'   alternatives = c("A", "B", "C"),
#'   ranked = TRUE,
#'   seed = 1
#' )
#'
#' @export
#'
#' @seealso
#' \itemize{
#'   \item [check_form()] for checking the model formula
#'   \item [overview_effects()] for an overview of the model effects
#'   \item [create_lagged_cov()] for creating lagged covariates
#'   \item [as_cov_names()] for re-labeling alternative-specific covariates
#'   \item [prepare_data()] for preparing empirical choice data
#'   \item [train_test()] for splitting choice data into a train and test subset
#' }

simulate_choices <- function(
    form, N, T = 1, J, re = NULL, alternatives = NULL, ordered = FALSE,
    ranked = FALSE, base = NULL, covariates = NULL, seed = NULL,
    true_parameter = list()) {
  ### check 'form'
  if (missing(form)) {
    stop("Please specify the model formula 'form'.",
         call. = FALSE
    )
  }
  check_form_out <- check_form(form = form, re = re, ordered = ordered)
  form <- check_form_out$form
  choice <- check_form_out$choice
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### check other inputs
  if (missing(N)) {
    stop("Please specify 'N'.",
         call. = FALSE
    )
  }
  if (!is.numeric(N) || N %% 1 != 0) {
    stop("'N' must be a non-negative number.",
         call. = FALSE
    )
  }
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  if (any(!is.numeric(T)) || any(T %% 1 != 0)) {
    stop("'T' must be non-negative or a vector of non-negative numbers.",
         call. = FALSE
    )
  }
  if (!is.numeric(J) || J %% 1 != 0 || !J >= 2) {
    stop("'J' must be a number greater or equal 2.",
         call. = FALSE
    )
  }
  if (is.null(alternatives)) {
    if (J > 26) {
      stop("Please specify 'alternatives'.",
           call. = FALSE
      )
    } else {
      alternatives <- LETTERS[1:J]
    }
  }
  if (length(alternatives) != J || !is.character(alternatives)) {
    stop("'alternatives' must be a character (vector) of length 'J'.",
         call. = FALSE
    )
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean",
         call. = FALSE
    )
  }
  if (!isTRUE(ranked) && !isFALSE(ranked)) {
    stop("'ranked' must be a boolean",
         call. = FALSE
    )
  }
  if (ordered == TRUE && ranked == TRUE) {
    stop("'ordered' and 'ranked' cannot both be TRUE.",
         call. = FALSE
    )
  }
  if (ordered == TRUE && J <= 2) {
    stop("'J' must be greater or equal 3 in the ordered probit model.",
         call. = FALSE
    )
  }
  if (ranked == TRUE && J <= 2) {
    stop("'J' must be greater or equal 3 in the ranked probit model.",
         call. = FALSE
    )
  }
  if (!is.null(covariates)) {
    for (i in 1:length(covariates)) {
      if (length(covariates[[i]]) != sum(T)) {
        stop(
          paste0(
            "In 'covariates', there must be ", sum(T), " values for '",
            names(covariates)[i], "'."
          ),
          call. = FALSE
        )
      }
    }
  }

  ### draw covariates
  if (!is.null(seed)) {
    set.seed(seed)
  }
  choice_data <- data.frame(
    "id" = rep(1:N, times = T),
    "idc" = unlist(sapply(T, seq_len, simplify = FALSE))
  )
  if (!ordered) {
    ### sort alternatives
    alternatives <- sort(alternatives)

    ### determine index of base alternative
    if (is.null(base)) {
      base <- alternatives[J]
      base_index <- J
    } else if (any(alternatives == base)) {
      base_index <- which(alternatives == base)
    } else {
      base <- alternatives[J]
      warning(
        paste0(
          "'base' not contained in alternative set.\n",
          "Set 'base = ", alternatives[J], "' instead."
        ),
        immediate. = TRUE, call. = FALSE
      )
      base_index <- J
    }
  }
  for (var in vars[[1]]) {
    for (alt in alternatives) {
      var_alt <- paste0(var, "_", alt)
      if (var_alt %in% names(covariates)) {
        cov <- matrix(covariates[[var_alt]], ncol = 1)
        covariates[[var_alt]] <- NULL
      } else {
        cov <- matrix(stats::rnorm(n = sum(T)), ncol = 1)
      }
      colnames(cov) <- var_alt
      choice_data <- cbind(choice_data, cov)
    }
  }
  for (var in vars[[2]]) {
    if (var %in% names(covariates)) {
      cov <- matrix(covariates[[var]], ncol = 1)
      covariates[[var]] <- NULL
    } else {
      cov <- matrix(stats::rnorm(n = sum(T)), ncol = 1)
    }
    colnames(cov) <- var
    choice_data <- cbind(choice_data, cov)
  }
  for (var in vars[[3]]) {
    for (alt in alternatives) {
      var_alt <- paste0(var, "_", alt)
      if (var_alt %in% names(covariates)) {
        cov <- matrix(covariates[[var_alt]], ncol = 1)
        covariates[[var_alt]] <- NULL
      } else {
        cov <- matrix(stats::rnorm(n = sum(T)), ncol = 1)
      }
      colnames(cov) <- var_alt
      choice_data <- cbind(choice_data, cov)
    }
  }

  ### report un-used elements in 'covariates'
  if (length(names(covariates)) > 0) {
    warning(
      paste(
        "The column(s)", paste(paste0("'", names(covariates), "'", collapse = ", ")),
        "in 'covariates' are ignored."
      ),
      call. = FALSE, immediate. = TRUE
    )
  }

  ### artificially add ASCs
  if (ASC) choice_data$ASC <- 1

  ### determine number and names of linear coefficients
  effects <- overview_effects(form, re, alternatives, base, ordered)
  P_f <- sum(effects$random == FALSE)
  P_r <- sum(effects$random == TRUE)

  ### check supplied and draw missing model parameters
  true_parameter <- do.call(
    what = RprobitB_parameter,
    args = c(
      list(
        "P_f" = P_f, "P_r" = P_r, "J" = J, "N" = N, "seed" = seed,
        "ordered" = ordered
      ),
      true_parameter
    )
  )

  ### transform 'choice_data' in list format 'data' and simulate choices
  data <- list()
  ids <- unique(choice_data[, "id"])
  N <- length(ids)
  T <- as.numeric(table(choice_data[, "id"]))

  ### simulate choices
  for (n in seq_len(N)) {
    data[[n]] <- list()
    data[[n]][["X"]] <- list()
    data_n <- choice_data[choice_data[, "id"] == ids[n], ]
    y_n <- numeric(T[n])

    for (t in seq_len(T[n])) {
      data_nt <- data_n[t, ]

      if (ordered) {
        X_nt <- as.matrix(data_nt[, vars[[2]]], nrow = 1)
        colnames(X_nt) <- vars[[2]]
      } else {
        X_nt <- matrix(NA_real_, nrow = J, ncol = 0)

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
          mat <- matrix(0, J, J)
          for (j in (1:J)[-base_index]) {
            mat[j, j] <- data_nt[, var]
          }
          mat <- mat[, -base_index, drop = FALSE]
          X_nt <- cbind(X_nt, mat)
          colnames(X_nt) <- c(old_names, paste0(var, "_", alternatives[(1:J)[-base_index]]))
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
      }

      ### sort covariates
      X_nt <- X_nt[, effects$effect, drop = FALSE]

      ### save in list
      data[[n]][["X"]][[t]] <- X_nt

      ### build coefficient vector
      if (P_f > 0 & P_r > 0) {
        coef <- c(true_parameter$alpha, true_parameter$beta[, n])
      } else if (P_f > 0 & P_r == 0) {
        coef <- true_parameter$alpha
      } else {
        coef <- true_parameter$beta[, n]
      }

      ### compute utility and choice decision
      if (ordered) {
        eps <- rnorm(n = 1, mean = 0, sd = sqrt(true_parameter$Sigma))
        if (P_f == 0 & P_r == 0) {
          U_nt <- eps
        } else {
          V_nt <- as.numeric(X_nt %*% coef)
          U_nt <- V_nt + eps
        }
        gamma <- c(0, cumsum(exp(true_parameter[["d"]])))
        y_nt_ind <- cut(U_nt,
                        breaks = c(-Inf, gamma, Inf),
                        right = TRUE, include.lowest = TRUE, labels = FALSE
        )
        y_n[t] <- alternatives[y_nt_ind]
      } else {
        eps <- as.vector(rmvnorm(mu = rep(0, J), Sigma = true_parameter$Sigma_full))
        if (P_f == 0 & P_r == 0) {
          U_nt <- eps
        } else {
          V_nt <- X_nt %*% coef
          U_nt <- V_nt + eps
        }
        if (ranked) {
          y_n[t] <- paste(alternatives[order(as.vector(U_nt), decreasing = TRUE)],
                          collapse = ","
          )
        } else {
          y_n[t] <- alternatives[which.max(U_nt)]
        }
      }
    }

    data[[n]][["y"]] <- y_n
  }

  ### save choices in 'choice_data'
  choice_data[choice] <- unlist(lapply(data, function(x) x[["y"]]))

  ### delete "ASC" from 'choice_data'
  if (ASC) choice_data$ASC <- NULL

  ### save cov names
  cov_names <- c(
    if (length(vars[[1]]) > 0) {
      paste(rep(vars[[1]], each = length(alternatives)), alternatives, sep = "_")
    },
    vars[[2]],
    if (length(vars[[3]]) > 0) {
      paste(rep(vars[[3]], each = length(alternatives)), alternatives, sep = "_")
    }
  )

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
    ordered = ordered,
    ranked = ranked,
    base = base,
    form = form,
    re = re,
    ASC = ASC,
    effects = effects,
    standardize = NULL,
    simulated = TRUE,
    choice_available = TRUE,
    true_parameter = true_parameter,
    res_var_names = list(
      "choice" = choice, "cov" = cov_names, "id" = "id",
      "idc" = "idc"
    )
  )

  ### return 'RprobitB_data' object
  return(out)
}
