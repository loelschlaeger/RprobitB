#' Prepare empirical choice data.
#'
#' @description
#' This function prepares empirical choice data for the RprobitB package.
#'
#' @details
#' See the vignette "Choice data" for more details:
#' \code{vignette("choice_data", package = "RprobitB")}.
#'
#' @inheritParams check_form
#' @param choice_data
#' A data frame of choice data with the following requirements:
#' \itemize{
#'   \item It **must** be in "wide" format, i.e. each row represents one choice
#'         occasion.
#'   \item It **must** contain a column named \code{id} which contains unique
#'         identifier for each decision maker.
#'   \item It **can** contain a column named \code{idc} which contains unique
#'         identifier for each choice situation of each decision maker.
#'         If this information is missing, these identifier are generated
#'         automatically by the appearance of the choices in the data set.
#'   \item It **can** contain a column named \code{choice} with the observed
#'         choices, where \code{choice} must match the name of the dependent
#'         variable in \code{form}.
#'         Such a column is required for model fitting but not for prediction.
#'   \item It **must** contain a column named *p_j* for each alternative
#'         specific covariate *p* in \code{form} and each choice alternative *j*
#'         in \code{alternatives}.
#'   \item It **must** contain a column named *q* for each covariate *q* in
#'         \code{form} that is constant across alternatives.
#' }
#' @param id
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each decision maker. The default is \code{"id"}.
#' @param idc
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each choice situation of each decision maker.
#' The default is \code{NULL}, in which case these identifier are generated
#' automatically.
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @examples
#' data("Train", package = "mlogit")
#' data <- prepare_data(
#'   form = choice ~ price + time + comfort + change | 1,
#'   choice_data = Train,
#'   re = c("price", "time"),
#'   id = "id",
#'   idc = "choiceid"
#' )
#' @export

prepare_data <- function(form, choice_data, re = NULL, alternatives = NULL,
                         id = "id", idc = NULL, standardize = NULL) {

  ### check 'form'
  check_form_out <- check_form(form = form, re = re)
  form <- check_form_out$form
  choice <- check_form_out$choice
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### check 'choice_data'
  if (!is.data.frame(choice_data)) {
    stop("'choice_data' must be a data frame.")
  }
  if (!(is.character(id) && length(id) == 1)) {
    stop("'id' must be a character.")
  }
  if (!id %in% colnames(choice_data)) {
    stop(paste0("Decider identification column '", id, "' not found in 'choice_data'."))
  }
  if (!is.null(idc)) {
    if (!(is.character(idc) && length(idc) == 1)) {
      stop("'idc' must be a character.")
    }
    if (!idc %in% colnames(choice_data)) {
      stop(paste0("Choice occasion identification column '", idc, "' not found in 'choice_data'."))
    }
  }

  ### check if 'choice_data' contains choices
  choice_available <- (choice %in% colnames(choice_data))
  if (!choice_available) {
    choice <- NA
    warning("No choices found.")
  }

  ### check if any data point is missing or infinite
  for (col in 1:ncol(choice_data)) {
    for (row in 1:nrow(choice_data)) {
      if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) || is.nan(choice_data[row, col])) {
        stop(paste0(
          "Please remove NAs, NaNs or infinite values in column '", colnames(choice_data)[col], "', row number ", row, "."
        ))
      }
    }
  }

  ### sort 'choice_data' by column 'id'
  choice_data <- choice_data[order(choice_data[, id]), ]

  ### create choice occasion ids
  if (is.null(idc)) {
    idc <- "idc"
    choice_data[, idc] <- unlist(sapply(table(choice_data[, id]), seq_len, simplify = FALSE))
  }

  ### sort 'choice_data' first by column 'id' and second by column 'idc'
  choice_data <- choice_data[order(choice_data[, id], choice_data[, idc]), ]

  ### identify / filter, sort and count alternatives
  if (is.null(alternatives)) {
    if (choice_available) {
      alternatives <- as.character(unique(choice_data[[choice]]))
    } else {
      stop("Please specify 'alternatives' if choices are not available.")
    }
  } else {
    if (!is.character(alternatives)) {
      stop("'alternatives' must be a character vector.")
    }
    if (choice_available) {
      choice_data <- choice_data[choice_data[[choice]] %in% alternatives, ]
      if (nrow(choice_data) == 0) {
        stop(paste(
          "No choices for", paste(alternatives, collapse = ", "), "found."
        ))
      }
    }
  }
  alternatives <- sort(alternatives)
  J <- length(alternatives)
  if (J <= 1) {
    stop("At least two alternatives are required.")
  }

  ### check if all required covariates are present in 'choice_data'
  for (var in vars[[2]]) {
    if (!var %in% names(choice_data)) {
      stop(paste0("Column '", var, "' not found in choice_data."))
    }
  }
  for (var in c(vars[[1]], vars[[3]])) {
    for (j in alternatives) {
      if (!paste0(var, "_", j) %in% names(choice_data)) {
        stop(paste0("Column '", paste0(var, "_", j), "' not found in 'choice_data'."))
      }
    }
  }

  ### determine number and names of linear coefficients
  linear_coeffs <- overview_effects(form, re, alternatives)
  P_f <- sum(linear_coeffs$re == FALSE)
  P_r <- sum(linear_coeffs$re == TRUE)
  linear_coeffs_names <- linear_coeffs$name

  ### add ASCs
  if (ASC) {
    choice_data[, "ASC"] <- 1
  }

  ### standardize covariates
  if (!is.null(standardize)) {
    if (!is.character(standardize)) {
      stop("'standardize' must be a character (vector).")
    }
    if (identical(standardize, "all")) {
      standardize <- c(
        apply(expand.grid(vars[[1]], alternatives), 1, paste, collapse = "_"),
        vars[[2]],
        apply(expand.grid(vars[[3]], alternatives), 1, paste, collapse = "_")
      )
    }
    if ("ASC" %in% standardize) {
      standardize <- standardize[-which(standardize == "ASC")]
    }
    for (var in vars[[2]]) {
      if (var %in% standardize) {
        choice_data[, var] <- scale(choice_data[, var])
      }
    }
    for (var in c(vars[[1]], vars[[3]])) {
      for (j in alternatives) {
        var_alt <- paste0(var, "_", j)
        if (var_alt %in% standardize) {
          choice_data[, var_alt] <- scale(choice_data[, var_alt])
        }
      }
    }
  }

  ### transform 'choice_data' in list format 'data'
  ids <- unique(choice_data[, id])
  N <- length(ids)
  T <- as.numeric(table(choice_data[, id]))
  data <- list()
  for (n in seq_len(N)) {
    data[[n]] <- list()
    data_n <- choice_data[choice_data[, id] == ids[n], ]
    X_n <- list()

    for (t in seq_len(T[n])) {
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
      X_nt <- X_nt[, linear_coeffs_names]

      ### save in list
      X_n[[t]] <- X_nt
    }

    data[[n]][["X"]] <- X_n
    data[[n]][["y"]] <- if (choice_available) data_n[[choice]] else NA
  }

  ### delete "ASC" from 'choice_data'
  if (ASC) {
    choice_data$ASC <- NULL
  }

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
    standardize = standardize,
    simulated = FALSE,
    choice_available = choice_available,
    true_parameter = NULL,
    res_var_names = list("choice" = choice, "id" = id, "idc" = idc)
  )

  ### return 'RprobitB_data' object
  return(out)
}
