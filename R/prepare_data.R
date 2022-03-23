#' Prepare empirical choice data
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
#' @param missing_data
#' Specifies how to handle missing entries (\code{NA, NaN, -Inf, Inf}) in
#' \code{choice_data}. The following options are available:
#' \itemize{
#'   \item \code{"complete_cases"}, which removes rows containing missing entries,
#'   \item \code{"zero_out"}, which replaces missing entries by zero,
#'   \item \code{"mean"}, which imputes missing entries by the covariate mean.
#' }
#'
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @examples
#' data("Train", package = "mlogit")
#' data <- prepare_data(
#'   form = choice ~ price + time + comfort + change | 0,
#'   choice_data = Train,
#'   re = c("price", "time"),
#'   id = "id",
#'   idc = "choiceid"
#' )
#' @export

prepare_data <- function(form, choice_data, re = NULL, alternatives = NULL,
                         id = "id", idc = NULL, standardize = NULL,
                         missing_data = "complete_cases") {

  ### check input
  if(!(is.character(missing_data) && length(missing_data) == 1 &&
       missing_data %in% c("complete_cases","zero_out","mean"))) {
    stop("'missing_data' must be one of 'complete_cases', 'zero_out' and 'mean'.", call. = FALSE)
  }

  ### check 'form'
  check_form_out <- check_form(form = form, re = re)
  form <- check_form_out$form
  choice <- check_form_out$choice
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### check 'choice_data'
  if (!is.data.frame(choice_data)) {
    stop("'choice_data' must be a data frame.", call. = FALSE)
  }
  if (!(is.character(id) && length(id) == 1)) {
    stop("'id' must be a character.", call. = FALSE)
  }
  if (!id %in% colnames(choice_data)) {
    stop(paste0("Decider identification column '", id, "' not found in 'choice_data'."), call. = FALSE)
  }
  if (!is.null(idc)) {
    if (!(is.character(idc) && length(idc) == 1)) {
      stop("'idc' must be a character.", call. = FALSE)
    }
    if (!idc %in% colnames(choice_data)) {
      stop(paste0("Choice occasion identification column '", idc, "' not found in 'choice_data'."), call. = FALSE)
    }
  }

  ### check if 'choice_data' contains choices
  choice_available <- (choice %in% colnames(choice_data))
  if (!choice_available) {
    choice <- NA
  }

  ### handle missing data
  pb <- RprobitB_progress(title = "Checking missing data", total = ncol(choice_data))
  if(missing_data == "complete_cases"){
    bad_rows <- c()
    for (col in 1:ncol(choice_data)) {
      RprobitB_pp(pb)
      for (row in 1:nrow(choice_data)) {
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) || is.nan(choice_data[row, col])) {
          bad_rows <- c(bad_rows, row)
        }
      }
    }
    if(length(bad_rows) > 0){
      choice_data <- choice_data[-unique(bad_rows),,drop=FALSE]
    }
  } else if(missing_data == "zero_out"){
    for (col in 1:ncol(choice_data)) {
      RprobitB_pp(pb)
      for (row in 1:nrow(choice_data)) {
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) || is.nan(choice_data[row, col])) {
          if(is.numeric(choice_data[, col])){
            choice_data[row, col] <- 0
          } else {
            stop("In 'choice_data', cannot apply 'zero_out' to entry [",row,",",col,"] because column is not numeric. Use 'complete_cases' instead.", call. = FALSE)
          }
        }
      }
    }
  } else if(missing_data == "mean"){
    for (col in 1:ncol(choice_data)) {
      RprobitB_pp(pb)
      for (row in 1:nrow(choice_data)) {
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) || is.nan(choice_data[row, col])) {
          if(is.numeric(choice_data[, col])){
            choice_data[row, col] <- mean(choice_data[, col, na.rm = TRUE])
          } else {
            stop("In 'choice_data', cannot apply 'mean' to entry [",row,",",col,"] because column is not numeric. Use 'complete_cases' instead.", call. = FALSE)
          }
        }
      }
    }
  }

  ### transform 'id' of 'choice_data' to factor
  choice_data[, id] <- as.factor(choice_data[, id])

  ### sort 'choice_data' by 'id'
  choice_data <- choice_data[order(choice_data[, id]), ]

  ### create choice occasion ids
  if (is.null(idc)) {
    idc <- "idc"
    choice_data[, idc] <- unlist(sapply(table(choice_data[, id]), seq_len, simplify = FALSE))
  }

  ### transform 'idc' of 'choice_data' to factor
  choice_data[, idc] <- as.factor(choice_data[, idc])

  ### sort 'choice_data' first by column 'id' and second by column 'idc'
  choice_data <- choice_data[order(choice_data[, id], choice_data[, idc]), ]

  ### identify / filter, sort and count alternatives
  if (is.null(alternatives)) {
    if (choice_available) {
      alternatives <- as.character(unique(choice_data[[choice]]))
    } else {
      stop("Please specify 'alternatives' if choices are not available.", call. = FALSE)
    }
  } else {
    if (!is.character(alternatives)) {
      stop("'alternatives' must be a character vector.", call. = FALSE)
    }
    if (choice_available) {
      choice_data <- choice_data[choice_data[[choice]] %in% alternatives, ]
      ### drop unused factor levels
      choice_data[,id] <- droplevels(choice_data[,id])
      choice_data[,idc] <- droplevels(choice_data[,idc])
      if (nrow(choice_data) == 0) {
        stop(paste(
          "No choices for", paste(alternatives, collapse = ", "), "found."
        ), call. = FALSE)
      }
    }
  }
  alternatives <- sort(alternatives)
  J <- length(alternatives)
  if (J <= 1) {
    stop("At least two alternatives are required.", call. = FALSE)
  }

  ### check if all required covariates are present in 'choice_data' and numerics
  for (var in vars[[2]]) {
    if (!var %in% names(choice_data)) {
      stop(paste0("Column '", var, "' not found in 'choice_data'."), call. = FALSE)
    }
    if (!is.numeric(choice_data[,var])){
      stop(paste0("Column '", var, "' in 'choice_data' is not numeric."), call. = FALSE)
    }
  }
  for (var in c(vars[[1]], vars[[3]])) {
    for (j in alternatives) {
      if (!paste0(var, "_", j) %in% names(choice_data)) {
        stop(paste0("Column '", paste0(var, "_", j), "' not found in 'choice_data'."), call. = FALSE)
      }
      if (!is.numeric(choice_data[,paste0(var, "_", j)])){
        stop(paste0("Column '", paste0(var, "_", j), "' in 'choice_data' is not numeric."), call. = FALSE)
      }
    }
  }

  ### determine number and names of linear coefficients
  linear_coefs <- overview_effects(form, re, alternatives)
  P_f <- sum(linear_coefs$re == FALSE)
  P_r <- sum(linear_coefs$re == TRUE)
  linear_coefs_names <- linear_coefs$name

  ### add ASCs
  if (ASC) {
    choice_data[, "ASC"] <- 1
  }

  ### standardize covariates
  if (!is.null(standardize)) {
    if (!is.character(standardize)) {
      stop("'standardize' must be a character (vector).", call. = FALSE)
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
  pb <- RprobitB_progress(title = "Preparing data", total = N)
  for (n in seq_len(N)) {
    RprobitB_pp(pb)
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
      X_nt <- X_nt[, linear_coefs_names, drop = FALSE]

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

  ### save cov names
  cov_names <- c(
    if(length(vars[[1]]) > 0)
      paste(rep(vars[[1]], each = length(alternatives)), alternatives, sep = "_"),
    vars[[2]],
    if(length(vars[[3]]) > 0)
      paste(rep(vars[[3]], each = length(alternatives)), alternatives, sep = "_"))

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
    linear_coefs = linear_coefs,
    standardize = standardize,
    simulated = FALSE,
    choice_available = choice_available,
    true_parameter = NULL,
    res_var_names = list("choice" = choice,
                         "cov" = cov_names,
                         "id" = id,
                         "idc" = idc)
  )

  ### return 'RprobitB_data' object
  return(out)
}

#' Relabel the alternative specific covariates
#'
#' @description
#' In {RprobitB}, alternative specific covariates must be named in the format
#' `"<covariate>_<alternative>"`. This convenience function to generates
#' the format for a given `choice_data` set.
#'
#' @param choice_data
#' A data frame.
#' @param cov
#' A character vector of the names of alternative specific covariates in
#' `choice_data`.
#' @param alternatives
#' A (character or numeric) vector of the alternative names.
#'
#' @return
#' The `choice_data` input with updated column names.
#'
#' @examples
#' data("Electricity", package = "mlogit")
#' cov <- c("pf","cl","loc","wk","tod","seas")
#' alternatives <- 1:4
#' colnames(Electricity)
#' Electricity <- as_cov_names(Electricity, cov, alternatives)
#' colnames(Electricity)
#'
#' @export

as_cov_names <- function(choice_data, cov, alternatives) {
  x <- colnames(choice_data)
  for(i in seq_len(length(x))){
    lab <- x[i]
    match_cov <- sapply(cov, function(x) grepl(x, lab))
    match_alt <- sapply(alternatives, function(x) grepl(x, lab))
    if(sum(match_cov) > 1 || sum(match_alt) > 1){
      stop("Failed due to ambiguity.", call. = FALSE)
    } else if(sum(match_cov) == 1 && sum(match_alt) == 1){
      x[i] <- paste0(cov[which(match_cov)],"_",alternatives[which(match_alt)])
    }
  }
  colnames(choice_data) <- x
  return(choice_data)
}

#' Get covariates of choice situation
#'
#' @description
#' This convenience function returns the covariates and the choices of specific
#' choice occasions.
#'
#' @param x
#' Either an object of class \code{RprobitB_data} or \code{RprobitB_fit}.
#' @param id
#' A numeric (vector), that specifies the decider(s).
#' @param idc
#' A numeric (vector), that specifies the choice occasion(s).
#' @param idc_label
#' The name of the column that contains the choice occasion identifier.
#' @return
#' A subset of the `choice_data` data frame specified in `prepare_data()`.
#'
#' @examples
#' data("model_train", package = "RprobitB")
#' get_cov(model_train, id = 1:2, idc = 1:2, idc_label = "choiceid")
#'
#' @export

get_cov <- function(x, id, idc, idc_label){
  if(inherits(x, "RprobitB_fit")){
    x <- x$data
  }
  if(inherits(x, "RprobitB_data")){
    id_label <- x$res_var_names$id
    idc_label <- if(missing(idc_label)) x$res_var_names$idc else idc_label
    if(missing(id)) id <- x$choice_data[[id_label]]
    if(missing(idc)) idc <- x$choice_data[[idc_label]]
    ind <- x$choice_data[[id_label]] %in% id & x$choice_data[[idc_label]] %in% idc
    out <- x$choice_data[ind,]
    if(nrow(out) == 0){
      stop("Requested choice occasion not found.", call. = FALSE)
    }
    return(out)
  } else {
    stop("'x' must be either an 'RprobitB_fit' or 'RprobitB_data' object.",
         call. = FALSE)
  }
}
