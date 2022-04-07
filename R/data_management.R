#' Check the model formula
#'
#' @description
#' This function checks the input \code{form}.
#'
#' @param form
#' A formula object that is used to specify the probit model.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{A} are names of alternative and choice situation specific
#'   covariates with a generic coefficient,
#'   \item \code{B} are names of choice situation specific covariates with
#'   alternative specific coefficients,
#'   \item and \code{C} are names of alternative and choice situation specific
#'   covariates with alternative specific coefficients.
#' }
#' Separate multiple covariates of one type by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model
#' (for all except for the last alternative due to identifiability).
#' They can be removed by adding \code{+0} in the second spot.
#' See the vignette on choice data for more details.
#' @param re
#' A character (vector) of covariates of \code{form} with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the alternative specific constants, include
#' \code{"ASC"} in \code{re}.
#'
#' @return
#' An object of class \code{RprobitB_formula}, which is a list that contains the
#' following elements:
#' \itemize{
#'   \item \code{form}:
#'   The input \code{form}.
#'   \item \code{choice}:
#'   The dependent variable in \code{form}.
#'   \item \code{re}:
#'   The input \code{re}, where covariates that are not part of \code{form}
#'   are removed.
#'   \item \code{vars}:
#'   A list of three character vectors of covariate names of the three
#'   covariate types.
#'   \item \code{ASC}:
#'   A boolean, determining whether the model has ASCs.
#' }
#'
#' @examples
#' form <- choice ~ price + time + comfort + change
#' re <- c("price", "time")
#' check_form(form = form, re = re)
#'
#' @export
#'
#' @seealso
#' [overview_effects()] for an overview of the model effects

check_form <- function(form, re = NULL) {

  ### check inputs
  if (!inherits(form, "formula")) {
    stop("'form' must be of class 'formula'.", call. = FALSE)
  }
  if (!is.null(re)) {
    if (!is.character(re)) {
      stop("'re' must be a character (vector).", call. = FALSE)
    }
  }

  ### extract name of depentend variable
  choice <- all.vars(form)[1]

  ### build 'vars'
  vars <- trimws(strsplit(as.character(form)[3], split = "|",
                          fixed = TRUE)[[1]])
  while (length(vars) < 3) {
    vars <- c(vars, NA)
  }
  vars <- lapply(strsplit(vars, split = "+", fixed = TRUE), trimws)

  ### build 'ASC'
  ASC <- ifelse(any(vars[[2]] %in% 0), FALSE, TRUE)
  for (i in 1:3) {
    vars[[i]] <- vars[[i]][!vars[[i]] %in% c(0, 1, NA)]
  }

  ### match 're' with 'form'
  if (!is.null(re)) {
    for (re_element in re) {
      if (!re_element %in% c("ASC", unlist(vars))) {
        re <- setdiff(re, re_element)
        warning(
          "The covariate '", re_element,
          "' in 're' is not part of 'form' and hence ignored.", call. = FALSE
        )
      }
    }
  }

  ### return
  out <- list(
    "form" = form,
    "choice" = choice,
    "re" = re,
    "vars" = vars,
    "ASC" = ASC
  )
  class(out) <- "RprobitB_formula"
  return(out)
}

#' @noRd
#' @export

print.RprobitB_formula <- function(x, ...) {
  print(x$form)
  cat("- dependent variable:", x$choice, "\n")
  for (i in 1:3) {
    cat("- type", i, "covariate(s):", paste(x$vars[[i]], collapse = ", "), "\n")
  }
  cat("- random effects:", paste(x$re, collapse = ", "), "\n")
  cat("- ASC:", x$ASC, "\n")
  return(invisible(x))
}

#' Effect overview
#'
#' @description
#' This function gives an overview of the model coefficients and whether they
#' are connected to random effects.
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' A data frame with the coefficient names and booleans indicating whether
#' they are connected to random effects.
#'
#' @examples
#' form <- choice ~ price + time + comfort + change | 1
#' re <- c("price", "time")
#' alternatives <- c("A", "B")
#' overview_effects(form = form, re = re, alternatives = alternatives)
#'
#' @export
#'
#' @seealso
#' [check_form()] for checking the model formula

overview_effects <- function(form, re = NULL, alternatives) {

  ### sort and count 'alternatives'
  alternatives <- sort(alternatives)
  J <- length(alternatives)

  ### check 'form'
  check_form_out <- check_form(form = form, re = re)
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### determine names of linear coefficients
  overview <- data.frame()
  for (var in vars[[1]]) {
    overview <- rbind(overview, c(var, var %in% re))
  }
  for (var in c(vars[[2]], if (ASC) "ASC")) {
    for (j in 1:(J - 1)) {
      overview <- rbind(overview,
                        c(paste0(var, "_", alternatives[j]), var %in% re))
    }
  }
  for (var in vars[[3]]) {
    for (j in 1:J) {
      overview <- rbind(overview,
                        c(paste0(var, "_", alternatives[j]), var %in% re))
    }
  }
  colnames(overview) <- c("name", "re")
  overview$re <- as.logical(overview$re)

  ### sort 'overview', first by 'random' and second by appearance in formula
  overview <- overview[order(overview$re, rownames(overview)), ]
  rownames(overview) <- NULL

  ### return 'overview'
  return(overview)
}

#' Create lagged choice covariates
#'
#' @description
#' This convenience function creates lagged choice covariates from a data frame
#' \code{choice_data}. This is useful if a choice should be explained by a
#' covariate from a previous choice occasion. The function is vectorized over
#' \code{column} and \code{k}.
#'
#' @details
#' Say that \code{choice_data} contains the column \code{column}. Then, the
#' function call
#' \preformatted{
#' create_lagged_cov(choice_data, column, k, id)
#' }
#' returns the input \code{choice_data} which includes a new column named
#' \code{column.k}. This column contains for each decider (based on
#' \code{id}) and each choice occasion the covariate faced before
#' \code{k} choice occasions. If this data point is not available, it is set to
#' \code{NA}. In particular, the first \code{k} values of \code{column.k} will
#' be \code{NA} (initial condition problem).
#'
#' @param choice_data
#' A data frame of choice data, which is assumed to be sorted by choice
#' occasions: First choice occasions on top.
#' @param column
#' A character, the column name in \code{choice_data}, i.e. the covariate name.
#' Can be a vector.
#' @param k
#' A positive number, the number of lags (in units of observations), see the
#' details. Can be a vector. The default is \code{k = 1}.
#' @param id
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each decision maker. The default is \code{id = "id"}.
#'
#' @return
#' The input data frame \code{choice_data} with the additional columns
#' named \code{column.k} for each element \code{column} and each number \code{k}
#' containing the lagged covariates.
#'
#' @examples
#' ### add covariate 'lost' and 'berserk' from previous choice occasion
#' \dontrun{
#' choice_data <- create_lagged_cov(
#'   choice_data = choice_berserk,
#'   column = c("lost", "berserk"),
#'   k = 1,
#'   id = "player_id"
#' )
#' }
#'
#' @export

create_lagged_cov <- function(choice_data, column, k = 1, id = "id") {

  ### check inputs
  if (!is.data.frame(choice_data)) {
    stop("'choice_data' must be a data frame.", call. = FALSE)
  }
  if (!is.character(column)) {
    stop("'column' must be a character (vector).", call. = FALSE)
  }
  if (!all(is.numeric(k) && k%%1==0 && k>0)) {
    stop("'k' must be a number or a vector of numbers.", call. = FALSE)
  }
  if (!is.character(id) || length(id) != 1) {
    stop("'id' must be a character.", call. = FALSE)
  }
  for(col in c(column,id)) {
    if(!col %in% colnames(choice_data)) {
      stop(paste0("Column '", col, "' not found in 'choice_data'."),
           call. = FALSE)
    }
  }

  ### loop over 'column' and 'k'
  for(col in column) for(k_val in k) {

    ### check if new columns already exist in 'choice_data'
    col_new <- paste(col,k_val,sep=".")
    if(col_new %in% colnames(choice_data)) {
      warning(
        paste0("Column '", col_new, "' already exists in 'choice_data'. ",
               "It would be overwritten, hence it is skipped."),
        call. = FALSE
      )
      next()
    }

    ### add column 'col.k'
    cols_old <- colnames(choice_data)
    choice_data <- cbind(choice_data, NA)
    colnames(choice_data) <- c(cols_old, col_new)

    ### build progress bar
    pb <- RprobitB_progress(title = paste("create",col_new),
                            total = length(unique(choice_data[[id]])))

    ### create lagged covariate 'col.k'
    for(id_val in unique(choice_data[[id]])) {
      RprobitB_pp(pb)
      id_rows <- which(choice_data[[id]] == id_val)
      for(i in seq_along(id_rows)[-(1:k_val)]) {
        choice_data[id_rows[i], col_new] <- choice_data[id_rows[i - k_val], col]
      }
    }
  }

  ### return updated 'choice_data'
  return(choice_data)
}

#' Relabel the alternative specific covariates to the required format
#'
#' @description
#' In {RprobitB}, alternative specific covariates must be named in the format
#' `"<covariate>_<alternative>"`. This convenience function generates
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

#' Prepare empirical choice data for estimation
#'
#' @description
#' This function prepares empirical choice data.
#'
#' @details
#' See [the vignette on choice data](https://loelschlaeger.de/RprobitB/articles/v02_choice_data.html)
#' for more details.
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
#'   \item It **must** contain a numeric column named *p_j* for each alternative
#'         specific covariate *p* in \code{form} and each choice alternative *j*
#'         in \code{alternatives}.
#'   \item It **must** contain a numeric column named *q* for each covariate *q*
#'         in \code{form} that is constant across alternatives.
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
#'   \item \code{"complete_cases"}, removes rows containing missing entries,
#'   \item \code{"zero_out"}, replaces missing entries by zero,
#'   \item \code{"mean"}, imputes missing entries by the covariate mean.
#' }
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
#'   idc = "choiceid",
#'   standardize = c("price", "time")
#' )
#'
#' @export
#'
#' @seealso
#' \itemize{
#'   \item [check_form()] for checking the model formula
#'   \item [overview_effects()] for an overview of the model effects
#'   \item [create_lagged_cov()] for creating lagged covariates
#'   \item [as_cov_names()] for renaming alternative-specific covariates
#'   \item [simulate_choices()] for simulating choice data
#'   \item [train_test()] for splitting choice data into a train and test subset
#' }

prepare_data <- function(form, choice_data, re = NULL, alternatives = NULL,
                         id = "id", idc = NULL, standardize = NULL,
                         missing_data = "complete_cases") {

  ### check input
  if(!(is.character(missing_data) && length(missing_data) == 1 &&
       missing_data %in% c("complete_cases","zero_out","mean"))) {
    stop(
      "'missing_data' must be either 'complete_cases', 'zero_out' or 'mean'.",
      call. = FALSE
      )
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
    stop(paste0("Decider identification column '", id,
                "' not found in 'choice_data'."), call. = FALSE)
  }
  if (!is.null(idc)) {
    if (!(is.character(idc) && length(idc) == 1)) {
      stop("'idc' must be a character.", call. = FALSE)
    }
    if (!idc %in% colnames(choice_data)) {
      stop(paste0("Choice occasion identification column '", idc,
                  "' not found in 'choice_data'."), call. = FALSE)
    }
  }

  ### check if 'choice_data' contains choices
  choice_available <- (choice %in% colnames(choice_data))
  if (!choice_available) {
    choice <- NA
  }

  ### handle missing data
  pb <- RprobitB_progress(title = "Checking missing data",
                          total = ncol(choice_data))
  if(missing_data == "complete_cases"){
    bad_rows <- c()
    for (col in 1:ncol(choice_data)) {
      RprobitB_pp(pb)
      for (row in 1:nrow(choice_data)) {
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) ||
            is.nan(choice_data[row, col])) {
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
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) ||
            is.nan(choice_data[row, col])) {
          if(is.numeric(choice_data[, col])){
            choice_data[row, col] <- 0
          } else {
            stop(
              "In 'choice_data', cannot apply 'zero_out' to entry [",row,
              ",",col,"] because column is not numeric. Use 'complete_cases' instead.",
              call. = FALSE
            )
          }
        }
      }
    }
  } else if(missing_data == "mean"){
    for (col in 1:ncol(choice_data)) {
      RprobitB_pp(pb)
      for (row in 1:nrow(choice_data)) {
        if (is.na(choice_data[row, col]) || is.infinite(choice_data[row, col]) ||
            is.nan(choice_data[row, col])) {
          if(is.numeric(choice_data[, col])){
            choice_data[row, col] <- mean(choice_data[, col, na.rm = TRUE])
          } else {
            stop(
              "In 'choice_data', cannot apply 'mean' to entry [",row,",",
              col,"] because column is not numeric. Use 'complete_cases' instead.",
              call. = FALSE)
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
    choice_data[, idc] <- unlist(sapply(table(choice_data[, id]), seq_len,
                                        simplify = FALSE))
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
      stop("Please specify 'alternatives' if choices are not available.",
           call. = FALSE)
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
      stop(paste0("Column '", var, "' not found in 'choice_data'."),
           call. = FALSE)
    }
    if (!is.numeric(choice_data[,var])){
      stop(paste0("Column '", var, "' in 'choice_data' is not numeric."),
           call. = FALSE)
    }
  }
  for (var in c(vars[[1]], vars[[3]])) {
    for (j in alternatives) {
      if (!paste0(var, "_", j) %in% names(choice_data)) {
        stop(
          paste0("Column '", paste0(var, "_", j), "' not found in 'choice_data'."),
          call. = FALSE
          )
      }
      if (!is.numeric(choice_data[,paste0(var, "_", j)])){
        stop(
          paste0("Column '", paste0(var, "_", j), "' in 'choice_data' is not numeric."),
          call. = FALSE
          )
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

#' Simulate choice data
#'
#' @description
#' This function simulates choice data.
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
#' @param ...
#' Optionally specify \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_full}, \code{beta}, or \code{z} for the simulation.
#' @inheritParams prepare_data
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cost | income | time,
#'   N = 100,
#'   T = 10,
#'   J = 2,
#'   re = c("cost", "time"),
#'   alternatives = c("car", "bus"),
#'   seed = 1,
#'   alpha = c(-1, 1),
#'   b = matrix(c(-1, -1, -0.5, -1.5, 0, -1), ncol = 2),
#'   C = 2
#' )
#' @export
#'
#' @importFrom stats rnorm
#' @import Rcpp
#'
#' @seealso
#' \itemize{
#'   \item [check_form()] for checking the model formula
#'   \item [overview_effects()] for an overview of the model effects
#'   \item [create_lagged_cov()] for creating lagged covariates
#'   \item [as_cov_names()] for renaming alternative-specific covariates
#'   \item [prepare_data()] for preparing empirical choice data
#'   \item [train_test()] for splitting choice data into a train and test subset
#' }

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
    stop("'N' must be a non-negative number.",
         call. = FALSE)
  }
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  if (any(!is.numeric(T)) || any(T %% 1 != 0)) {
    stop("'T' must be non-negative or a vector of non-negative numbers.",
         call. = FALSE)
  }
  if (!is.numeric(J) || J %% 1 != 0 || !J >= 2) {
    stop("'J' must be a number greater or equal 2.",
         call. = FALSE)
  }
  if (is.null(alternatives)) {
    if (J > 26) {
      stop("Please specify 'alternatives'.",
           call. = FALSE)
    } else {
      alternatives <- LETTERS[1:J]
    }
  }
  if (length(alternatives) != J || !is.character(alternatives)) {
    stop("'alternatives' must be a character (vector) of length 'J'.",
         call. = FALSE)
  }
  if (!is.null(covariates)) {
    for (i in 1:length(covariates)) {
      if (length(covariates[[i]]) != sum(T)) {
        stop(paste0("In 'covariates', there must be ", sum(T), " values for '",
                    names(covariates)[i], "'."), call. = FALSE)
      }
    }
  }

  ### sort alternatives
  alternatives <- sort(alternatives)

  ### draw covariates
  if (!is.null(seed)) {
    set.seed(seed)
  }
  choice_data <- data.frame(
    "id" = rep(1:N, times = T),
    "idc" = unlist(sapply(T, seq_len, simplify = FALSE))
  )
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

  ### report unsed elements in 'covariates'
  if (length(names(covariates)) > 0) {
    warning(paste("The column(s)", paste(paste0("'", names(covariates), "'",
                                                collapse = ", ")),
                  "in 'covariates' are ignored."), call. = FALSE)
  }

  ### add ASCs (for all but the last alternative)
  if (ASC) {
    choice_data$ASC <- 1
  }

  ### determine number and names of linear coefficients
  linear_coefs <- overview_effects(form, re, alternatives)
  P_f <- sum(linear_coefs$re == FALSE)
  P_r <- sum(linear_coefs$re == TRUE)
  linear_coefs_names <- linear_coefs$name

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
      X_nt <- X_nt[, linear_coefs_names, drop = FALSE]

      ### save in list
      data[[n]][["X"]][[t]] <- X_nt

      ### build coefficient vector
      if (P_f > 0 & P_r > 0) {
        coef <- c(true_parameter$alpha, true_parameter$beta[, n])
      }
      if (P_f > 0 & P_r == 0) {
        coef <- true_parameter$alpha
      }
      if (P_f == 0 & P_r > 0) {
        coef <- true_parameter$beta[, n]
      }
      if (P_f == 0 & P_r == 0) {
        coef <- NA
      }

      ### compute utility and choice decision
      eps <- as.vector(rmvnorm(mu = rep(0,J), Sigma = true_parameter$Sigma_full))
      if (P_f == 0 & P_r == 0) {
        U_nt <- eps
      } else {
        V_nt <- X_nt %*% coef
        U_nt <- V_nt + eps
      }
      y_n[t] <- alternatives[which.max(U_nt)]
    }

    data[[n]][["y"]] <- y_n
  }

  ### save choices in 'choice_data'
  choice_data[choice] <- unlist(lapply(data, function(x) x[["y"]]))

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
    standardize = NULL,
    simulated = TRUE,
    choice_available = TRUE,
    true_parameter = true_parameter,
    res_var_names = list("choice" = choice,
                         "cov" = cov_names,
                         "id" = "id",
                         "idc" = "idc")
  )

  ### return 'RprobitB_data' object
  return(out)
}


#' Split choice data set in two parts
#'
#' @description
#' This function splits choice data into a train and a test part.
#'
#' @details
#' See [the vignette on choice data](https://loelschlaeger.de/RprobitB/articles/v02_choice_data.html)
#' for more details.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param test_proportion
#' A number between 0 and 1, the proportion of the test subsample.
#' @param test_number
#' A positive integer, the number of observations in the test subsample.
#' @param by
#' One of \code{"N"} (split by deciders) and \code{"T"} (split by choice
#' occasions).
#' @param random
#' If \code{TRUE}, the subsamples are build randomly.
#' @param seed
#' Set a seed for building the subsamples randomly.
#'
#' @return
#' A list with two objects of class \code{RprobitB_data}, named \code{"train"}
#' and \code{"test"}.
#'
#' @examples
#' ### simulate choices for demonstration
#' x <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#'
#' ### 70% of deciders in the train subsample,
#' ### 30% of deciders in the test subsample
#' train_test(x, test_proportion = 0.3, by = "N")
#'
#' ### 2 randomly chosen choice occasions per decider in the test subsample,
#' ### the rest in the train subsample
#' train_test(x, test_number = 2, by = "T", random = TRUE, seed = 1)
#'
#' @export
#'
#' @importFrom stats na.omit
#' @importFrom utils tail

train_test <- function(x, test_proportion = NULL, test_number = NULL, by = "N",
                       random = FALSE, seed = NULL) {

  ### input checks
  if (class(x) != "RprobitB_data") {
    stop("'x' must be of class 'RprobitB_data'.", call. = FALSE)
  }
  if (is.null(test_proportion) && is.null(test_number)) {
    stop("Either 'test_proportion' or 'test_number' must be specified.",
         call. = FALSE)
  }
  if (!is.null(test_proportion) && !is.null(test_number)) {
    stop("Only one of 'test_proportion' and 'test_number' can be specified.",
         call. = FALSE)
  }
  if (!is.null(test_proportion)) {
    if (!(is.numeric(test_proportion) && length(test_proportion) == 1 &&
          all(test_proportion >= 0) && all(test_proportion <= 1))) {
      stop("'test_proportion' must be a number between 0 and 1.", call. = FALSE)
    }
  }
  if (!is.null(test_number)) {
    if (!(is.numeric(test_number) && length(test_number) == 1 &&
          all(test_number >= 0) && all(test_number %% 1 == 0))) {
      stop("'test_number' must be a positive integer.", call. = FALSE)
    }
  }
  if (!(length(by) == 1 && by %in% c("N", "T"))) {
    stop("'by' must be 'N' or 'T'.", call. = FALSE)
  }
  if (!is.logical(random)) {
    stop("'random' must be a boolean.", call. = FALSE)
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### create train and test data set
  train <- x
  test <- x

  if (by == "N") {
    ### split by deciders
    if (!is.null(test_proportion)) {
      size_test <- round(x$N * test_proportion)
    } else if (!is.null(test_number)) {
      size_test <- test_number
    }
    if (random) {
      ind_test <- sort(sample.int(x$N, size = size_test))
    } else {
      ind_test <- utils::tail(seq_len(x$N), size_test)
    }
    ind_train <- setdiff(seq_len(x$N), ind_test)

    ### remove elements from 'train'
    train$data <- train$data[ind_train]
    train$choice_data <- train$choice_data[train$choice_data$id %in% ind_train, ]
    train$N <- sum(ind_train != 0)
    train$T <- train$T[ind_train]
    if (!identical(train$true_parameter$beta, NA)) {
      train$true_parameter$beta <- train$true_parameter$beta[, ind_train,
                                                             drop = FALSE]
    }
    if (!identical(train$true_parameter$z, NA)) {
      train$true_parameter$z <- train$true_parameter$z[ind_train]
    }

    ### remove elements from 'test'
    test$data <- test$data[ind_test]
    test$choice_data <- test$choice_data[test$choice_data$id %in% ind_test, ]
    test$N <- sum(ind_test != 0)
    test$T <- test$T[ind_test]
    if (!identical(test$true_parameter$beta, NA)) {
      test$true_parameter$beta <- test$true_parameter$beta[, ind_test,
                                                           drop = FALSE]
    }
    if (!identical(test$true_parameter$z, NA)) {
      test$true_parameter$z <- test$true_parameter$z[ind_test]
    }
  } else if (by == "T") {
    ### split by choice occasions
    for (n in seq_len(x$N)) {
      if (!is.null(test_proportion)) {
        size_test <- round(x$T[n] * test_proportion)
      } else if (!is.null(test_number)) {
        size_test <- test_number
      }

      ### check 'size_test'
      if (size_test > x$T[n]) {
        warning(
          "Only ", x$T[n], " observation(s) available for decider ", n, ".",
          call. = FALSE
          )
        size_test <- x$T[n]
      }

      if (random) {
        ind_test <- sort(sample.int(x$T[n], size = size_test))
      } else {
        ind_test <- utils::tail(seq_len(x$T[n]), size_test)
      }
      ind_train <- setdiff(seq_len(x$T[n]), ind_test)

      ### check 'ind_test' and 'ind_train'
      if (sum(ind_test != 0) == 0) {
        warning("No observation(s) for decider ", n, " in test subsample.",
                call. = FALSE)
      }
      if (sum(ind_train != 0) == 0) {
        warning("No observation(s) for decider ", n, " in train subsample.",
                call. = FALSE)
      }

      ### remove elements from 'train'
      train$data[[n]] <- list("X" = train$data[[n]]$X[ind_train],
                              "y" = train$data[[n]]$y[ind_train])
      train$choice_data[train$choice_data$id == n
                        & !train$choice_data$idc %in% ind_train, ] <- NA
      train$choice_data <- stats::na.omit(train$choice_data)
      train$T[n] <- sum(ind_train != 0)

      ### remove elements from 'test'
      test$data[[n]] <- list("X" = test$data[[n]]$X[ind_test],
                             "y" = test$data[[n]]$y[ind_test])
      test$choice_data[test$choice_data$id == n &
                         !test$choice_data$idc %in% ind_test, ] <- NA
      test$choice_data <- stats::na.omit(test$choice_data)
      test$T[n] <- sum(ind_test != 0)
    }
  }

  ### return train and test data set
  return(list("train" = train, "test" = test))
}

#' Plot method for \code{RprobitB_data}
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_data}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param by_choice
#' Set to \code{TRUE} to group the covariates by the chosen alternatives.
#' @param alpha,position
#' Passed to \code{\link[ggplot2]{ggplot}}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot theme_bw theme geom_bar aes geom_histogram geom_density
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' data <- simulate_choices(
#'  form = choice ~ cost | 0,
#'  N = 100,
#'  T = 10,
#'  J = 2,
#'  alternatives = c("bus", "car"),
#'  alpha = -1
#' )
#' plot(data, by_choice = TRUE)

plot.RprobitB_data <- function(x, by_choice = FALSE, alpha = 1,
                               position = "dodge", ...) {

  ### extract the data to be plotted
  data_red <- x$choice_data[names(x$choice_data) %in%
                              unlist(x$res_var_names[c("choice","cov")])]

  ### transform covariates with less than 10 values to factors
  for(i in 1:ncol(data_red)){
    if(length(unique(data_red[,i])) < 10){
      data_red[,i] <- as.factor(data_red[,i])
    }
  }

  ### create basis of plot
  base_plot <- ggplot2::ggplot(data = data_red) +
    ggplot2::theme_bw() +
    ggplot2::scale_fill_brewer(palette="Set1") +
    ggplot2::scale_color_brewer(palette="Set1") +
    ggplot2::theme(legend.position="none") +
    ggplot2::labs(y = "")

  plots <- list()

  plots[[1]] <-  base_plot + ggplot2::geom_bar(
    mapping = ggplot2::aes(
      x = .data[[x$res_var_names$choice]],
      fill = if(by_choice) .data[[x$res_var_names$choice]] else NULL
      ),
    position = position, alpha = alpha)

  for(cov in setdiff(names(data_red), x$res_var_names$choice)) {

    if(is.factor(data_red[[cov]])){
      p <- ggplot2::geom_bar(
        mapping = ggplot2::aes(
          x = .data[[cov]],
          fill = if(by_choice) .data[[x$res_var_names$choice]] else NULL
          ),
        position = position, alpha = alpha
      )
    } else {
      p <- ggplot2::geom_freqpoly(
        mapping = ggplot2::aes(
          x = .data[[cov]],
          color = if(by_choice) .data[[x$res_var_names$choice]] else NULL
          ),
        alpha = alpha
      )
    }

    plots[[length(plots)+1]] <- base_plot + p
  }

  suppressMessages(gridExtra::grid.arrange(grobs = plots))
}
