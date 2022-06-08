#' Check the model formula
#'
#' @description
#' This function checks the input \code{form}.
#'
#' @param form
#' A \code{formula} object that is used to specify the model equation.
#' The structure is \code{choice ~ A | B | C}, where
#' \itemize{
#'   \item \code{choice} is the name of the dependent variable (the choices),
#'   \item \code{A} are names of alternative and choice situation specific
#'   covariates with a generic coefficient,
#'   \item \code{B} are names of choice situation specific covariates with
#'   alternative specific coefficients,
#'   \item and \code{C} are names of alternative and choice situation specific
#'   covariates with alternative specific coefficients.
#' }
#'
#' Multiple covariates (of one type) are seperated by a \code{+} sign.
#' By default, alternative specific constants (ASCs) are added to the model.
#' They can be removed by adding \code{+0} in the second spot.
#'
#' In the ordered probit model (\code{ordered = TRUE}), covariates are always
#' constant across alternatives and have generic coefficients. The
#' \code{formula} object here has the simple structure \code{choice ~ A}.
#' ASCs are not estimated.
#' @param re
#' A character (vector) of covariates of \code{form} with random effects.
#' If \code{re = NULL} (the default), there are no random effects.
#' To have random effects for the ASCs, include \code{"ASC"} in \code{re}.
#' @inheritParams RprobitB_data
#'
#' @return
#' A list that contains the following elements:
#' \itemize{
#'   \item The input \code{form}.
#'   \item The name \code{choice} of the dependent variable in \code{form}.
#'   \item The input \code{re}.
#'   \item A list \code{vars} of three character vectors of covariate names of
#'   the three covariate types.
#'   \item A boolean \code{ASC}, determining whether the model has ASCs.
#' }
#'
#' @examples
#' form <- choice ~ price + time + comfort + change
#' re <- c("price", "time")
#' RprobitB:::check_form(form = form, re = re)
#'
#' @seealso
#' [overview_effects()] for an overview of the model effects

check_form <- function(form, re = NULL, ordered = FALSE) {

  ### check inputs
  if (!inherits(form, "formula")) {
    stop("'form' must be of class 'formula'.", call. = FALSE)
  }
  if (!is.null(re)) {
    if (!is.character(re)) {
      stop("'re' must be a character (vector).", call. = FALSE)
    }
  }
  if (!is.logical(ordered)) {
    stop("'ordered' must be a boolean.", call. = FALSE)
  }

  ### extract name of dependent variable
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

  ### check the ordered case
  if (ordered) {
    vars[[2]] <- c(vars[[1]], vars[[2]], vars[[3]])
    vars[[1]] <- character()
    vars[[3]] <- character()
    re <- re[!re == "ASC"]
    ASC <- FALSE
  }

  ### match 're' with 'form'
  if (!is.null(re)) {
    for (re_element in re) {
      if (!re_element %in% c("ASC", unlist(vars))) {
        re <- setdiff(re, re_element)
        warning(
          "The covariate '", re_element,
          "' in 're' is not part of 'form' and hence ignored.", call. = FALSE,
          immediate. = TRUE
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
  return(out)
}

#' Effect overview
#'
#' @description
#' This function gives an overview of the effect names, whether the covariate
#' is alternative-specific, whether the coefficient is alternative-specific,
#' and whether it is a random effect.
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' A data frame, each row is a effect, columns are the effect name
#' \code{"effect"}, and booleans whether the covariate is alternative-specific
#' \code{"as_value"}, whether the coefficient is alternative-specific
#' \code{"as_coef"}, and whether it is a random effect \code{"random"}.
#'
#' @examples
#' overview_effects(
#'   form = choice ~ price + time + comfort + change | 1,
#'   re = c("price", "time"),
#'   alternatives = c("A", "B"),
#'   base = "A"
#' )
#'
#' @export
#'
#' @seealso
#' [check_form()] for checking the model formula specification.

overview_effects <- function(form, re = NULL, alternatives,
                             base = tail(alternatives, 1), ordered = FALSE) {

  ### check input
  if(missing(form)){
    stop("'Please specify 'form'.", call. = FALSE)
  }
  if(!inherits(form, "formula")) {
    stop("'form' must be of class 'formula'.", call. = FALSE)
  }
  if(!(is.null(re) || is.character(re))) {
    stop("'re' must be either 'NULL' or a character (vector).", call. = FALSE)
  }
  if(missing(alternatives)){
    stop("'Please specify 'alternatives'.", call. = FALSE)
  }
  if(!is.character(alternatives) || length(alternatives) < 2) {
    stop("'alternatives' must be a character vector of length greater or equal 2.",
         call. = FALSE)
  }
  if(!is.null(base)) {
    if(!(length(base) == 1 && is.character(base) && base %in% alternatives)) {
      stop("'base' must be one element of 'alternatives'.", call. = FALSE)
    }
  }
  if(!(length(ordered) == 1 && is.logical(ordered))) {
    stop("'ordered' must be a boolean.", call. = FALSE)
  }

  ### check 'form'
  check_form_out <- check_form(form = form, re = re, ordered = ordered)
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### build overview
  if(ordered){
    overview <- data.frame()
    for (var in vars[[2]]) {
      overview <- rbind(overview, c(var, FALSE, FALSE, var %in% re))
    }
  } else {
    ### sort and count 'alternatives'
    if(!ordered) alternatives <- sort(alternatives)
    J <- length(alternatives)

    ### determine index of base alternative
    if(is.null(base)){
      base_index <- J
    } else if (any(alternatives == base)) {
      base_index <- which(alternatives == base)
    } else {
      base <- alternatives[J]
      warning(paste0("'base' not contained in 'alternatives'. ",
                     "Set 'base = ", alternatives[J], "' instead."),
              immediate. = TRUE, call. = FALSE)
      base_index <- J
    }

    ### determine names of linear coefficients
    overview <- data.frame()
    for (var in vars[[1]]) {
      overview <- rbind(overview, c(var, TRUE, FALSE, var %in% re))
    }
    for (var in c(vars[[2]], if (ASC) "ASC")) {
      for (j in (1:J)[-base_index]) {
        overview <- rbind(overview,
                          c(paste0(var, "_", alternatives[j]), FALSE, TRUE, var %in% re))
      }
    }
    for (var in vars[[3]]) {
      for (j in 1:J) {
        overview <- rbind(overview,
                          c(paste0(var, "_", alternatives[j]), TRUE, TRUE, var %in% re))
      }
    }
  }
  colnames(overview) <- c("effect", "as_value", "as_coef", "random")
  overview$random <- as.logical(overview$random)

  ### sort 'overview', first by 'random' and second by appearance in the formula
  overview <- overview[order(overview$random, as.numeric(rownames(overview))), ]
  rownames(overview) <- NULL

  ### return 'overview'
  return(overview)
}

#' Create lagged choice covariates
#'
#' @description
#' This convenience function creates lagged choice covariates from a data frame
#' \code{choice_data}, which is assumed to be sorted by choice
#' occasions: First choice occasions on top. The function is vectorized over
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
#' @inheritParams prepare_data
#' @param column
#' A character, the column name in \code{choice_data}, i.e. the covariate name.
#' Can be a vector.
#' @param k
#' A positive number, the number of lags (in units of observations), see the
#' details. Can be a vector. The default is \code{k = 1}.
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
        call. = FALSE, immediate. = TRUE
      )
      next()
    }

    ### add column 'col.k'
    cols_old <- colnames(choice_data)
    choice_data <- cbind(choice_data, NA)
    colnames(choice_data) <- c(cols_old, col_new)

    ### preserve factors
    if(class(choice_data[[col]]) == "factor") {
      choice_data[[col_new]] <- factor(choice_data[[col_new]],
                                       levels = levels(choice_data[[col]]))
    }

    ### build progress bar
    pb <- RprobitB_pb(title = paste("create",col_new),
                      total = length(unique(choice_data[[id]])),
                      tail = "deciders")

    ### create lagged covariate 'col.k'
    for(id_val in unique(choice_data[[id]])) {
      RprobitB_pb_tick(pb)
      id_rows <- which(choice_data[[id]] == id_val)
      for(i in seq_along(id_rows)[-(1:k_val)]) {
        choice_data[id_rows[i], col_new] <- choice_data[id_rows[i - k_val], col]
      }
    }
  }

  ### return updated 'choice_data'
  return(choice_data)
}

#' Re-label the alternative specific covariates to the required format
#'
#' @description
#' In {RprobitB}, alternative specific covariates must be named in the format
#' \code{"<covariate>_<alternative>"}. This convenience function generates
#' the format for a given \code{choice_data} set.
#'
#' @inheritParams prepare_data
#' @param cov
#' A character vector of the names of alternative specific covariates in
#' \code{choice_data}.
#' @param alternatives
#' A (character or numeric) vector of the alternative names.
#'
#' @return
#' The \code{choice_data} input with updated column names.
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
#' Requirements for \code{choice_data}:
#' \itemize{
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
#'
#' See [the vignette on choice data](https://loelschlaeger.de/RprobitB/articles/v02_choice_data.html)
#' for more details.
#'
#' @inheritParams check_form
#' @param choice_data
#' A data frame of choice data in wide format, i.e. each row represents one
#' choice occasion.
#' @param id
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each decision maker. The default is \code{"id"}.
#' @param idc
#' A character, the name of the column in \code{choice_data} that contains
#' unique identifier for each choice situation of each decision maker.
#' The default is \code{NULL}, in which case these identifier are generated
#' automatically.
#' @inheritParams RprobitB_data
#' @inheritParams missing_data
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
#'   \item [as_cov_names()] for re-labeling alternative-specific covariates
#'   \item [simulate_choices()] for simulating choice data
#'   \item [train_test()] for splitting choice data into a train and test subset
#' }

prepare_data <- function(form, choice_data, re = NULL, alternatives = NULL,
                         ordered = FALSE, ranked = FALSE, base = NULL,
                         id = "id", idc = NULL, standardize = NULL,
                         impute = "complete_cases") {

  ### check 'form'
  check_form_out <- check_form(form = form, re = re, ordered = ordered)
  form <- check_form_out$form
  choice <- check_form_out$choice
  re <- check_form_out$re
  vars <- check_form_out$vars
  ASC <- check_form_out$ASC

  ### check other inputs
  if(!is.logical(ordered)) {
    stop("'ordered' must be a boolean", call. = FALSE)
  }
  if(!is.logical(ranked)) {
    stop("'ranked' must be a boolean", call. = FALSE)
  }

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
  choice_data <- missing_data(choice_data = choice_data, impute = impute)

  ### transform 'id' of 'choice_data' to factor
  choice_data[, id] <- as.factor(choice_data[, id])

  ### sort 'choice_data' by 'id'
  choice_data <- choice_data[order(choice_data[, id]), ]

  ### create choice occasion 'idc' (if not specified)
  if (is.null(idc)) {
    idc <- "idc"
    choice_data[, idc] <- unlist(sapply(table(choice_data[, id]), seq_len,
                                        simplify = FALSE))
  }

  ### transform 'idc' of 'choice_data' to factor
  choice_data[, idc] <- as.factor(choice_data[, idc])

  ### sort 'choice_data' first by column 'id' and second by column 'idc'
  choice_data <- choice_data[order(choice_data[, id], choice_data[, idc]), ]

  ### check alternative set
  if (ordered) {
    if (ranked) {
      stop("'ordered' and 'ranked' cannot both be TRUE.", call. = FALSE)
    }
    if (is.null(alternatives)) {
      stop("Please specify 'alternatives' if 'ordered = TRUE'.", call. = FALSE)
    }
  } else if (ranked) {
    stop("Not yet implemented.")
  } else {
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
  }
  J <- length(alternatives)
  if (J <= 1) {
    stop("At least two choice alternatives are required, only one was provided.",
         call. = FALSE)
  }

  ### determine index of base alternative
  if (ordered || !ASC || (length(vars[[1]]) == 0 && length(vars[[2]]) == 0 )) {
    base <- NULL
  } else {
    if(is.null(base)){
      base <- alternatives[J]
      base_index <- J
    } else if (any(alternatives == base)) {
      base_index <- which(alternatives == base)
    } else {
      base <- alternatives[J]
      warning(paste0("'base' not contained in 'alternatives'. ",
                     "Set 'base = ", alternatives[J], "' instead."),
              immediate. = TRUE, call. = FALSE)
      base_index <- J
    }
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
  effects <- overview_effects(form, re, alternatives, base, ordered)
  P_f <- sum(effects$random == FALSE)
  P_r <- sum(effects$random == TRUE)

  ### artificially add ASCs
  if (ASC) choice_data[, "ASC"] <- 1

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
      warning("Removed 'ASC' from 'standardize'.", call. = FALSE, immediate = TRUE)
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
  pb <- RprobitB_pb(title = "Preparing data", total = N, tail = "deciders")
  for (n in seq_len(N)) {
    RprobitB_pb_tick(pb)
    data[[n]] <- list()
    data_n <- choice_data[choice_data[, id] == ids[n], ]
    X_n <- list()

    for (t in seq_len(T[n])) {
      data_nt <- data_n[t, ]

      if(ordered) {
        X_nt <- matrix(data_nt[,vars[[2]]], nrow = 1)
        colnames(X_nt) <- vars[[2]]

      } else {
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
      X_n[[t]] <- X_nt
    }

    data[[n]][["X"]] <- X_n
    data[[n]][["y"]] <- if (choice_available) data_n[[choice]] else NA
  }

  ### delete "ASC" from 'choice_data'
  if (ASC) choice_data$ASC <- NULL

  ### save cov names
  cov_names <- c(
    if(length(vars[[1]]) > 0)
      paste(rep(vars[[1]], each = length(alternatives)), alternatives, sep = "_"),
    vars[[2]],
    if(length(vars[[3]]) > 0)
      paste(rep(vars[[3]], each = length(alternatives)), alternatives, sep = "_")
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
    standardize = standardize,
    simulated = FALSE,
    choice_available = choice_available,
    true_parameter = NULL,
    res_var_names = list("choice" = choice, "cov" = cov_names, "id" = id,
                         "idc" = idc)
  )

  ### return 'RprobitB_data' object
  return(out)
}

#' Handle missing choice data
#'
#' @description
#' This function checks for and replaces missing entries in \code{choice_data}.
#'
#' @inheritParams prepare_data
#' @param impute
#' A character that specifies how to handle missing entries (the elements of)
#' \code{as_missing}) in \code{choice_data}, one of:
#' \itemize{
#'   \item \code{"complete_cases"}, removes all rows containing missing entries
#'   (the default),
#'   \item \code{"zero"}, replaces missing entries by zero
#'   (only for numeric columns),
#'   \item \code{"mean"}, imputes missing entries by the covariate mean
#'   (only for numeric columns).
#' }
#' @param as_missing
#' A vector of elements that are interpreted as missing data entries,
#' the default is \code{as_missing = c(NA, NaN, -Inf, Inf)}.
#'
#' @return
#' The input \code{choice_data}, in which missing entries were addressed.
#'
#' @examples
#' choice_data <- data.frame("A" = c(1,NA,3), "B" = c(1,2,Inf))
#' missing_data(choice_data, "complete_cases")
#' missing_data(choice_data, "zero")
#' missing_data(choice_data, "mean")
#'
#' @export

missing_data <- function(choice_data, impute = "complete_cases",
                         as_missing = c(NA, NaN, -Inf, Inf)) {

  ### check input
  if (!is.data.frame(choice_data)) {
    stop("'choice_data' must be a data frame.", call. = FALSE)
  }
  if(!(is.character(impute) && length(impute) == 1 &&
       impute %in% c("complete_cases","zero","mean"))) {
    stop(
      "'impute' must be either 'complete_cases', 'zero' or 'mean'.",
      call. = FALSE
    )
  }

  ### find NA values
  na_pos <- which(sapply(choice_data, `%in%`, as_missing), arr.ind = TRUE)

  if(nrow(na_pos) > 0){

    ### imputation
    if(impute == "complete_cases"){
      choice_data <- choice_data[-na_pos[,"row"], , drop = FALSE]
    } else {
      if(any(sapply(unique(na_pos[,"col"]),
                    function(x) !is.numeric(choice_data[, x])))) {
        stop(paste0("Cannot apply 'impute = ", impute,
                    "' to columns that are not numeric."), call. = FALSE)
      }
      if(impute == "zero") {
        for(i in 1:nrow(na_pos)){
          choice_data[na_pos[i,"row"],na_pos[i,"col"]] <- 0
        }
      }
      if(impute == "mean") {
        for(i in 1:nrow(na_pos)){
          com_ind <- na_pos[which(na_pos[,"col"] == na_pos[i,"col"]), "row"]
          mean_data <- choice_data[, na_pos[i,"col"]][-com_ind]
          if(length(mean_data) == 0){
            stop(paste0("Cannot apply 'impute = mean' to column '",
                        colnames(choice_data)[na_pos[i,"col"]]), "'.",
                 call. = FALSE)
          }

          choice_data[na_pos[i,"row"],na_pos[i,"col"]] <- mean(mean_data,
                                                               na.rm = TRUE)
        }
      }
    }
  }

  ### return updated 'choice_data' object
  return(choice_data)
}

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
#' Optionally specify a named list with true parameters for \code{alpha},
#' \code{C}, \code{s}, \code{b}, \code{Omega}, \code{Sigma}, \code{Sigma_full},
#' \code{beta}, \code{z}, or \code{gamma} for the simulation.
#' See [the vignette on model definition](https://loelschlaeger.de/RprobitB/articles/v01_model_definition.html)
#' for definitions of these variables.
#' @inheritParams prepare_data
#'
#' @return
#' An object of class \code{RprobitB_data}.
#'
#' @examples
#' ### simulate data from a mixed probit model with two latent classes
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
#'     "b" = matrix(c(-1, -1, -0.5, -1.5, 0, -1), ncol = 2)),
#'     "C" = 2
#'   )
#' )
#'
#' ### simulate data from an ordered probit model
#' data <- simulate_choices(
#'   form = opinion_on_sth ~ age + gender,
#'   N = 10,
#'   T = 1:10,
#'   J = 5,
#'   alternatives = c("very good", "good", "indifferent", "bad", "very bad"),
#'   ordered = TRUE,
#'   covariates = list(
#'     "gender" = rep(sample(c(0,1), 10, replace = TRUE), times = 1:10)
#'     ),
#'   seed = 1,
#'   true_parameter = list(
#'     "alpha" = c(-1, 1),
#'     "gamma" = 0:3
#'   )
#' )
#'
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
#'   \item [as_cov_names()] for re-labeling alternative-specific covariates
#'   \item [prepare_data()] for preparing empirical choice data
#'   \item [train_test()] for splitting choice data into a train and test subset
#' }

simulate_choices <- function(form, N, T, J, re = NULL, alternatives = NULL,
                             ordered = FALSE, ranked = FALSE, base = NULL,
                             covariates = NULL, seed = NULL,
                             true_parameter = list()) {

  ### check 'form'
  check_form_out <- check_form(form = form, re = re, ordered = ordered)
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
  if(!is.logical(ordered)) {
    stop("'ordered' must be a boolean", call. = FALSE)
  }
  if(!is.logical(ranked)) {
    stop("'ranked' must be a boolean", call. = FALSE)
  }
  if(ordered == TRUE && ranked == TRUE) {
    stop("'ordered' and 'ranked' cannot both be TRUE.", call. = FALSE)
  }
  if (!is.null(covariates)) {
    for (i in 1:length(covariates)) {
      if (length(covariates[[i]]) != sum(T)) {
        stop(paste0("In 'covariates', there must be ", sum(T), " values for '",
                    names(covariates)[i], "'."), call. = FALSE)
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
  if(!ordered) {
    ### sort alternatives
    alternatives <- sort(alternatives)

    ### determine index of base alternative
    if(is.null(base)){
      base <- alternatives[J]
      base_index <- J
    } else if (any(alternatives == base)) {
      base_index <- which(alternatives == base)
    } else {
      base <- alternatives[J]
      warning(paste0("'base' not contained in alternative set.\n",
                     "Set 'base = ", alternatives[J], "' instead."),
              immediate. = TRUE, call. = FALSE)
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
    warning(paste(
      "The column(s)", paste(paste0("'", names(covariates), "'", collapse = ", ")),
      "in 'covariates' are ignored."), call. = FALSE, immediate. = TRUE
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
      list("P_f" = P_f, "P_r" = P_r, "J" = J, "N" = N, "seed" = seed,
           "ordered" = ordered),
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

      if(ordered) {
        X_nt <- as.matrix(data_nt[,vars[[2]]], nrow = 1)
        colnames(X_nt) <- vars[[2]]

      } else {
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
      if(ordered) {
        eps <- rnorm(n = 1, mean = 0, sd = sqrt(true_parameter$Sigma))
        if (P_f == 0 & P_r == 0) {
          U_nt <- eps
        } else {
          V_nt <- as.numeric(X_nt %*% coef)
          U_nt <- V_nt + eps
        }
        y_nt_ind <- cut(U_nt, breaks = c(-Inf, true_parameter$gamma, Inf),
                        right = TRUE, include.lowest = TRUE, labels = FALSE)
        y_n[t] <- alternatives[y_nt_ind]
      } else {
        eps <- as.vector(rmvnorm(mu = rep(0,J), Sigma = true_parameter$Sigma_full))
        if (P_f == 0 & P_r == 0) {
          U_nt <- eps
        } else {
          V_nt <- X_nt %*% coef
          U_nt <- V_nt + eps
        }
        y_n[t] <- alternatives[which.max(U_nt)]
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
    if(length(vars[[1]]) > 0)
      paste(rep(vars[[1]], each = length(alternatives)), alternatives, sep = "_"),
    vars[[2]],
    if(length(vars[[3]]) > 0)
      paste(rep(vars[[3]], each = length(alternatives)), alternatives, sep = "_")
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
    res_var_names = list("choice" = choice, "cov" = cov_names, "id" = "id",
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
  id <- unique(x$choice_data[[x$res_var_names$id]])

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
    train$choice_data <- x$choice_data[x$choice_data[[x$res_var_names$id]] %in% id[ind_train], ]
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
    test$choice_data <- x$choice_data[x$choice_data[[x$res_var_names$id]] %in% id[ind_test], ]
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
          call. = FALSE, immediate. = TRUE
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
                call. = FALSE, immediate. = TRUE)
      }
      if (sum(ind_train != 0) == 0) {
        warning("No observation(s) for decider ", n, " in train subsample.",
                call. = FALSE, immediate. = TRUE)
      }

      ### remove elements from 'train'
      train$data[[n]] <- list("X" = train$data[[n]]$X[ind_train],
                              "y" = train$data[[n]]$y[ind_train])
      train$choice_data[train$choice_data[[train$res_var_names$id]] == n
                        & !train$choice_data[[train$res_var_names$idc]] %in%
                          ind_train, ] <- NA
      train$choice_data <- stats::na.omit(train$choice_data)
      train$T[n] <- sum(ind_train != 0)

      ### remove elements from 'test'
      test$data[[n]] <- list("X" = test$data[[n]]$X[ind_test],
                             "y" = test$data[[n]]$y[ind_test])
      test$choice_data[test$choice_data[[test$res_var_names$id]] == n &
                         !test$choice_data[[test$res_var_names$idc]] %in%
                         ind_test, ] <- NA
      test$choice_data <- stats::na.omit(test$choice_data)
      test$T[n] <- sum(ind_test != 0)
    }
  }

  ### return train and test data set
  return(list("train" = train, "test" = test))
}

#' Create object of class \code{RprobitB_data}
#'
#' @description
#' This function constructs an object of class \code{RprobitB_data}.
#'
#' @param data
#' A list with the choice data.
#' The list has \code{N} elements.
#' Each element is a list with two elements, \code{X} and \code{y}, which are
#' the covariates and decisions for a decision maker. More precisely,
#' \code{X} is a list of \code{T} elements, where each element is a matrix of
#' dimension \code{J}x(\code{P_f}+\code{P_r}) and contains the characteristics
#' for one choice occasion.
#' \code{y} is a vector of length \code{T} and contains the labels for the
#' chosen alternatives.
#' @param N
#' The number (greater or equal 1) of decision makers.
#' @param T
#' The number (greater or equal 1) of choice occasions or a vector of choice
#' occasions of length \code{N} (i.e. a decision maker specific number).
#' @param J
#' The number (greater or equal 2) of choice alternatives.
#' @param P_f
#' The number of covariates connected to a fixed coefficient (can be 0).
#' @param P_r
#' The number of covariates connected to a random coefficient (can be 0).
#' @param alternatives
#' A character vector with the names of the choice alternatives.
#' If not specified, the choice set is defined by the observed choices.
#' @param ordered
#' A boolean, \code{FALSE} per default. If \code{TRUE}, the choice set
#' \code{alternatives} is assumed to be ordered from worst to best.
#' @param ranked
#' TBA
#' @param base
#' A character, the name of the base alternative for covariates that are not
#' alternative specific (i.e. type 2 covariates and ASCs). Ignored and set to
#' \code{NULL} if the model has no alternative specific covariates (e.g. in the
#' ordered probit model).
#' Per default, \code{base} is the last element of \code{alternatives}.
#' @param ASC
#' A boolean, determining whether the model has ASCs.
#' @param effects
#' A data frame with the effect names and booleans indicating whether
#' they are connected to random effects.
#' @param standardize
#' A character vector of names of covariates that get standardized.
#' Covariates of type 1 or 3 have to be addressed by
#' \code{<covariate>_<alternative>}.
#' If \code{standardize = "all"}, all covariates get standardized.
#' @param simulated
#' A boolean, if \code{TRUE} then \code{data} is simulated, otherwise
#' \code{data} is empirical.
#' @param choice_available
#' A boolean, if \code{TRUE} then \code{data} contains observed choices.
#' @inheritParams check_form
#' @inheritParams prepare_data
#' @param true_parameter
#' An object of class \code{RprobitB_parameters}.
#' @param res_var_names
#' A names list of reserved variable names in \code{choice_data}.
#'
#' @return
#' An object of class \code{RprobitB_data} with the arguments of this function
#' as elements.
#'
#' @keywords
#' internal

RprobitB_data <- function(data, choice_data, N, T, J, P_f, P_r, alternatives,
                          ordered, ranked, base, form, re, ASC, effects,
                          standardize, simulated, choice_available,
                          true_parameter, res_var_names) {

  ### check inputs
  stopifnot(is.list(data))
  stopifnot(is.numeric(N), N %% 1 == 0)
  stopifnot(is.numeric(T), T %% 1 == 0)
  stopifnot(is.numeric(J), J %% 1 == 0)
  stopifnot(is.numeric(P_f), P_f %% 1 == 0)
  stopifnot(is.numeric(P_r), P_r %% 1 == 0)
  stopifnot(is.character(alternatives) || J != length(alternatives))
  stopifnot(is.character(alternatives), base %in% alternatives)
  stopifnot(is.logical(ordered))
  stopifnot(is.logical(ranked))
  stopifnot(is.null(base) || (is.character(base) && length(base) == 1))
  stopifnot(inherits(form, "formula"))
  stopifnot(is.logical(simulated))
  stopifnot(is.logical(choice_available))
  if (!is.null(true_parameter)) {
    stopifnot(class(true_parameter) == "RprobitB_parameter")
  }

  ### create and return object of class "RprobitB_data"
  out <- list(
    "data" = data,
    "choice_data" = choice_data,
    "N" = N,
    "T" = T,
    "J" = J,
    "P_f" = P_f,
    "P_r" = P_r,
    "alternatives" = alternatives,
    "ordered" = ordered,
    "ranked" = ranked,
    "base" = base,
    "form" = form,
    "re" = re,
    "ASC" = ASC,
    "effects" = effects,
    "standardize" = standardize,
    "choice_available" = choice_available,
    "simulated" = simulated,
    "true_parameter" = true_parameter,
    "res_var_names" = res_var_names
  )
  class(out) <- "RprobitB_data"
  return(out)
}

#' @noRd
#' @export

print.RprobitB_data <- function(x, ...) {
  cat(
    ifelse(x$simulated, "Simulated", "Empirical"),
    "data of", sum(x$T),
    if(x$ordered) "(ordered)",
    if(x$ranked) "(ranked)",
    "choices.\n"
  )
  return(invisible(x))
}

#' @noRd
#' @export

summary.RprobitB_data <- function(object, ...) {

  ### check class of 'object'
  if (!inherits(object, "RprobitB_data")) {
    stop("Not of class 'RprobitB_data'.", call. = FALSE)
  }

  ### alternative frequency
  alt_freq <- data.frame(matrix(NA, nrow = 0, ncol = 1))
  colnames(alt_freq) <- "frequency"
  for (i in object$alternatives) {
    alt_freq[nrow(alt_freq) + 1, ] <-
      sum(unlist(lapply(object$data, function(x) x[["y"]])) == i)
    rownames(alt_freq)[nrow(alt_freq)] <- i
  }

  ### build 'summary.RprobitB_data' object
  out <- list(
    "simulated" = object$simulated,
    "N" = object$N,
    "T" = object$T,
    "form" = object$form,
    "re" = object$re,
    "effects" = object$effects,
    "alt_freq" = alt_freq
  )
  class(out) <- "summary.RprobitB_data"

  ### return 'summary.RprobitB_data' object
  return(out)
}


#' @export
#' @importFrom crayon underline
#' @noRd

print.summary.RprobitB_data <- function(x, ...) {
  overview <- data.frame(
    c(x$N,
      ifelse(length(unique(x$T)) == 1, x$T[1], paste0(min(x$T), "-", max(x$T))),
      sum(x$T),
      nrow(x$alt_freq),
      x$alt_freq$frequency
      )
  )
  rownames(overview) <- c("deciders", "choice occasions", "total choices",
                          "alternatives", paste0("- '", rownames(x$alt_freq), "'"))
  colnames(overview) <- c("count")
  print(overview)
  return(invisible(x))
}

#' Create object of class \code{RprobitB_parameter}
#'
#' @description
#' This function creates an object of class \code{RprobitB_parameter}.
#' If \code{sample = TRUE}, missing parameters are sampled. All parameters are
#' checked against the values of \code{P_f}, \code{P_r}, \code{J}, and \code{N}.
#'
#' @inheritParams RprobitB_data
#' @param C
#' The number (greater or equal 1) of latent classes of decision makers.
#' Set to \code{NA} if \code{P_r = 0}. Otherwise, \code{C = 1} per default.
#' @param alpha
#' The fixed coefficient vector of length \code{P_f}.
#' Set to \code{NA} if \code{P_f = 0}.
#' @param s
#' The vector of class weights of length \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' For identifiability, the vector must be non-ascending.
#' @param b
#' The matrix of class means as columns of dimension \code{P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Omega
#' The matrix of class covariance matrices as columns of dimension
#' \code{P_r*P_r} x \code{C}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param Sigma
#' The differenced error term covariance matrix of dimension
#' \code{J-1} x \code{J-1} with respect to alternative \code{J}.
#' In case of \code{ordered = TRUE}, a numeric, the single error term variance.
#' @param Sigma_full
#' The error term covariance matrix of dimension \code{J} x \code{J}.
#' Internally, \code{Sigma_full} gets differenced with respect to alternative
#' \code{J}, so it becomes an identified covariance matrix of dimension
#' \code{J-1} x \code{J-1}. \code{Sigma_full} is ignored if \code{Sigma} is
#' specified or \code{ordered = TRUE}.
#' @param beta
#' The matrix of the decision-maker specific coefficient vectors of dimension
#' \code{P_r} x \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param z
#' The vector of the allocation variables of length \code{N}.
#' Set to \code{NA} if \code{P_r = 0}.
#' @param gamma
#' The numberic vector of utility thresholds in the ordered case of length
#' \code{J-1}. The vector elements must be strictly increasing, and the first
#' element must be \code{0} (in order to fix the utility level).
#' @param sample
#' A boolean, if \code{TRUE} (default) missing parameters get sampled.
#' @param seed
#' Set a seed for the sampling of missing parameters.
#'
#' @return
#' An object of class \code{RprobitB_parameter}, i.e. a named list with the
#' model parameters \code{alpha}, \code{C}, \code{s}, \code{b}, \code{Omega},
#' \code{Sigma}, \code{Sigma_full}, \code{beta}, and \code{z}.
#'
#' @importFrom stats runif rnorm
#'
#' @export
#'
#' @examples
#' RprobitB_parameter(P_f = 1, P_r = 2, J = 3, N = 10)

RprobitB_parameter <- function(P_f, P_r, J, N, ordered = FALSE, alpha = NULL,
                               C = NULL, s = NULL, b = NULL, Omega = NULL,
                               Sigma = NULL, Sigma_full = NULL, beta = NULL,
                               z = NULL, gamma = NULL, seed = NULL,
                               sample = TRUE) {

  ### seed for sampling missing parameters
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### alpha
  if (P_f == 0) {
    alpha <- NA
  } else {
    if (is.null(alpha) && !sample) {
      alpha <- NA
    } else {
      if (is.null(alpha)) {
        alpha <- round(stats::runif(P_f, -3, 3), 1)
      }
      if (length(alpha) != P_f || !is.numeric(alpha)) {
        stop("'alpha' must be a numeric vector of length ", P_f, ".",
             call. = FALSE
        )
      }
      names(alpha) <- create_labels_alpha(P_f)
    }
  }

  ### C, s, b, Omega, z, beta
  if (P_r == 0) {
    C <- NA
    s <- NA
    b <- NA
    Omega <- NA
    z <- NA
    beta <- NA
  } else {

    ### C
    if (!is.null(C)) {
      if (!is.numeric(C) || !C %% 1 == 0 || !C > 0) {
        stop("'C' must be a number greater or equal 1.",
             call. = FALSE
        )
      }
    } else {
      C <- 1
    }

    ### s
    if (is.null(s) && !sample) {
      s <- NA
    } else {
      if (is.null(s)) {
        s <- round(sort(as.vector(rdirichlet(rep(1, C))), decreasing = TRUE), 2)
        s[C] <- 1 - sum(s[-C])
      }
      if (length(s) != C || !is.numeric(s) ||
          abs(sum(s) - 1) > .Machine$double.eps || is.unsorted(rev(s))) {
        stop("'s' must be a non-ascending numeric vector of length ", C,
             " which sums up to 1.",
             call. = FALSE
        )
      }
      names(s) <- create_labels_s(P_r, C)
    }

    ### b
    if (is.null(b) && !sample) {
      b <- NA
    } else {
      if (is.null(b)) {
        b <- matrix(0, nrow = P_r, ncol = C)
        for (c in 1:C) b[, c] <- round(stats::runif(P_r, -3, 3), 1)
      }
      b <- as.matrix(b)
      if (!is.numeric(b) || nrow(b) != P_r || ncol(b) != C) {
        stop("'b' must be a numeric matrix of dimension ", P_r, " x ", C, ".",
             call. = FALSE
        )
      }
      names(b) <- create_labels_b(P_r, C)
    }

    ### Omega
    if (is.null(Omega) && !sample) {
      Omega <- NA
    } else {
      if (is.null(Omega)) {
        Omega <- matrix(0, nrow = P_r * P_r, ncol = C)
        for (c in 1:C) {
          Omega[, c] <- as.vector(rwishart(P_r, diag(P_r))$W)
        }
      }
      Omega <- as.matrix(Omega)
      if (!is.numeric(Omega) || nrow(Omega) != P_r * P_r ||
          ncol(Omega) != C) {
        stop(
          "'Omega' must be a numeric matrix of dimension ", P_r * P_r, " x ",
          C, ".",
          call. = FALSE
        )
      }
      for (c in 1:C) {
        if (!is_covariance_matrix(matrix(Omega[, c], nrow = P_r, ncol = P_r))) {
          stop(paste("Column", c, "in 'Omega' builds no covariance matrix."),
               call. = FALSE
          )
        }
      }
      names(Omega) <- create_labels_Omega(P_r, C, cov_sym = TRUE)
    }

    ### z
    if (is.null(z) && !sample) {
      z <- NA
    } else {
      if (is.null(z)) {
        z <- sample(1:C, N, prob = s, replace = TRUE)
      }
      if (length(z) != N || !is.numeric(z) || !all(z %in% 1:C)) {
        stop(
          "'z' must be a numeric vector of length ", N,
          " with elements of value ", paste(seq_len(C), collapse = ", "), ".",
          call. = FALSE
        )
      }
    }

    ### beta
    if (is.null(beta) && !sample) {
      beta <- NA
    } else {
      if (is.null(beta)) {
        beta <- matrix(0, nrow = P_r, ncol = N)
        for (n in seq_len(N)) {
          beta[, n] <- b[, z[n]] +
            t(chol(matrix(Omega[, z[n]], nrow = P_r, ncol = P_r))) %*%
            stats::rnorm(P_r)
        }
      }
      if (!is.numeric(beta) || nrow(beta) != P_r ||
          ncol(beta) != N) {
        stop("'beta' must be a numeric matrix of dimension ", P_r, " x ", N,
             ".", call. = FALSE
        )
      }
    }
  }

  ### Sigma
  if (is.null(Sigma_full) && is.null(Sigma) && !sample) {
    Sigma <- NA
    Sigma_full <- NA
  } else {
    if (ordered) {
      Sigma_full <- NA
      if (is.null(Sigma)) {
        Sigma <- round(runif(1, min = 1, max = 3), 2)
      }
      if (length(Sigma) != 1 || !is.numeric(Sigma) || is.matrix(Sigma)) {
        stop("'Sigma' must be a single numeric value.", call. = FALSE)
      }
    } else {
      if (is.null(Sigma)) {
        if (is.null(Sigma_full)) {
          Sigma_full <- rwishart(J, diag(J))$W
        } else {
          Sigma_full <- as.matrix(Sigma_full)
        }
        Sigma <- delta(J, J) %*% Sigma_full %*% t(delta(J, J))
      } else {
        Sigma <- as.matrix(Sigma)
        Sigma_full <- undiff_Sigma(Sigma, i = J)
      }
      if (!(is_covariance_matrix(Sigma) && nrow(Sigma) == J - 1)) {
        stop("'Sigma' is not a differenced covariance matrix of dimension ",
             J - 1, " x ", J - 1, ".",
             call. = FALSE
        )
      }
      if (!(is_covariance_matrix(Sigma_full) && nrow(Sigma_full) == J)) {
        stop("'Sigma_full' is not a covariance matrix of dimension ", J,
             " x ", J, ".",
             call. = FALSE
        )
      }
      names(Sigma) <- create_labels_Sigma(J, cov_sym = TRUE)
      names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
    }
  }

  ### gamma
  if (ordered) {
    if(is.null(gamma)) {
      gamma <- round(c(0, cumsum(exp(runif(J-2)))), 2)
    }
    if(length(gamma) != J-1 || !is.numeric(gamma)) {
      stop("'gamma' must be a numeric vector of length J-1.", call. = FALSE)
    }
    if(gamma[1] != 0) {
      stop("The first element in 'gamma' must be 0.", call. = FALSE)
    }
    if(!all(diff(gamma) > 0)) {
      stop("The elements in 'gamma' must be strictly increasing.", call. = FALSE)
    }
  } else {
    gamma <- NA
  }

  ### build and return 'RprobitB_parameter'-object
  out <- list(
    "alpha" = alpha,
    "C" = C,
    "s" = s,
    "b" = b,
    "Omega" = Omega,
    "Sigma" = Sigma,
    "Sigma_full" = Sigma_full,
    "beta" = beta,
    "z" = z,
    "gamma" = gamma
  )
  class(out) <- "RprobitB_parameter"
  return(out)
}

#' @noRd
#' @param ...
#' Names of parameters to be printed. If not specified, all parameters are
#' printed.
#' @param digits
#' The number of printed decimal places.
#' @export

print.RprobitB_parameter <- function(x, ..., digits = 4) {
  pars <- list(...)
  ind <- if (length(pars) != 0) {
    sapply(pars, function(par) which(names(x) == par))
  } else {
    seq_along(x)
  }
  for (i in ind) {
    pprint(x[[i]], name = names(x)[i], desc = TRUE)
    cat("\n\n")
  }
  return(invisible(x))
}

