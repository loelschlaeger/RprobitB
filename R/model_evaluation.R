#' Create object of class \code{RprobitB_gibbs_samples_statistics}
#'
#' @description
#' This function creates an object of class
#' \code{RprobitB_gibbs_samples_statistics}.
#'
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}, which generally is located
#' as object \code{gibbs_samples} in an \code{RprobitB_model} object.
#' @param FUN
#' A (preferably named) list of functions that compute parameter statistics
#' from the Gibbs samples, for example
#' \itemize{
#'   \item \code{mean} for the mean,
#'   \item \code{sd} for the standard deviation,
#'   \item \code{min} for the minimum,
#'   \item \code{max} for the maximum,
#'   \item \code{median} for the median,
#'   \item \code{function(x) quantile(x, p)} for the \code{p}th quantile,
#'   \item \code{R_hat} for the Gelman-Rubin statistic.
#' }
#'
#' @return
#' An object of class \code{RprobitB_gibbs_samples_statistics}, which is a list
#' of statistics from \code{gibbs_samples} obtained by applying the elements of
#' \code{FUN}.
#'
#' @keywords
#' internal

RprobitB_gibbs_samples_statistics <- function(
    gibbs_samples, FUN = list("mean" = mean)) {

  ### check inputs
  if (!inherits(gibbs_samples,"RprobitB_gibbs_samples")) {
    stop("'gibbs_samples' must be of class 'RprobitB_gibbs_samples'.",
         call. = FALSE)
  }
  for (i in seq_len(length(FUN))) {
    if (!is.function(FUN[[i]])) {
      stop("Element ", i, " in 'FUN' is not of class 'function'.",
           call. = FALSE)
    }
    if (is.null(names(FUN)[i]) || names(FUN)[i] == "") {
      names(FUN)[i] <- paste0("FUN", i)
    }
  }
  if (any(sapply(FUN, class) != "function")) {
    stop("Not all elements of 'FUN' are functions.",
         call. = FALSE)
  }

  ### build 'RprobitB_gibbs_sample_statistics'
  statistics <- list()
  for (par in names(gibbs_samples[["gibbs_samples_nbt"]])) {
    statistics[[par]] <- matrix(
      NA,
      nrow = ncol(gibbs_samples[["gibbs_samples_nbt"]][[par]]), ncol = 0,
      dimnames = list(colnames(gibbs_samples[["gibbs_samples_nbt"]][[par]]))
    )
    for (i in seq_len(length(FUN))) {
      fun <- FUN[[i]]
      values <- apply(gibbs_samples[["gibbs_samples_nbt"]][[par]], 2, fun,
                      simplify = FALSE
      )
      nvalue <- length(values[[1]])
      labels <- colnames(gibbs_samples[["gibbs_samples_nbt"]][[par]])
      fun_name <- if (nvalue == 1) {
        names(FUN[i])
      } else {
        paste(names(FUN[i]), seq_len(nvalue), sep = "_", recycle0 = TRUE)
      }
      append <- matrix(NA_real_,
                       nrow = length(values), ncol = nvalue,
                       dimnames = list(labels, fun_name)
      )
      for (j in seq_len(length(values))) {
        append[j, ] <- values[[j]]
      }
      statistics[[par]] <- cbind(statistics[[par]], append)
    }
  }

  ### return
  class(statistics) <- "RprobitB_gibbs_samples_statistics"
  return(statistics)
}

#' @param x
#' An object of class \code{RprobitB_gibbs_samples_statistics}.
#' @param true
#' Either \code{NULL} or an object of class \code{RprobitB_parameter}.
#' @inheritParams print.summary.RprobitB_fit
#' @param ...
#' Ignored.
#'
#' @noRd
#'
#' @export
#' @importFrom crayon underline

print.RprobitB_gibbs_samples_statistics <- function(
    x, true = NULL, digits = 2, ...) {

  ### check inputs
  if (!inherits(x,"RprobitB_gibbs_samples_statistics")) {
    stop("'x' must be of class 'RprobitB_gibbs_samples_statistics'.",
         call. = FALSE)
  }
  if (!is.null(true)) {
    if (!inherits(true,"RprobitB_parameter")) {
      stop("'true' must be of class 'RprobitB_parameter'.",
           call. = FALSE)
    }
  }
  if (!(is.numeric(digits) && digits >= 0)) {
    stop("'digits' must a non-negative number.",
         call. = FALSE)
  }

  ### print statistics
  cols <- colnames(x[[1]])
  if (length(cols) > 0) {

    ### determine cell width
    cw <- max(digits + 5, max(nchar(cols)) + 1)

    ### print header of table
    cat(crayon::underline("Gibbs sample statistics\n"))
    header <- sprintf("%6s", " ")
    if (!is.null(true)) {
      header <- paste0(header, sprintf(paste0("%", cw + 1, "s"), "true"))
    }
    for (header_element in cols) {
      header <- paste0(
        header,
        sprintf(paste0("%", cw + 1, "s"), header_element)
      )
    }
    cat(header)

    ### determine order of parameters
    order_of_parameters <- c("alpha", "s", "b", "Omega", "Sigma", "d")

    ### ignore 's' if it is trivial
    if ("s" %in% names(x)) {
      if ((is.null(true) || true$C == 1) && length(x[["s"]] == 1)) {
        x[["s"]] <- NULL
      }
    }

    ### print table elements
    for (par_name in intersect(order_of_parameters, names(x))) {
      out <- x[[par_name]]
      if (!is.null(true)) {
        true_par_name <- true[[par_name]][rownames(out)]
        rownames_true <- names(true_par_name)[!is.na(names(true_par_name))]
        rownames_all <- union(rownames(out), rownames_true)
        out <- cbind(true_par_name, out)
        rownames(out) <- rownames_all
      }
      out <- round(out, digits)
      colnames(out) <- rep(sprintf(paste0("%", cw, "s"), " "), ncol(out))
      rownames(out) <- sprintf("%6s", rownames(out))
      writeLines(paste("\n", par_name))
      print(formatC(out,
                    format = "f", digits = digits, width = cw,
                    flag = ""
      ), quote = FALSE)
    }
  }
  return(invisible(x))
}

#' Filter Gibbs samples
#'
#' @description
#' This is a helper function that filters Gibbs samples.
#'
#' @param x
#' An object of class \code{RprobitB_gibbs_samples}.
#' @inheritParams parameter_labels
#'
#' @return
#' An object of class \code{RprobitB_gibbs_samples} filtered by the labels of
#' \code{\link{parameter_labels}}.
#'
#' @keywords
#' internal

filter_gibbs_samples <- function(
  x, P_f, P_r, J, C, cov_sym, ordered = FALSE,
  keep_par = c("s", "alpha", "b", "Omega", "Sigma", "d"), drop_par = NULL
  ) {
  labels <- parameter_labels(
    P_f, P_r, J, C, cov_sym, ordered, keep_par, drop_par)
  for (gs in names(x)) {
    for (par in names(x[[gs]])) {
      if (!par %in% names(labels)) {
        x[[gs]][[par]] <- NULL
      } else {
        cols <- intersect(colnames(x[[gs]][[par]]), labels[[par]])
        x[[gs]][[par]] <- x[[gs]][[par]][, cols, drop = FALSE]
      }
      x[[gs]] <- x[[gs]][lengths(x[[gs]]) != 0]
    }
  }
  return(x)
}


#' Classify deciders preference-based
#'
#' @description
#' This function classifies the deciders based on their allocation to the
#' components of the mixing distribution.
#'
#' @details
#' The function can only be used if the model has at least one random effect
#' (i.e. \code{P_r >= 1}) and at least two latent classes (i.e. \code{C >= 2}).
#'
#' In that case, let \eqn{z_1,\dots,z_N} denote the class allocations
#' of the \eqn{N} deciders based on their estimated mixed coefficients
#' \eqn{\beta = (\beta_1,\dots,\beta_N)}.
#' Independently for each decider \eqn{n}, the conditional probability
#' \eqn{\Pr(z_n = c \mid s,\beta_n,b,\Omega)} of having \eqn{\beta_n}
#' allocated to class \eqn{c} for \eqn{c=1,\dots,C} depends on the class
#' allocation vector \eqn{s}, the class means \eqn{b=(b_c)_c} and the class
#' covariance matrices \eqn{Omega=(Omega_c)_c} and is proportional to
#' \deqn{s_c \phi(\beta_n \mid b_c,Omega_c).}
#'
#' This function displays the relative frequencies of which each decider
#' was allocated to the classes during the Gibbs sampling. Only the
#' thinned samples after the burn-in period are considered.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param add_true
#' Set to \code{TRUE} to add true class memberships to output (if available).
#'
#' @return
#' A data frame. The row names are the decider ids. The first \code{C} columns
#' contain the relative frequencies with which the deciders are allocated to
#' the \code{C} classes. Next, the column \code{est} contains the estimated
#' class of the decider based on the highest allocation frequency. If
#' \code{add_true}, the next column \code{true} contains the true class
#' memberships.
#'
#' @seealso
#' [update_z()] for the updating function of the class allocation vector.
#'
#' @export

classification <- function(x, add_true = FALSE) {

  ### check input
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.",
         call. = FALSE)
  }
  if (!isTRUE(add_true) && !isFALSE(add_true)) {
    stop("'add_true' must be either TRUE or FALSE.",
         call. = FALSE)
  }
  if (x$data$P_r == 0) {
    stop("The model has no random coefficients.",
         call. = FALSE)
  }

  ### create allocation matrix
  allo_tables <- apply(
    X = x$gibbs_samples$gibbs_samples_nbt$z,
    MARGIN = 2,
    FUN = function(x) table(x),
    simplify = TRUE
  )
  allo_matrix <- matrix(0, nrow = x$data$N, ncol = x$latent_classes$C)
  for (n in 1:x$data$N) {
    for (c in 1:x$latent_classes$C) {
      freq <- allo_tables[[n]][c]
      if (!is.na(freq)) allo_matrix[n, c] <- freq
    }
  }
  allo_matrix <- allo_matrix / rowSums(allo_matrix)
  allo_matrix <- cbind(allo_matrix, apply(allo_matrix, 1, which.max))
  colnames(allo_matrix) <- c(1:x$latent_classes$C, "est")
  if (add_true) {
    allo_matrix <- cbind(allo_matrix, "true" = x$data$true_parameter$z)
  }
  allo_matrix <- as.data.frame(allo_matrix)
  row.names(allo_matrix) <- unique(x$data$choice_data[[x$data$res_var_names$id]])
  return(allo_matrix)
}


#' Predict choices
#'
#' @description
#' This function predicts the discrete choice behavior
#'
#' @details
#' Predictions are made based on the maximum predicted probability for each
#' choice alternative. See the vignette on choice prediction for a demonstration
#' on how to visualize the model's sensitivity and specificity by means of a
#' receiver operating characteristic (ROC) curve.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#' @param data
#' Either
#' \itemize{
#'   \item \code{NULL}, using the data in \code{object},
#'   \item an object of class \code{RprobitB_data}, for example the test part
#'         generated by \code{\link{train_test}},
#'   \item or a data frame of custom choice characteristics. It must have the
#'         same structure as `choice_data` used in \code{\link{prepare_data}}.
#'         Missing columns or \code{NA} values are set to 0.
#' }
#' @param overview
#' If \code{TRUE}, returns a confusion matrix.
#' @param digits
#' The number of digits of the returned choice probabilities. `digits = 2` per
#' default.
#' @param ...
#' Ignored.
#'
#' @return
#' Either a table if \code{overview = TRUE} or a data frame otherwise.
#'
#' @examples
#' data <- simulate_choices(
#'   form = choice ~ cov, N = 10, T = 10, J = 2, seed = 1
#' )
#' data <- train_test(data, test_proportion = 0.5)
#' model <- fit_model(data$train)
#'
#' predict(model)
#' predict(model, overview = FALSE)
#' predict(model, data = data$test)
#' predict(
#'   model,
#'   data = data.frame("cov_A" = c(1,1,NA,NA), "cov_B" = c(1,NA,1,NA)),
#'   overview = FALSE
#' )
#'
#' @export

predict.RprobitB_fit <- function(object, data = NULL, overview = TRUE,
                                 digits = 2, ...) {

  ### choose data
  if (is.null(data)) {
    data <- object$data
  } else if (is.data.frame(data)) {
    cov <- object$data$res_var_names$cov
    data_build <- matrix(NA_real_, nrow = nrow(data), ncol = 1 + length(cov))
    colnames(data_build) <- c("id", cov)
    data_build[,"id"] <- 1:nrow(data)
    for(col in colnames(data)){
      if(col %in% colnames(data_build)){
        data_build[,col] <- data[,col]
      }
    }
    data <- prepare_data(
      form = object$data$form,
      choice_data = as.data.frame(data_build),
      re = object$data$re,
      alternatives = object$data$alternatives,
      id = "id",
      idc = NULL,
      standardize = NULL,
      impute = "zero"
    )
  }
  if (!inherits(data,"RprobitB_data")) {
    stop("'data' is not of class 'RprobitB_data'.",
         call. = FALSE)
  }

  ### compute choice probabilities
  choice_probs <- as.data.frame(choice_probabilities(object, data = data))

  ### round choice probabilities
  choice_probs[data$alternatives] <- round(choice_probs[data$alternatives],
                                           digits = digits)

  ### check if true choices are available
  if (data$choice_available) {
    true_choices <- data$choice_data[[data$res_var_names$choice]]
    true_choices <- factor(true_choices, labels = data$alternatives)
  }

  ### predict
  prediction <- data$alternatives[apply(choice_probs[data$alternatives], 1, which.max)]

  ### create and return output
  if (overview) {
    if (data$choice_available) {
      out <- table(true_choices, prediction, dnn = c("true", "predicted"))
    } else {
      out <- table(prediction, dnn = c("prediction"))
    }
  } else {
    if (data$choice_available) {
      out <- cbind(choice_probs,
                   "true" = true_choices, "predicted" = prediction,
                   "correct" = (true_choices == prediction)
      )
    } else {
      out <- cbind(choice_probs, "prediction" = prediction)
    }
  }
  return(out)
}


#' Compute point estimates
#'
#' @description
#' This function computes the point estimates of an \code{\link{RprobitB_fit}}.
#' Per default, the \code{mean} of the Gibbs samples is used as a point estimate.
#' However, any statistic that computes a single numeric value out of a vector of
#' Gibbs samples can be specified for \code{FUN}.
#'
#' @param x
#' An object of class \code{\link{RprobitB_fit}}.
#' @param FUN
#' A function that computes a single numeric value out of a vector of numeric
#' values.
#'
#' @return
#' An object of class \code{\link{RprobitB_parameter}}.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' model <- fit_model(data)
#' point_estimates(model)
#' point_estimates(model, FUN = median)
#'
#' @export

point_estimates <- function(x, FUN = mean) {

  ### check input
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' is not of class 'RprobitB_fit'.",
         call. = FALSE)
  }
  if (!is.list(FUN)) {
    FUN <- list(FUN)
  }
  if (length(FUN) != 1 || !is.function(FUN[[1]])) {
    stop("'FUN' must be a function.",
         call. = FALSE)
  }

  ### extract meta parameters
  P_f <- x$data$P_f
  P_r <- x$data$P_r
  J <- x$data$J
  C <- x$latent_classes$C
  ordered <- x$data$ordered
  point_estimates <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = x$gibbs_samples, FUN = FUN
  )

  ### compute point estimates
  if (P_f > 0) {
    alpha <- as.numeric(point_estimates$alpha)
  } else {
    alpha <- NULL
  }
  if (P_r > 0) {
    s <- as.numeric(point_estimates$s)[1:C]
    b <- matrix(point_estimates$b, nrow = P_r, ncol = C)
    Omega <- matrix(point_estimates$Omega, nrow = P_r^2, ncol = C)
  } else {
    s <- NULL
    b <- NULL
    Omega <- NULL
  }
  if (ordered) {
    Sigma <- as.numeric(point_estimates$Sigma)
    d <- as.numeric(point_estimates$d)
  } else {
    Sigma <- matrix(point_estimates$Sigma, nrow = J - 1, ncol = J - 1)
    d <- NULL
  }

  ### build an return an object of class 'RprobitB_parameter'
  out <- RprobitB_parameter(
    P_f = P_f, P_r = P_r, J = J, ordered = ordered,
    alpha = alpha, C = C, s = s, b = b, Omega = Omega,
    Sigma = Sigma, d = d, sample = FALSE
  )
  return(out)
}


#' Create parameters labels
#'
#' @description
#' This function creates model parameter labels.
#'
#' @inheritParams RprobitB_data
#' @param cov_sym
#' Set to \code{TRUE} for labels of symmetric covariance elements.
#' @param keep_par
#' A vector of parameter names which are kept.
#' @param drop_par
#' A vector of parameter names which get dropped.
#'
#' @return
#' A list of labels for the selected model parameters.
#'
#' @examples
#' RprobitB:::parameter_labels(P_f = 1, P_r = 2, J = 3, C = 2, cov_sym = TRUE)
#'
#' @keywords
#' internal

parameter_labels <- function(
    P_f, P_r, J, C, cov_sym, ordered = FALSE,
    keep_par = c("s", "alpha", "b", "Omega", "Sigma", "d"), drop_par = NULL) {

  ### check inputs
  if (P_r > 0) {
    if (!(is.numeric(C) && C %% 1 == 0 && C >= 1)) {
      stop("'C' must be a number greater or equal 1.", call. = FALSE)
    }
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean.", call. = FALSE)
  }

  ### build labels
  labels <- list(
    "s" = create_labels_s(P_r, C),
    "alpha" = create_labels_alpha(P_f),
    "b" = create_labels_b(P_r, C),
    "Omega" = create_labels_Omega(P_r, C, cov_sym),
    "Sigma" = create_labels_Sigma(J, cov_sym, ordered),
    "d" = create_labels_d(J, ordered)
  )

  ### filter and return labels
  labels <- labels[lengths(labels) != 0 & names(labels) %in% keep_par &
                     !names(labels) %in% drop_par]
  return(labels)
}

#' Create labels for \code{s}
#' @description
#' This function creates labels for the model parameter \code{s}.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{s} of length \code{C} if
#' \code{P_r > 0} and \code{NULL} otherwise.
#' @examples
#' RprobitB:::create_labels_s(1,3)
#' @keywords
#' internal

create_labels_s <- function(P_r, C) {
  if (P_r > 0) {
    as.character(seq_len(C))
  } else {
    NULL
  }
}

#' Create labels for \code{alpha}
#' @description
#' This function creates labels for the model parameter \code{alpha}.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{alpha} of length \code{P_f}
#' if \code{P_f > 0} and \code{NULL} otherwise.
#' @examples
#' RprobitB:::create_labels_alpha(P_f = 3)
#' @keywords
#' internal

create_labels_alpha <- function(P_f) {
  if (P_f > 0) {
    as.character(seq_len(P_f))
  } else {
    NULL
  }
}

#' Create labels for \code{b}
#' @description
#' This function creates labels for the model parameter \code{b}.
#' @details
#' The labels are of the form \code{"c.p"}, where \code{c} is the latent class
#' number and \code{p} the index of the random coefficient.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{b} of length \code{P_r * C}
#' if \code{P_r > 0} and \code{NULL} otherwise.
#' @examples
#' RprobitB:::create_labels_b(2,3)
#' @keywords
#' internal

create_labels_b <- function(P_r, C) {
  if (P_r > 0) {
    paste0(
      as.character(rep(1:C, each = P_r)), rep(".", P_r * C),
      as.character(rep(1:P_r, times = C))
    )
  } else {
    NULL
  }
}

#' Create labels for \code{Omega}
#' @description
#' This function creates labels for the model parameter \code{Omega}.
#' @details
#' The labels are of the form \code{"c.p1,p2"}, where \code{c} is the latent class
#' number and \code{p1,p2} the indeces of two random coefficients.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{Omega} of length
#' \code{P_r^2 * C} if \code{P_r > 0} and \code{cov_sym = TRUE}
#' or of length \code{P_r*(P_r+1)/2*C} if \code{cov_sym = FALSE} and \code{NULL}
#' otherwise.
#' @examples
#' RprobitB:::create_labels_Omega(2, 3, cov_sym = TRUE)
#' RprobitB:::create_labels_Omega(2, 3, cov_sym = FALSE)
#' @keywords
#' internal

create_labels_Omega <- function(P_r, C, cov_sym) {
  if (P_r > 0) {
    Omega_id <- rep(TRUE, P_r * P_r)
    if (!cov_sym) {
      Omega_id[-which(lower.tri(matrix(NA, P_r, P_r), diag = TRUE) == TRUE)] <- FALSE
    }
    Omega_id <- rep(Omega_id, C)
    paste0(
      as.character(rep(1:C, each = P_r^2)), rep(".", P_r * C),
      as.character(rep(paste0(
        rep(1:P_r, each = P_r), ",",
        rep(1:P_r, times = P_r)
      ), times = C))
    )[Omega_id]
  } else {
    NULL
  }
}

#' Create labels for \code{Sigma}
#' @description
#' This function creates labels for the model parameter \code{Sigma}.
#' @details
#' The labels are of the form \code{"j1,j2"}, where \code{j1,j2} are indices
#' of the two alternatives \code{j1} and \code{j2}.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{Sigma} of length
#' \code{(J-1)^2} if \code{cov_sym = TRUE} or of length \code{J*(J-1)/2}
#' if \code{cov_sym = FALSE}.
#' If \code{ordered = TRUE}, \code{Sigma} has only one element.
#' @examples
#' RprobitB:::create_labels_Sigma(3, cov_sym = TRUE)
#' RprobitB:::create_labels_Sigma(4, cov_sym = FALSE)
#' RprobitB:::create_labels_Sigma(4, ordered = TRUE)
#' @keywords
#' internal

create_labels_Sigma <- function(J, cov_sym, ordered = FALSE) {
  if (ordered) {
    "1,1"
  } else {
    Sigma_id <- rep(TRUE, (J - 1) * (J - 1))
    if (!cov_sym) {
      ids <- which(lower.tri(matrix(NA, J - 1, J - 1), diag = TRUE) == TRUE)
      Sigma_id[-ids] <- FALSE
    }
    paste0(rep(1:(J - 1), each = J - 1), ",", rep(1:(J - 1),
                                                  times = J - 1))[Sigma_id]
  }
}

#' Create labels for \code{d}
#' @description
#' This function creates labels for the model parameter \code{d}.
#' @details
#' Note that \code{J} must be greater or equal \code{3} in the ordered probit
#' model.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{d} of length \code{J - 2} if
#' \code{ordered = TRUE} and \code{NULL} otherwise.
#' @examples
#' RprobitB:::create_labels_d(5, TRUE)
#' @keywords
#' internal

create_labels_d <- function(J, ordered) {
  if (ordered) {
    if (J < 3) {
      stop("'J' must be greater or equal 3 in the ordered probit model.",
           call. = FALSE)
    }
    as.character(seq_len(J-2))
  } else {
    NULL
  }
}

#' Extract model effects
#'
#' @description
#' This function extracts the estimated model effects.
#'
#' @return
#' An object of class \code{RprobitB_coef}.
#'
#' @param object
#' An object of class \code{RprobitB_fit}.
#' @param ...
#' Ignored.
#'
#' @export
#' @importFrom stats sd

coef.RprobitB_fit <- function(object, ...) {

  ### compute Gibbs samples statistics
  C <- object$latent_classes$C
  statistics <- RprobitB_gibbs_samples_statistics(
    gibbs_samples = filter_gibbs_samples(
      x = object$gibbs_samples,
      P_f = object$data$P_f,
      P_r = object$data$P_r,
      J = object$data$J,
      C = C,
      cov_sym = FALSE,
    ),
    FUN = c("mean" = mean, "sd" = stats::sd)
  )

  ### allocate space for output
  coef <- matrix(NA_real_, nrow = 0, ncol = 4)
  coef_name <- c()
  coef_class <- c()

  ### create entries for fixed-effect coefficients
  fixed_coefs <- object$data$effects[object$data$effects$random == FALSE, ]
  for (row in seq_len(nrow(fixed_coefs))) {
    coef <- rbind(coef, c(statistics$alpha[row, 1:2], NA_real_, NA_real_))
    coef_name <- c(coef_name, fixed_coefs[row, "effect"])
    coef_class <- c(coef_class, NA_real_)
  }

  ### create entries for random-effect coefficients
  random_coefs <- object$data$effects[object$data$effects$random == TRUE, ]
  for (row in seq_len(nrow(random_coefs))) {
    mean <- statistics$b[paste0(1:C, ".", row), 1]
    mean_sd <- statistics$b[paste0(1:C, ".", row), 2]
    var <- statistics$Omega[paste0(1:C, ".", row, ",", row), 1]
    var_sd <- statistics$Omega[paste0(1:C, ".", row, ",", row), 2]
    coef <- rbind(coef, cbind(mean, mean_sd, var, var_sd))
    coef_name <- c(coef_name, rep(random_coefs[row, "effect"], C))
    coef_class <- c(coef_class, 1:C)
  }

  ### create output
  rownames(coef) <- coef_name
  colnames(coef) <- c("mean", "mean_sd", "var", "var_sd")
  attr(coef, "coef_class") <- coef_class
  attr(coef, "s") <- statistics[["s"]]
  attr(coef, "C") <- C
  class(coef) <- "RprobitB_coef"
  return(coef)
}

#' @noRd
#' @export

print.RprobitB_coef <- function(x, ...) {
  classes <- sapply(
    attr(x, "coef_class"),
    function(cl) {
      if (is.na(cl) || attr(x, "C") == 1) {
        ""
      } else {
        paste0("[", cl, "]")
      }
    }
  )
  out <- data.frame(
    sprintf("%s %s", rownames(x), classes),
    sprintf("%.2f", x[, "mean"]),
    sprintf("(%.2f)", x[, "mean_sd"]),
    sprintf("%.2f", x[, "var"]),
    sprintf("(%.2f)", x[, "var_sd"])
  )
  colnames(out) <- c(" ", "Estimate", "(sd)", "Variance", "(sd)")
  if (all(is.na(x[, c("var", "var_sd")]))) {
    out <- out[, 1:3]
  }
  print(out)
}

#' @param sd
#' The number of standard deviations to display.
#' @param het
#' Set to \code{FALSE} to show the standard deviation of the estimate.
#' Set to \code{TRUE} to show the standard deviation of the mixing distribution.
#' @noRd
#' @export
#' @importFrom ggplot2 aes ggplot geom_vline geom_point geom_errorbar
#' position_dodge theme_minimal labs
#' @importFrom rlang .data

plot.RprobitB_coef <- function(x, sd = 1, het = FALSE, ...) {
  s <- attr(x, "s")
  x <- data.frame(
    "name" = rownames(x),
    "cl" = attr(x, "coef_class"),
    unclass(x)
  )
  mapping <- if (all(is.na(x$cl))) {
    ggplot2::aes(x = .data$mean, y = .data$name)
  } else {
    ggplot2::aes(x = .data$mean, y = .data$name, color = factor(.data$cl))
  }
  p <- ggplot2::ggplot(data = x, mapping = mapping) +
    ggplot2::geom_vline(aes(xintercept = 0), linetype = 2) +
    ggplot2::geom_point(
      size = 2,
      position = ggplot2::position_dodge(width = -0.3)
    ) +
    ggplot2::geom_errorbar(ggplot2::aes(
      xmin = .data$mean - sd * .data[[if (het) "var" else "mean_sd"]],
      xmax = .data$mean + sd * .data[[if (het) "var" else "mean_sd"]],
      width = 0
    ),
    position = ggplot2::position_dodge(width = -0.3)
    ) +
    ggplot2::theme_minimal() +
    ggplot2::labs(
      x = "",
      y = "",
      title = "Average effects",
      subtitle = paste(
        "The horizontal lines show \u00B1", sd,
        "standard deviation of the",
        ifelse(het, "mixing distribution", "estimate")
      ),
      color = "Class"
    )

  ### add class proportions
  if (!all(is.na(x$cl))) {
    p <- p + ggplot2::scale_color_discrete(
      labels = sprintf("%s (%.2f%%)", 1:nrow(s), s[, "mean"])
    )
  }

  suppressWarnings(print(p))
}

#' Extract estimated covariance matrix of mixing distribution
#'
#' @description
#' This convenience function returns the estimated covariance matrix of the
#' mixing distribution.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param cor
#' If \code{TRUE}, returns the correlation matrix instead.
#'
#' @return
#' The estimated covariance matrix of the mixing distribution. In case of
#' multiple classes, a list of matrices for each class.
#'
#' @export
#'
#' @importFrom stats cov2cor

cov_mix <- function(x, cor = FALSE) {
  if (x$data$P_r == 0) {
    stop("No random effects.", call. = FALSE)
  }
  est_Omega <- point_estimates(x)$Omega
  random <- NULL
  cov_names <- subset(x$data$effects, random == TRUE)$effect
  out <- list()
  for (c in 1:x$latent_classes$C) {
    out[[c]] <- matrix(est_Omega[, c], nrow = x$data$P_r)
    colnames(out[[c]]) <- rownames(out[[c]]) <- cov_names
  }
  if (cor) {
    out <- lapply(out, stats::cov2cor)
  }
  if (x$latent_classes$C == 1) {
    return(out[[1]])
  } else {
    return(out)
  }
}


#' Compute choice probabilities
#'
#' @description
#' This function returns the choice probabilities of an \code{RprobitB_fit}
#' object.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#' @param data
#' Either \code{NULL} or an object of class \code{RprobitB_data}. In the former
#' case, choice probabilities are computed for the data that was used for model
#' fitting. Alternatively, a new data set can be provided.
#' @param par_set
#' Specifying the parameter set for calculation and either
#' \itemize{
#'   \item a function that computes a posterior point estimate (the default is
#'         \code{mean()}),
#'   \item \code{"true"} to select the true parameter set,
#'   \item an object of class \code{RprobitB_parameter}.
#' }
#'
#' @return
#' A data frame of choice probabilities with choice situations in rows and
#' alternatives in columns. The first two columns are the decider identifier
#' \code{"id"} and the choice situation identifier \code{"idc"}.
#'
#' @examples
#' data <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#' x <- fit_model(data)
#' choice_probabilities(x)
#'
#' @export

choice_probabilities <- function(x, data = NULL, par_set = mean) {

  ### specify parameter set
  if (is.function(par_set)){
    parameter <- point_estimates(x, FUN = par_set)
  } else if (identical(par_set, "true")) {
    parameter <- x$data$true_parameter
    if (is.null(parameter)) {
      stop("True parameters are not available.",
           call. = FALSE)
    }
  } else if (inherits(par_set,"RprobitB_parameter")) {
    parameter <- par_set
  } else {
    stop(
      paste("'par_set' must be either a function, 'true' or an",
            "'RprobitB_parameter' object."),
      call. = FALSE
      )
  }

  ### choose data
  if (is.null(data)) {
    data <- x$data
  }
  if (!inherits(data, "RprobitB_data")) {
    stop("'data' is not of class 'RprobitB_data'.",
         call. = FALSE)
  }

  ### define progress bar
  pb <- RprobitB_pb(title = "Computing choice probabilities",
                    total = data$N,
                    tail = "deciders")

  ### compute probabilities
  probabilities <- matrix(NA_real_, nrow = 0, ncol = data$J)
  for (n in 1:data$N) {
    RprobitB_pb_tick(pb)
    for (t in 1:data$T[n]) {
      P_nt <- compute_choice_probabilities(
        X = data$data[[n]]$X[[t]],
        alternatives = 1:data$J,
        parameter = parameter,
        ordered = x$data$ordered
      )
      probabilities <- rbind(probabilities, P_nt)
    }
  }
  probabilities <- as.data.frame(probabilities)

  ### add decision maker ids
  probabilities <- cbind(data$choice_data[[data$res_var_names[["id"]]]],
                         data$choice_data[[data$res_var_names[["idc"]]]],
                         probabilities)
  colnames(probabilities) <- c(x$data$res_var_names[["id"]],
                               x$data$res_var_names[["idc"]],
                               data$alternatives)
  rownames(probabilities) <- NULL

  ### return probabilities
  out <- as.data.frame(probabilities)
  return(out)
}

#' Compute probit choice probabilities
#'
#' @description
#' This is a helper function for \code{\link{choice_probabilities}} and computes
#' the probit choice probabilities for a single choice situation with \code{J}
#' alternatives.
#'
#' @param X
#' A matrix of covariates with \code{J} rows and \code{P_f + P_r} columns, where
#' the first \code{P_f} columns are connected to fixed coefficients and the last
#' \code{P_r} columns are connected to random coefficients.
#' @param alternatives
#' A vector with unique integers from \code{1} to \code{J}, indicating the
#' alternatives for which choice probabilities are to be computed.
#' @param parameter
#' An object of class \code{RprobitB_parameter}.
#' @inheritParams RprobitB_data
#'
#' @return
#' A probability vector of length \code{length(alternatives)}.
#'
#' @keywords
#' internal
#'
#' @importFrom stats pnorm

compute_choice_probabilities <- function(
    X, alternatives, parameter, ordered = FALSE
    ) {

  ### unpack and check inputs
  if (!inherits(parameter, "RprobitB_parameter")) {
    stop("'parameter' is not of class 'RprobitB_parameter.",
         call. = FALSE)
  }
  alpha <- parameter$alpha
  s <- ifelse(is.na(parameter$s), 1, parameter$s)
  b <- parameter$b
  Omega <- parameter$Omega
  P_f <- ifelse(anyNA(alpha), 0, length(alpha))
  P_r <- ifelse(anyNA(parameter$s), 0, nrow(parameter$b))
  if (ordered) {
    Sigma <- parameter$Sigma
    d <- parameter$d
    gamma <- as.vector(d_to_gamma(d))
    J <- length(d) + 2
  } else {
    Sigma_full <- parameter$Sigma_full
    J <- nrow(Sigma_full)
  }

  ### check inputs
  if (!(is.numeric(alternatives) &&
        identical(alternatives, unique(alternatives)) &&
        length(setdiff(alternatives, 1:J)) == 0)) {
    stop("'alternatives' must be a vector with unique integers from 1 to 'J'.",
         call. = FALSE)
  }
  if (P_f > 0 || P_r > 0) {
    if (!is.matrix(X)) {
      stop("'X' must be a matrix.",
           call. = FALSE)
    }
    if (ncol(X) != (P_f + P_r)) {
      stop("'X' must have 'P_f'+'P_r' columns.",
           call. = FALSE)
    }
    if (!ordered && nrow(X) != J) {
      stop("'X' must have 'J' columns.",
           call. = FALSE)
    }
  }

  ### compute choice probabilities
  probabilities <- rep(NA_real_, J)
  for (j in alternatives) {
    if (ordered) {
      ub <- gamma[j+1]
      lb <- gamma[j]
      if (P_f > 0) {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                mu <- X %*% c(alpha, b[, c])
                sd <- sqrt(X[, -(1:P_f)] %*% matrix(Omega[, c], P_r, P_r) %*%
                  t(X[, -(1:P_f)]) + Sigma)
                s[c] * (stats::pnorm(q = ub - mu, mean = 0, sd = sd) -
                          stats::pnorm(q = lb - mu, mean = 0, sd = sd))
              }
            )
          )
        } else {
          mu <- X %*% alpha
          sd <- sqrt(Sigma)
          probabilities[j] <- pnorm(q = ub - mu, mean = 0, sd = sd) -
            pnorm(q = lb - mu, mean = 0, sd = sd)
        }
      } else {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                mu <- X %*% b[, c]
                sd <- sqrt(X %*% matrix(Omega[, c], P_r, P_r) %*%
                             t(X) + Sigma)
                s[c] * (pnorm(q = ub - mu, mean = 0, sd = sd) -
                          pnorm(q = lb - mu, mean = 0, sd = sd))
              }
            )
          )
        } else {
          mu <- 0
          sd <- sqrt(Sigma)
          probabilities[j] <- pnorm(q = ub - mu, mean = 0, sd = sd) -
            pnorm(q = lb - mu, mean = 0, sd = sd)
        }
      }
    } else {
      if (P_f > 0) {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                s[c] * mvtnorm::pmvnorm(
                  lower = rep(-Inf, J - 1),
                  upper = as.vector(-delta(J, j) %*% X %*% c(alpha, b[, c])),
                  mean = rep(0, J - 1),
                  sigma = delta(J, j) %*%
                    (X[, -(1:P_f)] %*% matrix(Omega[, c], P_r, P_r) %*%
                       t(X[, -(1:P_f)]) + Sigma_full) %*% t(delta(J, j))
                  )
              }
            )
          )
        } else {
          probabilities[j] <- mvtnorm::pmvnorm(
            lower = rep(-Inf, J - 1),
            upper = as.vector(-delta(J, j) %*% X %*% alpha),
            mean = rep(0, J - 1),
            sigma = delta(J, j) %*% Sigma_full %*% t(delta(J, j))
          )[1]
        }
      } else {
        if (P_r > 0) {
          probabilities[j] <- sum(
            sapply(
              X = seq_along(s),
              FUN = function(c) {
                s[c] * mvtnorm::pmvnorm(
                  lower = rep(-Inf, J - 1),
                  upper = as.vector(-delta(J, j) %*% X %*% b[, c]),
                  mean = rep(0, J - 1),
                  sigma = delta(J, j) %*%
                    (X %*% matrix(Omega[, c], P_r, P_r) %*% t(X) + Sigma_full) %*%
                    t(delta(J, j))
                  )
              }
            )
          )
        } else {
          probabilities[j] <- mvtnorm::pmvnorm(
            lower = rep(-Inf, J - 1),
            upper = rep(0, J - 1),
            mean = rep(0, J - 1),
            sigma = delta(J, j) %*% Sigma_full %*% t(delta(J, j))
          )[1]
        }
      }
    }
  }

  ### return probabilities
  return(probabilities)
}

#' Extract covariates of choice occasion
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
#'
#' @return
#' A subset of the `choice_data` data frame specified in `prepare_data()`.
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
      stop("Requested choice occasion not found.",
           call. = FALSE)
    }
    return(out)
  } else {
    stop("'x' must be either an 'RprobitB_fit' or 'RprobitB_data' object.",
         call. = FALSE)
  }
}
