#' Create parameters labels
#'
#' @description
#' This function creates model parameter labels.
#'
#' @param cov_sym
#' Set to \code{TRUE} for labels of symmetric covariance elements.
#'
#' @param keep_par,drop_par
#' A vector of parameter names which are kept or dropped.
#'
#' @inheritParams RprobitB_data
#'
#' @return
#' A list of labels for the selected model parameters.
#'
#' @keywords internal

parameter_labels <- function(
    P_f, P_r, J, C, cov_sym, ordered = FALSE,
    keep_par = c("s", "alpha", "b", "Omega", "Sigma", "d"), drop_par = NULL
  ) {

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

#' @rdname parameter_labels
#' @keywords internal

create_labels_s <- function(P_r, C) {
  if (P_r > 0) {
    as.character(seq_len(C))
  } else {
    NULL
  }
}

#' @rdname parameter_labels
#' @keywords internal

create_labels_alpha <- function(P_f) {
  if (P_f > 0) {
    as.character(seq_len(P_f))
  } else {
    NULL
  }
}

#' @rdname parameter_labels
#' @keywords internal

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

#' @rdname parameter_labels
#' @keywords internal

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

#' @rdname parameter_labels
#' @keywords internal

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
                                                  times = J - 1
    ))[Sigma_id]
  }
}

#' @rdname parameter_labels
#' @keywords internal

create_labels_d <- function(J, ordered) {
  if (ordered) {
    if (J < 3) {
      stop("'J' must be greater or equal 3 in the ordered probit model.",
           call. = FALSE
      )
    }
    as.character(seq_len(J - 2))
  } else {
    NULL
  }
}
