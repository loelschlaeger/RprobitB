#' Create parameters labels
#'
#' @description
#' This function model parameter labels.
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

parameter_labels <- function(P_f, P_r, J, C, cov_sym,
                             keep_par = c("s", "alpha", "b", "Omega", "Sigma"),
                             drop_par = NULL) {

  ### check inputs
  if (P_r > 0) {
    if (!(is.numeric(C) && C %% 1 == 0 && C >= 1)) {
      stop("'C' must be a number greater or equal 1.")
    }
  }

  ### build labels
  labels <- list(
    "s" = create_labels_s(P_r, C),
    "alpha" = create_labels_alpha(P_f),
    "b" = create_labels_b(P_r, C),
    "Omega" = create_labels_Omega(P_r, C, cov_sym),
    "Sigma" = create_labels_Sigma(J, cov_sym)
  )

  ### filter and return labels
  labels <- labels[lengths(labels) != 0 & names(labels) %in% keep_par & !names(labels) %in% drop_par]
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
#' of two alternatives.
#' @inheritParams parameter_labels
#' @return
#' A vector of labels for the model parameter \code{Sigma} of length
#' \code{(J-1)^2} if \code{cov_sym = TRUE} or of length \code{P_r*(P_r+1)/2*C}
#' if \code{cov_sym = FALSE}.
#' @examples
#' RprobitB:::create_labels_Sigma(3, cov_sym = TRUE)
#' RprobitB:::create_labels_Sigma(4, cov_sym = FALSE)
#' @keywords
#' internal

create_labels_Sigma <- function(J, cov_sym) {
  Sigma_id <- rep(TRUE, (J - 1) * (J - 1))
  if (!cov_sym) {
    Sigma_id[-which(lower.tri(matrix(NA, J - 1, J - 1), diag = TRUE) == TRUE)] <- FALSE
  }
  paste0(rep(1:(J - 1), each = J - 1), ",", rep(1:(J - 1), times = J - 1))[Sigma_id]
}
