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

undiff_Sigma <- function(Sigma, i, checks = TRUE, pos = TRUE, labels = TRUE) {

  J <- nrow(Sigma) + 1

  if (checks) {
    ### check inputs
    Sigma <- as.matrix(Sigma)
    oeli::assert_covariance_matrix(Sigma)
    checkmate::assert_int(i, lower = 1, upper = J)
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
    if (!oeli::test_covariance_matrix(Sigma_full)) {
      stop("Back-transformed matrix is no covariance matrix.",
           call. = FALSE
      )
    }

    ### check if back-differencing yields differenced matrix
    delta_i <- oeli::delta(ref = i, dim = J)
    Sigma_back <- delta_i %*% Sigma_full %*% t(delta_i)
    if (any(abs(Sigma_back - Sigma) > sqrt(.Machine$double.eps))) {
      stop("Back-differencing failed.",
           call. = FALSE
      )
    }
  }

  ### return undifferenced covariance matrix
  if (labels) {
    names(Sigma_full) <- create_labels_Sigma(J + 1, cov_sym = TRUE)
  }
  return(Sigma_full)
}
