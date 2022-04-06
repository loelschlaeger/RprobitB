#' Classify deciders based on their preferences
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

preference_classification <- function(x, add_true = FALSE) {

  ### check input
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.", call. = FALSE)
  }
  if (!is.logical(add_true) || length(add_true) != 1) {
    stop("'add_true' must be either TRUE or FALSE.")
  }
  if (x$data$P_r == 0) {
    stop("No classification available, because the model has no random coefficients.", call. = FALSE)
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
