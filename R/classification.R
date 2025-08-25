#' Preference-based classification of deciders
#'
#' @description
#' This function classifies the deciders based on their allocation to the
#' components of the mixing distribution.
#'
#' @details
#' The relative frequencies of which each decider was allocated to the classes
#' during the Gibbs sampling are displayed. Only the thinned samples after the
#' burn-in period are considered.
#'
#' @param x
#' An object of class \code{RprobitB_fit}.
#'
#' @param add_true
#' Set to \code{TRUE} to add true class memberships to output (if available).
#'
#' @return
#' A `data.frame`.
#'
#' The row names are the decider identifiers.
#'
#' The first `C` columns contain the relative frequencies with which the `
#' deciders are allocated to classes. Next, the column `est` contains the
#' estimated class of the decider based on the highest allocation frequency.
#'
#' If `add_true = TRUE`, the next column `true` contains the true class
#' memberships.
#'
#' @seealso
#' [update_z()] for the updating function of the class allocation vector.
#'
#' @export

classification <- function(x, add_true = FALSE) {

  ### check inputs
  if (!inherits(x, "RprobitB_fit")) {
    stop("'x' must be of class 'RprobitB_fit'.", call. = FALSE)
  }
  if (!isTRUE(add_true) && !isFALSE(add_true)) {
    stop("'add_true' must be either TRUE or FALSE.", call. = FALSE)
  }
  if (x$data$P_r == 0) {
    stop("The model has no random coefficients.", call. = FALSE)
  }
  if (isTRUE(add_true) && isFALSE(x$data$simulated)) {
    warning("True class memberships not available.", call. = FALSE)
    add_true <- FALSE
  }

  ### extract information
  samples <- x$gibbs_samples$gibbs_samples_nbt$z
  C <- x$latent_classes$C
  N <- x$data$N
  decider_ids <- unique(x$data$choice_data[[x$data$res_var_names$id]])

  ### create allocation matrix
  allo_tables <- apply(
    X = samples,
    MARGIN = 2,
    FUN = function(x) table(factor(x, levels = seq_len(C))),
    simplify = FALSE
  )
  allo_matrix <- matrix(0, nrow = N, ncol = C)
  for (n in seq_len(N)) for (c in seq_len(C)) {
    allo_matrix[n, c] <- allo_tables[[n]][c]
  }
  allo_matrix <- allo_matrix / rowSums(allo_matrix)
  allo_matrix <- cbind(allo_matrix, apply(allo_matrix, 1, which.max))
  colnames(allo_matrix) <- c(seq_len(C), "est")

  ### add true classes
  if (add_true) {
    allo_matrix <- cbind(allo_matrix, "true" = x$data$true_parameter$z)
  }

  ### return
  allo_matrix <- as.data.frame(allo_matrix)
  row.names(allo_matrix) <- decider_ids
  return(allo_matrix)
}

