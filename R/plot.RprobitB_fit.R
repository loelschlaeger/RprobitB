#' Visualize fitted probit model
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_fit}.
#'
#' @param x
#' An object of class \code{\link{RprobitB_fit}}.
#'
#' @param type
#' The type of plot, which can be one of:
#' \itemize{
#'   \item \code{"mixture"} to visualize the mixing distribution,
#'   \item \code{"acf"} for autocorrelation plots of the Gibbs samples,
#'   \item \code{"trace"} for trace plots of the Gibbs samples,
#'   \item \code{"class_seq"} to visualize the sequence of class numbers.
#' }
#'
#' @param ignore
#' The covariate or parameter names that do not get visualized.
#'
#' @param ...
#' Currently not used.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export

plot.RprobitB_fit <- function(x, type, ignore = NULL, ...) {

  ### check inputs
  if (!inherits(x, "RprobitB_fit")) {
    stop("Not of class 'RprobitB_fit'.")
  }
  if (missing(type) ||
      !(is.character(type) && length(type) == 1) ||
      !type %in% c("mixture", "acf", "trace", "class_seq")) {
    stop("'type' must be one of\n",
         "- 'mixture' (to visualize the mixing distribution)\n",
         "- 'acf' (for autocorrelation plots of the Gibbs samples)\n",
         "- 'trace' (for trace plots of the Gibbs samples)\n",
         "- 'class_seq' (to visualize the sequence of class numbers)",
         call. = FALSE
    )
  }
  if (!type %in% c("mixture", "acf", "trace", "class_seq")) {
    stop("Unknown 'type'.", call. = FALSE)
  }

  ### read ellipsis arguments
  add_par <- list(...)

  ### make plot type 'mixture'
  if (type == "mixture") {
    if (x$data$P_r == 0) {
      stop("Cannot plot a mixing distribution because the model has no random effects.",
           call. = FALSE
      )
    }
    est <- point_estimates(x)
    est_b <- apply(est$b, 2, as.numeric, simplify = F)
    est_Omega <- apply(est$Omega, 2, matrix, nrow = x$data$P_r, simplify = F)
    est_s <- est$s
    cov_names <- x$data$effects[x$data$effects$random == TRUE, "effect"]
    plots <- list()
    for (p1 in 1:x$data$P_r) {
      for (p2 in 1:x$data$P_r) {
        if (any(cov_names[c(p1, p2)] %in% ignore)) next
        plots <- append(plots, list(if (p1 == p2) {
          plot_mixture_marginal(
            mean = lapply(est_b, function(x) x[p1]),
            cov = lapply(est_Omega, function(x) x[p1, p1]),
            weights = est_s,
            name = cov_names[p1]
          )
        } else {
          plot_mixture_contour(
            means = lapply(est_b, function(x) x[c(p1, p2)]),
            covs = lapply(est_Omega, function(x) x[c(p1, p2), c(p1, p2)]),
            weights = est_s,
            names = cov_names[c(p1, p2)]
          )
        }))
      }
    }
    do.call(gridExtra::grid.arrange, c(plots, ncol = floor(sqrt(length(plots)))))
  }

  ### make plot type 'acf' and 'trace'
  if (type == "acf" || type == "trace") {
    if (x$latent_classes$C == 1) ignore <- c(ignore, "s")
    gs <- filter_gibbs_samples(
      x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
      C = x$latent_classes$C, cov_sym = FALSE, drop_par = ignore
    )$gibbs_samples_nbt
    pl <- parameter_labels(
      P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J, C = x$latent_classes$C,
      cov_sym = FALSE, drop_par = ignore
    )
    for (par_name in names(gs)) {
      gibbs_samples <- gs[[par_name, drop = FALSE]]
      par_labels <- paste(par_name, colnames(gibbs_samples), sep = "_")
      ignore_tmp <- par_labels %in% ignore
      gibbs_samples <- gibbs_samples[, !ignore_tmp, drop = FALSE]
      par_labels <- par_labels[!ignore_tmp]
      if (type == "acf") {
        plot_acf(gibbs_samples, par_labels)
      }
      if (type == "trace") {
        plot_trace(gibbs_samples, par_labels)
      }
    }
  }

  ### make plot type 'class_seq'
  if (type == "class_seq") {
    if (x$data$P_r == 0) {
      stop("Cannot show the class sequence because the model has no random effect.",
           call. = FALSE
      )
    }
    plot_class_seq(x[["class_sequence"]], B = x$B)
  }
}
