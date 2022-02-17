#' Plot method for \code{RprobitB_data}
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_data}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param alpha
#' Passed to \link[ggplot2]{geom_density}.
#' @param position
#' Passed to \link[ggplot2]{geom_density}.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom tidyr gather
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes scale_color_hue geom_density facet_wrap labs
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
#' plot(data)

plot.RprobitB_data <- function(x, alpha = 0.9, position = "identity", ...) {
  vis_data <- x$choice_data
  vis_data[unlist(x$res_var_names)] <- NULL
  vis_data <- tidyr::gather(vis_data)
  choices_extended <- rep(
    x$choice_data[[x$res_var_names$choice]],
    (dim(vis_data) / dim(x$choice_data))[1]
  )
  alternatives <- x$alternatives
  ggplot2::ggplot(vis_data, ggplot2::aes(x = .data$value, fill = as.factor(choices_extended))) +
    ggplot2::scale_fill_hue(labels = alternatives) +
    ggplot2::geom_density(alpha = alpha, position = position) +
    ggplot2::facet_wrap(~key, scales = "free") +
    ggplot2::labs(fill = "alternatives") +
    ggplot2::theme_bw()
}

#' Plot method for \code{RprobitB_fit}
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_fit}.
#'
#' @details
#' Some plot types have additional options that can be specified via
#' submitting the following parameters as ellipsis arguments.
#'
#' ## \code{"type = class_allocation"}
#' \itemize{
#'   \item A numeric vector \code{iterations} for plotting the class allocation
#'         at different iterations of the Gibbs sampler.
#'   \item A numeric \code{perc} between 0 and 1 to draw the \code{perc} percentile
#'         ellipsoids for the underlying Gaussian distributions
#'         (\code{perc = 0.95} per default).
#'   \item A numeric \code{sleep}, the number of seconds to pause after plotting.
#'         The default is 1.
#' }
#'
#' @param x
#' An object of class \code{\link{RprobitB_fit}}.
#' @param type
#' The type of plot, which can be one of:
#' \itemize{
#'   \item \code{"effects"} (the default) for visualizing the linear effects.
#'   \item \code{"mixture"} for visualizing the mixture distribution.
#'   \item \code{"acf"} for autocorrelation plots of the Gibbs samples.
#'   \item \code{"trace"} for trace plots of the Gibbs samples.
#'   \item \code{"class_seq"} for visualizing the sequence of class numbers.
#'   \item \code{"class_allocation"} for visualizing the class allocation
#'         (only if \code{P_r = 2}) at the final Gibbs sampler iteration.
#'         See the details section for visualization options.
#' }
#' @param ignore
#' A character (vector) of covariate or parameter names that do not get
#' visualized.
#' @param ...
#' Additional parameters, see the details section.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom graphics par

plot.RprobitB_fit <- function(x, type = "effects", ignore = NULL, ...) {

  ### check inputs
  if (!inherits(x, "RprobitB_fit")) {
    stop("Not of class 'RprobitB_fit'.")
  }
  if (!(is.character(type) && length(type) == 1)) {
    stop("'type' must be a (single) character.")
  }
  if (!type %in% c("effects", "mixture", "acf", "trace", "class_seq", "class_allocation")) {
    stop("Unknown 'type'.")
  }

  ### read ellipsis arguments
  add_par <- list(...)

  ### reset of 'par' settings
  oldpar <- graphics::par(no.readonly = TRUE)
  on.exit(suppressWarnings(graphics::par(oldpar)))

  ### determine 'par_names' and 'linear_coeffs'
  par_names <- c(
    if (x$data$P_f > 0) "alpha",
    if (x$data$P_r > 0) c("s", "b", "Omega"), "Sigma"
  )
  par_names <- setdiff(par_names, ignore)
  linear_coeffs <- x$data$linear_coeffs[!x$data$linear_coeffs$name %in% ignore, ]
  linear_coeffs_fe <- linear_coeffs[linear_coeffs$re == FALSE, ]
  linear_coeffs_re <- linear_coeffs[linear_coeffs$re == TRUE, ]
  linear_coeffs_re_orig <- linear_coeffs_re
  if (x$latent_classes$C > 1) {
    for (i in 1:x$data$P_r) {
      for (c in 1:x$latent_classes$C) {
        linear_coeffs_re[nrow(linear_coeffs_re) + 1, ] <-
          c(paste0(linear_coeffs_re[1, "name"], "_", c), linear_coeffs_re[1, "re"])
      }
      linear_coeffs_re <- linear_coeffs_re[-1, ]
    }
  }
  linear_coeffs <- rbind(linear_coeffs_fe, linear_coeffs_re)

  ### make plot type 'effects'
  if (type == "effects") {
    if (is.null(linear_coeffs$name) || all(!is.element(c("alpha", "b"), par_names))) {
      warning("Type 'effects' invalid because there are no effects.")
    } else {
      graphics::par(
        mfrow = c(1, 1), oma = c(0, 0, 0, 0),
        mar = c(3, 6, 1, 1), mgp = c(2, 1, 0), xpd = FALSE
      )
      plot_effects(
        gibbs_samples = x$gibbs_samples,
        coeff_names = linear_coeffs$name
      )
    }
  }

  ### make plot type 'mixture'
  if (type == "mixture") {
    if (is.null(linear_coeffs_re_orig$name) || !is.element("b", par_names)) {
      warning("Type 'mixture' invalid because there are no random effects.")
    } else {
      est <- point_estimates(x, FUN = mean)
      true <- x$data$true_parameter
      comb <- expand.grid(
        1:length(linear_coeffs_re_orig$name),
        1:length(linear_coeffs_re_orig$name)
      )
      graphics::par(
        mfrow = set_mfrow(nrow(comb)), oma = c(1, 1, 1, 1),
        mar = c(3, 3, 0, 0), mgp = c(2, 1, 0), xpd = NA
      )
      for (i in 1:nrow(comb)) {
        p1 <- comb[i, 1]
        p2 <- comb[i, 2]

        if (p1 == p2) {
          ### marginal plots
          mean_est <- list()
          sd_est <- list()
          weight_est <- est$s
          for (c in 1:x$latent_classes$C) {
            mean_est[[c]] <- est$b[paste0(c, ".", p1)]
            sd_est[[c]] <- sqrt(est$Omega[paste0(c, ".", p1, ",", p1)])
          }
          if (is.null(true)) {
            mean_true <- NULL
            sd_true <- NULL
            weight_true <- TRUE
          } else {
            mean_true <- list()
            sd_true <- list()
            weight_true <- true$s
            for (c in 1:x$data$true_parameter$C) {
              mean_true[[c]] <- est$b[paste0(c, ".", p1)]
              sd_true[[c]] <- sqrt(true$Omega[paste0(c, ".", p1, ",", p1)])
            }
          }
          plot_mixture_marginal(
            mean_est = mean_est,
            mean_true = mean_true,
            weight_est = weight_est,
            weight_true = weight_true,
            sd_est = sd_est,
            sd_true = sd_true,
            cov_name = linear_coeffs_re$name[p1]
          )
        } else {
          ### contour plots
          mean_est <- list()
          cov_est <- list()
          for (c in 1:x$latent_classes$C) {
            mean_est[[c]] <- est$b[paste0(c, ".", c(p1, p2))]
            cov_est[[c]] <- matrix(est$Omega[paste0(
              c, ".",
              as.vector(outer(c(p1, p2), c(p1, p2), paste, sep = ","))
            )], 2, 2)
          }
          if (is.null(true)) {
            beta_true <- NULL
          } else {
            beta_true <- x$data$true_parameter$beta[c(p1, p2), ]
          }
          plot_mixture_contour(
            mean_est = mean_est,
            weight_est = est$s,
            cov_est = cov_est,
            beta_true = beta_true,
            cov_names = linear_coeffs_re_orig$name[c(p1, p2)]
          )
        }
      }
    }
  }

  ### make plot type 'trace'
  if (type == "trace") {
    if (is.null(par_names)) {
      warning("Type 'trace' invalid because there are no parameters.")
    } else {
      gibbs_samples_nbt_filtered <- filter_gibbs_samples(
        x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
        C = x$latent_classes$C, cov_sym = FALSE,
        keep_par = par_names
      )$gibbs_samples_nbt
      graphics::par(
        mfrow = set_mfrow(length(par_names)), oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), xpd = FALSE, las = 1
      )
      for (par_name in par_names) {
        gibbs_samples <- gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
        plot_trace(
          gibbs_samples = gibbs_samples,
          par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
        )
      }
    }
  }

  ### make plot type 'acf'
  if (type == "acf") {
    if (is.null(par_names)) {
      warning("Type 'trace' invalid because there are no parameters.")
    } else {
      ### remove parameters
      if (x$latent_classes$C == 1) {
        keep_par <- setdiff(par_names, "s")
      } else {
        keep_par <- par_names
      }
      gibbs_samples_nbt_filtered <- filter_gibbs_samples(
        x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
        C = x$latent_classes$C, cov_sym = FALSE,
        keep_par = keep_par
      )$gibbs_samples_nbt
      graphics::par(
        mfrow = set_mfrow(sum(sapply(gibbs_samples_nbt_filtered, ncol))),
        oma = c(0, 0, 0, 0), mar = c(3, 3, 1, 1), mgp = c(2, 1, 0), xpd = FALSE,
        las = 1
      )
      for (par_name in keep_par) {
        gibbs_samples <- gibbs_samples_nbt_filtered[[par_name, drop = FALSE]]
        plot_acf(
          gibbs_samples = gibbs_samples,
          par_labels = paste(par_name, colnames(gibbs_samples), sep = "_")
        )
      }
    }
  }

  ### make plot type 'class_seq'
  if (type == "class_seq") {
    plot_class_seq(x[["class_sequence"]])
  }

  ### make plot type 'class_allocation'
  if (type == "class_allocation") {
    if (x[["data"]][["P_r"]] != 2) {
      stop("Plot type 'class_allocation' only available  if P_r = 2.")
    }
    gibbs_samples <- x[["gibbs_samples"]][["gibbs_samples_n"]]
    if(is.null(add_par[["iterations"]])){
      iterations <- x$R
    } else {
      iterations <- unique(add_par[["iterations"]])
      iterations <- iterations[iterations <= x$R]
    }
    if(is.null(add_par[["perc"]])){
      perc <- 0.95
    } else {
      perc <- add_par[["perc"]]
    }
    if(is.null(add_par[["sleep"]])){
      sleep <- 1
    } else {
      sleep <- add_par[["sleep"]]
    }
    for (r in iterations) {
      beta <- gibbs_samples[["beta"]][[r]]
      z <- gibbs_samples[["z"]][r,]
      b <- matrix(gibbs_samples[["b"]][r,], nrow = 2)
      Omega <- matrix(gibbs_samples[["Omega"]][r,], nrow = 4)
      plot_class_allocation(beta, z, b, Omega, r = r, perc = perc, sleep = sleep)
    }
  }
}

#' Autocorrelation plot of Gibbs samples.
#'
#' @description
#' This function plots the autocorrelation of the Gibbs samples, including the
#' total sample size \code{SS}, effective sample size \code{ESS} and the factor
#' \code{SS/ESS}.
#'
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector of length equal to the number of columns of
#' \code{gibbs_samples}, containing labels for the Gibbs samples.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom stats acf
#' @importFrom graphics title legend

plot_acf <- function(gibbs_samples, par_labels) {
  for (c in 1:ncol(gibbs_samples)) {
    ### compute autocorrelation and produce plot
    rho <- stats::acf(gibbs_samples[, c], las = 1, main = "")
    graphics::title(par_labels[c], line = -1)

    ### compute effective sample size
    SS <- length(gibbs_samples[, c])
    ESS <- min(SS / (1 + 2 * sum(rho$acf)), SS)
    graphics::legend("topright",
      x.intersp = -0.5, bg = "white",
      legend = sprintf(
        "%s %.0f", paste0(c("SS", "ESS", "factor"), ":"),
        c(SS, ESS, SS / ESS)
      )
    )
  }
}

#' Visualizing the linear effects.
#'
#' @description
#' This function visualizes the linear effects of the covariates on the choices
#' together with an uncertainty interval of plus / minus one standard deviation.
#'
#' @param gibbs_samples
#' An object of class \code{RprobitB_gibbs_samples}.
#' @param coeff_names
#' A character vector of coefficient names.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom stats sd
#' @importFrom graphics axis segments abline

plot_effects <- function(gibbs_samples, coeff_names) {

  ### extract means and sds
  means <- unlist(RprobitB_gibbs_samples_statistics(gibbs_samples, list(mean))[c("alpha", "b")])
  sds <- unlist(RprobitB_gibbs_samples_statistics(gibbs_samples, list(stats::sd))[c("alpha", "b")])

  ### determine coefficient labels
  labels <- coeff_names

  ### plot means
  xlim <- c(min(c(means - sds), 0), max(c(means + sds), 0))
  plot(
    x = means, y = 1:length(means),
    yaxt = "n", ylab = "", xlab = "", xlim = xlim, main = ""
  )

  ### add uncertainty interval
  graphics::axis(2, at = 1:length(means), labels = labels, las = 1)
  for (n in 1:length(means)) {
    graphics::segments(x0 = means[n] - sds[n], y0 = n, x1 = means[n] + sds[n], y1 = n)
  }

  ### mark zero
  graphics::abline(v = 0, lty = 2)
}

#' Plotting mixing distribution contours.
#'
#' @description
#' This function plots contours of the estimated mixing distributions and adds
#' the true beta values for comparison if available.
#'
#' @param mean_est
#' A list of length \code{C}, where each element is a vector of two
#' estimated class means.
#' @param weight_est
#' A numeric vector of length \code{C} with estimated class weights.
#' @param cov_est
#' A list of length \code{C}, where each element is an estimated class
#' covariance matrix.
#' @param beta_true
#' Either \code{NULL} or a matrix of \code{C} rows with true \code{beta} values.
#' @param cov_names
#' Either \code{NULL} or a vector of two covariate names.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom graphics points contour

plot_mixture_contour <- function(mean_est, weight_est, cov_est, beta_true = NULL,
                                 cov_names = NULL) {

  ### check inputs
  true_avail <- !is.null(beta_true)

  ### extract number of classes
  stopifnot(
    length(mean_est) == length(weight_est),
    length(weight_est) == length(cov_est)
  )
  C_est <- length(mean_est)

  ### specify grid
  xmin <- min(sapply(
    1:C_est,
    function(c) mean_est[[c]][1] - 3 * sqrt(cov_est[[c]][1, 1])
  ))
  xmax <- max(sapply(
    1:C_est,
    function(c) mean_est[[c]][1] + 3 * sqrt(cov_est[[c]][1, 1])
  ))
  ymin <- min(sapply(
    1:C_est,
    function(c) mean_est[[c]][2] - 3 * sqrt(cov_est[[c]][2, 2])
  ))
  ymax <- max(sapply(
    1:C_est,
    function(c) mean_est[[c]][2] + 3 * sqrt(cov_est[[c]][2, 2])
  ))
  grid_x <- seq(xmin, xmax, length.out = 200)
  grid_y <- seq(ymin, ymax, length.out = 200)

  ### compute density of estimated mixture distribution
  prob <- matrix(0, nrow = length(grid_x), ncol = length(grid_y))
  for (i in seq_len(length(grid_x))) {
    for (j in seq_len(length(grid_x))) {
      for (c in 1:C_est) {
        prob[i, j] <- prob[i, j] + weight_est[c] *
          mvtnorm::dmvnorm(
            x = t(matrix(c(grid_x[i], grid_y[j]))),
            mean = mean_est[[c]],
            sigma = cov_est[[c]]
          )
      }
    }
  }

  ### specify limits
  xlim <- c(
    min(grid_x[which(rowSums(prob) > 1e-2)]),
    max(grid_x[which(rowSums(prob) > 1e-2)])
  )
  ylim <- c(
    min(grid_y[which(colSums(prob) > 1e-2)]),
    max(grid_y[which(colSums(prob) > 1e-2)])
  )

  ### initialize plot
  plot(0,
    type = "n", xlim = xlim, ylim = ylim,
    xlab = bquote(paste(beta[.(cov_names[1])])),
    ylab = bquote(paste(beta[.(cov_names[2])])),
    main = ""
  )

  ### add true beta values
  if (true_avail) {
    graphics::points(x = beta_true[1, ], y = beta_true[2, ], pch = 16, col = "black")
  }

  ### add contour
  graphics::contour(add = TRUE, grid_x, grid_y, prob, labcex = 0.75)
}

#' Plotting marginal mixing distributions.
#'
#' @description
#' This function plots the estimated mixing distributions with respect to one
#' covariate and adds the true marginal mixing distribution for comparison if
#' available.
#'
#' @param mean_est
#' A list of length \code{C}, where each element is an estimated class mean.
#' @param mean_true
#' Either \code{NULL} or a list of length \code{C}, where each element is a true
#' class mean.
#' @param weight_est
#' A numeric vector of length \code{C} with estimated class weights.
#' @param weight_true
#' Either \code{NULL} or a numeric vector of length \code{C} with true class
#' weights.
#' @param sd_est
#' A list of length \code{C}, where each element is an estimated class standard
#' deviation.
#' @param sd_true
#' Either \code{NULL} or a list of length \code{C}, where each element is a true
#' class standard deviation.
#' @param cov_name
#' Either \code{NULL} or the name of the corresponding covariate.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom stats dnorm
#' @importFrom graphics title lines

plot_mixture_marginal <- function(mean_est, mean_true = NULL, weight_est,
                                  weight_true = NULL, sd_est, sd_true = NULL,
                                  cov_name = NULL) {

  ### check if true parameters are available
  true_avail <- !(is.null(mean_true) || is.null(weight_true) || is.null(sd_true))

  ### extract number of classes
  stopifnot(
    length(mean_est) == length(weight_est),
    length(weight_est) == length(sd_est)
  )
  C_est <- length(mean_est)
  if (true_avail) {
    stopifnot(
      length(mean_true) == length(weight_true),
      length(weight_true) == length(sd_true)
    )
    C_true <- length(mean_true)
  } else {
    C_true <- 1
  }

  ### specify x-range
  xlim <- c(
    min(
      unlist(mean_est) - 3 * unlist(sd_est),
      unlist(mean_true) - 3 * unlist(sd_true)
    ),
    max(
      unlist(mean_est) + 3 * unlist(sd_est),
      unlist(mean_true) + 3 * unlist(sd_true)
    )
  )
  x <- seq(xlim[1], xlim[2], 0.01)

  ### compute mixture components
  mixture_est <- matrix(NA, nrow = length(x), ncol = C_est)
  for (c in 1:C_est) {
    mixture_est[, c] <- weight_est[c] * stats::dnorm(x,
      mean = mean_est[[c]],
      sd = sd_est[[c]]
    )
  }
  if (true_avail) {
    mixture_true <- matrix(NA, nrow = length(x), ncol = C_true)
    for (c in 1:C_true) {
      mixture_true[, c] <- weight_true[c] * stats::dnorm(x,
        mean = mean_true[[c]],
        sd = sd_true[[c]]
      )
    }
  }

  ### specify y-range
  ylim <- c(0, max(rowSums(mixture_est), if (true_avail) rowSums(mixture_true)))

  ### initialize plot
  plot(0, xlim = xlim, ylim = ylim, type = "n", main = "", xlab = "", ylab = "")
  graphics::title(
    main = "",
    xlab = bquote(paste(beta[.(cov_name)])),
    ylab = ""
  )

  ### add full mixture
  graphics::lines(x, rowSums(mixture_est), col = "black", lty = 1, lwd = 2)
  if (true_avail) {
    graphics::lines(x, rowSums(mixture_true), col = "black", lty = 2, lwd = 2)
  }

  ### add mixture components
  col <- viridis::magma(
    n = max(C_est, C_true), begin = 0.1, end = 0.9,
    alpha = 0.6
  )
  if (C_est > 1) {
    for (c in 1:C_est) {
      lines(x, mixture_est[, c], col = col[c], lty = 1, lwd = 2)
    }
  }
  if (true_avail && C_true > 1) {
    for (c in 1:C_true) {
      lines(x, mixture_true[, c], col = col[c], lty = 2, lwd = 2)
    }
  }
}

#' Visualizing the trace of Gibbs samples.
#'
#' @description
#' This function plots traces of the Gibbs samples.
#'
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector of length equal to the number of columns of
#' \code{gibbs_samples}, containing labels for the Gibbs samples.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom graphics legend axis
#' @importFrom stats plot.ts
#' @importFrom viridis magma

plot_trace <- function(gibbs_samples, par_labels) {

  ### define colors
  col <- viridis::magma(n = ncol(gibbs_samples), begin = 0.1, end = 0.9, alpha = 0.6)

  ### plot trace
  stats::plot.ts(gibbs_samples,
    plot.type = "single",
    ylim = c(min(gibbs_samples), max(gibbs_samples)),
    col = col, xlab = "", ylab = "", xaxt = "n", main = ""
  )

  ### add info
  graphics::axis(side = 1, at = c(1, nrow(gibbs_samples)), labels = c("B+1", "R"))
  graphics::legend("topright", legend = par_labels, lty = 1, col = col, cex = 0.75)
}

#' Visualizing the number of classes during Gibbs sampling
#'
#' @description
#' This function plots the number of latent Glasses during Gibbs sampling
#' to visualize the class updating.
#'
#' @inheritParams RprobitB_fit
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @noRd
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes geom_point labs theme_minimal expand_limits

plot_class_seq <- function(class_sequence) {
  data <- data.frame(i = 1:length(class_sequence), c = class_sequence)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$i, y = .data$c)) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Iteration", y = "Number of classes") +
    ggplot2::theme_minimal() +
    ggplot2::expand_limits(y=0)
  print(plot)
}

#' Plot class allocation (for \code{P_r = 2} only)
#' @description
#' This function plots the allocation of decision-maker specific coefficient vectors
#' \code{beta} given the allocation vector \code{z}, the class means \code{b},
#' and the class covariance matrices \code{Omega}.
#' @details
#' Only in the two-dimensional case, i.e. only if \code{P_r = 2}.
#' @inheritParams RprobitB_parameter
#' @param ...
#' Optional visualization parameters:
#' \itemize{
#'   \item \code{colors}, a character vector of color specifications,
#'   \item \code{perc}, a numeric between 0 and 1 to draw the \code{perc} percentile
#'         ellipsoids for the underlying Gaussian distributions (\code{perc = 0.95} per default),
#'   \item \code{r}, the current iteration number of the Gibbs sampler to be displayed in the legend,
#'   \item \code{sleep}, the number of seconds to pause after plotting.
#' }
#' @return
#' No return value. Draws a plot to the current device.
#' @keywords
#' internal
#' @examples
#' b <- matrix(c(-1,1,1,1), ncol = 2)
#' Omega <- matrix(c(0.8,0.5,0.5,1,0.5,-0.2,-0.2,0.3), ncol = 2)
#' z <- rep(1:2, each = 10)
#' beta <- sapply(z, function(z) rmvnorm(mu = b[,z], Sigma = matrix(Omega[,z], ncol = 2)))
#' RprobitB:::plot_class_allocation(beta = beta, z = z, b = b, Omega = Omega,
#'                                  colors = c("red","blue"), perc = 0.5, r = 1)
#' @importFrom mixtools ellipse
#' @importFrom graphics legend

plot_class_allocation <- function(beta, z, b, Omega, ...) {
  m <- as.vector(table(z))
  graphic_pars <- list(...)
  if(!is.null(graphic_pars[["colors"]])){
    colors <- graphic_pars[["colors"]]
  } else {
    colors <- c('black','forestgreen', 'red2', 'orange', 'cornflowerblue',
                'magenta', 'darkolivegreen4', 'indianred1', 'tan4', 'darkblue',
                'mediumorchid1', 'firebrick4', 'yellowgreen', 'lightsalmon', 'tan3',
                'tan1', 'darkgray', 'wheat4', '#DDAD4B', 'chartreuse',
                'seagreen1', 'moccasin', 'mediumvioletred', 'seagreen','cadetblue1',
                'darkolivegreen1' , 'tan2', 'tomato3', '#7CE3D8', 'gainsboro')
  }
  plot(t(beta), xlab = bquote(beta[1]), ylab = bquote(beta[2]))
  points(t(beta), col = colors[z], pch = 19)
  if(!is.null(graphic_pars[["perc"]])){
    perc <- graphic_pars[["perc"]]
  } else {
    perc <- 0.95
  }
  for(c in 1:length(m)){
    mixtools::ellipse(mu = b[,c], sigma = matrix(Omega[,c], ncol = nrow(Omega)/2),
                      alpha = 1 - perc, npoints = 250, col = colors[c])
  }
  if(!is.null(graphic_pars[["r"]])){
    title = paste("Iteration", graphic_pars[["r"]])
  } else {
    title = NULL
  }
  graphics::legend("topleft", legend = paste0("class ", 1:length(m), " (", round(m / sum(m) * 100), "%)"),
                   pch = 19, col = colors[1:length(m)], title = title)
  if(!is.null(graphic_pars[["sleep"]])){
    Sys.sleep(graphic_pars[["sleep"]])
  }
}