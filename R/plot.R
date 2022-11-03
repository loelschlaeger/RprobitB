#' Visualize choice data
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
#'  true_parameter = list("alpha" = -1)
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

  ### keep order of alternatives in case of the ordered probit model
  if (x$ordered) {
    data_red[[x$res_var_names$choice]] <- factor(
      data_red[[x$res_var_names$choice]],
      levels = x$alternatives
    )
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


#' Visualize fitted probit model
#'
#' @description
#' This function is the plot method for an object of class \code{RprobitB_fit}.
#'
#' @param x
#' An object of class \code{\link{RprobitB_fit}}.
#' @param type
#' The type of plot, which can be one of:
#' \itemize{
#'   \item \code{"mixture"} to visualize the mixing distribution,
#'   \item \code{"acf"} for autocorrelation plots of the Gibbs samples,
#'   \item \code{"trace"} for trace plots of the Gibbs samples,
#'   \item \code{"class_seq"} to visualize the sequence of class numbers.
#' }
#' See the details section for visualization options.
#' @param ignore
#' A character (vector) of covariate or parameter names that do not get
#' visualized.
#' @param ...
#' Ignored.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom graphics par

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
         call. = FALSE)
  }
  if (!type %in% c("mixture", "acf", "trace", "class_seq")) {
    stop("Unknown 'type'.", call. = FALSE)
  }

  ### read ellipsis arguments
  add_par <- list(...)

  ### make plot type 'mixture'
  if (type == "mixture") {
    if(x$data$P_r == 0){
      stop("Cannot plot a mixing distribution because the model has no random effects.",
           call. = FALSE)
    }
    est <- point_estimates(x)
    est_b <- apply(est$b, 2, as.numeric, simplify =  F)
    est_Omega <- apply(est$Omega, 2, matrix, nrow = x$data$P_r, simplify = F)
    est_s <- est$s
    cov_names <- x$data$effects[x$data$effects$random == TRUE, "effect"]
    plots <- list()
    for(p1 in 1:x$data$P_r) for(p2 in 1:x$data$P_r) {
      if(any(cov_names[c(p1,p2)] %in% ignore)) next
      plots <- append(plots, list(if(p1 == p2){
        plot_mixture_marginal(
          mean = lapply(est_b, function(x) x[p1]),
          cov = lapply(est_Omega, function(x) x[p1,p1]),
          weights = est_s,
          name = cov_names[p1])
      } else {
        plot_mixture_contour(
          means = lapply(est_b, function(x) x[c(p1,p2)]),
          covs = lapply(est_Omega, function(x) x[c(p1,p2),c(p1,p2)]),
          weights = est_s,
          names = cov_names[c(p1,p2)])
      }))
    }
    do.call(gridExtra::grid.arrange, c(plots, ncol = floor(sqrt(length(plots)))))
  }

  ### make plot type 'acf' and 'trace'
  if (type == "acf" || type == "trace") {
    if (x$latent_classes$C == 1) ignore <- c(ignore, "s")
    gs <- filter_gibbs_samples(
      x = x$gibbs_samples, P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J,
      C = x$latent_classes$C, cov_sym = FALSE, drop_par = ignore)$gibbs_samples_nbt
    pl <- parameter_labels(
      P_f = x$data$P_f, P_r = x$data$P_r, J = x$data$J, C = x$latent_classes$C,
      cov_sym = FALSE, drop_par = ignore)
    for (par_name in names(gs)) {
      gibbs_samples <- gs[[par_name, drop = FALSE]]
      par_labels <- paste(par_name, colnames(gibbs_samples), sep = "_")
      ignore_tmp <- par_labels %in% ignore
      gibbs_samples <- gibbs_samples[,!ignore_tmp,drop=FALSE]
      par_labels <- par_labels[!ignore_tmp]
      if(type == "acf") {
        plot_acf(gibbs_samples, par_labels)
      }
      if(type == "trace") {
        plot_trace(gibbs_samples, par_labels)
      }
    }
  }

  ### make plot type 'class_seq'
  if (type == "class_seq") {
    if(x$data$P_r == 0){
      stop("Cannot show the class sequence because the model has no random effect.",
           call. = FALSE)
    }
    plot_class_seq(x[["class_sequence"]], B = x$B)
  }
}

#' Autocorrelation plot of Gibbs samples
#'
#' @description
#' This function plots the autocorrelation of the Gibbs samples. The plots
#' include the total Gibbs sample size \code{TSS} and the effective sample size
#' \code{ESS}, see the details.
#'
#' @details
#' The effective sample size is the value
#' \deqn{TSS / \sqrt{1 + 2\sum_{k\geq 1} \rho_k}},
#' where \eqn{\rho_k} is the auto correlation between the chain offset by
#' \eqn{k} positions. The auto correlations are estimated via
#' \code{\link[stats]{spec.ar}}.
#'
#' @param gibbs_samples
#' A matrix of Gibbs samples.
#' @param par_labels
#' A character vector with labels for the Gibbs samples, of length equal to the
#' number of columns of \code{gibbs_samples}.
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @examples
#' gibbs_samples <- matrix(arima.sim(list(order=c(1,0,0), ar = 0.5), n = 100))
#' RprobitB:::plot_acf(gibbs_samples, par_labels = "simulated AR(1) process")
#'
#' @importFrom stats spec.ar
#' @importFrom graphics title legend

plot_acf <- function(gibbs_samples, par_labels) {
  for (c in 1:ncol(gibbs_samples)) {
    x <- gibbs_samples[, c]
    sum_rho <- (stats::spec.ar(x, plot = F)$spec[1]/var(x) - 1) / 2
    stats::acf(x, las = 1, main = "")
    graphics::title(par_labels[c], line = 1)
    TSS <- length(x)
    ESS <- min(TSS / (1 + 2 * sum_rho), TSS)
    graphics::legend("topright", x.intersp = -0.5, bg = "white",
      legend = sprintf("%s %.0f", paste0(c("TSS", "ESS"), ":"), c(TSS, ESS))
    )
  }
}

#' Plot marginal mixing distributions
#'
#' @description
#' This function plots an estimated marginal mixing distributions.
#'
#' @param mean
#' The class means.
#' @param cov
#' The class covariances.
#' @param weights
#' The class weights.
#' @param name
#' The covariate name.
#' @return
#' An object of class \code{ggplot}.
#'
#' @keywords
#' internal
#'
#' @examples
#' mean <- list(1,2)
#' cov <- list(0.1,1)
#' weights <- c(0.3,0.7)
#' name <- "test"
#' RprobitB:::plot_mixture_marginal(mean, cov, weights, name)
#'
#' @importFrom ggplot2 ggplot aes geom_line labs geom_text
#' @importFrom stats dnorm

plot_mixture_marginal <- function(mean, cov, weights, name) {
  C <- length(weights)
  x_min <- min(mapply(function(x,y) x-3*y, mean, cov))
  x_max <- max(mapply(function(x,y) x+3*y, mean, cov))
  x <- seq(x_min, x_max, length.out = 200)
  y <- Reduce("+", sapply(1:C, function(c) weights[c] *
                            stats::dnorm(x, mean[[c]], sd = cov[[c]]),
                          simplify = FALSE))

  xint <- grp <- NULL
  out <- ggplot2::ggplot(data = data.frame(x = x, y = y), ggplot2::aes(x, y)) +
    ggplot2::geom_line() +
    ggplot2::labs(x = bquote(beta[.(name)]), y = "")

  if(C > 1) {
    class_means <- data.frame(xint = unlist(mean), grp = factor(1:C))
    out <- out +
      ggplot2::geom_text(
        data = class_means,
        mapping = ggplot2::aes(x = xint, y = 0, label = grp,  color = grp),
        size = 5,
        show.legend = FALSE)
  }

  return(out)
}

#' Plot bivariate contour of mixing distributions
#'
#' @description
#' This function plots an estimated bivariate contour mixing distributions.
#'
#' @param means
#' The class means.
#' @param covs
#' The class covariances.
#' @param weights
#' The class weights.
#' @param names
#' The covariate names.
#' @return
#' An object of class \code{ggplot}.
#'
#' @keywords
#' internal
#'
#' @examples
#' means <- list(c(0,0),c(1,1))
#' covs <- list(diag(2),0.5*diag(2))
#' weights <- c(0.3,0.7)
#' names <- c("A","B")
#' RprobitB:::plot_mixture_contour(means, covs, weights, names)
#'
#' @importFrom ggplot2 ggplot aes geom_contour labs geom_text
#' @importFrom rlang .data

plot_mixture_contour <- function(means, covs, weights, names) {
  C <- length(weights)
  x_min <- min(mapply(function(x,y) x[1] - 5 * y[1,1], means, covs))
  x_max <- max(mapply(function(x,y) x[1] + 5 * y[1,1], means, covs))
  y_min <- min(mapply(function(x,y) x[2] - 5 * y[2,2], means, covs))
  y_max <- max(mapply(function(x,y) x[2] + 5 * y[2,2], means, covs))
  data.grid <- expand.grid(x = seq(x_min, x_max, length.out = 200),
                           y = seq(y_min, y_max, length.out = 200))
  z <- Reduce("+", sapply(1:C, function(c)
    mvtnorm::dmvnorm(data.grid, means[[c]], covs[[c]]), simplify = FALSE))

  x <- y <- grp <- NULL
  out <- ggplot2::ggplot(data = cbind(data.grid, z),
                  ggplot2::aes(x = .data$x, y = .data$y, z = .data$z)) +
    ggplot2::geom_contour() +
    ggplot2::labs(x = bquote(beta[.(names[1])]), y = bquote(beta[.(names[2])]))

  if(C > 1) {
    class_means <- data.frame(
      x = sapply(means, "[[", 1), y = sapply(means, "[[", 2), z = 0,
      grp = factor(1:C))
    out <- out +
      ggplot2::geom_text(
        data = class_means,
        mapping = ggplot2::aes(x = x, y = y, label = grp, color = grp),
        size = 5,
        show.legend = FALSE)
  }

  return(out)
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
    col = col, xlab = "Iteration", ylab = "", xaxt = "n", main = "", las = 1
  )

  ### add info
  graphics::axis(side = 1, at = c(1, nrow(gibbs_samples)),
                 labels = c("B+1", "R"))
  graphics::legend("topright", legend = par_labels, lty = 1, col = col,
                   cex = 0.75, bg = "white")
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

plot_class_seq <- function(class_sequence, B) {
  data <- data.frame(i = 1:length(class_sequence), c = class_sequence)
  plot <- ggplot2::ggplot(data, ggplot2::aes(x = .data$i, y = .data$c)) +
    ggplot2::geom_line() +
    ggplot2::labs(title = "Number of classes during Gibbs sampling",
                  subtitle = "The grey area shows the updating phase",
                  x = "Iteration",
                  y = "") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_continuous() +
    ggplot2::scale_y_continuous(
      breaks = 1:max(class_sequence),
      labels = as.character(1:max(class_sequence)),
      minor_breaks = NULL) +
    ggplot2::expand_limits(y = 1) +
    ggplot2::annotate(geom = "rect",
                      xmin = 0, xmax = B, ymin = -Inf, ymax = Inf,
                      fill = "grey", alpha = 0.2)
  print(plot)
}

#' Plot class allocation (for \code{P_r = 2} only)
#'
#' @description
#' This function plots the allocation of decision-maker specific coefficient vectors
#' \code{beta} given the allocation vector \code{z}, the class means \code{b},
#' and the class covariance matrices \code{Omega}.
#'
#' @details
#' Only applicable in the two-dimensional case, i.e. only if \code{P_r = 2}.
#'
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
#'
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @keywords
#' internal
#'
#' @examples
#' b <- matrix(c(-1,1,1,1), ncol = 2)
#' Omega <- matrix(c(0.8,0.5,0.5,1,0.5,-0.2,-0.2,0.3), ncol = 2)
#' z <- rep(1:2, each = 10)
#' beta <- sapply(z, function(z) rmvnorm(mu = b[,z], Sigma = matrix(Omega[,z], ncol = 2)))
#' RprobitB:::plot_class_allocation(beta = beta, z = z, b = b, Omega = Omega,
#'                                  colors = c("red","blue"), perc = 0.5, r = 1)
#' @importFrom mixtools ellipse
#' @importFrom graphics legend points

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
  graphics::points(t(beta), col = colors[z], pch = 19)
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

#' Plot ROC curve
#'
#' @description
#' This function draws receiver operating characteristic (ROC) curves.
#'
#' @param ...
#' One or more \code{RprobitB_fit} objects or \code{data.frame}s of choice
#' probability.
#' @param reference
#' The reference alternative.
#' @return
#' No return value. Draws a plot to the current device.
#'
#' @export
#'
#' @importFrom plotROC geom_roc style_roc
#' @importFrom ggplot2 element_blank

plot_roc <- function(..., reference = NULL) {
  D <- name <- NULL
  models <- as.list(list(...))
  model_names <- unlist(lapply(sys.call()[-1], as.character))[1:length(models)]
  pred_merge <- NULL
  for(m in 1:length(models)) {
    if(inherits(models[[m]], "RprobitB_fit")){
      if(is.null(reference)){
        reference <- models[[m]]$data$alternatives[1]
      }
      pred <- predict.RprobitB_fit(models[[m]], overview = FALSE, digits = 8)
      true <- ifelse(pred$true == reference, 1, 0)
      if(is.null(pred_merge)){
        pred_merge <- data.frame(true)
        colnames(pred_merge) <- paste0("d_",model_names[m])
      } else {
        pred_merge[,paste0("d_",model_names[m])] <- true
      }
      pred_merge[,model_names[m]] <- pred[reference]
    } else {
      if(is.null(reference)){
        reference <- colnames(models[[m]])[1]
      }
      if(is.null(pred_merge)){
        stop("Not implemented yet.", call. = FALSE)
      } else {
        pred_merge[,model_names[m]] <- models[[m]][,reference]
      }
    }
  }
  if(length(models) > 1) {
    pred_merge <- plotROC::melt_roc(
      data = pred_merge, d = paste0("d_",model_names[1]), m = model_names)
  } else {
    colnames(pred_merge) <- c("D","M")
  }
  if(length(models) > 1) {
    plot <- ggplot2::ggplot(data = pred_merge,
                            ggplot2::aes(m = M, d = D, color = name))
  } else {
    plot <- ggplot2::ggplot(data = pred_merge, ggplot2::aes(m = M, d = D))
  }
  plot <- plot + plotROC::geom_roc(n.cuts = 20, labels = FALSE) +
    plotROC::style_roc(theme = ggplot2::theme_grey) +
    theme(legend.position = "top") +
    theme(legend.title = ggplot2::element_blank())
  print(plot)
  return(plot)
}
