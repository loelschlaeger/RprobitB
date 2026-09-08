cross_product_sum <- function(matrices, P, J, ordered) {
  initial <- matrix(0, nrow = P^2, ncol = if (ordered) 1L else (J - 1L)^2)
  Reduce(`+`, lapply(matrices, function(x) {
    if (ordered) {
      x <- as.numeric(x)
      t(kronecker(t(x), t(x)))
    } else {
      kronecker(t(x), t(x))
    }
  }), init = initial)
}

check_fit <- function(x, var_name = "object") {
  oeli::input_check_response(
    checkmate::check_class(x, "RprobitB_fit"), var_name
  )
  invisible(x)
}

check_probability <- function(x, var_name = "level") {
  oeli::input_check_response(
    checkmate::check_number(x, finite = TRUE), var_name
  )
  if (x <= 0 || x >= 1) {
    oeli::input_check_response(
      "Must be strictly between zero and one.", var_name
    )
  }
  invisible(x)
}

check_finite_matrix <- function(x, var_name) {
  oeli::input_check_response(
    checkmate::check_matrix(x, mode = "numeric", any.missing = FALSE),
    var_name
  )
  oeli::input_check_response(
    checkmate::check_numeric(as.numeric(x), finite = TRUE, any.missing = FALSE),
    var_name
  )
  invisible(x)
}

check_finite_numeric <- function(
  x, var_name, len = NULL, min.len = NULL, lower = -Inf, upper = Inf
) {
  oeli::input_check_response(
    oeli::check_numeric_vector(
      x,
      lower = lower, upper = upper, finite = TRUE, any.missing = FALSE,
      len = len, min.len = min.len
    ),
    var_name
  )
  invisible(x)
}

check_matrix_dimensions <- function(x, rows, columns, var_name) {
  oeli::input_check_response(
    checkmate::check_matrix(
      x,
      mode = "numeric", any.missing = FALSE, nrows = rows, ncols = columns
    ),
    var_name
  )
  invisible(x)
}

select_columns <- function(design, columns) {
  if (!length(columns)) {
    return(NA)
  }
  lapply(design, function(x) x[, columns, drop = FALSE])
}

summarize_draws <- function(values, level) {
  alpha <- (1 - level) / 2
  data.frame(
    mean = mean(values, na.rm = TRUE),
    sd = stats::sd(values, na.rm = TRUE),
    lower = unname(stats::quantile(values, alpha, na.rm = TRUE)),
    upper = unname(stats::quantile(values, 1 - alpha, na.rm = TRUE))
  )
}

posterior_mode <- function(x) {
  x <- as.vector(x)
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_real_)
  tolerance <- sqrt(.Machine$double.eps) * max(1, abs(mean(x)))
  if (diff(range(x)) <= tolerance) return(mean(x))
  rounded <- round(x)
  if (all(abs(x - rounded) <= tolerance)) x <- rounded
  values <- sort(unique(x))
  if (all(x == round(x))) {
    frequencies <- tabulate(match(x, values), nbins = length(values))
    return(values[which.max(frequencies)])
  }
  estimate <- stats::density(x, from = min(x), to = max(x))
  estimate$x[which.max(estimate$y)]
}

individual_variables <- function(object) {
  variables <- dimnames(object$draws)$variable
  variables[startsWith(variables, "individual[")]
}
