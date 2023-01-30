#' Define choice covariates
#'
#' @description
#' These functions create and validate an object of class
#' \code{\link{RprobitB_covariates}}, which contains the covariate matrices,
#' see the details.
#'
#' \code{\link{sample_RprobitB_covariates}} samples covariates.
#'
#' @param RprobitB_data
#' An \code{\link{RprobitB_data}} object.
#' @param RprobitB_formula
#' An \code{\link{RprobitB_formula}} object.
#' @param RprobitB_alternatives
#' An \code{\link{RprobitB_alternatives}} object.
#'
#' @details
#' # Covariate matrices
#' TODO
#'
#' # Sample covariates
#' TODO
#'
#' @return
#' An \code{\link{RprobitB_covariates}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\code{}}{}
#' }
#'
#' @keywords object

RprobitB_covariates <- function(
    RprobitB_data, RprobitB_formula, RprobitB_alternatives
  ) {

  ### transform 'RprobitB_data' to 'RprobitB_covariates'


  ### validate 'RprobitB_covariates'
  validate_RprobitB_covariates(

  )
}

#' @rdname RprobitB_covariates
#' @param x
#' An \code{\link{RprobitB_covariates}} object.

is.RprobitB_covariates <- function(x) {
  inherits(x, "RprobitB_covariates")
}

#' @rdname RprobitB_covariates
#' @inheritParams RprobitB_formula
#' @inheritParams expand_T
#' @inheritParams RprobitB_alternatives
#' @inheritParams RprobitB_data
#' @param sampler
#' A named \code{list}, each element is a \code{function} that draws covariates,
#' see the details.
#' Covariates that are not specified in \code{sampler} are drawn from
#' \code{sampler[[".default"]]}.
#' By default, \code{".default" = function() stats::rnorm(n = 1, mean = 0, sd = 9)}.
#' @export
#' @importFrom stats rnorm

sample_RprobitB_covariates <- function(
  formula, N, J, T = 1, alternatives = LETTERS[1:J], base = alternatives[1],
  re = NULL, ordered = FALSE, seed = NULL,
  sampler = list(".default" = function() stats::rnorm(n = 1, mean = 0, sd = 9))
) {

  ### input checks
  T <- expand_T(N = N, T = T)
  RprobitB_formula <- RprobitB_formula(
    formula = formula, re = re, ordered = ordered
  )
  RprobitB_alternatives <- RprobitB_alternatives(
    J = J, alternatives = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    RprobitB_formula = RprobitB_formula,
    RprobitB_alternatives = RprobitB_alternatives
  )
  if (!is.list(values)) {
    RprobitB_stop(
      "Input 'values' is misspecified.",
      "It should be a `list`, see the documentation."
    )
  }
  if (!".default" %in% names(values)) {
    values[[".default"]] <- function() stats::rnorm(n = 1, mean = 0, sd = 9)
  } else {
    if (!is.function(values[[".default"]])) {
      RprobitB_stop(
        "Element '.default' in input list 'values' is misspecified.",
        "It should be a `function`, see the documentation."
      )
    }
    try_default_sampler <- try(values[[".default"]](), silent = TRUE)
    if (!is_single_numeric(try_default_sampler)) {
      RprobitB_stop(
        "Element '.default' in input list 'values' is misspecified.",
        "I tried to call `values[[\".default\"]]()`.",
        "The result was not the expected single `numeric` value."
      )
    }
  }

  ### helper functions for setting covariates
  ### x: either
  ### as:
  covariates_from_function <- function (fun, as) {
    try_fun <- try(fun(), silent = TRUE)
    if (!(is.numeric(try_fun) && is.vector(try_fun))) {
      RprobitB_stop(

      )
    }
    if (as) {
      if (length(try_fun) != J) {

      }

    } else {
      if (length(try_fun) != 1) {

      }

    }
  }
  covariates_from_vector <- function (vec, as) {

  }

  ### draw covariate values
  if (!is.null(seed)) {
    set.seed(seed)
  }
  data <- data.frame(
    "N" = rep(1:N, times = T),
    "T" = unlist(sapply(T, seq_len))
  )
  for (e in 1:nrow(effects)) {
    effect <- effects[e, ]
    if (startsWith(effect$name, "ASC_")) {
      next
    }
    if (effect$name %in% names(sampler)) {
      x <- values[[effect$name]]
    } else {
      x <- values[[".default"]]
    }
    if (is.function(x)) {
      data[[effect$name]] <- covariates_from_function(x, effect$as_cov)
    } else if (is.numeric(x)) {
      data[[effect$name]] <- covariates_from_vector(x, effect$as_cov)
    } else {
      RprobitB_stop()
    }





    if (!effect$name %in% names(values)) {
      if (effect$as_cov) {
        data[[effect$name]] <- lapply(1:nrow(data), function(n) replicate(J, values$.default()))
      } else {
        data[[effect$name]] <- as.list(replicate(nrow(data), values$.default()))
      }
    } else {
      custom <- values[[effect$name]]
      if (is.numeric(custom)) {
        tryCatch(
          data[effect] <- custom,
          error = function(cond) {
            RprobitB_stop("error")
          },
          warning = error
        )
      } else if (is.function(custom)) {
        tryCatch(
          data[effect] <- replicate(nrow(data), custom()),
          error = function(cond) {
            RprobitB_stop("error")
          }
        )
      } else {
        RprobitB_stop(
          "stop"
        )
      }
    }
  }

  ### transform 'data' to 'RprobitB_covariates'
  x <- list()
  for (n in 1:N) {
    x[[n]] <- list()
    for (t in 1:T[n]) {
      data_index <- which(data$N == n & data$T == t)
      X_nt <- matrix(data = NA_real_, nrow = J, ncol = 0)
      rownames(X_nt) <- RprobitB_alternatives$alternatives
      for (e in 1:nrow(effects)) {
        effect <- effects[e, ]
        if (effect$as_coef) {
          j_split <- strsplit(effect$name, "_(?!.*_)", perl = TRUE)[[1]]
          j_prefix <- j_split[1]
          j_name <- j_split[2]
          j_index <- which(RprobitB_alternatives$alternatives == j_name)
          X_nt_add <- numeric(J)
          if (j_prefix == "ASC") {
            X_nt_add[j_index] <- 1
          } else {
            if (effect$as_cov) {
              X_nt_add[j_index] <- sampler_default()
            } else {
              j_other <- RprobitB_alternatives$alternatives[-j_index]
              j_other_column_name <- paste0(j_prefix, "_", j_other)
              if (any(j_other_column_name %in% colnames(X_nt))) {
                column_index <- which(j_other_column_name %in% colnames(X_nt))[1]
                j_other_index <- which(RprobitB_alternatives$alternatives == j_other[column_index])
                X_nt_add[j_index] <- X_nt[j_other_index, column_index]
              } else {
                X_nt_add[j_index] <- draw_covariate[[j_prefix]]()
              }
            }
          }
        } else {
          if (effect$as_cov) {
            X_nt_add <- unlist(data[data_index, effect$name])
          } else {

          }
        }
        X_nt <- cbind(X_nt, X_nt_add)
        colnames(X_nt)[ncol(X_nt)] <- effect$name
      }
      x[[n]][[t]] <- X_nt
    }
  }

  ### validate 'RprobitB_covariates'
  validate_RprobitB_covariates(
    x = x, formula = formula, N = N, J = J, T = T, alternatives = alternatives,
    base = base, re = re, ordered = ordered
  )
}

#' @rdname RprobitB_covariates

validate_RprobitB_covariates <- function(
  x = list(), formula, N, J, T = 1, alternatives = LETTERS[1:J],
  base = alternatives[1], re = NULL, ordered = FALSE
) {

  ### input checks
  if (!is.list(x)) {
    RprobitB_stop(
      "Input 'x' is not a `list`."
    )
  }

  ### construct objects
  RprobitB_formula <- RprobitB_formula(
    formula = formula, re = re, ordered = ordered
  )
  RprobitB_alternatives <- RprobitB_alternatives(
    J = J, alternatives = alternatives, base = base, ordered = ordered
  )
  effects <- overview_effects(
    RprobitB_formula = RprobitB_formula,
    RprobitB_alternatives = RprobitB_alternatives
  )

  ### validate 'RprobitB_covariates'


  ### return validated 'RprobitB_covariates' object
  structure(x, class = c("RprobitB_covariates", "list"))
}

#' @rdname RprobitB_covariates
#' @param ...
#' Currently not used.
#' @exportS3Method

print.RprobitB_covariates <- function(x, ...) {
  if (!is.RprobitB_covariates(x)) {
    RprobitB_stop(
      "Input 'x' is not of class `RprobitB_covariates`.",
      "See `?RprobitB_covariates` to create such an object."
    )
  }
  print.default(x)
  invisible(x)
}

#' Expand \code{T}
#'
#' @description
#' This function expands the number of choice occasions \code{T} to a
#' \code{vector} of length \code{N}.
#'
#' @param N
#' A positive \code{integer}, the number of deciders.
#' @param T
#' A positive \code{integer}, the number of choice occasions per decider.
#' Can also be a \code{vector} of length \code{N} for a variable number of
#' choice occasions per decider.
#' By default, \code{T = 1}.
#'
#' @return
#' An \code{integer} \code{vector} of length \code{N}.
#'
#' @examples
#' \dontrun{
#' expand_T(N = 10, T = 2)
#' expand_T(N = 10, T = 1:10)
#' }
#'
#' @keywords internal

expand_T <- function(N, T = 1) {
  if (missing(N)) {
    RprobitB_stop(
      "Please specify the input 'N'.",
      "It should be a positive `integer`, the number of deciders."
    )
  }
  if (!is_positive_integer(N)) {
    RprobitB_stop(
      "Input 'N' is misspecified.",
      "It should be a positive `integer`, the number of deciders."
    )
  }
  if (!(is.numeric(T) && is.vector(T))) {
    RprobitB_stop(
      "Input 'T' is misspecified.",
      "It should be a `numeric` (`vector`)."
    )
  }
  if (length(T) == 1) {
    T <- rep(T, N)
  }
  if (length(T) != N) {
    RprobitB_stop(
      "Input 'T' is misspecified.",
      glue::glue("It should be a `vector` of length 'N = {N}'.")
    )
  }
  if (!all(sapply(T, is_positive_integer))) {
    RprobitB_stop(
      "Input 'T' is misspecified.",
      "It should be a `vector` of `integer` only."
    )
  }
  as.integer(T)
}
