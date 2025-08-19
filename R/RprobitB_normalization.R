#' Create object of class `RprobitB_normalization`
#'
#' @description
#' This function creates an object of class `RprobitB_normalization`,
#' which determines the utility scale and level.
#'
#' @details
#' Any choice model has to be normalized with respect to the utility level and
#' scale.
#' \itemize{
#'   \item For level normalization, \code{{RprobitB}} takes utility differences
#'         with respect to one alternative.
#'         For the ordered model where only one utility is modeled,
#'         \code{{RprobitB}} fixes the first utility threshold to 0.
#'   \item For scale normalization, \code{{RprobitB}} fixes one model parameter.
#'         Per default, the first error-term variance is fixed to `1`.
#'         This is specified via `scale = "Sigma_1,1 := 1"`.
#'         Alternatively, any error-term variance or any non-random coefficient
#'         can be fixed.
#' }
#'
#' @param level
#' The alternative name with respect to which utility differences are computed.
#' Currently, only differences with respect to the last alternative can be
#' computed.
#' @param scale
#' A character which determines the utility scale. It is of the form
#' `<parameter> := <value>`, where `<parameter>` is either the name of a fixed
#' effect or `Sigma_<j>,<j>` for the `<j>`th diagonal element of `Sigma`, and
#' `<value>` is the value of the fixed parameter.
#' @inheritParams overview_effects
#' @inheritParams RprobitB_data
#'
#' @return
#' An object of class `RprobitB_normalization`, which is a list of
#' \itemize{
#'   \item `level`, a list with the elements `level` (the number of the
#'         alternative specified by the input `level`) and `name` (the name of
#'         the alternative, i.e. the input `level`), or alternatively
#'         \code{NA} in the ordered probit case,
#'   \item and `scale`, a list with the elements `parameter` (either `"s"` for
#'         an element of `Sigma` or `"a"`for an element of `alpha`), the
#'         parameter `index`, and the fixed `value`. If `parameter = "a"`, also
#'         the `name` of the fixed effect.
#' }
#'
#' @keywords internal

RprobitB_normalization <- function(
    level, scale = "Sigma_1,1 := 1", form, re = NULL, alternatives, base,
    ordered = FALSE) {
  ### check inputs
  if (missing(alternatives)) {
    stop("Please specify 'alternatives'.",
         call. = FALSE
    )
  }
  if (!is.character(alternatives)) {
    stop("'alternatives' must be a character vector",
         call. = FALSE
    )
  }
  if (missing(level) || is.null(level)) {
    level <- tail(alternatives, n = 1)
  }
  if (level != tail(alternatives, n = 1)) {
    level <- tail(alternatives, n = 1)
    warning(
      paste0(
        "Currently, only alternatives with respect to the last ",
        "alternative can be computed.\nTherefore, 'level' = ",
        tail(alternatives, n = 1), "' is set."
      ),
      immediate. = TRUE, call. = FALSE
    )
  }
  if (missing(form)) {
    stop("Please specify 'form'.",
         call. = FALSE
    )
  }
  if (!(is.character(level) && length(level) == 1 && level %in% alternatives)) {
    stop("'level' must be one element of 'alternatives'.",
         call. = FALSE
    )
  }
  if (!(is.character(scale) && length(scale) == 1)) {
    stop("'scale' must be a single character.",
         call. = FALSE
    )
  }
  scale <- gsub(" ", "", scale, fixed = TRUE)
  if (!grepl(":=", scale, fixed = TRUE)) {
    stop("'scale' is not in format '<parameter> := <value>'.",
         call. = FALSE
    )
  }
  if (missing(base)) {
    stop("Please specify 'base'.",
         call. = FALSE
    )
  }
  if (!isTRUE(ordered) && !isFALSE(ordered)) {
    stop("'ordered' must be a boolean.",
         call. = FALSE
    )
  }

  ### set 'level'
  if (ordered) {
    level <- NA
  } else {
    alt_name <- level
    level <- which(alternatives == level)
    level <- list("level" = level, "name" = alt_name)
  }

  ### set 'scale'
  effects <- overview_effects(
    form = form, re = re, alternatives = alternatives,
    base = base
  )
  parameter <- strsplit(scale, ":=", fixed = TRUE)[[1]][1]
  par_name <- NA
  if (parameter %in% effects[["effect"]]) {
    index <- which(parameter == effects[["effect"]])
    if (effects[index, "random"]) {
      stop(
        paste0(
          "'", parameter, "' is a random effect and cannot be used ",
          "for scale normalization."
        ),
        call. = FALSE
      )
    }
    par_name <- parameter
    parameter <- "a"
  } else {
    parameter_split <- strsplit(parameter, split = "_")[[1]]
    if (parameter_split[1] %in% c("Sigma", "sigma")) {
      parameter <- "s"
      index <- suppressWarnings(
        as.numeric(strsplit(parameter_split[2], split = ",")[[1]][1])
      )
      if (is.na(index) || index %% 1 != 0 || index <= 0) {
        stop(
          paste(
            "'<parameter>' in 'scale = <parameter> := <value>' is not",
            "in the form 'Sigma_<j>,<j>' for an integer <j>."
          ),
          call. = FALSE
        )
      }
      if (index > length(alternatives)) {
        stop(
          paste(
            "'<j>' in 'Sigma_<j>,<j>' for '<parameter>' in",
            "'scale = <parameter> := <value>' must not be greater than",
            "the length of 'alternatives'."
          ),
          call. = FALSE
        )
      }
    } else {
      stop("Please check the specification of 'scale'.",
           call. = FALSE
      )
    }
  }
  value <- suppressWarnings(
    as.numeric(strsplit(scale, ":=", fixed = TRUE)[[1]][2])
  )
  if (is.na(value)) {
    stop(
      paste(
        "'<value>' in 'scale = <parameter> := <value>' is not",
        "a numeric value."
      ),
      call. = FALSE
    )
  }
  if (value == 0) {
    stop("'<value>' in 'scale = <parameter> := <value>' must be non-zero.",
         call. = FALSE
    )
  }
  if (value < 0 && parameter == "s") {
    stop("'<value>' in 'scale = <parameter> := <value>' must be non-zero ",
         "when fixing an error term variance.",
         call. = FALSE
    )
  }
  scale <- list(
    "parameter" = parameter, "index" = index, "value" = value,
    "name" = par_name
  )

  ### create and return object of class 'RprobitB_normalization'
  out <- list("level" = level, "scale" = scale)
  class(out) <- "RprobitB_normalization"
  return(out)
}

#' @rdname RprobitB_normalization
#' @param x
#' An object of class \code{RprobitB_normalization}.
#' @param ...
#' Currently not used.
#' @exportS3Method

print.RprobitB_normalization <- function(x, ...) {
  if (identical(NA, x$level)) {
    cat(paste0(
      "Level: Fixed first utility threshold to 0.\n"
    ))
    cat(paste0(
      "Scale: Error term variance fixed to ", x$scale$value, ".\n"
    ))
  } else {
    cat(paste0(
      "Level: Utility differences with respect to alternative '",
      x$level$name, "'.\n"
    ))
    if (x$scale$parameter == "a") {
      cat(paste0(
        "Scale: Coefficient of effect '", x$scale$name, "' (alpha_", x$scale$index,
        ") fixed to ", x$scale$value, ".\n"
      ))
    }
    if (x$scale$parameter == "s") {
      cat(paste0(
        "Scale: Coefficient of the ", x$scale$index, ". error term variance ",
        "fixed to ", x$scale$value, ".\n"
      ))
    }
  }
  return(invisible(x))
}
