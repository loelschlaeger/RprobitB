#' Create object of class `RprobitB_latent_classes`
#'
#' @description
#' This function creates an object of class `RprobitB_latent_classes` which
#' defines the number of latent classes and their updating scheme.
#'
#' @param latent_classes \[`list()` | `NULL`\]\cr
#' Optionally parameters specifying the number of latent classes and their
#' updating scheme. The values in brackets are the default.
#'
#' - `C` (`1`): The fixed number (greater or equal 1) of (initial) classes.
#' - `wb_update` (`FALSE`): Set to `TRUE` for weight-based class updates.
#' - `dp_update` (`FALSE`): Set to `TRUE` for Dirichlet process class updates.
#' - `Cmax` (`10`): The maximum number of latent classes.
#'
#' The following specifications are used for the weight-based updating scheme:
#'
#' - `buffer` (`50`): The number of iterations to wait before the next update.
#' - `epsmin` (`0.01`): The threshold weight for removing a latent class.
#' - `epsmax` (`0.7`): The threshold weight for splitting a latent class.
#' - `deltamin` (`0.1`): The minimum mean distance before merging two classes.
#' - `deltashift` (`0.5`): The scale for shifting the class means after a split.
#'
#' @param x
#' An object of class `RprobitB_latent_classes`.
#'
#' @param ...
#' Currently not used.
#'
#' @return
#' An object of class `RprobitB_latent_classes`.
#'
#' @keywords internal

RprobitB_latent_classes <- function(latent_classes = NULL) {

  ### input checks
  if (is.null(latent_classes)) latent_classes <- list()
  oeli::input_check_response(
    check = checkmate::check_list(latent_classes),
    var_name = "latent_classes"
  )

  ### set default values
  if (is.null(latent_classes[["C"]])) {
    latent_classes[["C"]] <- 1
  }
  if (is.null(latent_classes[["wb_update"]])) {
    latent_classes[["wb_update"]] <- FALSE
  }
  if (is.null(latent_classes[["dp_update"]])) {
    latent_classes[["dp_update"]] <- FALSE
  }
  if (is.null(latent_classes[["Cmax"]])) {
    latent_classes[["Cmax"]] <- 10
  }
  if (is.null(latent_classes[["buffer"]])) {
    latent_classes[["buffer"]] <- 50
  }
  if (is.null(latent_classes[["epsmin"]])) {
    latent_classes[["epsmin"]] <- 0.01
  }
  if (is.null(latent_classes[["epsmax"]])) {
    latent_classes[["epsmax"]] <- 0.7
  }
  if (is.null(latent_classes[["deltamin"]])) {
    latent_classes[["deltamin"]] <- 0.1
  }
  if (is.null(latent_classes[["deltashift"]])) {
    latent_classes[["deltashift"]] <- 0.5
  }

  ### check specifications
  oeli::input_check_response(
    check = checkmate::check_int(latent_classes[["C"]], lower = 1),
    var_name = "latent_classes$C"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(latent_classes[["wb_update"]]),
    var_name = "latent_classes$wb_update"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(latent_classes[["dp_update"]]),
    var_name = "latent_classes$dp_update"
  )
  oeli::input_check_response(
    check = checkmate::check_int(
      latent_classes[["Cmax"]], lower = latent_classes[["C"]]
    ),
    var_name = "latent_classes$Cmax"
  )
  oeli::input_check_response(
    check = checkmate::check_int(latent_classes[["buffer"]], lower = 1),
    var_name = "latent_classes$buffer"
  )
  oeli::input_check_response(
    check = checkmate::check_number(
      latent_classes[["epsmin"]], lower = 0, upper = 1
    ),
    var_name = "latent_classes$epsmin"
  )
  oeli::input_check_response(
    check = checkmate::check_number(
      latent_classes[["epsmax"]], lower = 0, upper = 1
    ),
    var_name = "latent_classes$epsmax"
  )
  oeli::input_check_response(
    check = checkmate::check_number(
      latent_classes[["deltamin"]], lower = 0, finite = TRUE
    ),
    var_name = "latent_classes$deltamin"
  )
  oeli::input_check_response(
    check = checkmate::check_number(
      latent_classes[["deltashift"]], lower = 0, finite = TRUE
    ),
    var_name = "latent_classes$deltashift"
  )

  ### add flag for class update
  if (latent_classes[["wb_update"]] || latent_classes[["dp_update"]]) {
    latent_classes[["class_update"]] <- TRUE
  } else {
    latent_classes[["class_update"]] <- FALSE
  }

  ### clean specifications
  if (!latent_classes[["wb_update"]]) {
    wb_spec_names <- c("buffer", "epsmin", "epsmax", "deltamin", "deltashift")
    latent_classes[wb_spec_names] <- NULL
  }
  if (!latent_classes[["class_update"]]) {
    latent_classes["Cmax"] <- NULL
  }

  ### return object
  structure(latent_classes, class = "RprobitB_latent_classes")
}

#' @rdname RprobitB_latent_classes
#' @exportS3Method

print.RprobitB_latent_classes <- function(x, ...) {
  cat(crayon::underline("Latent classes\n"))
  if (!x[["class_update"]]) {
    cat("C =", x$C, "\n")
  } else {
    cat("Dirichlet process update:", x[["dp_update"]], "\n")
    cat("Weight-based update:", x[["wb_update"]], "\n")
    cat("Maximum classes:", x$Cmax, "\n")
    if (x[["wb_update"]]) {
      cat("Updating buffer:", x$buffer, "\n")
      cat("Minimum class weight:", x$epsmin, "\n")
      cat("Maximum class weight:", x$epsmax, "\n")
      cat("Mimumum class distance:", x$deltamin, "\n")
    }
  }
  invisible(x)
}
