#' Define choice data for probit model
#'
#' @description
#' These functions create, validate, summarize and visualize an object of class
#' \code{\link{RprobitB_data}}, which contains the choice data for a probit
#' model, see details.
#'
#' @param ranked
#' TODO
#'
#' @inheritSection simulate_choices Choice simulation
#' @inheritSection prepare_data Choice data
#'
#' @return
#' An \code{\link{RprobitB_data}} object.
#'
#' It contains the elements: TODO
#' \describe{
#'   \item{\code{}}{}
#' }
#'
#' @keywords internal object

RprobitB_data <- function() {

}

#' @rdname RprobitB_data
#' @param x,object
#' An \code{\link{RprobitB_data}} object.

is.RprobitB_data <- function(x) {
  inherits(x, "RprobitB_data")
}

validate_RprobitB_data <- function(x, RprobitB_formula) {

}

#' @rdname RprobitB_data
#' @param ...
#' Currently not used.

summary.RprobitB_data <- function(object, ...) {

}

#' @noRd
#' @exportS3Method

print.summary.RprobitB_date <- function(x, ...) {

}

#' @rdname RprobitB_data
#' @exportS3Method

print.RprobitB_data <- function(x, ...) {

}

#' @rdname RprobitB_data
#' @exportS3Method

plot.RprobitB_data <- function(x, ...) {

}
