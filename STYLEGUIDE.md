# Classes

`{RprobitB}` uses the `S3` OOS. Each class should be named `RprobitB_...` and
be saved in an eponymous `.R` file. The `{roxygen2}` documentation should be in 
the form:

```r
#' Define <what?>
#'
#' @description
#' This function constructs an object of class \code{\link{RprobitB_...}}, which 
#' contains <what?>.
#'
#' @param <name>
#' A \code{<type>}, <description>.
#'
#' @return
#' An \code{\link{RprobitB_...}} object.
#'
#' It contains the elements:
#' \describe{
#'   \item{\ode{<name>}}{The <description>.}
#' }
#' 
#' @details (optional)
#'
#' @examples
#' \dontrun{
#' # add example
#' }
#'
#' @keywords internal object

RprobitB_... <- function() {}
```

Each class has at least 3 methods:

1. `is.RprobitB_...` 
2. `validate_RprobitB_...`
3. `print.RprobitB_...`

Document them with

- `@rdname RprobitB_...`
- and (if appropriate) `@exportS3method`

# Functions

The `{roxygen2}` documentation of functions should be in the form:

```r
#' Verb + subject
#' 
#' @description
#' This function / These functions <what?>.
#' 
#' @param <name>
#' A \code{<type>}, <description>.
#' (By default, \code{<name> = ...}.)
#' @param ...
#' Currently not used.
#' 
#' @return
#' A \code{<type>}, <description>.
#' 
#' @examples
#' ...
#' 
#' (@importFrom ... ...)
#' 
#' (@export) 
#' 
#' (@keywords (internal) (utils) (cpp))
#' 
#' @seealso 
#' \itemize{
#'   \item[fun()] for <what?>
#' }

fun_name <- function() {}
```

# Tests

Every `.R` file has a test companion.

# Expressions

- error term
- positive definite
- by default
- Inverse-Wishart
