# Expressions

- error term
- positive definite
- by default

# Variables

- `diff_alt`

# Roxygen

```r
#' Verb + subject
#' 
#' This function / These functions ... .
#' 
#' @param C
#' An \code{integer}, the number (greater or equal 1) of latent classes of 
#' decision makers.
#' By default, \code{C = 1}.
#' @param s
#' A \code{numeric} of length \code{C}, the vector of class weights.
#' For identifiability, the vector must be descending.
#' By default, \code{s = rep(1,C)/C}.
#' @param ...
#' Not used.
#' 
#' @return
#' A \code{matrix}, the ... .
#' 
#' @examples
#' ...
#' 
#' (@importFrom ... ...)
#' (@export)
#' @keywords internal object utils
#' @seealso [fun()] to ...

fun_name <- function(...) {...}
```

# Tests

Every `.R` file has a test companion.
