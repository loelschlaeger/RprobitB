#' Check \code{distr}
#' @description
#' Function that checks the input \code{distr}.
#' @param distr
#' A named list of number generation functions from which the covariates are
#' drawn. Each element of \code{distr} must be of the form
#' \code{"cov" = list("name" = "<name of the number generation function>", ...)},
#' where \code{cov} is the name of the covariate and \code{...} are required
#' parameters for the number generation function.
#' Covariates for which no distribution is specified are drawn from a standard
#' normal distribution.
#' Possible number generation functions are
#' \itemize{
#'   \item functions of the type \code{r*} from base R (e.g. \code{rnorm}) where
#'         all required parameters (except for \code{n}) must be specified,
#'   \item the function \code{sample}, where all required parameters
#'         (except for \code{size}) must be specified.
#' }
#' @examples
#' check_distr(distr = list("price_bus" = list("name"= "rgamma", shape = 1),
#'                          "cars" = list("name" = "sample", x = 0:2,
#'                                        replace = TRUE)))
#' @return
#' The checked input \code{distr}

check_distr = function(distr) {

  ### check if 'distr' is a list
  if(!is.list(distr))
    stop("'distr' must be a list.")

  ### check if 'distr' has proper elements
  for(i in seq_len(length(distr))){

    ### set 'size' and 'n' arguments to 1
    if(distr[[i]][["name"]] == "sample"){
      distr[[i]][["size"]] = 1
    } else if(grepl("^r",distr[[i]][["name"]])) {
      distr[[i]][["n"]] = 1
    } else {
      stop(
        paste0("The 'name' of element '", names(distr)[i],
               "' in 'distr' is not a valid number generation function name."))
    }

    ### check if element in 'distr' gives single numeric draw
    out = try(do.call(what = distr[[i]][["name"]],
                      args = distr[[i]][names(distr[[i]]) != "name"]),
              silent = TRUE)
    if(inherits(out,"try-error") || length(out) != 1 || !is.numeric(out))
      stop(paste0("Could not interpret element '", names(distr)[i],
                  "' in 'distr'."))
  }

  ### return checked 'distr'
  return(distr)

}
