#' Print method for \code{RprobitB_parameter_statistics}.
#' @param x
#' An object of class \code{RprobitB_parameter_statistics}.
#' @param true
#' Either \code{NULL} or an object of class \code{RprobitB_true_parameter}.
#' @param statistics
#' A character vector, specifying the output of parameter statistics. Possible
#' values are \code{"mean"}, \code{"sd"}, \code{"min"}, \code{"q.25"},
#' \code{"median"}, \code{"q.75"}, \code{"max"}, and \code{"R^"}.
#' @param digits
#' An integer, specifying the number of decimal places for the statistics.
#' @param ...
#' Ignored.
#' @export

print.RprobitB_parameter_statistics = function(
  x, true = NULL, statistics = c("mean", "sd", "R^"), digits = 2, ...) {

  ### select 'statistics'
  statistics = intersect(statistics, colnames(x[[1]]))

  if(length(statistics) > 0){

    ### make 'digits' non-negative
    digits = max(digits, 0)

    ### determine cell width
    cw = max(digits + 5, max(nchar(statistics))+1)

    ### print header of table
    cat("Parameter statistics:\n")
    header = sprintf("%6s"," ")
    if(!is.null(true))
      header = paste0(header,sprintf(paste0("%", cw+1,"s"),"true"))
    for(header_element in statistics)
      header = paste0(header,
                      sprintf(paste0("%", cw+1,"s"),header_element))
    cat(header)

    ### print table elements
    order_of_parameters = c("alpha","s","b","Omega","Sigma")
    for(par_name in intersect(order_of_parameters,names(x))){
      out = x[[par_name]][,statistics,drop=FALSE]
      if(!is.null(true))
        out = cbind(as.vector(true[[par_name]]),out)
      out = round(out, digits)
      colnames(out) = rep(sprintf(paste0("%",cw,"s")," "), ncol(out))
      rownames(out) = sprintf("%6s", rownames(out))
      writeLines(paste("\n",par_name))
      print(formatC(out, format='f', digits=digits, width = cw,
                    flag = ""), quote = FALSE)
    }
  }

  return(invisible(x))
}
