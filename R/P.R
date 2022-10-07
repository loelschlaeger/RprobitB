P <- function(form, re) {
  check_form(form, re)
  P_f <- ...
  P_r <- ...
  P <- P_f + P_r
  structure(
    as.integer(P),
    P_f = P_f,
    P_r = P_r
  )
}
