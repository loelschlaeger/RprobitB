This is a resubmission of version 2.0.0.

* CRAN test NOTE: Examples with CPU (user + system) or elapsed time > 5s: interpret.
  Comment: We made the numerical differentiation in `interpret()` faster. The example now runs in less than half a second on our machine, and no example takes longer than one second.

* CRAN test NOTE: Overall checktime 27 min > 10 min.
  Comment: We shortened the Markov chains and reduced the data sets in the vignettes. Re-building the vignettes is much faster now.
