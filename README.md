# RprobitB <img src='sticker/sticker.png' align="right" height="136" />
[![CRAN status](https://www.r-pkg.org/badges/version-last-release/RprobitB)](https://www.r-pkg.org/badges/version-last-release/RprobitB)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)](https://cranlogs.r-pkg.org/badges/grand-total/RprobitB)

**RprobitB** is an R package that can be used to fit (latent class) (mixed) (multinomial) probit models to simulated or empirical data. 

Do you found a bug or request a feature? Please [tell us](https://github.com/loelschlaeger/RprobitB/issues)!

## Using RprobitB

To install the latest version of **RprobitB**, run `install.packages("RprobitB")` in your R console.

## Using RprobitB

To use RprobitB, follow these steps:

1. Run `install.packages("RprobitB")` in your R console to install the latest version of **RprobitB**.
2. Specify the model, see the vignette **Introduction to RprobitB** for details.
3. Run `RprobitB::fit_mnp(<list of model specifications>)`.
4. You get on-screen information and model results in an output folder, see the package vignette for details.

## Example

The code below fits a mixed multinomial probit model with

- `P_f = 1` fixed  
- and `P_r = 2` random coefficients

to simulated data with

- `N = 100` decision makers,
- variable choice occasions between `T = 10` and `T = 20`,
- `J = 3` choice alternatives,
- and `C = 2` true latent classes.

The number of latent classes is updated, because `do_lcus = TRUE` is set. The Gibbs sampler draws `R = 20000` samples. By default, the model is named `id = "test"` and results are saved in `rdir = "tempdir()"` (the path of the per-session temporary directory). 


```r
set.seed(1)

### model specification
model = list("N" = 100, "T" = sample(10:20,100,replace=TRUE), "J" = 3, "P_f" = 1, "P_r" = 2, "C" = 2)
lcus  = list("do_lcus" = TRUE)
mcmc  = list("R" = 20000)

### start estimation (about 3 minutes computation time)
RprobitB::fit_mnp("model" = model, "lcus" = lcus, "mcmc" = mcmc)
```
