# RprobitB <img src='sticker/sticker.png' align="right" height="136" />

RprobitB is an R package that can be used to fit latent class mixed multinomial probit (LCMMNP) models to simulated or empirical data. 

## License
RprobitB is licensed under the GNU General Public License v3.0. 

## Contact

Author and Maintainer: [Lennart Oelschläger](mailto:lennart.oelschlaeger@uni-bielefeld.de)

Do you found a bug or request a feature? Please [tell us](https://github.com/loelschlaeger/RprobitB/issues)!

## Installing RprobitB

To install the latest version of RprobitB, run `install.packages("RprobitB")` in your R console.

## Using RprobitB

To use RprobitB, follow these steps:

1. Specify the model, see [below](#specifying-a-lcmmnp-model-in-rprobitb) for details.
2. Run `RprobitB::rpb(<list of model specifications>)`.
3. You get on-screen information and model results in an output folder, see the package vignette for details.

## Specifying a LCMMNP model in RprobitB

RprobitB specifications are grouped in the named lists 

- `model` (model information),
- `data` (data information),
- `parm` (true parameter values),
- `lcus` (latent class updating scheme parameters),
- `init` (initial values for the Gibbs sampler),
- `prior` (prior parameters),
- `mcmc` (Markov chain Monte Carlo parameters),
- `norm` (normalization information),
- `out` (output settings).

You can either specify none, all, or selected parameters. Unspecified parameters are set to [default values](#default-specifications-of-rprobitb).

### `model` 

- `N`, the number (greater or equal one) of decision makers
- `T`, the number (greater or equal one) or vector (of length `N`) of choice occasions for each decision maker
- `J`, the number (greater or equal two) of choice alternatives (fixed across decision makers and choice occasions)
- `P_f`, the number of attributes that are connected to fixed coefficients (can be zero)
- `P_r`, the number of attributes that are connected to random, decision maker specific coefficients (can be zero)
- `C`, the number of latent classes (ignored if `P_r = 0`)

### `data`

If `data = NULL`, data is simulated from the model defined by `model` and `parm`.

To model empirical data, specify 

- `data_raw`, the data frame of choice data in wide format, must contain columns named "id" (unique identifier for each decision maker) and "choice" (the chosen alternatives)
- `cov_col`, a numeric vector specifying the columns of `data_raw` with covariates
- `cov_ord`, a character vector specifying the order of the covariates, where fixed-coefficient covariates come first (required for specifying random coefficients and interpretation of the model results)
- `cov_zst`, a boolean determining whether covariates get z-standardized

### `parm`

- `alpha`, the fixed coefficient vector (of length `model[["P_f"]]`)
- `s`, the vector of class weights (of length `model[["C"]]`)
- `b`, the matrix of class means as columns (of dimension `model[["P_r"]]` x `model[["C"]]`)
- `Omega`, the matrix of class covariance matrices as columns (of dimension `model[["P_r"]]*model[["P_r"]]` x `model[["C"]]`)
- `Sigma`, the error term covariance matrix (of dimension `model[["J"]]-1` x `model[["J"]]-1`)

### `lcus`

- `do_lcus`, a boolean determining whether to update the number of latent classes
- `C0`, the initial number of latent classes
- `Cmax`, the maximal number of latent classes (greater or equal `lcus[["C0"]]`)
- `buffer`, the buffer for the updating (number of iterations to wait before the next update)
- `epsmin`, the threshold weight for removing latent classes (between 0 and 1)
- `epsmax`, the threshold weight for splitting latent classes (between 0 and 1)
- `distmin`, the threshold for joining latent classes (greater 0)

### `init`

- `at_true`, a boolean determining whether to initialize at the true parameter values (only for simulated data)
- `alpha0`, the initial fixed coefficient vector (of length `model[["P_f"]]`)
- `b0`, the initial matrix of the class means as columns (of dimension `model[["P_r"]]` x `model[["C"]]`)
- `Omega0`, the inital matrix of the class covariance matrices as columns (of dimension `model[["P_r"]]*model[["P_r"]]` x `model[["C"]]`)
- `Sigma0`, the initial error term covariance matrix (of dimension `model[["J"]]-1` x `model[["J"]]-1`)
- `U0`, the initial matrix of utilities (of dimension `model[["J"]]-1` x `model[["N"]]*max(model[["T"]])`)
- `beta0`, the initial matrix of random coefficients (of dimension `model[["P_r"]]` x `model[["N"]]`)
- `m0`, the initial vector of class sizes (of length `model[["C"]]`)

### `prior`

A priori, `parm[["alpha"]]` ~ Normal(`eta`,`Psi`) with

- `eta`, the expectation vector (of length `model[["P_f"]]`)
- `Psi`, the covariance matrix (of dimension `model[["P_f"]]` x `model[["P_f"]]`)

A priori, `parm[["s"]]` ~ Dirichlet(`delta`) with

- `delta`, the concentration parameter (of length 1)

A priori, `parm[["b"]][,c]` ~ Normal(`xi`,`D`) with

- `xi`, the expectation vector (of length `model[["P_r"]]`)
- `D`, the covariance matrix (of dimension `model[["P_r"]]` x `model[["P_r"]]`)

A priori, `matrix(parm[["Omega"]][,c],nrow=model[["P_r"]],ncol=model[["P_r"]])` ~ Inverse_Wishart(`nu`,`Theta`) with

- `nu`, the degrees of freedom (greater than `model[["P_r"]]`)
- `Theta`, the scale matrix (of dimension `model[["P_r"]]` x `model[["P_r"]]`)

A priori, `parm[["Sigma"]]` ~ Inverse_Wishart(`kappa`,`E`) with

- `kappa`, the degrees of freedom (greater than `model[["J"]]-1`)
- `E`, the scale matrix (of dimension `model[["J"]]-1` x `model[["J"]]-1`)

### `mcmc`

- `R`: the number of iterations
- `B`: the length of the burn-in period
- `Q`: the thinning parameter
- `nprint`: the step number for printing the sampling progress

### `norm`

RprobitB automatically normalizes with respect to level by computing utility differences, where `model[["J"]]` is the base alternative. The normalization with respect to scale can be specified:

- `parameter`: the normalized parameter (either `"a"` for a fixed non-random linear coefficient or `"s"` for an error-term variance) 
- `index`: the index of the parameter (between 1 and `model[["P_f"]]` or 1 and `model[["J"]]-1`, respectively)
- `value`: the value for the fixed parameter (greater 0 if `parameter = "s"`)

### `out`

- `id`: a character, identifying the model
- `rdir`: a character, defining the (relative) path of the folder with the model results
- `pp`: a boolean, determining whether progress plots should be created (in any case only if `model$P_r=2`)
- `results`: a boolean, determining whether estimated parameters should be returned by the function `rpb`.
- `waic`: a boolean, determining whether to compute the widely applicable information criterion (WAIC)

## Default specifications of RprobitB

### `model` 

- `N = 100`
- `T = 10`
- `J = 2`
- `P_f = 1`
- `P_r = 0`
- `C = NA`

### `data`

`NULL`

### `parm`

Per default, parameters are randomly drawn.

### `lcus`

- `do_lcus = FALSE`
- `C0 = 5`
- `Cmax = 10`
- `buffer = 100`
- `epsmin = 0.01`
- `epsmax = 0.99`
- `distmin = 0.1`

### `init`

- `at_true = FALSE`
- `alpha0`: zero vector
- `b0`: zero matrices for each latent class
- `Omega0`: unity matrices for each latent class
- `Sigma0`: unity matrix
- `U0`: zero matrix
- `beta0`: zero matrix
- `m0`: each latent class has twice the membership than the previous one

### `prior`

- `eta = numeric(model[["P_f"]])`
- `Psi = matrix(1,model[["P_f"]],model[["P_f"]]); diag(Psi) = 5`
- `delta = 1`
- `xi = numeric(model[["P_r"]])`
- `D = matrix(1,model[["P_r"]],model[["P_r"]]); diag(D) = 5`
- `nu = model[["P_r"]]+2`
- `Theta = matrix(1,model[["P_r"]],model[["P_r"]]); diag(Theta) = 5`
- `kappa = model[["J"]]+1`
- `E = matrix(1,model[["J"]]-1,model[["J"]]-1); diag(E) = 5`

### `mcmc`
- `R = 10000`
- `B = R/2`
- `Q = 100`
- `nprint = floor(R/10)`

### `norm`
- `parameter = "s"`
- `index = "1"`
- `value = "1"`

### `out`
- `id = test`
- `rdir = tempdir()`
- `pp = FALSE`
- `return = FALSE`
- `waic = FALSE`

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
RprobitB::rpb("model" = model, "lcus" = lcus, "mcmc" = mcmc)
```
