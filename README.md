---
title: RprobitB README
author: Lennart Oelschläger
date: 04.05.2021
output: html_document
bibliography: ref.bib  
nocite: '@*'
---

**RprobitB** is an R package that can be used to fit *[latent class mixed multinomial probit (LCMMNP) models](#lcmmnp-model)* to simulated or empirical data. **RprobitB** is licensed under the [GNU General Public License v3.0](https://choosealicense.com/licenses/gpl-3.0/). 

The package name **RprobitB** is a portmanteau, combining *R* (the programming language), *probit* (the model name) and *B* (for Bayes, the estimation method).

**RprobitB** is able to

- estimate probit models on discrete choice panel data,
- approximate any underlying mixing distributions through a mixture of normal distributions,
- *[update the number of latent classes](#latent-class-updating-scheme)* within the algorithm on a weight-based strategy,
- perform estimation in a Bayesian framework via Gibbs sampling, thereby avoiding numerical maximization or approximation of the model's likelihood.

At this point, you may ask yourself one of the following questions:

1. [How to install RprobitB?](#installing-rprobitb)
2. [How to use RprobitB?](#using-rprobitb)
3. [How is the LCMMNP model defined?](#lcmmnp-model)
4. [How does the latent class updating scheme work?](#latent-class-updating-scheme)
5. [How to specify a LCMMNP model in RprobitB?](#specifying-a-lcmmnp-model-in-rprobitb)

Do you found a bug or request a feature? Please tell us: [lennart.oelschlaeger@uni-bielefeld.de](mailto:lennart.oelschlaeger@uni-bielefeld.de).

## Installing RprobitB

To install the latest version of **RprobitB**, run `install.packages("RprobitB")` in your R console.

## Using RprobitB

To use **RprobitB**, follow these steps:

1. Specify the model, see *[below](#specifying-a-lcmmnp-model-in-rprobitb)* for details.
2. Run `RprobitB::rpb(<list of model specifications>)`.
3. You get *[on-screen information](#on-screen-information)* and *[model results](#model-results)* in an output folder. 

## LCMMNP model
Assume that we observe the choices of $N$ decision makers which decide between $J$ alternatives at each of $T$ choice occasions.[^1] Specific to each decision maker, alternative and choice occasion, we furthermore observe $P_f+P_r$ choice attributes that we use to explain the choices. The first $P_f$ attributes are connected to fixed coefficients, the other $P_r$ attributes to random coefficients following a joint  distribution mixed across decision makers. Person $n$'s utility $\tilde{U}_{ntj}$ for alternative $j$ at choice occasion $t$ is modelled as
\begin{equation}
\tilde{U}_{ntj} = \tilde{W}_{ntj}'\alpha + \tilde{X}_{ntj}'\beta_n + \tilde{\epsilon}_{ntj}
\end{equation}
for $n=1,\dots,N$, $t=1,\dots,T$ and $j=1,\dots,J$, where

- $\tilde{W}_{ntj}$ is a vector of $P_f$ characteristics of $j$ as faced by  $n$ at $t$ corresponding to the fixed coefficient vector $\alpha \in {\mathbb R}^{P_f}$,
- $\tilde{X}_{ntj}$ is a vector of $P_r$ characteristics of $j$ as faced by  $n$ at $t$ corresponding to the random, decision maker-specific coefficient vector $\beta_n \in {\mathbb R}^{P_r}$, where $\beta_n$ is distributed according to some $P_r$-variate distribution $g_{P_r}$,
- and $(\tilde{\epsilon}_{nt:}) = (\tilde{\epsilon}_{nt1},\dots,\tilde{\epsilon}_{ntJ})' \sim \text{MVN}_{J} (0,\tilde{\Sigma})$ is the models' error term vector for $n$ at $t$, which in the probit model is assumed to be multivariate normally distributed with zero mean and covariance matrix $\tilde{\Sigma}$.

As is well known, any utility model needs to be normalized with respect to level and scale in order to be identified. Therefore, we consider the transformed model
\begin{equation}
U_{ntj} = W_{ntj}'\alpha + X_{ntj}'\beta_n + \epsilon_{ntj},
\end{equation}
$n=1,\dots,N$, $t=1,\dots,T$ and $j=1,\dots,J-1$, where (choosing $J$ as the reference alternative) $U_{ntj}=\tilde{U}_{ntj} - \tilde{U}_{ntJ}$, $W_{ntj}=\tilde{W}_{ntj}-\tilde{W}_{ntJ}$, $X_{ntj}=\tilde{X}_{ntj}-\tilde{X}_{ntJ}$ and $\epsilon_{ntj}=\tilde{\epsilon}_{ntj}-\tilde{\epsilon}_{ntJ}$, where $(\epsilon_{nt:}) = (\epsilon_{nt1},...,\epsilon_{nt(J-1)})'  \sim \text{MVN}_{J-1} (0,\Sigma)$ and $\Sigma$ denotes a covariance matrix with the top-left element restricted to one. While taking utility differences in order to normalize the model with respect to level is a standard procedure, alternatives to fixing an error term variance in order to normalize with respect to scale exist, for example fixing an element of $\alpha$.

Let $y_{nt}=j$ denote the event that decision maker $n$ chooses alternative $j$ at choice occasion $t$. Assuming utility maximizing behaviour of the decision makers, the decisions are linked to the utilities via
\begin{equation}
y_{nt} = \sum_{j=1}^{J-1} j\cdot 1 \left (U_{ntj}=\max_i U_{nti}>0 \right) + J \cdot 1\left (U_{ntj}<0 ~\text{for all}~j\right), 
\end{equation}
where $1(A)$ equals $1$ if condition $A$ is true and $0$ else.

We approximate the mixing distribution $g_{P_r}$ for the random coefficients\footnote{Here and below we use the abbreviation $(\beta_n)_n$ as a shortcut to $(\beta_n)_{n =1,...,N}$ the collection of vectors $\beta_n,n=1,...,N$.} $\beta=(\beta_n)_{n}$ by a mixture of $P_r$-variate normal densities $\phi_{P_r}$ with mean vectors $b=(b_c)_{c}$ and covariance matrices $\Omega=(\Omega_c)_{c}$ using $C$ components, i.e.
\begin{equation}
\beta_n\mid b,\Omega \sim \sum_{c=1}^{C} s_c \phi_{P_r} (\cdot \mid b_c,\Omega_c),
\end{equation}
where $(s_c)_{c}$ are weights satisfying $0 < s_c\leq 1$ for $c=1,\dots,C$ and $\sum_c s_c=1$. 
One interpretation of the latent class model is obtained by introducing variables $z=(z_n)_n$ allocating each decision maker $n$ to class $c$ with probability $s_c$, i.e.
\begin{equation}
\text{Prob}(z_n=c)=s_c \quad \text{and} \quad \beta_n \mid z,b,\Omega \sim \phi_{P_r}(\cdot \mid b_{z_n},\Omega_{z_n}).
\end{equation}
We call this model the *latent class mixed multinomial probit* (LCMMNP) model.[^2]

[^1]: For notational simplicity, the number of choice occasions $T$ is assumend to be the same for each decision maker here. However, **RprobitB** allows for a different number of choice occasions for each decision maker.
[^2]: We note that the model collapses to the (normally) mixed multinomial probit model if $P_r>0$ and $C=1$ and to the basic multinomial probit model if $P_r=0$.

## Latent class updating scheme
Updating the number $C$ of latent classes is done within the algorithm by executing the following weight-based updating scheme.

- Class $c$ is removed, if $s_c<\epsilon_{\text{min}}$, i.e. if the class weight $s_c$ drops below some threshold $\epsilon_{\text{min}}$. This case indicates that class $c$ has a negligible impact on the mixing distribution.
- Class $c$ is splitted into two classes $c_1$ and $c_2$, if $s_c>\epsilon_\text{max}$. This case indicates that class $c$ has a high influence on the mixing distribution whose approximation can potentially be improved by increasing the resolution in directions of high variance. Therefore, the class means $b_{c_1}$ and $b_{c_2}$ of the new classes $c_1$ and $c_2$ are shifted in opposite directions from the class mean $b_c$ of the old class $c$ in the direction of the highest variance.
- Two classes $c_1$ and $c_2$ are joined to one class $c$, if $\lVert b_{c_1} - b_{c_2} \rVert<\epsilon_{\text{distmin}}$, i.e. if the euclidean distance between the class means $b_{c_1}$ and $b_{c_2}$  drops below some threshold $\epsilon_{\text{distmin}}$. This case indicates location redundancy which should be repealed. The parameters of $c$ are assigned by adding the values of $s$ from $c_1$ and $c_2$ and averaging the values for $b$ and $\Omega$.

## Specifying a LCMMNP model in RprobitB

**RprobitB** specifications are grouped in the named lists 

- `model` (model information),
- `data` (data information),
- `parm` (true parameter values),
- `lcus` (latent class updating scheme parameters),
- `init` (initial values for the Gibbs sampler),
- `prior` (prior parameters),
- `mcmc` (Markov chain Monte Carlo parameters),
- `norm` (normalization information),
- `out` (output settings).

You can either specify none, all, or selected parameters. Unspecified parameters are set to *[default values](#default-specifications-of-rprobitb)*.

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

- `data_raw`, the data frame of choice data in wide format[^3], must contain columns named "id" (unique identifier for each decision maker) and "choice" (the chosen alternatives)
- `cov_col`, a numeric vector specifying the columns of `data_raw` with covariates
- `cov_ord`, a character vector specifying the order of the covariates, where fixed-coefficient covariates come first (required for specifying random coefficients and interpretation of the model results)
- `cov_zst`, a boolean determining whether covariates get z-standardized

[^3]: The *wide* data format presents each different covariate in a separate column. See the *[vignette of the mlogit package](https://cran.r-project.org/package=mlogit/vignettes/c2.formula.data.html#wide-format)* for an example.

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

**RprobitB** automatically normalizes with respect to level by computing utility differences, where `model[["J"]]` is the base alternative. The normalization with respect to scale can be specified:

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

## Example: Simulated data

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

## Example: Empirical data

The code below fits a mixed multinomial probit model with `P_f = 2` fixed  and `P_r = 2` random coefficients to the *["Train" dataset of the mlogit package](https://cran.r-project.org/package=mlogit/vignettes/c2.formula.data.html#wide-format)* with

- covariates in the columns `"cov_col" = 4:11`,
- "price" and "comfort" linked to fixed and "time" and "change" linked to random coefficients via `"cov_ord" = c("price","comfort","time","change")` (remember that fixed coefficients come first),
- and z-standardized covariates (`"cov_zst" = TRUE`).

For normalization, the price coefficient (`"parameter" = "a", "index" = 1`) is fixed to `"value" = -1`.

```r
### load Train data
data("Train", package = "mlogit")

### model specification
model = list("P_f" = 2, "P_r" = 2)
data  = list("data_raw" = Train, "cov_col" = 4:11, "cov_ord" = c("price","comfort","time","change"), "cov_zst" = TRUE)
lcus  = list("do_lcus" = TRUE)
mcmc  = list("R" = 1e5)
norm  = list("parameter" = "a", "index" = 1, "value" = -1)
out   = list("id" = "train", "pp" = TRUE)

### start estimation (about 20 minutes computation time)
RprobitB::rpb("model" = model, "data" = data, "lcus" = lcus, "mcmc" = mcmc, "norm" = norm, "out" = out)

```

## On-screen information
During estimation, you get the following on-screen information:

- a summary of the model, normalization, and Gibbs sampler settings, and (if `lcus[["do_lcus"]]=TRUE`) the latent class updating scheme parameters
- the sampling progress with expected time for completion (ETA)
- a summary of the posterior distribution (where *x.* denotes latent class number *x* in case of `lcus[["do_lcus"]]=TRUE`) with 
   - the true parameter values (only for simulated data)
   - the posterior mean
   - the posterior standard deviation
   - the 5% and 95% quantile of the posterior
   - the *[Gelman-Rubin statistic](https://bookdown.org/rdpeng/advstatcomp/monitoring-convergence.html#gelman-rubin-statistic)* (R^)
- the model's WAIC value (if `out[["waic"]]=TRUE`)
- the path to the model results

## Model results
In the output folder `out[["rdir"]]/out[["id"]]`, you can find the files

- *protocol.txt*, a copy of the on-screen information,
- *several .rds-files* of inputs and outputs,
- and different model visualizations:
   - *acf.pdf*, the autocorrelation of the Gibbs samples with the *[effective sample size](https://mc-stan.org/docs/2_18/reference-manual/effective-sample-size-section.html)*,
   - *marginal.pdf*, estimated marginal distributions,
   - *trace.pdf*, plots of the traces of the Gibbs samples,
   - and, if `model[["P_r"]] > 0`, *contour.pdf*, contour plot and progress contour plots (only if `out[["pp"]] = TRUE`) of the Gibbs samples.

## References
