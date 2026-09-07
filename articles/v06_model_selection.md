# Model selection

The task of model selection targets the question: If there are several
competing models, how do I choose the most appropriate one? This
vignette[^1] outlines the model selection tools implemented in
[RprobitB](https://loelschlaeger.de/RprobitB/).

For illustration, we revisit the probit model of travelers deciding
between two fictional train route alternatives from [the vignette on
model
fitting](https://loelschlaeger.de/RprobitB/articles/v03_model_fitting.html):

``` r

form <- choice ~ price + time + change + comfort | 0
data <- prepare_data(form = form, choice_data = train_choice, id = "deciderID", idc = "occasionID")
model_train <- fit_model(
  data = data,
  scale = "price := -1",
  R = 1000, B = 500, Q = 10
)
```

As a competing model, we consider explaining the choices only by the
alternative’s price:[^2]

``` r

model_train_sparse <- update(model_train, form = choice ~ price | 0)
```

## The `model_selection()` function

The
[`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
function takes an arbitrary number of `RprobitB_fit` objects and returns
a matrix of model selection criteria:

``` r

model_selection(model_train, model_train_sparse)
#>      model_train model_train_sparse
#> npar           4                  1
#> LL      -1727.77           -1865.86
#> AIC      3463.54            3733.72
#> BIC      3487.47            3739.70
```

Specifying `criteria` for
[`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
is optional. Per default,
`criteria = c("npar", "LL", "AIC", "BIC")`.[^3] The available model
selection criteria are explained in the following.

### `npar`

`"npar"` yields the number of model parameters, which is computed by the
[`npar()`](https://loelschlaeger.de/RprobitB/reference/npar.md) method:

``` r

npar(model_train, model_train_sparse)
#> [1] 4 1
```

### `LL`

If `"LL"` is included in `criteria`,
[`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
returns the model’s log-likelihood values. They can also be directly
accessed via the [`logLik()`](https://rdrr.io/r/stats/logLik.html)
method:[^4]

``` r

logLik(model_train)
#> 'log Lik.' -1727.771 (df=4)
logLik(model_train_sparse)
#> 'log Lik.' -1865.861 (df=1)
```

### `AIC`

Including `"AIC"` yields the Akaike’s Information Criterion ([Akaike
1974](#ref-Akaike1974)), which is computed as
``` math
-2 \cdot \text{LL} + k \cdot \text{npar},
```
where $`\text{LL}`$ is the [model’s log-likelihood value](#ll), $`k`$ is
the penalty per parameter with $`k = 2`$ per default for the classical
AIC, and $`\text{npar}`$ is the number of parameters in the fitted
model.

Alternatively, the [`AIC()`](https://rdrr.io/r/stats/AIC.html) method
also returns the AIC values:

``` r

AIC(model_train, model_train_sparse, k = 2)
#>                    df      AIC
#> model_train         4 3463.543
#> model_train_sparse  1 3733.722
```

The AIC quantifies the trade-off between over- and under-fitting, where
smaller values are preferred. Here, the increase in goodness of fit
justifies the additional 3 parameters of `model_train`.

### `BIC`

Similar to the AIC, `"BIC"` yields the Bayesian Information Criterion
([Schwarz 1978](#ref-Schwarz1978)), which is defined as
``` math
-2 \cdot \text{LL} + \log{(\text{nobs})} \cdot \text{npar},
```
where $`\text{LL}`$ is the [model’s log-likelihood value](#ll),
$`\text{nobs}`$ is the number of data points (which can be accessed via
the [`nobs()`](https://rdrr.io/r/stats/nobs.html) method), and
$`\text{npar}`$ is the [number of parameters in the fitted
model](#npar). The interpretation is analogue to AIC.

[RprobitB](https://loelschlaeger.de/RprobitB/) also provides a method
for the `BIC` value:

``` r

BIC(model_train, model_train_sparse)
#>                    df      BIC
#> model_train         4 3487.472
#> model_train_sparse  1 3739.704
```

### `WAIC` (with `se(WAIC)` and `pWAIC`)

WAIC is short for Widely Applicable (or Watanabe-Akaike) Information
Criterion ([Watanabe and Opper 2010](#ref-Watanabe2010)). As for AIC and
BIC, the smaller the WAIC value the better the model. Including `"WAIC"`
in `criteria` yields the WAIC value, its standard error `se(WAIC)`, and
the effective number of parameters `pWAIC`, see below.

The WAIC is defined as

``` math
-2  \cdot \text{lppd} + 2\cdot p_\text{WAIC},
```

where $`\text{lppd}`$ stands for log pointwise predictive density and
$`p_\text{WAIC}`$ is a penalty term proportional to the variance in the
posterior distribution that is sometimes called effective number of
parameters, see McElreath ([2020](#ref-McElreath2020)) for a reference.

The $`\text{lppd}`$ is approximated as follows. Let
``` math
p_{si} = \Pr(y_i\mid \theta_s)
```
be the probability of observation $`y_i`$ (here the single choices)
given the $`s`$-th set $`\theta_s`$ of parameter samples from the
posterior. Then

``` math
\text{lppd} = \sum_i \log \left( S^{-1} \sum_s p_{si} \right).
```
The penalty term is computed as the sum over the variances in
log-probability for each observation:
``` math
p_\text{WAIC} = \sum_i \mathbb{V}_{\theta}  \log (p_{si}) . 
```
The $`\text{WAIC}`$ has a standard error of
``` math
\sqrt{n \cdot \mathbb{V}_i \left[-2 \left(\text{lppd} - \mathbb{V}_{\theta}  \log (p_{si})  \right)\right]},
```
where $`n`$ is the number of choices.

Before computing the WAIC of an object, the probabilities $`p_{si}`$
must be computed via the
[`compute_p_si()`](https://loelschlaeger.de/RprobitB/reference/compute_p_si.md)
function. This computation can be very time consuming. To decrease the
computation time, the function offers parallelization via specifying the
number `ncores` of parallel CPU cores.

``` r

model_train <- compute_p_si(model_train, ncores = 1)
model_train_sparse <- compute_p_si(model_train_sparse, ncores = 1)
```

Afterwards, the WAIC can be accessed as follows:

``` r

WAIC(model_train)
#> 3463.46 (0.21)
WAIC(model_train_sparse)
#> 3733.56 (0.06)
```

### `MMLL`

`"MMLL"` in `criteria` stands for marginal model log-likelihood. The
model’s marginal likelihood $`\Pr(y\mid M)`$ for a model $`M`$ and data
$`y`$ is required for the computation of Bayes factors, see below. In
general, the term has no closed form and must be approximated
numerically.

[RprobitB](https://loelschlaeger.de/RprobitB/) uses the posterior Gibbs
samples to approximate the model’s marginal likelihood via the posterior
harmonic mean estimator ([Newton and Raftery 1994](#ref-Newton1994)):
Let $`S`$ denote the number of available posterior samples
$`\theta_1,\dots,\theta_S`$. Then,
``` math
\Pr(y\mid M) = \left(\mathbb{E}_\text{posterior} 1/\Pr(y\mid \theta,M) \right)^{-1} \approx \left( \frac{1}{S} \sum_s 1/\Pr(y\mid \theta_s,M) \right) ^{-1} = \tilde{\Pr}(y\mid M).
```

By the law of large numbers, $`\tilde{\Pr}(y\mid M) \to \Pr(y\mid M)`$
almost surely as $`S \to \infty`$.

As for the [WAIC](#waic), computing the MMLL relies on the probabilities
$`p_{si} = \Pr(y_i\mid \theta_s)`$, which must first be computed via the
[`compute_p_si()`](https://loelschlaeger.de/RprobitB/reference/compute_p_si.md)
function. Afterwards, the
[`mml()`](https://loelschlaeger.de/RprobitB/reference/mml.md) function
can be called with an `RprobitB_fit` object as input. The function
returns the `RprobitB_fit` object, where the marginal likelihood value
is saved as the entry `"mml"` and the marginal log-likelihood value as
the attribute `"mmll"`:

``` r

model_train <- mml(model_train)
attr(model_train$mml, "mmll")
#> [1] -1731.584
```

There are two options for improving the approximation: As for the WAIC,
you can use more posterior samples. Alternatively, you can combine the
posterior harmonic mean estimate with the prior arithmetic mean
estimator ([Hammersley and Handscomb 1964](#ref-Hammersley1964)): For
this approach, $`S`$ samples $`\theta_1,\dots,\theta_S`$ are drawn from
the model’s prior distribution. Then,

``` math
\Pr(y\mid M) = \mathbb{E}_\text{prior} \Pr(y\mid \theta,M) \approx \frac{1}{S} \sum_s \Pr(y\mid \theta_s,M) = \tilde{\Pr}(y\mid M).
```

Again, it holds by the law of large numbers, that
$`\tilde{\Pr}(y\mid M) \to \Pr(y\mid M)`$ almost surely as
$`S \to \infty`$. The final approximation of the model’s marginal
likelihood than is a weighted sum of the posterior harmonic mean
estimate and the prior arithmetic mean estimate, where the weights are
determined by the sample sizes.

To use the prior arithmetic mean estimator, call the
[`mml()`](https://loelschlaeger.de/RprobitB/reference/mml.md) function
with a specification of the number of prior draws `S` and set
`recompute = TRUE`. Note that the prior arithmetic mean estimator works
well if the prior and posterior distribution have a similar shape and
strong overlap, as Gronau et al. ([2017](#ref-Gronau2017)) points out.
Otherwise, most of the sampled prior values result in a likelihood value
close to zero, thereby contributing only marginally to the
approximation. In this case, a very large number `S` of prior samples is
required.

### `BF`

The Bayes factor is an index of relative posterior model plausibility of
one model over another ([Marin and Robert 2014](#ref-Marin2014)). Given
data $`\texttt{y}`$ and two models $`\texttt{mod0}`$ and
$`\texttt{mod1}`$, it is defined as

``` math
BF(\texttt{mod0},\texttt{mod1}) = \frac{\Pr(\texttt{mod0} \mid \texttt{y})}{\Pr(\texttt{mod1} \mid \texttt{y})} = \frac{\Pr(\texttt{y} \mid \texttt{mod0} )}{\Pr(\texttt{y} \mid \texttt{mod1})} / \frac{\Pr(\texttt{mod0})}{\Pr(\texttt{mod1})}.
```

The ratio $`\Pr(\texttt{mod0}) / \Pr(\texttt{mod1})`$ expresses the
factor by which $`\texttt{mod0}`$ a priori is assumed to be the correct
model. Per default, [RprobitB](https://loelschlaeger.de/RprobitB/) sets
$`\Pr(\texttt{mod0}) = \Pr(\texttt{mod1}) = 0.5`$. The front part
$`\Pr(\texttt{y} \mid \texttt{mod0} ) / \Pr(\texttt{y} \mid \texttt{mod1})`$
is the ratio of the [marginal model likelihoods](#mmll). A value of
$`BF(\texttt{mod0},\texttt{mod1}) > 1`$ means that the model
$`\texttt{mod0}`$ is more strongly supported by the data under
consideration than $`\texttt{mod1}`$.

Adding `"BF"` to the `criteria` argument of `model_selection` yields the
Bayes factors. We find a decisive evidence ([Jeffreys
1998](#ref-Jeffreys1998)) in favour of `model_train`.

``` r

model_selection(model_train, model_train_sparse, criteria = "BF")
#>                          model_train model_train_sparse
#> BF(*,model_train)                  1             < 0.01
#> BF(*,model_train_sparse)       > 100                  1
```

### `pred_acc`

Finally, adding `"pred_acc"` to the `criteria` argument for the
[`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
function returns the share of correctly predicted choices. From the
output of
[`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md)
above (or alternatively the one in the following) we deduce that
`model_train` correctly predicts about 6% of the choices more than
`model_train_sparse`:[^5]

``` r

pred_acc(model_train, model_train_sparse)
#> [1] 0.6944350 0.6340048
```

## References

Akaike, H. 1974. “A New Look at the Statistical Model Identification.”
*IEEE Transactions on Automatic Control* 19.

Gronau, Q. F., A. Sarafoglou, D. Matzke, et al. 2017. “A Tutorial on
Bridge Sampling.” *Journal of Mathematical Psychology* 81.

Hammersley, J. M., and D. C. Handscomb. 1964. “General Principles of the
Monte Carlo Method.” *Springer Verlag*.

Jeffreys, H. 1998. *The Theory of Probability*. OUP Oxford.

Marin, J., and C. Robert. 2014. *Bayesian Essentials with R*. Springer
Verlag.

McElreath, R. 2020. *Statistical Rethinking: A Bayesian Course with
Examples in R and Stan*. Chapman; Hall/CRC.

Newton, M. A., and A. E. Raftery. 1994. “Approximate Bayesian Inference
with the Weighted Likelihood Bootstrap.” *Journal of the Royal
Statistical Society: Series B (Methodological)* 56 (1).

Schwarz, G. 1978. “Estimating the Dimension of a Model.” *The Annals of
Statistics* 6.

Watanabe, S., and M. Opper. 2010. “Asymptotic Equivalence of Bayes Cross
Validation and Widely Applicable Information Criterion in Singular
Learning Theory.” *Journal of Machine Learning Research* 11 (12).

[^1]: This vignette is built using R 4.6.1 with the
    [RprobitB](https://loelschlaeger.de/RprobitB/) 1.2.0.9000 package.

[^2]: The [`update()`](https://rdrr.io/r/stats/update.html) function
    helps to estimate a new version of `model_train` with new
    specifications. Here, only `form` has been changed.

[^3]: To show the model formulas in the output of
    [`model_selection()`](https://loelschlaeger.de/RprobitB/reference/model_selection.md),
    add the argument `add_form = TRUE`.

[^4]: The log-likelihood values are per default computed at the point
    estimates derived from the Gibbs sample means.
    [`logLik()`](https://rdrr.io/r/stats/logLik.html) has the argument
    `par_set`, where alternative statistics for the point estimate can
    be specified.

[^5]: See the vignette on choice prediction for more nuanced performance
    comparison in terms of sensitivity and specificity.
