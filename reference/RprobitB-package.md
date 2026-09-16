# RprobitB: Bayesian probit choice modeling

Fits Bayesian probit models for binary, multinomial, ordered, and ranked
choices in cross-sectional and panel data.

## Details

### Model

Decider `n` faces `J` alternatives on choice occasion `t`. Alternative
`j` has the latent utility
`U[n,t,j] = X[n,t,j] %*% beta[n] + epsilon[n,t,j]`, where the covariate
vector `X[n,t,j]` follows from `formula` and the error vector across
alternatives is multivariate normal with covariance `Sigma`. Unordered
choices select the alternative with the largest utility, ranked choices
order all utilities, and ordered choices compare one utility with
increasing thresholds `gamma`.

Fixed coefficients are identical for all deciders. Random coefficients
named in `random_effects` vary across deciders and follow a multivariate
normal distribution with mean `mu` and covariance `Omega`. Effects named
in `latent_class_effects` differ between `classes` latent classes with
weights `weight`: a random effect then has class-specific means and
covariances, another coefficient one value per class. Log-normal effects
apply [`exp()`](https://rdrr.io/r/base/Log.html) or `-exp()` to the
latent normal coefficient. Utilities are identified only up to level and
scale, see the normalization details in
[`fit()`](https://loelschlaeger.de/RprobitB/reference/fit.md).

### Estimation

Posterior draws come from a Gibbs sampler that augments the latent
utilities (Albert and Chib 1993; McCulloch and Rossi 1994; Imai and van
Dyk 2005). Heterogeneity can follow a finite mixture, a sparse
overfitted finite mixture (Rousseau and Mengersen 2011;
Frühwirth-Schnatter and Malsiner-Walli 2019), or a Dirichlet process
mixture with Neal's (2000) auxiliary-parameter allocation update and the
precision update of Escobar and West (1995). Independent chains run
through the [**future**](https://future.futureverse.org/) framework, and
retained draws use the [**posterior**](https://mc-stan.org/posterior/)
format.

### Evaluation

[`predict()`](https://rdrr.io/r/stats/predict.html),
[`residuals()`](https://rdrr.io/r/stats/residuals.html),
[`logLik()`](https://rdrr.io/r/stats/logLik.html),
[`WAIC()`](https://loelschlaeger.de/RprobitB/reference/WAIC.md),
[`loo()`](https://loelschlaeger.de/RprobitB/reference/loo.RprobitB_fit.md),
and
[`bayes_factor()`](https://loelschlaeger.de/RprobitB/reference/bayes_factor.md)
evaluate choice probabilities and likelihoods with
[**choicedata**](https://loelschlaeger.de/choicedata/). Panel
likelihoods of mixed models integrate over the random coefficients with
the GHK simulator (Train 2009). Information criteria follow Vehtari,
Gelman, and Gabry (2017), and Bayes factors use bridge sampling (Gronau
et al. 2017).

## References

Albert JH, Chib S (1993). “Bayesian Analysis of Binary and Polychotomous
Response Data.” *Journal of the American Statistical Association*,
**88**(422), 669–679.
[doi:10.1080/01621459.1993.10476321](https://doi.org/10.1080/01621459.1993.10476321)
.

Escobar MD, West M (1995). “Bayesian Density Estimation and Inference
Using Mixtures.” *Journal of the American Statistical Association*,
**90**(430), 577–588.
[doi:10.1080/01621459.1995.10476550](https://doi.org/10.1080/01621459.1995.10476550)
.

Frühwirth-Schnatter S, Malsiner-Walli G (2019). “From Here to Infinity:
Sparse Finite versus Dirichlet Process Mixtures in Model-Based
Clustering.” *Advances in Data Analysis and Classification*, **13**(1),
33–64.
[doi:10.1007/s11634-018-0329-y](https://doi.org/10.1007/s11634-018-0329-y)
.

Gronau QF, Sarafoglou A, Matzke D, Ly A, Boehm U, Marsman M, Leslie DS,
Forster JJ, Wagenmakers E, Steingroever H (2017). “A Tutorial on Bridge
Sampling.” *Journal of Mathematical Psychology*, **81**, 80–97.
[doi:10.1016/j.jmp.2017.09.005](https://doi.org/10.1016/j.jmp.2017.09.005)
.

Imai K, van Dyk DA (2005). “A Bayesian Analysis of the Multinomial
Probit Model Using Marginal Data Augmentation.” *Journal of
Econometrics*, **124**(2), 311–334.
[doi:10.1016/j.jeconom.2004.02.002](https://doi.org/10.1016/j.jeconom.2004.02.002)
.

McCulloch RE, Rossi PE (1994). “An Exact Likelihood Analysis of the
Multinomial Probit Model.” *Journal of Econometrics*, **64**(1-2),
207–240.
[doi:10.1016/0304-4076(94)90064-7](https://doi.org/10.1016/0304-4076%2894%2990064-7)
.

Neal RM (2000). “Markov Chain Sampling Methods for Dirichlet Process
Mixture Models.” *Journal of Computational and Graphical Statistics*,
**9**(2), 249–265.
[doi:10.1080/10618600.2000.10474879](https://doi.org/10.1080/10618600.2000.10474879)
.

Oelschläger L, Bauer D (2021). “Bayes Estimation of Latent Class Mixed
Multinomial Probit Models.” In *Proceedings of the 100th Annual Meeting
of the Transportation Research Board*.
<https://trid.trb.org/view/1759753>.

Oelschläger L (2026). *Overcoming Challenges in Modeling Choice Behavior
Heterogeneity*. Ph.D. thesis, Bielefeld University, Bielefeld, Germany.
<https://pub.uni-bielefeld.de/record/3014719>.

Rousseau J, Mengersen K (2011). “Asymptotic Behaviour of the Posterior
Distribution in Overfitted Mixture Models.” *Journal of the Royal
Statistical Society: Series B (Statistical Methodology)*, **73**(5),
689–710.
[doi:10.1111/j.1467-9868.2011.00781.x](https://doi.org/10.1111/j.1467-9868.2011.00781.x)
.

Train KE (2009). *Discrete Choice Methods with Simulation*, 2 edition.
Cambridge University Press, Cambridge.
[doi:10.1017/CBO9780511805271](https://doi.org/10.1017/CBO9780511805271)
.

Vehtari A, Gelman A, Gabry J (2017). “Practical Bayesian Model
Evaluation Using Leave-One-Out Cross-Validation and WAIC.” *Statistics
and Computing*, **27**(5), 1413–1432.
[doi:10.1007/s11222-016-9696-4](https://doi.org/10.1007/s11222-016-9696-4)
.

## See also

Useful links:

- <https://loelschlaeger.de/RprobitB/>

- <https://github.com/loelschlaeger/RprobitB>

- Report bugs at <https://github.com/loelschlaeger/RprobitB/issues>

## Author

**Maintainer**: Lennart Oelschläger <oelschlaeger.lennart@gmail.com>
([ORCID](https://orcid.org/0000-0001-5421-9313))

Authors:

- Lennart Oelschläger <oelschlaeger.lennart@gmail.com>
  ([ORCID](https://orcid.org/0000-0001-5421-9313))

Other contributors:

- Dietmar Bauer <dietmar.bauer@uni-bielefeld.de>
  ([ORCID](https://orcid.org/0000-0003-2920-7032)) \[contributor\]
