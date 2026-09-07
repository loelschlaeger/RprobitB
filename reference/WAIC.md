# Compute WAIC value

This function computes the WAIC value of an `RprobitB_fit` object.

## Usage

``` r
WAIC(x)

# S3 method for class 'RprobitB_waic'
print(x, digits = 2, ...)

# S3 method for class 'RprobitB_waic'
plot(x, ...)
```

## Arguments

- x:

  An object of class `RprobitB_fit`.

## Value

A numeric, the WAIC value, with the following attributes:

- `se_waic`, the standard error of the WAIC value,

- `lppd`, the log pointwise predictive density,

- `p_waic`, the effective number of parameters,

- `p_waic_vec`, the vector of summands of `p_waic`,

- `p_si`, the output of
  [`compute_p_si`](https://loelschlaeger.de/RprobitB/reference/compute_p_si.md).

## Details

WAIC is short for Widely Applicable (or Watanabe-Akaike) Information
Criterion. As for AIC and BIC, the smaller the WAIC value the better the
model. Its definition is \$\$WAIC = -2 \cdot lppd + 2 \cdot
p\_{WAIC},\$\$ where \\lppd\\ stands for log pointwise predictive
density and \\p\_{WAIC}\\ is a penalty term proportional to the variance
in the posterior distribution that is sometimes called effective number
of parameters. The \\lppd\\ is approximated as follows. Let \$\$p\_{is}
= \Pr(y_i\mid \theta_s)\$\$ be the probability of observation \\y_i\\
given the \\s\\th set \\\theta_s\\ of parameter samples from the
posterior. Then \$\$lppd = \sum_i \log S^{-1} \sum_s p\_{si}.\$\$ The
penalty term is computed as the sum over the variances in
log-probability for each observation: \$\$p\_{WAIC} = \sum_i V\_{\theta}
\left\[ \log p\_{si} \right\].\$\$
