# Compute ordered probit log-likelihood

Compute ordered probit log-likelihood

## Usage

``` r
ll_ordered(d, y, sys, Tvec)
```

## Arguments

- d:

  \[`numeric(J - 2)`\]  
  Threshold increments.

- y:

  \[`matrix(nrow = N, ncol = max(Tvec))`\]  
  Choices `1,...,J` for each decider in each choice occasion.

- sys:

  \[`matrix(nrow = N, ncol = max(Tvec))`\]  
  Systematic utilties for each decider in each choice occasion.

- Tvec:

  \[`integer(N)`\]  
  Number of choice occasions per decider.

## Value

The ordered probit log-likelihood value.

## Examples

``` r
d <- c(0, 0, 0)
y <- matrix(c(1, 2, 1, NA), ncol = 2)
sys <- matrix(c(0, 0, 0, NA), ncol = 2)
Tvec <- c(2, 1)
ll_ordered(d = d, y = y, sys = sys, Tvec = Tvec)
#> [1] -2.461157
```
