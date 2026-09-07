# Weight-based class updates

Weight-based class updates

## Usage

``` r
update_classes_wb(
  s,
  b,
  Omega,
  epsmin = 0.01,
  epsmax = 0.7,
  deltamin = 0.1,
  deltashift = 0.5,
  identify_classes = FALSE,
  Cmax = 10L
)
```

## Arguments

- s:

  \[`numeric(C)`\]  
  The vector of class weights.

- b:

  \[`matrix(nrow = P_r, ncol = C)`\]  
  The matrix of class means as columns.

- Omega:

  \[`matrix(nrow = P_r * P_r, ncol = C)`\]  
  The matrix of vectorized class covariance matrices as columns.

- epsmin:

  \[`numeric(1)`\]  
  The threshold weight for removing a class.

- epsmax:

  \[`numeric(1)`\]  
  The threshold weight for splitting a class.

- deltamin:

  \[`numeric(1)`\]  
  The threshold difference in class means for joining two classes.

- deltashift:

  \[`numeric(1)`\]  
  The scale for shifting the class means after a split.

- identify_classes:

  \[`logical(1)`\]  
  Identify classes by decreasing class weights?

- Cmax:

  \[`integer(1)`\]  
  The maximum number of classes, used to allocate space.

## Value

A list of updated values for `s`, `b`, and `Omega` and the indicator
`update_type` which signals the type of class update:

- `0`: no update

- `1`: removed class

- `2`: split class

- `3`: merged classes

## Details

The following updating rules apply:

- Class \\c\\ is removed if \\s_c \< \epsilon\_{min}\\.

- Class \\c\\ is split into two classes, if \\s_c \> \epsilon\_{max}\\.

- Two classes \\c_1\\ and \\c_2\\ are merged to one class, if
  \\\|\|b\_{c_1} - b\_{c_2}\|\| \< \delta\_{min}\\.

## Examples

``` r
s <- c(0.7, 0.3)
b <- matrix(c(1, 1, 1, -1), ncol = 2)
Omega <- matrix(c(0.5, 0.3, 0.3, 0.5, 1, -0.1, -0.1, 0.8), ncol = 2)

### no update
update_classes_wb(s = s, b = b, Omega = Omega)
#> $s
#> [1] 0.7 0.3
#> 
#> $b
#>      [,1] [,2]
#> [1,]    1    1
#> [2,]    1   -1
#> 
#> $Omega
#>      [,1] [,2]
#> [1,]  0.5  1.0
#> [2,]  0.3 -0.1
#> [3,]  0.3 -0.1
#> [4,]  0.5  0.8
#> 
#> $update_type
#> [1] 0
#> 

### remove class 2
update_classes_wb(s = s, b = b, Omega = Omega, epsmin = 0.31)
#> $s
#> [1] 1
#> 
#> $b
#>      [,1]
#> [1,]    1
#> [2,]    1
#> 
#> $Omega
#>      [,1]
#> [1,]  0.5
#> [2,]  0.3
#> [3,]  0.3
#> [4,]  0.5
#> 
#> $update_type
#> [1] 1
#> 

### split class 1
update_classes_wb(s = s, b = b, Omega = Omega, epsmax = 0.69)
#> $s
#> [1] 0.35 0.35 0.30
#> 
#> $b
#>          [,1]      [,2] [,3]
#> [1,] 1.316228 0.6837722    1
#> [2,] 1.316228 0.6837722   -1
#> 
#> $Omega
#>      [,1] [,2] [,3]
#> [1,]  0.5  0.5  1.0
#> [2,]  0.3  0.3 -0.1
#> [3,]  0.3  0.3 -0.1
#> [4,]  0.5  0.5  0.8
#> 
#> $update_type
#> [1] 2
#> 

### merge classes 1 and 2
update_classes_wb(s = s, b = b, Omega = Omega, deltamin = 3)
#> $s
#> [1] 1
#> 
#> $b
#>      [,1]
#> [1,]    1
#> [2,]    0
#> 
#> $Omega
#>      [,1]
#> [1,] 0.75
#> [2,] 0.10
#> [3,] 0.10
#> [4,] 0.65
#> 
#> $update_type
#> [1] 3
#> 
```
