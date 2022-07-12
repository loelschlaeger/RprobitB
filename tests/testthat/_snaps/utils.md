# building delta matrix works

    Code
      RprobitB:::delta(3, 1)
    Output
           [,1] [,2] [,3]
      [1,]   -1    1    0
      [2,]   -1    0    1

# Gelman-Rubin statistic can be computed

    Code
      R_hat(samples)
    Output
      [1] 1.009085

# printing abbreviated matrices and vectors works

    Code
      RprobitB:::pprint(x = 1, name = "single integer")
    Output
      single integer : 1

---

    Code
      RprobitB:::pprint(x = LETTERS[1:26], name = "letters")
    Output
      letters : character vector of length 26 
      
      A B C ... Z

---

    Code
      RprobitB:::pprint(x = matrix(rnorm(100), ncol = 1), name = "single column matrix")
    Output
      single column matrix : 100 x 1 matrix of doubles 
      
                [,1]
      [1,]   -0.6973
      [2,]    1.1350
      [3,]    1.1119
      ...        ...
      [100,] -0.7666

---

    Code
      RprobitB:::pprint(x = matrix(1:100, nrow = 1), name = "single row matrix")
    Output
      single row matrix : 1 x 100 matrix of integers 
      
           [,1] [,2] [,3] ... [,100]
      [1,]    1    2    3 ...    100

---

    Code
      RprobitB:::pprint(x = matrix(LETTERS[1:24], ncol = 6), name = "big matrix")
    Output
      big matrix : 4 x 6 matrix of characters 
      
           [,1] [,2] [,3] ... [,6]
      [1,]    A    E    I ...    U
      [2,]    B    F    J ...    V
      [3,]    C    G    K ...    W
      [4,]    D    H    L ...    X

# computation of permutations works

    Code
      RprobitB:::permutations(x = c("a", "b", "c"))
    Output
      [[1]]
      [1] "a" "b" "c"
      
      [[2]]
      [1] "a" "c" "b"
      
      [[3]]
      [1] "b" "a" "c"
      
      [[4]]
      [1] "b" "c" "a"
      
      [[5]]
      [1] "c" "a" "b"
      
      [[6]]
      [1] "c" "b" "a"
      

