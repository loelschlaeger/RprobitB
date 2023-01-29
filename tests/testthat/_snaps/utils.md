# print_matrix works

    Code
      print_matrix(x = 1)
    Output
      1

---

    Code
      print_matrix(x = 1.5, label = "single numeric")
    Output
      single numeric : 1.5

---

    Code
      print_matrix(x = 1.5, label = "single numeric", simplify = TRUE)
    Output
      single numeric : 1.5

---

    Code
      print_matrix(x = LETTERS[1:26])
    Output
      character vector of length 26 
      A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], label = "letters")
    Output
      letters : character vector of length 26 
      A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], label = "letters", details = FALSE)
    Output
      letters : A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], label = "letters", simplify = TRUE)
    Output
      letters : A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], rowdots = 1)
    Output
      character vector of length 26 
      A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], rowdots = 26)
    Output
      character vector of length 26 
      A B C ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], coldots = 1)
    Output
      character vector of length 26 
      ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], coldots = 25)
    Output
      character vector of length 26 
      A B C D E F G H I J K L M N O P Q R S T U V W X ... Z

---

    Code
      print_matrix(x = LETTERS[1:26], coldots = 26)
    Output
      character vector of length 26 
      A B C D E F G H I J K L M N O P Q R S T U V W X Y Z

---

    Code
      print_matrix(x = LETTERS[1:26], coldots = 10)
    Output
      character vector of length 26 
      A B C D E F G H I ... Z

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6))
    Output
      4 x 6 matrix of characters 
           [,1] [,2] [,3] ... [,6]
      [1,]    A    E    I ...    U
      [2,]    B    F    J ...    V
      [3,]    C    G    K ...    W
      [4,]    D    H    L ...    X

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6), label = "big matrix")
    Output
      big matrix : 4 x 6 matrix of characters 
           [,1] [,2] [,3] ... [,6]
      [1,]    A    E    I ...    U
      [2,]    B    F    J ...    V
      [3,]    C    G    K ...    W
      [4,]    D    H    L ...    X

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6), rowdots = 2)
    Output
      4 x 6 matrix of characters 
           [,1] [,2] [,3] ... [,6]
      [1,]    A    E    I ...    U
      ...   ...  ...  ... ...  ...
      [4,]    D    H    L ...    X

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6), details = FALSE)
    Output
           [,1] [,2] [,3] ... [,6]
      [1,]    A    E    I ...    U
      [2,]    B    F    J ...    V
      [3,]    C    G    K ...    W
      [4,]    D    H    L ...    X

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6), coldots = 1, rowdots = 1)
    Output
      4 x 6 matrix of characters 
           ... [,6]
      ...  ...  ...
      [4,] ...    X

---

    Code
      print_matrix(x = matrix(LETTERS[1:24], ncol = 6), simplify = TRUE, coldots = 2,
      rowdots = 2)
    Output
      [ A ... U; ... ... ...; D ... X ]

---

    Code
      print_matrix(x = diag(5), coldots = 2, rowdots = 3, simplify = TRUE, digits = 0)
    Output
      [ 1 ... 0; 0 ... 0; ... ... ...; 0 ... 1 ]

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1))
    Output
      1 x 100 matrix of doubles 
           [,1] [,2] [,3] ... [,100]
      [1,]    1    2    3 ...    100

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1), label = "single row matrix")
    Output
      single row matrix : 1 x 100 matrix of doubles 
           [,1] [,2] [,3] ... [,100]
      [1,]    1    2    3 ...    100

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1), details = FALSE)
    Output
           [,1] [,2] [,3] ... [,100]
      [1,]    1    2    3 ...    100

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1), coldots = 1)
    Output
      1 x 100 matrix of doubles 
           ... [,100]
      [1,] ...    100

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1), rowdots = 1)
    Output
      1 x 100 matrix of doubles 
           [,1] [,2] [,3] ... [,100]
      [1,]    1    2    3 ...    100

---

    Code
      print_matrix(x = matrix(1:100, nrow = 1), simplify = TRUE, coldots = 5)
    Output
      [ 1 2 3 4 ... 100 ]

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1))
    Output
      100 x 1 matrix of doubles 
             [,1]
      [1,]      1
      [2,]      2
      [3,]      3
      ...     ...
      [100,]  100

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1), label = "single column matrix")
    Output
      single column matrix : 100 x 1 matrix of doubles 
             [,1]
      [1,]      1
      [2,]      2
      [3,]      3
      ...     ...
      [100,]  100

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1), details = FALSE)
    Output
             [,1]
      [1,]      1
      [2,]      2
      [3,]      3
      ...     ...
      [100,]  100

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1), coldots = 1)
    Output
      100 x 1 matrix of doubles 
             [,1]
      [1,]      1
      [2,]      2
      [3,]      3
      ...     ...
      [100,]  100

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1), rowdots = 1)
    Output
      100 x 1 matrix of doubles 
             [,1]
      ...     ...
      [100,]  100

---

    Code
      print_matrix(x = matrix(1:100, ncol = 1), simplify = TRUE, coldots = 5)
    Output
      [ 1; 2; 3; ...; 100 ]

---

    Code
      print_matrix(x = diag(2), simplify = TRUE)
    Output
      [ 1 0; 0 1 ]

---

    Code
      print_matrix(x = diag(2), simplify = FALSE)
    Output
      2 x 2 matrix of doubles 
           [,1] [,2]
      [1,]    1    0
      [2,]    0    1

---

    Code
      x <- diag(3)
      rownames(x) <- c("la", "le", "lu")
      colnames(x) <- c("x", "y", "z")
      print_matrix(x)
    Output
      3 x 3 matrix of doubles 
         x y z
      la 1 0 0
      le 0 1 0
      lu 0 0 1

