# P

    Code
      predict(model, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  33   2
        car   1  19

# P train test

    Code
      predict(model, data = data$test, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  13   0
        car   1  13

# MNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    18   2     0
        car     2  24     0
        train   0   0     9

# MMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    14   2     1
        car     2  26     1
        train   0   1     8

# LCMMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    35   8    13
        car     8  69    23
        train  15  15   114

# ULCMMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    56  31    20
        car    32 127    27
        train  19  27   161

