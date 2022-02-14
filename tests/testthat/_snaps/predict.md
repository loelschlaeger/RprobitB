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
        bus    35   7    14
        car     9  63    28
        train  14  12   118

# ULCMMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    51  33    23
        car    29 127    30
        train  16  29   162

