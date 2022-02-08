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
        bus    11   3     3
        car     0  29     0
        train   0   1     8

# LCMMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    41   7     8
        car    12  58    30
        train  17  12   115

# ULCMMNP

    Code
      predict(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    46  38    23
        car    24 136    26
        train  16  40   151

