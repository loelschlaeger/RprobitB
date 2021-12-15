# P

    Code
      predict_choices(model, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  33   2
        car   1  19

# P train test

    Code
      predict_choices(model, data = data$test, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  13   0
        car   1  13

# MNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    20   1     0
        car     1  24     0
        train   0   0     9

# MMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    14   2     1
        car     2  26     1
        train   0   1     8

# LCMMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    37  10    15
        car    14  52    31
        train  15  13   113

# ULCMMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    68  28    19
        car    34 115    30
        train  21  26   159

