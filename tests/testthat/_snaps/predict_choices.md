# P

    Code
      predict_choices(model, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  30   1
        car   2  22

# P train test

    Code
      predict_choices(model, data = data$test, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  13   3
        car   2   9

# MNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    20   2     0
        car     1  17     2
        train   0   0    13

# MMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    18   1     2
        car     2  16     2
        train   1   0    13

# LCMMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    31  11    17
        car    12  61    23
        train   7  18   120

# ULCMMNP

    Code
      predict_choices(model, overview = TRUE)
    Output
             predicted
      true    bus car train
        bus    42  22    33
        car    13 122    42
        train  16  33   177

