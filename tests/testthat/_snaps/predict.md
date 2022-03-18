# choice prediction works

    Code
      predict(model, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  21   1
        car   3   3

---

    Code
      predict(model, overview = FALSE)
    Output
         id idc          bus          car true predicted correct
      1   1   1 1.0000000000 1.356194e-12  bus       bus    TRUE
      2   2   1 0.7184736204 2.815264e-01  car       bus   FALSE
      3   2   2 0.8072964760 1.927035e-01  bus       bus    TRUE
      4   3   1 0.8835778216 1.164222e-01  bus       bus    TRUE
      5   3   2 0.9994933600 5.066400e-04  bus       bus    TRUE
      6   3   3 0.9608649471 3.913505e-02  bus       bus    TRUE
      7   4   1 0.9613344346 3.866557e-02  bus       bus    TRUE
      8   4   2 0.9999923684 7.631641e-06  bus       bus    TRUE
      9   4   3 0.8774825076 1.225175e-01  bus       bus    TRUE
      10  4   4 0.5864540759 4.135459e-01  bus       bus    TRUE
      11  5   1 0.9934363755 6.563624e-03  bus       bus    TRUE
      12  5   2 1.0000000000 7.494136e-16  bus       bus    TRUE
      13  5   3 0.9999999594 4.064446e-08  bus       bus    TRUE
      14  5   4 0.9641280873 3.587191e-02  bus       bus    TRUE
      15  5   5 0.9992695864 7.304136e-04  bus       bus    TRUE
      16  6   1 0.4448868338 5.551132e-01  bus       car   FALSE
      17  6   2 0.9742170679 2.578293e-02  bus       bus    TRUE
      18  6   3 0.9997245903 2.754097e-04  bus       bus    TRUE
      19  6   4 0.0007793819 9.992206e-01  car       car    TRUE
      20  6   5 0.9761241788 2.387582e-02  bus       bus    TRUE
      21  6   6 0.4858885967 5.141114e-01  car       car    TRUE
      22  7   1 0.9533036438 4.669636e-02  bus       bus    TRUE
      23  7   2 0.5071626696 4.928373e-01  car       bus   FALSE
      24  7   3 0.8135347260 1.864653e-01  bus       bus    TRUE
      25  7   4 0.0360823472 9.639177e-01  car       car    TRUE
      26  7   5 0.9991823061 8.176939e-04  bus       bus    TRUE
      27  7   6 0.9948218341 5.178166e-03  bus       bus    TRUE
      28  7   7 0.5133001557 4.866998e-01  car       bus   FALSE

---

    Code
      predict(model, data = data$test, overview = TRUE)
    Output
           predicted
      true  bus car
        bus  13   0
        car   1  13

