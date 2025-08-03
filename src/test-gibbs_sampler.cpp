#include <testthat.h>
#include "gibbs_sampler.h"

context("update_s") {

  test_that("concentration s can be updated") {
    int delta = 1;
    arma::vec m(3);
    m[0] = 10;
    m[1] = 8;
    m[2] = 5;
    arma::vec s_update = update_s(delta, m);
    expect_true(s_update.size() == 3);
  }

}
