#' Split choice data.
#'
#' @description
#' This function splits choice data into a train and a test part.
#'
#' @details
#' See the vignette "Choice data" for more details:
#' \code{vignette("choice_data", package = "RprobitB")}.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#' @param test_proportion
#' A number between 0 and 1 that gives the proportion of the test subsample.
#' @param test_number
#' A positive integer that gives the number of observations in the test subsample.
#' @param by
#' One of \code{"N"} (split by deciders) and \code{"T"} (split by choice occasions).
#' @param random
#' If \code{TRUE}, the subsamples are build randomly.
#' @param seed
#' Set a seed for building the subsamples randomly.
#'
#' @return
#' A list with two objects of class \code{RprobitB_data}, named \code{"train"}
#' and \code{"test"}.
#'
#' @examples
#' x <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#'
#' # 70% of deciders in the train subsample, 30% of deciders in the test subsample
#' train_test(x, test_proportion = 0.3, by = "N")
#'
#' # 2 randomly chosen choice occasions per decider in the test subsample, the rest
#' # in the train subsample
#' train_test(x, test_number = 2, by = "T", random = TRUE, seed = 1)
#'
#' @export
#'
#' @importFrom stats na.omit
#' @importFrom utils tail

train_test <- function(x, test_proportion = NULL, test_number = NULL, by = "N",
                       random = FALSE, seed = NULL) {

  ### input checks
  if (class(x) != "RprobitB_data") {
    stop("'x' must be of class 'RprobitB_data'.")
  }
  if (is.null(test_proportion) && is.null(test_number)) {
    stop("Either 'test_proportion' or 'test_number' must be specified.")
  }
  if (!is.null(test_proportion) && !is.null(test_number)) {
    stop("Only one of 'test_proportion' and 'test_number' can be specified.")
  }
  if (!is.null(test_proportion)) {
    if (!(is.numeric(test_proportion) && length(test_proportion) == 1 &&
      all(test_proportion >= 0) && all(test_proportion <= 1))) {
      stop("'test_proportion' must be a number between 0 and 1.")
    }
  }
  if (!is.null(test_number)) {
    if (!(is.numeric(test_number) && length(test_number) == 1 &&
      all(test_number >= 0) && all(test_number %% 1 == 0))) {
      stop("'test_number' must be a positive integer.")
    }
  }
  if (!(length(by) == 1 && by %in% c("N", "T"))) {
    stop("'by' must be 'N' or 'T'.")
  }
  if (!is.logical(random)) {
    stop("'random' must be a boolean.")
  }
  if (!is.null(seed)) {
    set.seed(seed)
  }

  ### create train and test data set
  train <- x
  test <- x

  if (by == "N") {
    ### split by deciders
    if (!is.null(test_proportion)) {
      size_test <- round(x$N * test_proportion)
    } else if (!is.null(test_number)) {
      size_test <- test_number
    }
    if (random) {
      ind_test <- sort(sample.int(x$N, size = size_test))
    } else {
      ind_test <- utils::tail(seq_len(x$N), size_test)
    }
    ind_train <- setdiff(seq_len(x$N), ind_test)

    ### remove elements from 'train'
    train$data <- train$data[ind_train]
    train$choice_data <- train$choice_data[train$choice_data$id %in% ind_train, ]
    train$N <- sum(ind_train != 0)
    train$T <- train$T[ind_train]
    if (!identical(train$true_parameter$beta, NA)) {
      train$true_parameter$beta <- train$true_parameter$beta[, ind_train, drop = FALSE]
    }
    if (!identical(train$true_parameter$z, NA)) {
      train$true_parameter$z <- train$true_parameter$z[ind_train]
    }

    ### remove elements from 'test'
    test$data <- test$data[ind_test]
    test$choice_data <- test$choice_data[test$choice_data$id %in% ind_test, ]
    test$N <- sum(ind_test != 0)
    test$T <- test$T[ind_test]
    if (!identical(test$true_parameter$beta, NA)) {
      test$true_parameter$beta <- test$true_parameter$beta[, ind_test, drop = FALSE]
    }
    if (!identical(test$true_parameter$z, NA)) {
      test$true_parameter$z <- test$true_parameter$z[ind_test]
    }
  } else if (by == "T") {
    ### split by choice occasions
    for (n in seq_len(x$N)) {
      if (!is.null(test_proportion)) {
        size_test <- round(x$T[n] * test_proportion)
      } else if (!is.null(test_number)) {
        size_test <- test_number
      }

      ### check 'size_test'
      if (size_test > x$T[n]) {
        warning("Only ", x$T[n], " observation(s) available for decider ", n, ".")
        size_test <- x$T[n]
      }

      if (random) {
        ind_test <- sort(sample.int(x$T[n], size = size_test))
      } else {
        ind_test <- utils::tail(seq_len(x$T[n]), size_test)
      }
      ind_train <- setdiff(seq_len(x$T[n]), ind_test)

      ### check 'ind_test' and 'ind_train'
      if (sum(ind_test != 0) == 0) {
        warning("No observation(s) for decider ", n, " in test subsample.")
      }
      if (sum(ind_train != 0) == 0) {
        warning("No observation(s) for decider ", n, " in train subsample.")
      }

      ### remove elements from 'train'
      train$data[[n]] <- list("X" = train$data[[n]]$X[ind_train], "y" = train$data[[n]]$y[ind_train])
      train$choice_data[train$choice_data$id == n & !train$choice_data$idc %in% ind_train, ] <- NA
      train$choice_data <- stats::na.omit(train$choice_data)
      train$T[n] <- sum(ind_train != 0)

      ### remove elements from 'test'
      test$data[[n]] <- list("X" = test$data[[n]]$X[ind_test], "y" = test$data[[n]]$y[ind_test])
      test$choice_data[test$choice_data$id == n & !test$choice_data$idc %in% ind_test, ] <- NA
      test$choice_data <- stats::na.omit(test$choice_data)
      test$T[n] <- sum(ind_test != 0)
    }
  }

  ### return train and test data set
  return(list("train" = train, "test" = test))
}
