#' Split choice data into train and test subset
#'
#' @description
#' This function splits choice data into a train and a test part.
#'
#' @param x
#' An object of class \code{RprobitB_data}.
#'
#' @param test_proportion \[`numeric(1)`\]\cr
#' The proportion of the test subset.
#'
#' @param test_number \[`integer(1)`\]\cr
#' The number of observations in the test subset.
#'
#' @param by \[`character(1)`\]\cr
#' Either \code{"N"} (split by deciders) and \code{"T"} (split by occasions).
#'
#' @param random \[`logical(1)`\]\cr
#' Build subsets randomly?
#'
#' @return
#' A list with two objects of class \code{RprobitB_data}, named \code{"train"}
#' and \code{"test"}.
#'
#' @examples
#' ### simulate choices for demonstration
#' x <- simulate_choices(form = choice ~ covariate, N = 10, T = 10, J = 2)
#'
#' ### 70% of deciders in the train subsample,
#' ### 30% of deciders in the test subsample
#' train_test(x, test_proportion = 0.3, by = "N")
#'
#' ### 2 randomly chosen choice occasions per decider in the test subsample,
#' ### the rest in the train subsample
#' train_test(x, test_number = 2, by = "T", random = TRUE)
#'
#' @export

train_test <- function(
    x, test_proportion = NULL, test_number = NULL, by = "N", random = FALSE
  ) {

  ### input checks
  oeli::input_check_response(
    check = checkmate::check_class(x, "RprobitB_data"),
    var_name = "x"
  )
  if (is.null(test_proportion) && is.null(test_number)) {
    stop(
      "Either 'test_proportion' or 'test_number' must be specified.",
      call. = FALSE
    )
  }
  if (!is.null(test_proportion) && !is.null(test_number)) {
    stop(
      "Only one of 'test_proportion' and 'test_number' can be specified.",
      call. = FALSE
    )
  }
  oeli::input_check_response(
    check = checkmate::check_number(
      test_proportion, lower = 0, upper = 1, null.ok = TRUE
    ),
    var_name = "test_proportion"
  )
  oeli::input_check_response(
    check = checkmate::check_count(test_number, null.ok = TRUE),
    var_name = "test_number"
  )
  oeli::input_check_response(
    check = checkmate::check_choice(by, choices = c("N", "T")),
    var_name = "by"
  )
  oeli::input_check_response(
    check = checkmate::check_flag(random),
    var_name = "random"
  )

  ### create train and test data set
  train <- x
  test <- x
  id <- unique(x$choice_data[[x$res_var_names$id]])

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
    ids <- x$choice_data[[x$res_var_names$id]] %in% id[ind_train]
    train$choice_data <- x$choice_data[ids, ]
    train$N <- sum(ind_train != 0)
    train$T <- train$T[ind_train]
    if (!identical(train$true_parameter$beta, NA)) {
      train$true_parameter$beta <-
        train$true_parameter$beta[, ind_train, drop = FALSE]
    }
    if (!identical(train$true_parameter$z, NA)) {
      train$true_parameter$z <- train$true_parameter$z[ind_train]
    }

    ### remove elements from 'test'
    test$data <- test$data[ind_test]
    ids <- x$choice_data[[x$res_var_names$id]] %in% id[ind_test]
    test$choice_data <- x$choice_data[ids, ]
    test$N <- sum(ind_test != 0)
    test$T <- test$T[ind_test]
    if (!identical(test$true_parameter$beta, NA)) {
      test$true_parameter$beta <-
        test$true_parameter$beta[, ind_test, drop = FALSE]
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
        warning(
          "Only ", x$T[n], " observation(s) available for decider ", n, ".",
          call. = FALSE, immediate. = TRUE
        )
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
        warning(
          "No observation(s) for decider ", n, " in test subsample.",
          call. = FALSE, immediate. = TRUE
        )
      }
      if (sum(ind_train != 0) == 0) {
        warning(
          "No observation(s) for decider ", n, " in train subsample.",
          call. = FALSE, immediate. = TRUE
        )
      }

      ### remove elements from 'train'
      train$data[[n]] <- list(
        "X" = train$data[[n]]$X[ind_train],
        "y" = train$data[[n]]$y[ind_train]
      )
      ids <- train$choice_data[[train$res_var_names$id]] == n &
        !train$choice_data[[train$res_var_names$idc]] %in% ind_train
      train$choice_data[ids, ] <- NA
      train$choice_data <- stats::na.omit(train$choice_data)
      train$T[n] <- sum(ind_train != 0)

      ### remove elements from 'test'
      test$data[[n]] <- list(
        "X" = test$data[[n]]$X[ind_test],
        "y" = test$data[[n]]$y[ind_test]
      )
      ids <- test$choice_data[[test$res_var_names$id]] == n &
        !test$choice_data[[test$res_var_names$idc]] %in% ind_test
      test$choice_data[ids, ] <- NA
      test$choice_data <- stats::na.omit(test$choice_data)
      test$T[n] <- sum(ind_test != 0)
    }
  }

  ### return train and test data set
  list("train" = train, "test" = test)
}
