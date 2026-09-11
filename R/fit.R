#' Fit a Bayesian probit choice model
#'
#' @description
#' Fits a Bayesian probit choice model to a `data.frame` of choice data. If
#' `data = NULL`, probit choice data are simulated with the
#' [**choicedata**](https://loelschlaeger.de/choicedata/) package before
#' estimation.
#'
#' @inherit choicedata::choice_formula params
#' @inherit choicedata::choice_data params
#'
#' @param data \[`data.frame` | `NULL`\]\cr
#' Empirical choice data. `NULL` simulates data before fitting. In long
#' format, a choice occasion may list only its available alternatives, see
#' the details on individual choice sets.
#'
#' @param alternatives \[`character()` | `NULL`\]\cr
#' Alternative labels. Required if `choice_type = "ordered"`, then in
#' increasing order of the response levels. Otherwise, `NULL` takes them
#' from `data` or, if `data = NULL`, uses capital letters.
#'
#' @param base \[`character(1)` | `NULL`\]\cr
#' The alternative whose utility is subtracted from all others, see the
#' details on the normalization. Coefficients that vary across
#' alternatives are then expressed relative to it, and none is estimated
#' for it. `NULL` uses the first model alternative. Not used if
#' `choice_type = "ordered"`, which has a single utility per occasion.
#'
#' @param choice_type \[`character(1)`\]\cr
#' What the response records, and how the model explains it:
#'
#' - `"unordered"`: the chosen alternative. Every alternative has its own
#'   utility, and the alternative with the greatest utility is chosen.
#' - `"ordered"`: a level of the ordered scale given by `alternatives`.
#'   One utility per choice occasion is compared with increasing
#'   thresholds, and the level is the interval it falls into.
#' - `"ranked"`: a complete ranking of the alternatives. Every alternative
#'   has its own utility, and the ranking orders them by utility.
#'
#' @param format \[`character(1)`\]\cr
#' The layout of `data`:
#'
#' - `"wide"` has one row per choice occasion, where covariate columns that
#'   vary across alternatives end in the alternative name.
#' - `"long"` has one row per choice occasion and alternative, named in
#'   `column_alternative`.
#'
#' @param scale \[`NULL` | named `numeric(1)`\]\cr
#' Utility scale normalization, see the details on the normalization. `NULL`
#' fixes the error variance of the first utility difference to one.
#' Otherwise one named value:
#'
#' - `c(<effect> = <value>)` fixes a non-random coefficient, e.g.
#'   `scale = c(price = -1)`.
#' - `c("Sigma_<alternative>,<alternative>" = <value>)` fixes the error
#'   variance of the utility difference between a non-base alternative and the
#'   base alternative to a positive value, e.g. `scale = c("Sigma_B,B" = 1)`.
#' - `c(Sigma = <value>)` fixes the error variance if
#'   `choice_type = "ordered"`.
#'
#' @param prior \[`named list()` | `NULL`\]\cr
#' Parameters of the prior distributions that replace the defaults, see the
#' details on the prior distribution for the component names and default
#' values.
#'
#' @param classes \[`integer(1)`\]\cr
#' Number of latent classes between which the `latent_class_effects` differ.
#' For `class_update = "dirichlet_process"` or `"weight_based"`, this is the
#' number of classes the sampler starts from.
#'
#' @param class_update \[`character(1)`\]\cr
#' Mixture specification:
#'
#' - `"fixed"` fits a finite mixture with `classes` components.
#' - `"sparse"` fits an overfitted finite mixture with `classes` components
#'   and infers the occupied number through a sparse Dirichlet weight prior.
#' - `"dirichlet_process"` samples the occupied class count with a Dirichlet
#'   process allocation sampler.
#' - `"weight_based"` adapts the number of components during warmup with a
#'   weight-threshold split, removal, and merge heuristic.
#'
#' @param max_classes \[`integer(1)`\]\cr
#' Largest number of latent classes the sampler may reach when
#' `class_update = "dirichlet_process"` or `"weight_based"` changes their
#' number. `fit()` warns if the retained draws reach it, then refit with a
#' larger value.
#'
#' @param weight_based_control \[`named list()` | `NULL`\]\cr
#' Tuning constants of `class_update = "weight_based"`, see the details on
#' the number of latent classes. `NULL` uses the defaults:
#'
#' - `buffer = 50`: minimum number of iterations between two updates.
#' - `epsmin = 0.01`: remove the smallest class if its weight falls below
#'   this value.
#' - `epsmax = 0.7`: split the largest class if its weight exceeds this
#'   value.
#' - `deltamin = 0.1`: merge the closest pair of classes if the distance
#'   of their means falls below this value.
#' - `deltashift = 0.5`: displacement of the two means after a split, in
#'   within-class standard deviations.
#'
#' @param iterations \[`integer(1)`\]\cr
#' Total MCMC iterations per chain, including warmup.
#'
#' @param warmup \[`integer(1)`\]\cr
#' Initial iterations discarded from each chain.
#'
#' @param thin \[`integer(1)`\]\cr
#' Interval between retained post-warmup draws.
#'
#' @param chains \[`integer(1)`\]\cr
#' Number of independent MCMC chains.
#'
#' @param save_individual_draws \[`logical(1)`\]\cr
#' Retain the posterior draws of the individual random coefficients?
#' They are required for `coef(level = "individual")` and
#' `predict(type = "conditional")`.
#'
#' Enable this only when you need them, because they dominate the memory
#' the fitted object occupies.
#'
#' @param n_deciders \[`integer(1)`\]\cr
#' Number of deciders to simulate when `data = NULL`.
#'
#' @param n_occasions \[`integer(1)` | `integer(n_deciders)`\]\cr
#' Simulated occasions for each decider.
#'
#' @param n_alternatives \[`integer(1)` | `NULL`\]\cr
#' Number of simulated alternatives. `NULL` uses `length(alternatives)` or,
#' if `alternatives = NULL`, two unordered or three ordered or ranked
#' alternatives labeled with capital letters.
#'
#' @param covariates \[`named list()` | `NULL`\]\cr
#' Optional covariate values for the simulated data. Names are covariate
#' columns of the simulated wide `data.frame`, such as `price_A`, and each
#' element is a vector with one value per simulated choice occasion.
#' Unspecified covariates are generated by
#' [choicedata::generate_choice_covariates()].
#'
#' @param dgp_parameters \[`named list()` | `NULL`\]\cr
#' The parameters that generate the simulated data, named like the
#' arguments of [choicedata::choice_parameters()]:
#'
#' - `beta`: the coefficient vector, or a list of one vector per latent
#'   class. A named vector is matched to the effects by name and may omit
#'   effects, whose coefficients are then drawn.
#' - `Omega`: the covariance matrix of the random effects, or a list of
#'   one matrix per latent class.
#' - `Sigma`: the error covariance matrix, or the error variance if
#'   `choice_type = "ordered"`.
#' - `gamma`: the thresholds if `choice_type = "ordered"`.
#' - `weights`: the class weights if `classes > 1`.
#'
#' Unspecified parameters are drawn at random.
#'
#' @param progress \[`logical(1)`\]\cr
#' Show progress?
#'
#' @return An `RprobitB_fit` object, which is a `list` with the components:
#'
#' - `call`: the matched call.
#' - `data`: the data used.
#' - `model`: the model specification.
#' - `prior`: the prior specification.
#' - `draws`: the posterior draws.
#' - `sampler`: iterations, warmup, thinning, chains, and elapsed times.
#' - `simulation`: the `dgp_parameters` that generated the data and the
#'   simulation sizes if `data = NULL`, otherwise `NULL`.
#'
#' @inheritSection choicedata::choice_formula Specifying the model formula
#' @inheritSection choicedata::choice_formula Specifying random effects
#' @inheritSection choicedata::choice_formula Specifying latent class effects
#'
#' @section Normalization:
#' Utilities are identified only up to level and scale. `base` selects the
#' alternative whose utility is subtracted from all others, and `scale` fixes
#' one parameter to identify the scale. The sampler fixes the error variance
#' of the first utility difference at one and draws the error covariance by
#' the marginal data augmentation of Imai and van Dyk (2005), which expands
#' the scale of the latent utilities in every iteration and returns to the
#' fixed one afterwards, so that the covariance and the coefficients move
#' freely. Every retained draw is then rescaled to the normalization in
#' `scale`.
#'
#' @section Individual choice sets:
#' Unordered choices may be made from occasion-specific subsets of the
#' alternatives. In long format, an occasion lists only the rows of its
#' available alternatives. The latent utilities of unavailable alternatives
#' are imputed from their conditional distribution without a truncation, so
#' they do not restrict the choice. Predictions assign probability zero to
#' unavailable alternatives. Ordered and ranked models require complete
#' choice sets.
#'
#' @section Random effects and mixture models:
#' An unnamed `random_effects` vector is a shorthand for correlated normal
#' effects, so `random_effects = c("price", "time")` is the same as
#' `random_effects = c(price = "cn", time = "cn")`.
#'
#' A mixture model divides the deciders into `classes` latent classes and
#' allocates every decider to exactly one of them. `latent_class_effects`
#' decides which mixture model this is:
#'
#' - A random effect named there follows a class-specific normal
#'   distribution. This is the latent class mixed probit model, reported as
#'   `mu[<effect>,<class>]` and `Omega[<effect>,<effect>,<class>]`.
#' - An effect that is named there but has no random effect is a single
#'   coefficient per class. This is the classical latent class model,
#'   reported as `beta[<effect>,<class>]`.
#'
#' @section Number of latent classes:
#' `class_update` decides how many of the latent classes are used:
#'
#' - `"fixed"` keeps all `classes` classes occupied, so the posterior is the
#'   finite-mixture posterior given that all of them are used.
#' - `"sparse"` starts from `classes` components, deliberately more than
#'   expected, and empties the superfluous ones through a small symmetric
#'   Dirichlet weight prior (Rousseau and Mengersen 2011;
#'   Frühwirth-Schnatter and Malsiner-Walli 2019).
#' - `"dirichlet_process"` creates and removes occupied components with
#'   Neal's (2000) auxiliary-parameter allocation update, up to
#'   `max_classes`. Its precision hyperparameter is updated with the
#'   beta-gamma augmentation of Escobar and West (1995).
#' - `"weight_based"` splits, removes, and merges components during warmup
#'   and then keeps their number fixed. Every `buffer` iterations it removes the
#'   component below `epsmin`, splits the component above `epsmax`, or merges
#'   the closest pair of component means below `deltamin`, attempting at most
#'   one change in that order. A split moves the two means by `deltashift` times
#'   the leading within-class standard deviation. Updates stop after warmup, and
#'   retained iterations condition on the dimension selected separately by each
#'   chain.
#'
#' @section Class labels:
#' Numbering the latent classes differently describes the same mixture, every
#' fit with more than one possible class therefore relabels its
#' retained draws before they are summarized. The representative assignment
#' of deciders to classes is the draw that is closest to the posterior
#' co-clustering matrix in the least-squares sense (Dahl 2006). Every draw
#' is then renumbered to agree with it, using the equivalence classes
#' representatives assignment of Papastamoulis and Iliopoulos (2010), and
#' the same permutation is applied to weights, means, covariances, and
#' allocations. Finally the classes are numbered by decreasing posterior
#' mean weight.
#'
#' @section Prior distribution:
#' The prior is conjugate where available; the finite-mixture concentration is
#' updated by a log-scale Metropolis-Hastings step when it has a gamma
#' hyperprior. `prior` is a named list that overrides the following defaults,
#' where `P_f`, `P_l`, and `P_r` count the fixed effects without latent
#' classes, the latent class effects, and the random effects, and `J` the
#' alternatives:
#'
#' - `fixed_mean` \[`numeric(P_f)`\] and `fixed_covariance`
#'   \[`matrix(P_f, P_f)`\]: normal prior for the fixed coefficients without
#'   latent classes, default `rep(0, P_f)` and `10 * diag(P_f)`.
#' - `latent_class_mean` \[`numeric(P_l)`\] and `latent_class_covariance`
#'   \[`matrix(P_l, P_l)`\]: normal prior for the class-specific values of
#'   the latent class effects, the same for every class, default
#'   `rep(0, P_l)` and `10 * diag(P_l)`.
#' - `random_mean` \[`numeric(P_r)`\] and `random_mean_covariance`
#'   \[`matrix(P_r, P_r)`\]: normal prior for the means of the random
#'   coefficients, default `rep(0, P_r)` and `10 * diag(P_r)`.
#' - `random_covariance_df` \[`integer(1)`\] and `random_covariance_scale`
#'   \[`matrix(P_r, P_r)`\]: inverse Wishart prior for the covariance
#'   matrices of the random coefficients, default `P_r + 2` and `diag(P_r)`.
#'   Entries between uncorrelated random effects must be zero. In a mixture,
#'   both priors apply to the block of the random effects with latent classes
#'   in every class and to the block without latent classes once.
#' - `class_concentration` \[`numeric(1)` | named `numeric(2)`\]: fixed
#'   symmetric Dirichlet concentration for finite weights or precision of the
#'   Dirichlet process. A named `c(shape = ..., rate = ...)` instead places a
#'   gamma hyperprior on it. Defaults are `1` for a fixed finite or weight-based
#'   mixture,
#'   `c(shape = 1, rate = 200)` for a sparse finite mixture, and
#'   `c(shape = 2, rate = 4)` for a Dirichlet process mixture.
#' - `error_covariance_df` \[`integer(1)`\] and `error_covariance_scale`
#'   \[`matrix(J - 1, J - 1)`\]: inverse Wishart prior for the unrestricted
#'   error covariance of the utility differences, default `J + 1` and
#'   `diag(J - 1)`. The prior of the identified covariance is that of the
#'   unrestricted covariance divided by its first diagonal element. Not used
#'   for ordered models.
#' - `threshold_mean` \[`numeric(J - 2)`\] and `threshold_covariance`
#'   \[`matrix(J - 2, J - 2)`\]: normal prior for the logarithmic increments
#'   between the ordered thresholds, default `rep(0, J - 2)` and
#'   `diag(J - 2)`. Only used for ordered models.
#'
#' @references
#' \insertRef{Dahl2006}{RprobitB}
#'
#' \insertRef{Escobar1995}{RprobitB}
#'
#' \insertRef{FruehwirthSchnatter2019}{RprobitB}
#'
#' \insertRef{Greene2003}{RprobitB}
#'
#' \insertRef{Imai2005a}{RprobitB}
#'
#' \insertRef{Neal2000}{RprobitB}
#'
#' \insertRef{Oelschlaeger2021}{RprobitB}
#'
#' \insertRef{Oelschlaeger2026c}{RprobitB}
#'
#' \insertRef{Papastamoulis2010}{RprobitB}
#'
#' \insertRef{Rousseau2011}{RprobitB}
#' @export
#' @keywords models
#'
#' @examples
#' ### Fit a probit model to panel choice data
#' data("train_choice", package = "choicedata")
#' model <- fit(
#'   choice ~ price + time + change + comfort | 0,
#'   data = train_choice,
#'   column_occasion = "occasionID",
#'   scale = c(price = -1), # other coefficients are willingness-to-pay
#'   chains = 1
#' )
#' summary(model)
#' interpret(model)
#'
#' ### Simulate choice data and compare the estimates with the truth
#' set.seed(1)
#' simulated <- fit(
#'   choice ~ x | y,
#'   dgp_parameters = list(beta = c(x = 1, y_B = -1)),
#'   covariates = list(y = rpois(400, lambda = 3)),
#'   iterations = 2000,
#'   chains = 1,
#'   n_deciders = 400
#' )
#' head(model.frame(simulated))
#' summary(simulated)
#' confint(simulated)

fit <- function(
  formula,
  data = NULL,
  random_effects = character(),
  latent_class_effects = character(),
  alternatives = NULL,
  base = NULL,
  choice_type = c("unordered", "ordered", "ranked"),
  format = c("wide", "long"),
  column_decider = "deciderID",
  column_occasion = NULL,
  column_alternative = NULL,
  delimiter = "_",
  scale = NULL,
  prior = NULL,
  classes = 1L,
  class_update = c("fixed", "sparse", "dirichlet_process", "weight_based"),
  max_classes = 10L,
  weight_based_control = NULL,
  iterations = 1000L,
  warmup = iterations %/% 2L,
  thin = 1L,
  chains = 4L,
  save_individual_draws = FALSE,
  n_deciders = 100L,
  n_occasions = 1L,
  n_alternatives = NULL,
  covariates = NULL,
  dgp_parameters = NULL,
  progress = interactive()
) {

  # input checks
  oeli::input_check_response(checkmate::check_formula(formula), "formula")
  oeli::input_check_response(
    checkmate::check_data_frame(data, null.ok = TRUE), "data"
  )
  if (missing(choice_type)) choice_type <- "unordered"
  oeli::input_check_response(
    checkmate::check_choice(
      choice_type,
      choices = c("unordered", "ordered", "ranked")
    ),
    "choice_type"
  )
  if (missing(format)) format <- "wide"
  oeli::input_check_response(
    checkmate::check_choice(format, choices = c("wide", "long")),
    "format"
  )
  if (missing(class_update)) class_update <- "fixed"
  oeli::input_check_response(
    checkmate::check_choice(
      class_update,
      choices = c("fixed", "sparse", "dirichlet_process", "weight_based")
    ),
    "class_update"
  )
  oeli::input_check_response(
    checkmate::check_int(classes, lower = 1L), "classes"
  )
  oeli::input_check_response(
    checkmate::check_character(
      alternatives,
      min.chars = 1, any.missing = FALSE,
      unique = TRUE, null.ok = TRUE
    ),
    "alternatives"
  )
  oeli::input_check_response(
    checkmate::check_string(base, min.chars = 1, null.ok = TRUE), "base"
  )
  oeli::input_check_response(
    checkmate::check_string(column_decider, min.chars = 1), "column_decider"
  )
  oeli::input_check_response(
    checkmate::check_string(column_occasion, min.chars = 1, null.ok = TRUE),
    "column_occasion"
  )
  oeli::input_check_response(
    checkmate::check_string(column_alternative, min.chars = 1, null.ok = TRUE),
    "column_alternative"
  )
  oeli::input_check_response(
    checkmate::check_string(delimiter, min.chars = 1), "delimiter"
  )
  oeli::input_check_response(
    checkmate::check_list(prior, null.ok = TRUE), "prior"
  )
  oeli::input_check_response(
    checkmate::check_list(weight_based_control, null.ok = TRUE),
    "weight_based_control"
  )
  oeli::input_check_response(
    checkmate::check_number(scale, finite = TRUE, null.ok = TRUE), "scale"
  )
  oeli::input_check_response(
    checkmate::check_int(max_classes, lower = 1L), "max_classes"
  )
  oeli::input_check_response(
    checkmate::check_int(iterations, lower = 2L), "iterations"
  )
  oeli::input_check_response(
    checkmate::check_int(warmup, lower = 0L, upper = iterations - 1L), "warmup"
  )
  oeli::input_check_response(
    checkmate::check_int(thin, lower = 1L, upper = iterations - 1L), "thin"
  )
  oeli::input_check_response(checkmate::check_int(chains, lower = 1L), "chains")
  oeli::input_check_response(checkmate::check_flag(progress), "progress")
  oeli::input_check_response(
    checkmate::check_flag(save_individual_draws), "save_individual_draws"
  )
  oeli::input_check_response(
    checkmate::check_int(n_deciders, lower = 1L), "n_deciders"
  )
  oeli::input_check_response(
    checkmate::check_integerish(
      n_occasions,
      lower = 1L, any.missing = FALSE, min.len = 1L
    ),
    "n_occasions"
  )
  oeli::input_check_response(
    checkmate::check_int(n_alternatives, lower = 2L, null.ok = TRUE),
    "n_alternatives"
  )
  oeli::input_check_response(
    checkmate::check_list(covariates, null.ok = TRUE), "covariates"
  )
  oeli::input_check_response(
    checkmate::check_list(dgp_parameters, null.ok = TRUE), "dgp_parameters"
  )
  mixing_types <- c("cn", "n", "cln", "ln", "cln-", "ln-")
  oeli::input_check_response(
    checkmate::check_character(
      random_effects,
      min.chars = 1, any.missing = FALSE
    ),
    "random_effects"
  )
  random_names <- names(random_effects)
  named_random <- length(random_effects) && !is.null(random_names)
  if (named_random) {
    oeli::input_check_response(
      checkmate::check_character(
        random_names,
        min.chars = 1, any.missing = FALSE, unique = TRUE
      ),
      "random_effects"
    )
    oeli::input_check_response(
      checkmate::check_subset(random_effects, choices = mixing_types),
      "random_effects"
    )
    random_specification <- random_effects
  } else {
    oeli::input_check_response(
      checkmate::check_character(
        random_effects,
        min.chars = 1, any.missing = FALSE, unique = TRUE
      ),
      "random_effects"
    )
    random_specification <- stats::setNames(
      rep("cn", length(random_effects)), random_effects
    )
  }
  oeli::input_check_response(
    checkmate::check_character(
      latent_class_effects, min.chars = 1, any.missing = FALSE, unique = TRUE
    ),
    "latent_class_effects"
  )
  changing_dimension <- class_update %in% c("dirichlet_process", "weight_based")
  if (changing_dimension && max_classes < classes) {
    oeli::input_check_response(
      "Must not be smaller than `classes`.", "max_classes"
    )
  }
  if (!changing_dimension && !missing(max_classes)) {
    oeli::input_check_response(
      paste(
        "Is only used for `class_update = \"dirichlet_process\"` or",
        "`class_update = \"weight_based\"`."
      ),
      "max_classes"
    )
  }
  if (
    !identical(class_update, "weight_based") &&
      !is.null(weight_based_control)
  ) {
    oeli::input_check_response(
      "Is only used for `class_update = \"weight_based\"`.",
      "weight_based_control"
    )
  }
  if (identical(class_update, "weight_based")) {
    control <- list(
      buffer = 50L, epsmin = 0.01, epsmax = 0.7, deltamin = 0.1,
      deltashift = 0.5
    )
    if (!is.null(weight_based_control)) {
      oeli::input_check_response(
        checkmate::check_character(
          names(weight_based_control),
          min.chars = 1, any.missing = FALSE, unique = TRUE
        ),
        "names(weight_based_control)"
      )
      unknown <- setdiff(names(weight_based_control), names(control))
      if (length(unknown)) {
        oeli::input_check_response(
          paste0(
            "Contains unknown component(s): ",
            paste(unknown, collapse = ", "), "."
          ),
          "weight_based_control"
        )
      }
      control[names(weight_based_control)] <- weight_based_control
    }
    oeli::input_check_response(
      checkmate::check_int(control$buffer, lower = 1L),
      "weight_based_control$buffer"
    )
    oeli::input_check_response(
      checkmate::check_number(control$epsmin, lower = 0, upper = 1),
      "weight_based_control$epsmin"
    )
    oeli::input_check_response(
      checkmate::check_number(control$epsmax, lower = 0, upper = 1),
      "weight_based_control$epsmax"
    )
    if (control$epsmin >= control$epsmax) {
      oeli::input_check_response(
        "Must be smaller than `weight_based_control$epsmax`.",
        "weight_based_control$epsmin"
      )
    }
    oeli::input_check_response(
      checkmate::check_number(control$deltamin, lower = 0, finite = TRUE),
      "weight_based_control$deltamin"
    )
    oeli::input_check_response(
      checkmate::check_number(control$deltashift, lower = 0, finite = TRUE),
      "weight_based_control$deltashift"
    )
    control$buffer <- as.integer(control$buffer)
    weight_based_control <- control
  }
  if (
    identical(class_update, "weight_based") &&
      warmup < weight_based_control$buffer
  ) {
    oeli::input_check_response(
      "Must be at least `weight_based_control$buffer`.", "warmup"
    )
  }
  if (identical(class_update, "sparse") && classes < 2L) {
    oeli::input_check_response(
      "Must be at least two for a sparse finite mixture.", "classes"
    )
  }
  if ((if (changing_dimension) max_classes else classes) > 15L) {
    oeli::input_check_response(
      "Relabeling currently supports at most 15 classes.",
      if (changing_dimension) "max_classes" else "classes"
    )
  }

  # parse model formula
  environment(formula) <- globalenv()
  choice_formula <- choicedata::choice_formula(
    formula = formula,
    error_term = "probit",
    random_effects = random_specification,
    latent_class_effects = latent_class_effects
  )
  response <- choice_formula$choice
  simulated <- is.null(data)
  simulation_parameters <- NULL

  # simulation arguments are only allowed without data
  if (!simulated) {
    supplied <- c(
      if (!missing(n_deciders)) "n_deciders",
      if (!missing(n_occasions)) "n_occasions",
      if (!missing(n_alternatives)) "n_alternatives",
      if (!missing(covariates)) "covariates",
      if (!missing(dgp_parameters)) "dgp_parameters"
    )
    if (length(supplied)) {
      oeli::input_check_response(
        paste0(
          "Simulation argument(s) ", paste(supplied, collapse = ", "),
          " can only be used when `data = NULL`."
        ),
        "data"
      )
    }
  }

  # the choice data are simulated or taken from the data frame
  if (simulated) {
    if (!length(n_occasions) %in% c(1L, n_deciders)) {
      oeli::input_check_response(
        "Must contain one value or one value per decider.", "n_occasions"
      )
    }
    J <- if (is.null(n_alternatives)) {
      if (is.null(alternatives)) {
        if (identical(choice_type, "unordered")) 2L else 3L
      } else {
        length(alternatives)
      }
    } else {
      n_alternatives
    }
    if (!is.null(alternatives) && length(alternatives) != J) {
      oeli::input_check_response(
        "Must have length `n_alternatives`.", "alternatives"
      )
    }
    if (is.null(alternatives)) {
      if (J > length(LETTERS)) {
        oeli::input_check_response(
          "Must be supplied for more than 26 alternatives.", "alternatives"
        )
      }
      alternatives <- LETTERS[seq_len(J)]
    }
    choice_alternatives <- choicedata::choice_alternatives(
      J = J,
      alternatives = alternatives,
      base = base,
      ordered = identical(choice_type, "ordered")
    )
    choice_effects <- choicedata::choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      delimiter = delimiter
    )
    simulation_occasion <- column_occasion
    if (is.null(simulation_occasion) && any(n_occasions > 1L)) {
      simulation_occasion <- "occasionID"
    }
    identifiers <- choicedata::generate_choice_identifiers(
      N = n_deciders,
      Tp = n_occasions,
      column_decider = column_decider,
      column_occasion = simulation_occasion
    )
    choice_covariates <- choicedata::generate_choice_covariates(
      choice_effects = choice_effects,
      choice_identifiers = identifiers,
      delimiter = delimiter
    )
    if (!is.null(covariates)) {
      oeli::input_check_response(
        checkmate::check_character(
          names(covariates),
          min.chars = 1, any.missing = FALSE, unique = TRUE
        ),
        "covariates"
      )
      unknown <- setdiff(
        names(covariates), choicedata::covariate_names(choice_effects)
      )
      if (length(unknown)) {
        oeli::input_check_response(
          paste0(
            "Contains unknown covariate(s): ",
            paste(unknown, collapse = ", "),
            "."
          ),
          "covariates"
        )
      }
      for (name in names(covariates)) {
        oeli::input_check_response(
          checkmate::check_atomic_vector(
            covariates[[name]],
            any.missing = FALSE,
            len = nrow(choice_covariates)
          ),
          paste0("covariates$", name)
        )
        choice_covariates[[name]] <- covariates[[name]]
      }
    }
    fixed_parameters <- if (is.null(dgp_parameters)) {
      choicedata::choice_parameters()
    } else {
      oeli::input_check_response(
        checkmate::check_character(
          names(dgp_parameters),
          min.chars = 1,
          any.missing = FALSE, unique = TRUE
        ),
        "dgp_parameters"
      )
      do.call(choicedata::choice_parameters, dgp_parameters)
    }
    if (classes > 1L && !any(choice_effects$latent_class)) {
      oeli::input_check_response(
        "Mixture models require an effect in `latent_class_effects`.",
        "classes"
      )
    }
    simulation_parameters <- choicedata::generate_choice_parameters(
      choice_effects = choice_effects,
      fixed_parameters = fixed_parameters,
      C = classes
    )
    choice_data <- choicedata::generate_choice_data(
      choice_effects = choice_effects,
      choice_identifiers = identifiers,
      choice_covariates = choice_covariates,
      choice_parameters = simulation_parameters,
      column_choice = response,
      choice_type = choice_type
    )
    column_occasion <- simulation_occasion
    format <- "wide"
  } else {
    if (identical(choice_type, "ordered")) {
      if (is.null(alternatives)) {
        oeli::input_check_response(
          "Must give the ordered response levels.", "alternatives"
        )
      }
    } else {
      observed <- if (identical(format, "long")) {
        has_alternative_column <- !is.null(column_alternative) &&
          column_alternative %in% names(data)
        if (!has_alternative_column) {
          oeli::input_check_response(
            "Must name a column in long data.", "column_alternative"
          )
        }
        unique(as.character(data[[column_alternative]]))
      } else {
        long <- choicedata::wide_to_long(
          data_frame = data,
          column_choice = response,
          column_alternative = ".alternative",
          delimiter = delimiter,
          choice_type = choice_type
        )
        unique(as.character(long$.alternative))
      }

      # the model alternatives must cover the ones in the data
      if (is.null(alternatives)) {
        alternatives <- observed
      } else {
        unnamed <- setdiff(stats::na.omit(observed), alternatives)
        if (length(unnamed)) {
          oeli::input_check_response(
            paste0(
              "Must name every alternative in `data`. Missing: ",
              paste(unnamed, collapse = ", "),
              ". To fit a smaller choice set, remove the choice occasions ",
              "of the other alternatives from `data`."
            ),
            "alternatives"
          )
        }
      }
    }
    choice_alternatives <- choicedata::choice_alternatives(
      J = length(alternatives),
      alternatives = alternatives,
      base = base,
      ordered = identical(choice_type, "ordered")
    )
    choice_data <- choicedata::choice_data(
      data_frame = data,
      format = format,
      column_choice = response,
      column_decider = column_decider,
      column_occasion = column_occasion,
      column_alternative = column_alternative,
      delimiter = delimiter,
      choice_type = choice_type
    )
    choice_effects <- choicedata::choice_effects(
      choice_formula = choice_formula,
      choice_alternatives = choice_alternatives,
      choice_data = choice_data,
      delimiter = delimiter
    )
  }

  # design matrices and responses of the observed choice occasions
  design <- choicedata::design_matrices(choice_data, choice_effects)
  J <- length(choice_alternatives)
  availability <- attr(design, "availability")
  complete <- all(lengths(availability) == J)
  if (!complete && !identical(choice_type, "unordered")) {
    oeli::input_check_response(
      "Every choice occasion must contain all model alternatives.", "data"
    )
  }
  identifiers <- choicedata::extract_choice_identifiers(choice_data)
  Tp <- attr(design, "Tp")
  alternatives <- as.character(choice_alternatives)
  data_roles <- list(
    format = format,
    column_decider = column_decider,
    column_occasion = column_occasion,
    column_alternative = column_alternative,
    delimiter = delimiter
  )

  # one response per occasion
  frame <- as.data.frame(choice_data)
  key_columns <- c(column_decider, column_occasion)
  key <- function(x) do.call(paste, c(unname(x[key_columns]), sep = "\r"))
  occasion_key <- key(identifiers)
  frame_key <- key(frame)
  response_values <- rep(NA_character_, nrow(identifiers))
  if (identical(choice_type, "ranked")) {
    ranks <- if (identical(format, "wide")) {
      rows <- match(occasion_key, frame_key)
      columns <- paste(response, alternatives, sep = delimiter)
      values <- as.matrix(frame[rows, columns, drop = FALSE])
      colnames(values) <- alternatives
      asplit(values, 1L)
    } else {
      split(
        stats::setNames(
          frame[[response]], as.character(frame[[column_alternative]])
        ),
        factor(frame_key, levels = occasion_key)
      )
    }
    response_values <- vapply(ranks, function(rank) {
      rank <- rank[!is.na(rank)]
      if (!length(rank)) {
        return(NA_character_)
      }
      paste(names(sort(rank)), collapse = ",")
    }, character(1), USE.NAMES = FALSE)
  } else if (identical(format, "long") &&
    identical(choice_type, "unordered")) {
    selected <- which(frame[[response]] == 1L)
    counts <- tabulate(
      match(frame_key[selected], occasion_key), nbins = length(occasion_key)
    )
    unique_choice <- counts == 1L
    rows <- selected[match(occasion_key[unique_choice], frame_key[selected])]
    response_values[unique_choice] <- as.character(
      frame[[column_alternative]][rows]
    )
  } else {
    answered <- which(!is.na(frame[[response]]))
    rows <- answered[match(occasion_key, frame_key[answered])]
    response_values <- as.character(frame[[response]][rows])
  }
  observed <- !is.na(response_values)
  if (!any(observed)) {
    oeli::input_check_response(
      "Must contain at least one observed response.", "data"
    )
  }

  # the random effects and the data passed to the sampler
  random <- !is.na(choice_effects$mixing)
  random_mixing <- as.character(choice_effects$mixing[random])
  random_distribution <- ifelse(
    sub("^c", "", random_mixing) == "n",
    0L,
    ifelse(endsWith(random_mixing, "-"), -1L, 1L)
  )
  random_correlated <- startsWith(random_mixing, "c")
  in_lc <- choice_effects$latent_class
  latent_class <- in_lc & !random
  mixture <- classes > 1L || !identical(class_update, "fixed")
  if (mixture && !any(in_lc)) {
    oeli::input_check_response(
      "Mixture models require an effect in `latent_class_effects`.",
      if (classes > 1L) "classes" else "class_update"
    )
  }
  if (any(in_lc) && !mixture) {
    oeli::input_check_response(
      "Latent class effects require more than one class.", "classes"
    )
  }
  if (
    any(random_correlated & in_lc[random]) &&
      any(random_correlated & !in_lc[random])
  ) {
    correlated_effects <- choice_effects$effect_name[random][random_correlated]
    correlated_class <- in_lc[random][random_correlated]
    cli::cli_warn(
      c(
        paste0(
          "Random effects with and without latent classes are uncorrelated, ",
          "so no covariance is estimated between ",
          paste0("`", correlated_effects[correlated_class], "`",
                 collapse = ", "), " and ",
          paste0("`", correlated_effects[!correlated_class], "`",
                 collapse = ", "), "."
        ),
        "i" = "Name all of them in `latent_class_effects` to correlate them."
      ),
      call = NULL
    )
  }
  if (identical(class_update, "weight_based") && !any(random & in_lc)) {
    oeli::input_check_response(
      paste(
        "The weight-based class update requires a random effect with a",
        "latent class effect."
      ),
      "class_update"
    )
  }
  sampler_identifiers <- identifiers[observed, , drop = FALSE]
  sampler_deciders <- unique(sampler_identifiers[[column_decider]])
  sampler_data <- list(
    design = unclass(design[observed]),
    responses = response_values[observed],
    availability = if (!complete) availability[observed],
    N = length(sampler_deciders),
    T = tabulate(
      match(sampler_identifiers[[column_decider]], sampler_deciders),
      nbins = length(sampler_deciders)
    ),
    J = J,
    P_f = sum(!random & !latent_class),
    P_l = sum(latent_class),
    P_r = sum(random),
    latent_class = latent_class[!random],
    random_distribution = as.integer(random_distribution),
    random_correlated = as.logical(random_correlated),
    random_class_specific = in_lc[random],
    alternatives = alternatives,
    ordered = identical(choice_type, "ordered"),
    ranked = identical(choice_type, "ranked")
  )
  if (
    !class_update %in% c("sparse", "weight_based") && classes > sampler_data$N
  ) {
    oeli::input_check_response(
      "Cannot exceed the number of deciders if every class must be occupied.",
      "classes"
    )
  }

  # the latent class settings of the sampler
  latent_classes <- list(
    C = as.integer(classes),
    weight_based_update = identical(class_update, "weight_based"),
    dp_update = identical(class_update, "dirichlet_process"),
    nonempty = identical(class_update, "fixed")
  )
  if (changing_dimension) {
    # more occupied classes than deciders are impossible
    max_classes <- min(max_classes, sampler_data$N)
    latent_classes$Cmax <- as.integer(max_classes)
  }
  if (identical(class_update, "weight_based")) {
    latent_classes <- c(latent_classes, weight_based_control)
  }

  # the normalization fixes the utility level and scale
  level <- if (sampler_data$ordered) {
    NA
  } else {
    list(
      level = match(attr(choice_alternatives, "base"), alternatives),
      name = attr(choice_alternatives, "base")
    )
  }
  scale_specification <- if (is.null(scale)) {
    list(parameter = "Sigma", index = 1L, value = 1, name = NA_character_)
  } else {
    oeli::input_check_response(
      checkmate::check_character(
        names(scale),
        min.chars = 1, any.missing = FALSE, len = 1L
      ),
      "scale"
    )
    parameter <- names(scale)
    effect_index <- match(parameter, choice_effects$effect_name)
    if (!is.na(effect_index)) {
      if (random[effect_index]) {
        oeli::input_check_response("Cannot fix a random coefficient.", "scale")
      }
      if (scale == 0) {
        oeli::input_check_response(
          "A fixed coefficient must be non-zero.", "scale"
        )
      }
      if (latent_class[effect_index]) {
        oeli::input_check_response(
          "Cannot fix a coefficient with latent classes.", "scale"
        )
      }
      list(
        parameter = "alpha",
        index = match(effect_index, which(!random & !latent_class)),
        value = unname(scale),
        name = parameter
      )
    } else {
      variance <- sub("^Sigma_", "", parameter)
      variance <- strsplit(variance, ",", fixed = TRUE)[[1]]
      if (sampler_data$ordered) {
        valid_variance <- identical(parameter, "Sigma")
        variance_index <- 1L
      } else {
        valid_variance <- length(variance) == 2L &&
          identical(variance[1], variance[2])
        variance_index <- match(
          variance[1], alternatives[-level$level]
        )
        valid_variance <- valid_variance && !is.na(variance_index)
      }
      if (!valid_variance || scale <= 0) {
        oeli::input_check_response(
          "Must name a fixed coefficient or positive estimable variance.",
          "scale"
        )
      }
      list(
        parameter = "Sigma",
        index = variance_index,
        value = unname(scale),
        name = parameter
      )
    }
  }
  normalization <- structure(
    list(level = level, scale = scale_specification),
    class = "RprobitB_normalization"
  )

  # the prior combines the applicable components with their defaults
  P_f <- sampler_data$P_f
  P_l <- sampler_data$P_l
  P_r <- sampler_data$P_r
  ordered <- sampler_data$ordered
  if (is.null(prior)) prior <- list()
  if (length(prior)) {
    oeli::input_check_response(
      checkmate::check_character(
        names(prior), min.chars = 1, any.missing = FALSE, unique = TRUE
      ),
      "prior"
    )
  }
  applicable <- c(
    if (P_f) "fixed_mean",
    if (P_f) "fixed_covariance",
    if (P_l) "latent_class_mean",
    if (P_l) "latent_class_covariance",
    if (P_r || P_l) "class_concentration",
    if (P_r) "random_mean",
    if (P_r) "random_mean_covariance",
    if (P_r) "random_covariance_df",
    if (P_r) "random_covariance_scale",
    if (!ordered) "error_covariance_df",
    if (!ordered) "error_covariance_scale",
    if (ordered) "threshold_mean",
    if (ordered) "threshold_covariance"
  )
  unknown <- setdiff(names(prior), applicable)
  if (length(unknown)) {
    oeli::input_check_response(
      paste0(
        "Contains inapplicable component(s): ",
        paste(unknown, collapse = ", "), "."
      ),
      "prior"
    )
  }
  prior_values <- list(
    fixed_mean = numeric(P_f),
    fixed_covariance = 10 * diag(P_f),
    latent_class_mean = numeric(P_l),
    latent_class_covariance = 10 * diag(P_l),
    class_concentration = switch(class_update,
      fixed = 1,
      sparse = c(shape = 1, rate = 200),
      dirichlet_process = c(shape = 2, rate = 4),
      weight_based = 1
    ),
    random_mean = numeric(P_r),
    random_mean_covariance = 10 * diag(P_r),
    random_covariance_df = P_r + 2L,
    random_covariance_scale = diag(P_r),
    error_covariance_df = J + 1L,
    error_covariance_scale = diag(J - 1L),
    threshold_mean = numeric(J - 2L),
    threshold_covariance = diag(J - 2L)
  )[applicable]
  prior_values[names(prior)] <- prior
  if (P_f) {
    oeli::input_check_response(
      oeli::check_numeric_vector(
        prior_values$fixed_mean, finite = TRUE, any.missing = FALSE, len = P_f
      ),
      "prior$fixed_mean"
    )
    oeli::input_check_response(
      oeli::check_covariance_matrix(prior_values$fixed_covariance, dim = P_f),
      "prior$fixed_covariance"
    )
  }
  if (P_l) {
    oeli::input_check_response(
      oeli::check_numeric_vector(
        prior_values$latent_class_mean, finite = TRUE, any.missing = FALSE,
        len = P_l
      ),
      "prior$latent_class_mean"
    )
    oeli::input_check_response(
      oeli::check_covariance_matrix(
        prior_values$latent_class_covariance, dim = P_l
      ),
      "prior$latent_class_covariance"
    )
  }
  if (P_r || P_l) {
    concentration <- prior_values$class_concentration
    oeli::input_check_response(
      oeli::check_numeric_vector(
        concentration, lower = 0, finite = TRUE, any.missing = FALSE,
        min.len = 1L, max.len = 2L
      ),
      "prior$class_concentration"
    )
    if (any(concentration == 0)) {
      oeli::input_check_response(
        "Must be greater than zero.", "prior$class_concentration"
      )
    }
    if (
      length(concentration) == 2L &&
        !setequal(names(concentration), c("shape", "rate"))
    ) {
      oeli::input_check_response(
        "A hyperprior must be named `c(shape = ..., rate = ...)`.",
        "prior$class_concentration"
      )
    }
  }
  if (P_r) {
    oeli::input_check_response(
      oeli::check_numeric_vector(
        prior_values$random_mean, finite = TRUE, any.missing = FALSE,
        len = P_r
      ),
      "prior$random_mean"
    )
    oeli::input_check_response(
      oeli::check_covariance_matrix(
        prior_values$random_mean_covariance, dim = P_r
      ),
      "prior$random_mean_covariance"
    )
    oeli::input_check_response(
      checkmate::check_int(prior_values$random_covariance_df, lower = P_r),
      "prior$random_covariance_df"
    )
    oeli::input_check_response(
      oeli::check_covariance_matrix(
        prior_values$random_covariance_scale, dim = P_r
      ),
      "prior$random_covariance_scale"
    )
    covariance_mask <- outer(random_correlated, random_correlated, `&`)
    diag(covariance_mask) <- TRUE
    if (any(
      abs(prior_values$random_covariance_scale[!covariance_mask]) >
        sqrt(.Machine$double.eps)
    )) {
      oeli::input_check_response(
        "Must be zero between uncorrelated random effects.",
        "prior$random_covariance_scale"
      )
    }
  }
  if (!ordered) {
    oeli::input_check_response(
      checkmate::check_int(prior_values$error_covariance_df, lower = J - 1L),
      "prior$error_covariance_df"
    )
    oeli::input_check_response(
      oeli::check_covariance_matrix(
        prior_values$error_covariance_scale, dim = J - 1L
      ),
      "prior$error_covariance_scale"
    )
  } else {
    oeli::input_check_response(
      oeli::check_numeric_vector(
        prior_values$threshold_mean, finite = TRUE, any.missing = FALSE,
        len = J - 2L
      ),
      "prior$threshold_mean"
    )
    if (J > 2L) {
      oeli::input_check_response(
        oeli::check_covariance_matrix(
          prior_values$threshold_covariance, dim = J - 2L
        ),
        "prior$threshold_covariance"
      )
    }
  }
  concentration <- if (P_r || P_l) {
    prior_values$class_concentration
  } else {
    NA_real_
  }
  hyperprior <- (P_r || P_l) && length(concentration) == 2L
  prior_sampler <- list(
    mu_alpha_0 = if (P_f) prior_values$fixed_mean else NA,
    Sigma_alpha_0 = if (P_f) prior_values$fixed_covariance else NA,
    mu_lambda_0 = if (P_l) prior_values$latent_class_mean else NA,
    Sigma_lambda_0 = if (P_l) prior_values$latent_class_covariance else NA,
    delta = if (hyperprior) {
      unname(concentration["shape"] / concentration["rate"])
    } else {
      unname(concentration[1L])
    },
    sample_delta = hyperprior,
    delta_shape = if (hyperprior) unname(concentration["shape"]) else NA_real_,
    delta_rate = if (hyperprior) unname(concentration["rate"]) else NA_real_,
    mu_b_0 = if (P_r) prior_values$random_mean else NA,
    Sigma_b_0 = if (P_r) prior_values$random_mean_covariance else NA,
    n_Omega_0 = if (P_r) prior_values$random_covariance_df else NA,
    V_Omega_0 = if (P_r) prior_values$random_covariance_scale else NA,
    n_Sigma_0 = if (!ordered) prior_values$error_covariance_df else NA,
    V_Sigma_0 = if (!ordered) prior_values$error_covariance_scale else NA,
    mu_d_0 = if (ordered) prior_values$threshold_mean else NA,
    Sigma_d_0 = if (ordered) prior_values$threshold_covariance else NA
  )

  # the design matrices are differenced with respect to the base alternative
  Tvec <- as.integer(sampler_data$T)
  transformed_design <- if (ordered) {
    lapply(sampler_data$design, function(x) matrix(as.numeric(x), nrow = 1L))
  } else {
    difference <- oeli::delta(ref = normalization$level$level, dim = J)
    lapply(sampler_data$design, function(x) difference %*% x)
  }

  # the responses are coded as indices into the choice set
  choice_set <- if (sampler_data$ranked) {
    vapply(
      oeli::permutations(alternatives), paste, character(1), collapse = ","
    )
  } else {
    alternatives
  }
  response_index <- match(sampler_data$responses, choice_set)
  if (!ordered && !sampler_data$ranked) {
    non_base <- setdiff(seq_len(J), normalization$level$level)
    response_index <- match(response_index, non_base)
    response_index[is.na(response_index)] <- J
  }
  y <- matrix(NA_integer_, nrow = sampler_data$N, ncol = max(Tvec))
  starts <- cumsum(Tvec) - Tvec + 1L
  ends <- cumsum(Tvec)
  for (n in seq_len(sampler_data$N)) {
    y[n, seq_len(Tvec[n])] <- response_index[starts[n]:ends[n]]
  }

  # availability in the differenced coding, with the base alternative last
  available <- if (is.null(sampler_data$availability)) {
    NULL
  } else {
    base_last <- c(
      setdiff(seq_len(J), normalization$level$level), normalization$level$level
    )
    available <- t(vapply(sampler_data$availability, function(set) {
      as.integer(seq_len(J) %in% set)[base_last]
    }, integer(J)))
    if (any(available[cbind(seq_len(nrow(available)), response_index)] != 1L)) {
      oeli::input_check_response(
        "Chosen alternatives must be available.", "data"
      )
    }
    available
  }

  # the design matrices are split by effect type, and their cross products
  # and the differencing matrices of ranked choices are precomputed
  fixed_columns <- seq_len(P_f + P_l)
  in_class <- sampler_data$latent_class
  W <- select_columns(transformed_design, fixed_columns[!in_class])
  L <- select_columns(transformed_design, fixed_columns[in_class])
  X <- select_columns(transformed_design, P_f + P_l + seq_len(P_r))
  LkL <- if (P_l) {
    lapply(seq_len(sampler_data$N), function(n) {
      cross_product_sum(L[starts[n]:ends[n]], P_l, J, ordered)
    })
  } else {
    NA
  }
  XkX <- if (P_r) {
    lapply(seq_len(sampler_data$N), function(n) {
      cross_product_sum(X[starts[n]:ends[n]], P_r, J, ordered)
    })
  } else {
    NA
  }
  rdiff <- if (sampler_data$ranked) {
    difference <- oeli::delta(ref = normalization$level$level, dim = J)
    inverse <- round(t(difference) %*% solve(difference %*% t(difference)))
    lapply(oeli::permutations(alternatives), function(permutation) {
      ranking <- match(permutation, alternatives)
      operator <- matrix(0, nrow = J - 1L, ncol = J)
      for (j in seq_len(J - 1L)) {
        operator[j, ranking[j]] <- -1
        operator[j, ranking[j + 1L]] <- 1
      }
      operator %*% inverse
    })
  } else {
    NA
  }

  # the statistics passed to the sampler
  statistics <- list(
    N = sampler_data$N,
    T = sampler_data$T,
    J = J,
    P_f = P_f,
    P_l = P_l,
    P_r = P_r,
    random_distribution = as.integer(sampler_data$random_distribution),
    random_correlated = sampler_data$random_correlated,
    random_class_specific = sampler_data$random_class_specific,
    Tvec = Tvec,
    csTvec = cumsum(Tvec) - Tvec,
    W = W,
    L = L,
    X = X,
    y = y,
    available = available,
    WkW = if (P_f) cross_product_sum(W, P_f, J, ordered) else NA,
    LkL = LkL,
    XkX = XkX,
    rdiff = rdiff
  )

  # the Gibbs sampler runs one or several chains
  timer_start <- Sys.time()
  sample_chains <- function() {
    progressor <- if (progress) {
      progressr::progressor(steps = chains * iterations)
    }
    dynamic <- changing_dimension
    run_chain <- function(chain) {
      chain_start <- Sys.time()
      reported <- 0L
      report <- function(iteration, total, classes) {
        progressor(
          amount = iteration - reported,
          message = paste0(
            "Gibbs sampler (chain ", chain, ") - ", iteration, " of ", total,
            " iterations", if (dynamic) paste0(" (C = ", classes, ")")
          )
        )
        reported <<- iteration
      }
      samples <- gibbs_sampler(
        sufficient_statistics = statistics,
        prior = prior_sampler,
        latent_classes = unclass(latent_classes),
        R = iterations,
        B = warmup,
        ordered = sampler_data$ordered,
        ranked = sampler_data$ranked,
        save_beta_draws = save_individual_draws && sampler_data$P_r > 0L,
        progress = if (progress) report
      )
      if (!sampler_data$P_f) samples$alpha <- NULL
      if (!sampler_data$P_l) samples$lambda <- NULL
      if (!sampler_data$P_r) samples[c("b", "Omega", "beta")] <- NULL
      if (!sampler_data$P_r && !sampler_data$P_l) {
        samples[c("s", "z", "class_sequence", "delta")] <- NULL
      }
      if (!isTRUE(prior_sampler$sample_delta)) samples$delta <- NULL
      if (!sampler_data$ordered) samples$d <- NULL
      list(
        samples = samples,
        elapsed = as.numeric(
          difftime(Sys.time(), chain_start, units = "secs")
        )
      )
    }
    if (chains == 1L) {
      list(run_chain(1L))
    } else {
      future.apply::future_lapply(
        seq_len(chains),
        run_chain,
        future.seed = TRUE
      )
    }
  }
  chain_results <- if (progress) {
    progressr::with_progress(sample_chains(), enable = TRUE)
  } else {
    sample_chains()
  }
  timer_end <- Sys.time()

  # the number of reserved classes is read from the samples
  relabel_classes <- if (changing_dimension) {
    ncol(chain_results[[1L]]$samples$s)
  } else {
    classes
  }
  if (relabel_classes > 1L) {
    # the reference partition minimizes Dahl's loss on a subset of the draws
    retained <- seq.int(
      warmup + 1L, nrow(chain_results[[1L]]$samples$z), by = thin
    )
    allocation <- do.call(rbind, lapply(chain_results, function(result) {
      result$samples$z[retained, , drop = FALSE]
    }))
    co_clustering <- matrix(0, ncol(allocation), ncol(allocation))
    for (class in seq_len(relabel_classes)) {
      membership <- allocation == class
      co_clustering <- co_clustering + crossprod(membership)
    }
    co_clustering <- co_clustering / nrow(allocation)
    candidates <- unique(round(seq(1, nrow(allocation), length.out = 100L)))
    loss <- vapply(candidates, function(draw) {
      same <- outer(allocation[draw, ], allocation[draw, ], `==`)
      sum((same - co_clustering)^2)
    }, numeric(1))
    reference <- allocation[candidates[which.min(loss)], ]

    # the draws are matched to the reference until the reference stabilizes
    mappings <- matrix(NA_integer_, nrow(allocation), relabel_classes)
    for (iteration in seq_len(20L)) {
      relabeled <- allocation
      for (draw in seq_len(nrow(allocation))) {
        mappings[draw, ] <- label_mapping(
          allocation[draw, ], reference, relabel_classes
        )
        relabeled[draw, ] <- mappings[draw, allocation[draw, ]]
      }
      updated <- apply(relabeled, 2L, function(value) {
        which.max(tabulate(value, nbins = relabel_classes))
      })
      if (identical(updated, reference)) break
      reference <- updated
    }

    # the final labels sort the classes by decreasing mean weight
    n_retained <- length(retained)
    weights <- do.call(rbind, lapply(seq_len(nrow(allocation)), function(draw) {
      s <- chain_results[[(draw - 1L) %/% n_retained + 1L]]$samples$s
      s[retained[(draw - 1L) %% n_retained + 1L], ][
        match(seq_len(relabel_classes), mappings[draw, ])
      ]
    }))
    final <- integer(relabel_classes)
    by_weight <- order(colMeans(weights), decreasing = TRUE)
    final[by_weight] <- seq_len(relabel_classes)
    mappings <- t(apply(mappings, 1L, function(mapping) final[mapping]))

    # the class-specific draws of every chain are permuted accordingly
    offset <- 0L
    for (chain in seq_along(chain_results)) {
      samples <- chain_results[[chain]]$samples
      for (index in seq_along(retained)) {
        row <- retained[index]
        mapping <- mappings[offset + index, ]
        inverse <- match(seq_len(relabel_classes), mapping)
        samples$s[row, ] <- samples$s[row, inverse]
        samples$z[row, ] <- mapping[samples$z[row, ]]
        for (name in c("b", "Omega", "lambda")) {
          if (is.null(samples[[name]])) next
          size <- ncol(samples[[name]]) %/% relabel_classes
          columns <- unlist(lapply(inverse, function(class) {
            (class - 1L) * size + seq_len(size)
          }))
          samples[[name]][row, ] <- samples[[name]][row, columns]
        }
      }
      chain_results[[chain]]$samples <- samples
      offset <- offset + n_retained
    }
  }
  chain_draws <- lapply(chain_results, function(result) {
    transform_chain_draws(
      samples = result$samples,
      warmup = warmup,
      thin = thin,
      effects = choice_effects,
      normalization = normalization,
      decider_ids = sampler_deciders,
      class_update = class_update
    )
  })
  reference_variables <- colnames(chain_draws[[1L]])
  if (!all(vapply(
    chain_draws,
    function(value) identical(colnames(value), reference_variables),
    logical(1)
  ))) {
    cli::cli_abort(
      "MCMC chains produced incompatible posterior variables.",
      call = NULL
    )
  }
  retained_iterations <- attr(chain_draws[[1L]], "iteration")
  draws <- array(
    NA_real_,
    dim = c(nrow(chain_draws[[1L]]), chains, ncol(chain_draws[[1L]])),
    dimnames = list(
      iteration = as.character(retained_iterations),
      chain = as.character(seq_len(chains)),
      variable = reference_variables
    )
  )
  for (chain in seq_len(chains)) draws[, chain, ] <- chain_draws[[chain]]
  draws <- posterior::as_draws_array(draws)
  attr(draws, "warmup") <- as.integer(warmup)
  attr(draws, "thin") <- as.integer(thin)
  if (
    changing_dimension && "n_classes" %in% reference_variables &&
      max(as.numeric(draws[, , "n_classes"])) >= max_classes
  ) {
    cli::cli_warn(
      c(
        paste(
          "The retained draws reach the maximum of", max_classes,
          "latent classes."
        ),
        "i" = "Increase `max_classes` and refit to check the bound."
      ),
      call = NULL
    )
  }

  # the fitted object collects the model, sampler, and simulation details
  model_classes <- list(
    initial = as.integer(classes),
    update = class_update,
    maximum = if (changing_dimension) {
      as.integer(max_classes)
    } else {
      as.integer(classes)
    }
  )
  if (identical(class_update, "weight_based")) {
    model_classes$control <- weight_based_control
  }
  model <- list(
    formula = choice_formula$formula,
    random_effects = random_specification,
    latent_class_effects = choice_effects$effect_name[in_lc],
    choice_type = choice_type,
    alternatives = choice_alternatives,
    data_roles = data_roles,
    responses = response_values,
    effects = choice_effects,
    normalization = normalization,
    deciders = sampler_deciders,
    latent_classes = model_classes
  )
  elapsed <- as.numeric(difftime(timer_end, timer_start, units = "secs"))
  chain_elapsed <- vapply(chain_results, `[[`, numeric(1), "elapsed")
  sampler <- list(
    iterations = as.integer(iterations),
    warmup = as.integer(warmup),
    thin = as.integer(thin),
    chains = as.integer(chains),
    retained_per_chain = length(seq.int(1L, iterations - warmup, by = thin)),
    save_individual_draws = isTRUE(save_individual_draws) &&
      sampler_data$P_r > 0L,
    elapsed = c(total = elapsed, stats::setNames(
      chain_elapsed, paste0("chain_", seq_len(chains))
    ))
  )
  # the data-generating parameters on the scale of the draws
  dgp <- NULL
  if (simulated) {
    parameters <- simulation_parameters
    with_classes <- sampler_data$P_r > 0L || sampler_data$P_l > 0L
    means <- if (is.list(parameters$beta)) {
      parameters$beta
    } else {
      list(parameters$beta)
    }
    dgp_classes <- length(means)
    covariances <- if (is.list(parameters$Omega)) {
      parameters$Omega
    } else {
      rep(list(parameters$Omega), dgp_classes)
    }
    weights <- if (with_classes) parameters$weights
    if (with_classes && is.null(weights)) {
      weights <- rep(1 / dgp_classes, dgp_classes)
    }
    if (dgp_classes > 1L) {
      by_weight <- order(weights, decreasing = TRUE)
      means <- means[by_weight]
      covariances <- covariances[by_weight]
      weights <- weights[by_weight]
    }
    error_covariance <- if (ordered) {
      as.numeric(parameters$Sigma)
    } else {
      difference <- oeli::delta(ref = normalization$level$level, dim = J)
      as.numeric(difference %*% parameters$Sigma %*% t(difference))
    }
    truth <- list(
      alpha = matrix(means[[1L]][!random & !latent_class], nrow = 1L),
      s = if (with_classes) matrix(weights, nrow = 1L),
      b = if (sampler_data$P_r) {
        matrix(unlist(lapply(means, `[`, random)), nrow = 1L)
      },
      Omega = if (sampler_data$P_r) {
        matrix(unlist(lapply(covariances, as.numeric)), nrow = 1L)
      },
      lambda = if (sampler_data$P_l) {
        matrix(unlist(lapply(means, `[`, latent_class)), nrow = 1L)
      },
      Sigma = matrix(error_covariance, nrow = 1L),
      d = if (ordered) matrix(log(diff(parameters$gamma)), nrow = 1L)
    )
    values <- transform_chain_draws(
      samples = truth,
      warmup = 0L,
      thin = 1L,
      effects = choice_effects,
      normalization = normalization,
      decider_ids = character(),
      class_update = "fixed"
    )
    dgp <- stats::setNames(as.numeric(values[1L, ]), colnames(values))
    if (!identical(class_update, "fixed")) {
      dgp <- c(dgp, n_classes = sum(weights > 0))
    }
  }
  simulation <- if (simulated) {
    list(
      dgp_parameters = simulation_parameters,
      dgp = dgp,
      n_deciders = as.integer(n_deciders),
      n_occasions = as.integer(Tp),
      n_alternatives = as.integer(J)
    )
  } else {
    NULL
  }

  # the fitted model object
  structure(
    list(
      call = match.call(),
      data = choice_data,
      model = model,
      prior = prior_values,
      draws = draws,
      sampler = sampler,
      simulation = simulation
    ),
    class = "RprobitB_fit"
  )
}
