make_test_fit <- function(
  data = NULL, random_effects = character(), chains = 1L,
  save_individual_draws = TRUE
) {
  if (is.null(data)) {
    data <- data.frame(
      deciderID = 1:4,
      choice = c("A", "B", "A", "B"),
      x_A = c(0, 1, 0, 1),
      x_B = c(1, 0, 1, 0)
    )
  }
  fit(
    choice ~ x | 0,
    data = data,
    random_effects = random_effects,
    column_occasion = if ("occasionID" %in% names(data)) "occasionID" else NULL,
    iterations = 20L,
    warmup = 10L,
    chains = chains,
    progress = FALSE,
    save_individual_draws = save_individual_draws
  )
}
