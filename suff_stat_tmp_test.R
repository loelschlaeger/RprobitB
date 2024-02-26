data <- simulate_choices(
  form = choice ~ var | 0, N = 100, T = 10, J = 3, seed = 1
)
model <- fit_model(data = data, R = 1000, seed = 1)
summary(model)



data <- data$data

split_data(data)


