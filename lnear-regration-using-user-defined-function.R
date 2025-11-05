advertising <- c(10, 20, 30, 40, 50, 60, 70, 80)
sales <- c(15, 25, 35, 45, 55, 65, 70, 85)

data <- data.frame(advertising, sales)


# --- User-defined linear regression function ---
my_linear_regression <- function(x, y) {
  # Step 1: Calculate means
  x_mean <- mean(x)
  y_mean <- mean(y)
  
  # Step 2: Calculate slope (b1)
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  b1 <- numerator / denominator
  
  # Step 3: Calculate intercept (b0)
  b0 <- y_mean - b1 * x_mean
  
  # Step 4: Create a function to predict new values
  predict <- function(new_x) {
    return (b0 + b1 * new_x)
  }
  
  # Step 5: Return results as a list
  result <- list(intercept = b0,
                 slope = b1,
                 predict = predict)
  return(result)
}

# Fit the model
model <- my_linear_regression(data$advertising, data$sales)

# Print the results
cat("Intercept (b0):", model$intercept, "\n")
cat("Slope (b1):", model$slope, "\n")


# Predict sales for advertising = 90
predicted_value <- model$predict(90)
cat("Predicted Sales for Advertising = 90:", predicted_value, "\n")


# Plot data points
plot(data$advertising, data$sales, pch = 19, col = "blue",
     xlab = "Advertising", ylab = "Sales",
     main = "User-defined Linear Regression")

# Add regression line using calculated coefficients
abline(a = model$intercept, b = model$slope, col = "red", lwd = 2)


