advertising <- c(10, 20, 30, 40, 50, 60, 70, 80)
sales <- c(15, 25, 35, 45, 55, 65, 70, 85)

data <- data.frame(advertising, sales)



my_linear_regression <- function(x, y) {
 
  x_mean <- mean(x)
  y_mean <- mean(y)
  
 
  numerator <- sum((x - x_mean) * (y - y_mean))
  denominator <- sum((x - x_mean)^2)
  b1 <- numerator / denominator
  
 
  b0 <- y_mean - b1 * x_mean
  
  
  predict <- function(new_x) {
    return (b0 + b1 * new_x)
  }
  
 
  result <- list(intercept = b0,
                 slope = b1,
                 predict = predict)
  return(result)
}


model <- my_linear_regression(data$advertising, data$sales)


cat("Intercept (b0):", model$intercept, "\n")
cat("Slope (b1):", model$slope, "\n")



predicted_value <- model$predict(90)
cat("Predicted Sales for Advertising = 90:", predicted_value, "\n")



plot(data$advertising, data$sales, pch = 19, col = "blue",
     xlab = "Advertising", ylab = "Sales",
     main = "User-defined Linear Regression")


abline(a = model$intercept, b = model$slope, col = "red", lwd = 2)




