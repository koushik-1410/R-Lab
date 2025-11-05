# Example data
advertising <- c(10, 20, 30, 40, 50, 60, 70, 80)
sales <- c(15, 25, 35, 45, 55, 65, 70, 85)

data <- data.frame(advertising, sales)
print(data)


# Build the linear regression model
model <- lm(sales ~ advertising, data = data)

# Display model summary
summary(model)


# New test data
new_data <- data.frame(advertising = 90)

# Predict sales
predicted_sales <- predict(model, new_data)
print(predicted_sales)


# Plot the data points
plot(data$advertising, data$sales,
     col = "blue", pch = 19,
     xlab = "Advertising Spend (₹)",
     ylab = "Sales (₹)",
     main = "Linear Regression Model")

# Add the regression line
abline(model, col = "red", lwd = 2)
