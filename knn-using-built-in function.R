# Load necessary library
library(class)

# Training data
brightness <- c(40, 50, 60, 10, 70, 60, 25)
saturation <- c(20, 50, 90, 25, 90, 10, 80)
class <- c("R", "B", "B", "R", "B", "R", "B")

train_data <- data.frame(brightness, saturation, class)

# Test data
test_data <- data.frame(brightness = 20, saturation = 35)

# Use KNN (k = 3 for example)
predicted_class <- knn(train = train_data[, 1:2],   # brightness & saturation
                       test = test_data,
                       cl = train_data$class,       # class labels
                       k = 3)

# Print the predicted class
print(predicted_class)



