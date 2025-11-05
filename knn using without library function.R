# ----- Training Data -----
brightness <- c(40, 50, 60, 10, 70, 60, 25)
saturation <- c(20, 50, 90, 25, 90, 10, 80)
class <- c("R", "B", "B", "R", "B", "R", "B")

train_data <- data.frame(brightness, saturation, class)

# ----- Test Data -----
test_data <- data.frame(brightness = 20, saturation = 35)

# ----- User-defined KNN function -----
knn_manual <- function(train, test, cl, k = 3) {
  
  # Step 1: Calculate Euclidean distance between test and each training point
  distances <- sqrt((train$brightness - test$brightness)^2 +
                      (train$saturation - test$saturation)^2)
  
  # Step 2: Combine distances with class labels
  distance_data <- data.frame(distance = distances, class = cl)
  
  # Step 3: Sort by distance (ascending order)
  sorted_data <- distance_data[order(distance_data$distance), ]
  
  # Step 4: Pick top 'k' nearest neighbors
  nearest_neighbors <- sorted_data[1:k, ]
  
  # Step 5: Count frequency of each class
  class_counts <- table(nearest_neighbors$class)
  
  # Step 6: Select the class with the highest frequency
  predicted_class <- names(which.max(class_counts))
  
  return(predicted_class)
}

# ----- Test the function -----
predicted <- knn_manual(train_data[, 1:2], test_data, train_data$class, k = 3)

# Output
print(paste("Predicted class for the test data is:", predicted))
