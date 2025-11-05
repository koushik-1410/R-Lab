brightness <- c(40, 50, 60, 10, 70, 60, 25)
saturation <- c(20, 50, 90, 25, 90, 10, 80)

data <- data.frame(brightness, saturation)
print(data)

# Apply K-means with k = 2 clusters
set.seed(123)  # ensures reproducibility
kmeans_result <- kmeans(data, centers = 2, nstart = 10)

# View results
print(kmeans_result)


# Add the cluster number to the original dataset
data$cluster <- kmeans_result$cluster

# View updated data
print(data)

# Plot the clusters
plot(data$brightness, data$saturation,
     col = data$cluster, pch = 19,
     xlab = "Brightness", ylab = "Saturation",
     main = "K-Means Clustering")

# Add cluster centers
points(kmeans_result$centers[,1], kmeans_result$centers[,2],
       col = 1:2, pch = 8, cex = 2)


