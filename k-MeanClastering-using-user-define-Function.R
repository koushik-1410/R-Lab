
brightness <- c(40, 50, 60, 10, 70, 60, 25)
saturation <- c(20, 50, 90, 25, 90, 10, 80)
data <- data.frame(brightness, saturation)


my_kmeans <- function(data, k, max_iter = 100) {
  set.seed(123)  
  
  
  centers <- data[sample(1:nrow(data), k), ]
  
  for (iter in 1:max_iter) {
    
    cluster_assignment <- c()
    
    for (i in 1:nrow(data)) {
     
      distances <- apply(centers, 1, function(center) {
        sqrt(sum((data[i, ] - center) ^ 2))
      })
      
      cluster_assignment[i] <- which.min(distances)
    }
    
 
    new_centers <- centers
    for (j in 1:k) {
      cluster_points <- data[cluster_assignment == j, ]
      if (nrow(cluster_points) > 0) {
        new_centers[j, ] <- colMeans(cluster_points)
      }
    }
    
    if (all(abs(new_centers - centers) < 1e-6)) {
      cat("✅ Converged after", iter, "iterations\n")
      break
    }
    
    centers <- new_centers
  }
 
  result <- list(centers = centers, clusters = cluster_assignment)
  return(result)
}



result <- my_kmeans(data, k = 2)
print(result$centers)
print(result$clusters)



data$cluster <- result$clusters

plot(data$brightness, data$saturation,
     col = data$cluster, pch = 19,
     xlab = "Brightness", ylab = "Saturation",
     main = "User-defined K-Means Clustering")

points(result$centers$brightness, result$centers$saturation,
       col = 1:2, pch = 8, cex = 2)

