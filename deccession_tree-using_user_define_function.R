# ---------------------- TRAINING DATA ----------------------
train <- data.frame(
  age = c('youth','youth','mid-aged','senior','senior','senior','mid-aged','youth','youth','senior','youth','mid-aged','mid-aged','senior'),
  income = c('high','high','high','medium','low','low','low','medium','low','medium','medium','medium','high','medium'),
  student = c('no','no','no','no','yes','yes','yes','no','yes','yes','yes','no','yes','no'),
  credit_rating = c('fair','excellent','fair','fair','fair','excellent','excellent','fair','fair','fair','excellent','excellent','fair','excellent'),
  buys_comp = c('no','no','yes','no','no','yes','yes','no','yes','no','yes','yes','yes','yes')
)

# ---------------------- ENTROPY FUNCTION ----------------------
entropy <- function(labels) {
  probs <- table(labels) / length(labels)
  sum(probs * log2(probs))
}

# ---------------------- INFORMATION GAIN FUNCTION ----------------------
info_gain <- function(data, feature, target) {
  total_entropy <- entropy(data[[target]])
  values <- unique(data[[feature]])
  weighted_entropy <- 0
  
  for (val in values) {
    subset <- data[data[[feature]] == val, ]
    subset_entropy <- entropy(subset[[target]])
    weighted_entropy <- weighted_entropy + (nrow(subset) / nrow(data)) * subset_entropy
  }
  
  gain <- total_entropy - weighted_entropy
  return(gain)
}

# ---------------------- BUILD TREE FUNCTION (ID3) ----------------------
build_tree <- function(data, target, features) {
  # Base case 1: all samples have same target
  if (length(unique(data[[target]])) == 1) {
    return(unique(data[[target]])[1])
  }
  
  # Base case 2: no more features left
  if (length(features) == 0) {
    counts <- table(data[[target]])
    return(names(which.max(counts)))
  }
  
  # Find the feature with the highest information gain
  gains <- sapply(features, function(f) info_gain(data, f, target))
  best_feature <- features[which.max(gains)]
  
  tree <- list()
  tree[[best_feature]] <- list()
  
  # For each value of the best feature, build a subtree
  for (val in unique(data[[best_feature]])) {
    subset <- data[data[[best_feature]] == val, ]
    
    if (nrow(subset) == 0) {
      counts <- table(data[[target]])
      tree[[best_feature]][[val]] <- names(which.max(counts))
    } else {
      remaining_features <- setdiff(features, best_feature)
      tree[[best_feature]][[val]] <- build_tree(subset, target, remaining_features)
    }
  }
  
  return(tree)
}

# ---------------------- TRAIN THE TREE ----------------------
features <- setdiff(names(train), "buys_comp")
decision_tree <- build_tree(train, "buys_comp", features)

print("Decision Tree:")
print(decision_tree)

# ---------------------- PREDICTION FUNCTION ----------------------
predict_tree <- function(tree, test) {
  if (class(tree) != "list") {
    return(tree)
  }
  
  feature <- names(tree)[1]
  value <- as.character(test[[feature]])
  
  if (!value %in% names(tree[[feature]])) {
    return(NA) # Unknown value
  }
  
  subtree <- tree[[feature]][[value]]
  return(predict_tree(subtree, test))
}

# ---------------------- TEST DATA ----------------------
test_data <- data.frame(
  age = "youth",
  income = "medium",
  student = "yes",
  credit_rating = "fair"
)

prediction <- predict_tree(decision_tree, test_data)
cat("Predicted class for the test data:", prediction, "\n")
