# ------------------- TRAINING DATA -------------------
train <- data.frame(
  age = c('youth','youth','mid-aged','senior','senior','senior','mid-aged','youth','youth','senior','youth','mid-aged','mid-aged','senior'),
  income = c('high','high','high','medium','low','low','low','medium','low','medium','medium','medium','high','medium'),
  student = c('no','no','no','no','yes','yes','yes','no','yes','yes','yes','no','yes','no'),
  credit_rating = c('fair','excellent','fair','fair','fair','excellent','excellent','fair','fair','fair','excellent','excellent','fair','excellent'),
  buys_comp = c('no','no','yes','no','no','yes','yes','no','yes','no','yes','yes','yes','yes')
)

# ------------------- USER-DEFINED NAIVE BAYES FUNCTION -------------------
naive_bayes_manual <- function(train, test) {
  target <- "buys_comp"
  features <- setdiff(names(train), target)
  
  classes <- unique(train[[target]])
  total <- nrow(train)

  # Step 1: Compute prior probabilities for each class
  priors <- sapply(classes, function(c) {
    sum(train[[target]] == c) / total
  })
            
  # Step 2: Compute conditional probabilities for each feature given each class
  cond_probs <- list()
  for (f in features) {
    cond_probs[[f]] <- list()
    for (c in classes) {
      subset_data <- train[train[[target]] == c, ]
      probs <- table(subset_data[[f]]) / nrow(subset_data)
      cond_probs[[f]][[c]] <- probs
    }
  }
  
  # Step 3: Compute posterior probabilities for the test case
  posteriors <- c()
  for (c in classes) {
    posterior <- priors[c]
    for (f in features) {
      val <- as.character(test[[f]])
      # if value not in training data for that class, assign small smoothing value
      prob <- cond_probs[[f]][[c]][val]
      if (is.na(prob)) prob <- 1e-6
      posterior <- posterior * prob
    }
    posteriors[c] <- posterior
  }
  
  # Step 4: Choose the class with highest posterior
  predicted_class <- names(which.max(posteriors))
  
  # Optional: print probabilities
  print(posteriors)
  
  return(predicted_class)
}

# ------------------- TEST DATA ---------------------
test_data <- data.frame(
  age = "youth",
  income = "medium",
  student = "yes",
  credit_rating = "fair"
)

# ------------------- RUN MODEL ---------------------
prediction <- naive_bayes_manual(train, test_data)
cat("Predicted class for the test data:", prediction, "\n")


