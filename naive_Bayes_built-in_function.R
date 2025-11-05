#install.packages("e1071")
library(e1071)
train <- data.frame(
  age = c('youth','youth','mid-aged','senior','senior','senior','mid-aged','youth','youth','senior','youth','mid-aged','mid-aged','senior'),
  income = c('high','high','high','medium','low','low','low','medium','low','medium','medium','medium','high','medium'),
  student = c('no','no','no','no','yes','yes','yes','no','yes','yes','yes','no','yes','no'),
  credit_rating = c('fair','excellent','fair','fair','fair','excellent','excellent','fair','fair','fair','excellent','excellent','fair','excellent'),
  buys_comp = c('no','no','yes','no','no','yes','yes','no','yes','no','yes','yes','yes','yes')
)
train
# Train the model using naiveBayes()
model <- naiveBayes(buys_comp ~ age + income + student + credit_rating, data = train)

test_data <- data.frame(age="youth", income="medium", student="yes", credit_rating="fair")


prediction <- predict(model, test_data)
print(prediction)
#model
    
   
   
     

     
 