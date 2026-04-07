MLOPS <- function(D,E) {
  source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get the datasets
  Lista <- DataOps("2023-01-01", "2023-01-05") # Access the individual datasets from the list
  
  D1 <- Lista$dataset1
  D2 <- Lista$dataset2
  D3 <- Lista$dataset3
  D4 <- Lista$dataset4
  
  set.seed (2024)
  Ta_train <- sample (1: nrow(D1), nrow(D1)*0.75)
  Ta_test <- D1[-Ta_train, "AirTemp"]
  length(Ta_train)
  length(Ta_test)
  
  RF.Ta <- randomForest(`AirTemp`~., data = D1, subset = Ta_train , importance = TRUE)
  RF.Ta
  
  
  plot(RF.Ta$predicted)
  plot(RF.Ta$mse)
  
  
  yhat.bag <- predict(RF.Ta , newdata = D1[-Ta_train , ])
  plot(yhat.bag , Ta_test)
  abline (0, 1)
  mean (( yhat.bag - Ta_test)^2)
 
  
}

MLOPS("2023-01-01", "2023-03-25")
 