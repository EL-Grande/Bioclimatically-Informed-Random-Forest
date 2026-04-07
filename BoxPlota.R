# Load required libraries
library(randomForest)
library(dygraphs)


myFunc <- function(){
  
  # Generate random data
  set.seed(123)
  n <- 100
  weather_data <- data.frame(matrix(runif(n * 7), n, 7))
  max_temp <- rowSums(weather_data) + rnorm(n, mean = 0, sd = 2)
  
  # Split the data into training and testing sets
  train_indices <- sample(1:n, 0.8 * n)
  train_data <- weather_data[train_indices, ]
  train_labels <- max_temp[train_indices]
  test_data <- weather_data[-train_indices, ]
  test_labels <- max_temp[-train_indices]
  
  # Train the Random Forest model
  rf_model <- randomForest(train_labels ~ ., data = train_data, ntree = 100)
  
  # Predictions on the test set
  predictions <- predict(rf_model, newdata = test_data)
  
  # Residuals for different scaling techniques
  residuals_raw <- test_labels - predictions
  residuals_zscore <- residuals_raw / sd(residuals_raw)
  residuals_minmax <- (residuals_raw - min(residuals_raw)) / (max(residuals_raw) - min(residuals_raw))
  residuals_magnitude <- residuals_raw / sqrt(sum(residuals_raw^2))
  
  # Combine residuals into a list
  residuals_list <- list(residuals_raw, residuals_zscore, residuals_minmax, residuals_magnitude)
  
  return(residuals_list)
}

residuals_listA<-myFunc()

# Function to create box plot
createBoxPlot <- function(residuals_list, names_vector, title, ylab) {
  boxplot(residuals_list, names = names_vector,
          main = title, ylab = ylab, pch = "*", cex = 2)
}



# Create a box plot with multiple boxes using the function
BoxPlts <- createBoxPlot(residuals_listA, c("Raw Data", "Z-Score", "Min-Max", "Magnitude"),
                         "Residuals Box Plots", "Residuals")

# Display box plots
BoxPlts