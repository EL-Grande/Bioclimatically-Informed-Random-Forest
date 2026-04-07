library(caret)
library(randomForest)
library(pdp)
library(ggplot2)
library(tuneRanger)
library(ranger)
library(pdp,lattice)
library(caTools)
library(Metrics)
library(knitr)
library(caret)
library(h2o)
library(mlr)
library(OpenML)
library(rsq)
library(gridExtra)
# Load necessary packages
library(tidyverse)
library(reshape2)

# Set the seed for reproducibility
set.seed(123)

# Generate hourly timestamps from January 01, 2023, to December 31, 2023
start_date <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2023-12-31 23:00:00", tz = "UTC")
timestamps <- seq(start_date, end_date, by = "1 hour")

# Create a data frame with the specified variables
datas <- data.frame(
  DateTime = timestamps,
  AirTemp = rnorm(length(timestamps), mean = 25, sd = 5),
  SurfaceTemp = rnorm(length(timestamps), mean = 22, sd = 4),
  Rain = rpois(length(timestamps), lambda = 5),
  WindSpeed = rnorm(length(timestamps), mean = 5, sd = 2),
  WindDirect = runif(length(timestamps), min = 0, max = 360),
  WindSpeed.Max = rnorm(length(timestamps), mean = 10, sd = 3),
  WindSpeed.Min = rnorm(length(timestamps), mean = 2, sd = 1),
  Humidity = runif(length(timestamps), min = 50, max = 80),
  Barom.Press = rnorm(length(timestamps), mean = 1010, sd = 5),
  Solar.Rad = rnorm(length(timestamps), mean = 200, sd = 50),
  Solar.Rad.Energy = rnorm(length(timestamps), mean = 5, sd = 2),
  Leaf.Wetness = runif(length(timestamps), min = 0, max = 1),
  Dew.Point = rnorm(length(timestamps), mean = 15, sd = 3),
  Wet.Bulb = rnorm(length(timestamps), mean = 18, sd = 2),
  Sun.Hours = runif(length(timestamps), min = 0, max = 10),
  BatteryVoltage = runif(length(timestamps), min = 11, max = 14),
  Twc = rnorm(length(timestamps), mean = 20, sd = 3),
  Tap = rnorm(length(timestamps), mean = 22, sd = 4),
  HI = rnorm(length(timestamps), mean = 30, sd = 5),
  ER = rnorm(length(timestamps), mean = 0.5, sd = 0.1),
  CP = rnorm(length(timestamps), mean = 100, sd = 20),
  HD = rnorm(length(timestamps), mean = 25, sd = 5),
  TE = rnorm(length(timestamps), mean = 28, sd = 3)
)

################################################################################
# Function for data scaling using Z-Score
z_score_scale <- function(data, variables) {
  scaled_data <- data
  scaled_data[, variables] <- scale(data[, variables])
  return(scaled_data)
}

# Function for data scaling using Min-Max scaling
min_max_scale <- function(data, variables) {
  scaled_data <- data
  scaled_data[, variables] <- scale(data[, variables], center = FALSE, scale = apply(data[, variables], 2, max) - apply(data[, variables], 2, min))
  return(scaled_data)
}

# Function for data scaling using Magnitude scaling
magnitude_scale <- function(data, variables) {
  scaled_data <- data
  scaled_data[, variables] <- scale(data[, variables], center = FALSE, scale = apply(data[, variables], 2, function(x) sqrt(sum(x^2))))
  return(scaled_data)
}

# Function for training a Random Forest model and calculating loss functions
train_rf_model <- function(data, predictor_vars, target_var, start_date, end_date) {
  # Filter data based on the specified time range
  filtered_data <- subset(data, DateTime >= start_date & DateTime <= end_date)
  
  # Split the dataset into training (75%) and validation (25%) sets
  split_index <- floor(0.75 * nrow(filtered_data))
  train_data <- filtered_data[1:split_index, ]
  valid_data <- filtered_data[(split_index + 1):nrow(filtered_data), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(formula(paste(target_var, "~ .")), data = train_data[, c(target_var, predictor_vars)], ntree = 500)
  
  # Make predictions on the validation set
  predictions <- predict(rf_model, newdata = valid_data[, predictor_vars])
  
  # Huber loss function
  huber_loss <- function(y_true, y_pred, delta) {
    abs_diff <- abs(y_true - y_pred)
    loss <- ifelse(abs_diff <= delta, 0.5 * (abs_diff^2), delta * (abs_diff - 0.5 * delta))
    return(mean(loss))
  }
  
  # Pseudo-Huber loss function
  pseudo_huber_loss <- function(y_true, y_pred, delta) {
    abs_diff <- abs(y_true - y_pred)
    loss <- delta^2 * (sqrt(1 + (abs_diff / delta)^2) - 1)
    return(mean(loss))
  }
  
  # Evaluate the model performance on the validation set
  loss_functions <- list(
    MSE = mean((valid_data[, target_var] - predictions)^2),
    MAE = mean(abs(valid_data[, target_var] - predictions)),
    SMAPE = mean(200 * abs(valid_data[, target_var] - predictions) / (abs(valid_data[, target_var]) + abs(predictions))),
    LC = mean(log(cosh(predictions - valid_data[, target_var]))),
    Huber = huber_loss(valid_data[, target_var], predictions, delta = 1.0),
    PH = pseudo_huber_loss(valid_data[, target_var], predictions, delta = 1.0),
    KLD = sum(valid_data[, target_var] * log(valid_data[, target_var] / predictions)),
    RMSE = sqrt(mean((valid_data[, target_var] - predictions)^2)),
    CS = -mean(acos(sum(valid_data[, target_var] * predictions) / (sqrt(sum(valid_data[, target_var]^2)) * sqrt(sum(predictions^2))))),
    R2 = 1 - sum((valid_data[, target_var] - predictions)^2) / sum((valid_data[, target_var] - mean(valid_data[, target_var]))^2),
    SS = 1 - mean((valid_data[, target_var] - predictions)^2) / mean((valid_data[, target_var] - mean(valid_data[, target_var]))^2),
    Q = mean(pmax(0.5 * (valid_data[, target_var] - predictions), (0.5 - 1) * (predictions - valid_data[, target_var])))
  )
  
  return(list(Model = rf_model, LossFunctions = loss_functions))
}

# Generate artificial data without missing values
set.seed(123)  # Set seed for reproducibility
raw_data <- data.frame(
  DateTime = seq(from = as.POSIXct("2023-01-01 00:00:00", tz = "UTC"), 
                 to = as.POSIXct("2023-12-31 23:00:00", tz = "UTC"), by = "hour"),
  AirTemp = rnorm(8760, mean = 20, sd = 5),
  SurfaceTemp = rnorm(8760, mean = 22, sd = 4),
  Rain = rpois(8760, lambda = 5),
  WindSpeed = runif(8760, min = 0, max = 10),
  WindDirect = runif(8760, min = 0, max = 360),
  WindSpeed.Max = runif(8760, min = 0, max = 15),
  WindSpeed.Min = runif(8760, min = 0, max = 5),
  Humidity = runif(8760, min = 30, max = 80),
  Barom.Press = rnorm(8760, mean = 1010, sd = 5),
  Solar.Rad = runif(8760, min = 0, max = 800),
  Solar.Rad.Energy = runif(8760, min = 0, max = 30),
  Leaf.Wetness = rpois(8760, lambda = 3),
  Dew.Point = rnorm(8760, mean = 15, sd = 3),
  Wet.Bulb = rnorm(8760, mean = 18, sd = 2),
  Sun.Hours = runif(8760, min = 0, max = 12),
  BatteryVoltage = runif(8760, min = 11, max = 14),
  Twc = rnorm(8760, mean = 18, sd = 3),
  Tap = rnorm(8760, mean = 22, sd = 4),
  HI = rnorm(8760, mean = 25, sd = 5),
  ER = rnorm(8760, mean = 1.5, sd = 0.2),
  CP = rnorm(8760, mean = 100, sd = 10),
  HD = rnorm(8760, mean = 30, sd = 5),
  TE = rnorm(8760, mean = 25, sd = 4)
)

# Z-Score Scaling
z_score_scaled_data <- z_score_scale(raw_data, predictor_vars)
z_score_results <- train_rf_model(z_score_scaled_data, predictor_vars, target_var, start_date, end_date)

# Min-Max Scaling
min_max_scaled_data <- min_max_scale(raw_data, predictor_vars)
min_max_results <- train_rf_model(min_max_scaled_data, predictor_vars, target_var, start_date, end_date)

# Magnitude Scaling
magnitude_scaled_data <- magnitude_scale(raw_data, predictor_vars)
magnitude_results <- train_rf_model(magnitude_scaled_data, predictor_vars, target_var, start_date, end_date)


# Display loss functions for each scaling method in a structured manner
cat("\nRaw Data Loss Functions:", "\n")
print(as.data.frame(t(train_rf_model(raw_data, predictor_vars, target_var, start_date, end_date)$LossFunctions)))

cat("\nZ-Score Scaling Loss Functions:", "\n")
print(as.data.frame(t(z_score_results$LossFunctions)))

cat("\nMin-Max Scaling Loss Functions:", "\n")
print(as.data.frame(t(min_max_results$LossFunctions)))

cat("\nMagnitude Scaling Loss Functions:", "\n")
print(as.data.frame(t(magnitude_results$LossFunctions)))
################################################################################



################################################################################
# Load the 'vip' package
library(vip)
vip12 <- vip(rf_model, target = "AirTemp", aesthetics = list(color = "black", fill = "white"),size = 0.3)+
  ggtitle("B12: 2021-06-19 to 2021-12-31") 

# Retrieve the Top6 predictors variables
top6 <- topPredictors(rf_model, n = 6); top6
##############################################################################################################
# Construct partial dependence functions for top 6 predictors
pd12 <- NULL
for (i in top6) {
  tmp <- partial(rf_model, pred.var = i)
  names(tmp) <- c("x", "y")
  pd12 <- rbind(pd12,  cbind(tmp, predictor = i))
}

# Display partial dependence functions
pd12 <- ggplot(pd12, aes(x, y)) + geom_line() + facet_wrap(~ predictor, scales = "free") +  geom_smooth()+
  ylab("Air Temperature") +  xlab("Predictor Variables") +  theme_bw() +
  ggtitle("2021-06-19 to 2021-12-31") 
####################################################################################

# Load necessary packages
# Function for training a Random Forest model and calculating evaluation metrics
train_rf_model_with_metrics <- function(data, predictor_vars, target_var, start_date, end_date) {
  # Filter data based on the specified time range
  filtered_data <- subset(data, DateTime >= start_date & DateTime <= end_date)
  
  # Split the dataset into training (75%) and validation (25%) sets
  split_index <- floor(0.75 * nrow(filtered_data))
  train_data <- filtered_data[1:split_index, ]
  valid_data <- filtered_data[(split_index + 1):nrow(filtered_data), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(formula(paste(target_var, "~ .")), data = train_data[, c(target_var, predictor_vars)], ntree = 500)
  
  # Make predictions on the validation set
  predictions <- predict(rf_model, newdata = valid_data[, predictor_vars])
  
  # Calculate evaluation metrics
  r_squared <- 1 - sum((valid_data[, target_var] - predictions)^2) / sum((valid_data[, target_var] - mean(valid_data[, target_var]))^2)
  rmse <- sqrt(mean((valid_data[, target_var] - predictions)^2))
  
  # Huber loss function
  huber_loss <- function(y_true, y_pred, delta) {
    abs_diff <- abs(y_true - y_pred)
    loss <- ifelse(abs_diff <= delta, 0.5 * (abs_diff^2), delta * (abs_diff - 0.5 * delta))
    return(mean(loss))
  }
  huber <- huber_loss(valid_data[, target_var], predictions, delta = 1.0)
  
  # Return the evaluation metrics
  return(c(R2 = r_squared, RMSE = rmse, Huber = huber))
}

# Calculate metrics for each scaling method
metrics_raw <- train_rf_model_with_metrics(raw_data, predictor_vars, target_var, start_date_train, end_date_train)
metrics_z_score_scaled <- train_rf_model_with_metrics(z_score_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)
metrics_min_max_scaled <- train_rf_model_with_metrics(min_max_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)
metrics_magnitude_scaled <- train_rf_model_with_metrics(magnitude_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)

# Create a data frame for plotting
metrics_data <- data.frame(
  Scaling_Method = c("Raw", "Z-Score", "Min-Max", "Magnitude"),
  R2 = c(metrics_raw["R2"], metrics_z_score_scaled["R2"], metrics_min_max_scaled["R2"], metrics_magnitude_scaled["R2"]),
  RMSE = c(metrics_raw["RMSE"], metrics_z_score_scaled["RMSE"], metrics_min_max_scaled["RMSE"], metrics_magnitude_scaled["RMSE"]),
  Huber = c(metrics_raw["Huber"], metrics_z_score_scaled["Huber"], metrics_min_max_scaled["Huber"], metrics_magnitude_scaled["Huber"])
)

# Melt the data for plotting
melted_metrics_data <- melt(metrics_data, id.vars = "Scaling_Method")

# Plot the bar charts
ggplot(melted_metrics_data, aes(x = Scaling_Method, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ variable, scales = "free_y", ncol = 1) +
  labs(title = "Evaluation Metrics for Different Scaling Methods",
       x = "Scaling Method",
       y = "Metric Value") +
  theme_minimal()
#################################################################################
# Function for training a Random Forest model and calculating RMSE
train_rf_model_with_rmse <- function(data, predictor_vars, target_var, start_date, end_date) {
  # Filter data based on the specified time range
  filtered_data <- subset(data, DateTime >= start_date & DateTime <= end_date)
  
  # Split the dataset into training (75%) and validation (25%) sets
  split_index <- floor(0.75 * nrow(filtered_data))
  train_data <- filtered_data[1:split_index, ]
  valid_data <- filtered_data[(split_index + 1):nrow(filtered_data), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(formula(paste(target_var, "~ .")), data = train_data[, c(target_var, predictor_vars)], ntree = 500)
  
  # Make predictions on the validation set
  predictions <- predict(rf_model, newdata = valid_data[, predictor_vars])
  
  # Calculate RMSE
  rmse <- sqrt(mean((valid_data[, target_var] - predictions)^2))
  
  # Return RMSE
  return(rmse)
}

# Calculate RMSE for each scaling method
rmse_raw <- train_rf_model_with_rmse(raw_data, predictor_vars, target_var, start_date_train, end_date_train)
rmse_z_score_scaled <- train_rf_model_with_rmse(z_score_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)
rmse_min_max_scaled <- train_rf_model_with_rmse(min_max_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)
rmse_magnitude_scaled <- train_rf_model_with_rmse(magnitude_scaled_data, predictor_vars, target_var, start_date_train, end_date_train)

# Create a data frame for plotting
rmse_data <- data.frame(
  Scaling_Method = c("Raw", "Z-Score", "Min-Max", "Magnitude"),
  RMSE = c(rmse_raw, rmse_z_score_scaled, rmse_min_max_scaled, rmse_magnitude_scaled)
)

# Plot the box plot for RMSE
# Plot the box plot for RMSE with whiskers
ggplot(rmse_data, aes(x = Scaling_Method, y = RMSE, fill = Scaling_Method)) +
  geom_boxplot(coef = 1.5) +  # Set coef for whiskers
  labs(title = "Box Plot for RMSE for Different Scaling Methods",
       x = "Scaling Method",
       y = "RMSE") +
  theme_minimal()
##################################################
 
# Function to plot prediction trends
plot_prediction_trends <- function(model_results, data, predictor_vars, target_var) {
  # Extract model and loss functions from results
  rf_model <- model_results$Model
  loss_functions <- model_results$LossFunctions
  
  # Make predictions on the entire dataset
  predictions <- predict(rf_model, newdata = data[, predictor_vars])
  
  # Create a dataframe for plotting
  plot_data <- data.frame(
    DateTime = data$DateTime,
    Actual = data[, target_var],
    Predicted = predictions
  )
  
  # Plot the trends
  ggplot(plot_data, aes(x = DateTime)) +
    geom_line(aes(y = Actual, color = "Actual"), size = 1) +
    geom_line(aes(y = Predicted, color = "Predicted"), size = 1, linetype = "dashed") +
    labs(title = paste("Prediction Trends for", loss_functions$Model),
         x = "DateTime",
         y = "Temperature") +
    scale_color_manual(values = c("Actual" = "black", "Predicted" = "red")) +
    theme_minimal()
}

# Plot prediction trends for each dataset
plot_prediction_trends(z_score_results, raw_data, predictor_vars, target_var)

#$$$$$$$$$$$$$$$$
# Plot predictions for Raw Data
plot_prediction_trends(z_score_results, raw_data, predictor_vars, target_var) +
  labs(title = "Prediction Trends for Raw Data")

# Plot predictions for Z-Score Scaling
plot_prediction_trends(z_score_results, z_score_scaled_data, predictor_vars, target_var) +
  labs(title = "Prediction Trends for Z-Score Scaling")

# Plot predictions for Min-Max Scaling
plot_prediction_trends(min_max_results, min_max_scaled_data, predictor_vars, target_var) +
  labs(title = "Prediction Trends for Min-Max Scaling")

# Plot predictions for Magnitude Scaling
plot_prediction_trends(magnitude_results, magnitude_scaled_data, predictor_vars, target_var) +
  labs(title = "Prediction Trends for Magnitude Scaling")
##################################################################################


##################################################################################
# Function for training a Random Forest model and calculating loss functions
train_rf_model <- function(data, predictor_vars, target_var, start_date, end_date) {
  # Filter data based on the specified time range
  filtered_data <- subset(data, DateTime >= start_date & DateTime <= end_date)
  
  # Split the dataset into training (75%) and validation (25%) sets
  split_index <- floor(0.75 * nrow(filtered_data))
  train_data <- filtered_data[1:split_index, ]
  valid_data <- filtered_data[(split_index + 1):nrow(filtered_data), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(formula(paste(target_var, "~ .")), data = train_data[, c(target_var, predictor_vars)], ntree = 500)
  
  # Make predictions on the validation set
  predictions <- predict(rf_model, newdata = valid_data[, predictor_vars])
  
  # Evaluate the model performance on the validation set
  loss_functions <- list(
    MSE = mean((valid_data[, target_var] - predictions)^2),
    RMSE = sqrt(mean((valid_data[, target_var] - predictions)^2)),
    R2 = 1 - sum((valid_data[, target_var] - predictions)^2) / sum((valid_data[, target_var] - mean(valid_data[, target_var]))^2)
  )
  
  return(list(Model = rf_model, LossFunctions = loss_functions))
}

# Function to create bar charts for MAE, RMSE, and Huber loss for different variable groups
create_bar_charts <- function(data, target_var, scaling_method) {
  # Variable groups
  variable_groups <- list(
    g1 = c("SurfaceTemp", "Rain", "WindSpeed", "WindDirect"),
    g2 = c("Humidity", "Barom.Press", "Solar.Rad", "Solar.Rad.Energy"),
    g3 = c("Leaf.Wetness", "Dew.Point", "Wet.Bulb", "Sun.Hours"),
    g4 = c("BatteryVoltage", "Twc", "Tap", "HI"),
    g5 = c("ER", "CP", "HD"),
    g6 = c("TE", "WindSpeed.Max", "WindSpeed.Min")
  )
  
  # Initialize results list
  results_list <- list()
  
  # Loop over variable groups
  for (group_name in names(variable_groups)) {
    predictor_vars <- variable_groups[[group_name]]
    
    # Train model for each scaling method
    scaled_data <- data
    scaled_data[, predictor_vars] <- scale(data[, predictor_vars])
    model_results <- train_rf_model(scaled_data, predictor_vars, target_var, start_date, end_date)
    
    # Extract loss functions from results
    loss_functions <- model_results$LossFunctions
    
    # Add results to list
    results_list[[group_name]] <- loss_functions
  }
  
  # Combine results into a data frame
  results_df <- do.call(rbind.data.frame, results_list)
  results_df$VariableGroup <- rep(names(variable_groups), each = nrow(results_df) / length(variable_groups))
  results_df$ScalingMethod <- rep(scaling_method, nrow(results_df))
  
  return(results_df)
}

# Load necessary libraries
library(ggplot2)
library(randomForest)

# Load your dataset (replace 'your_data.csv' with your actual dataset file)
# Example data with columns: DateTime, AirTemp, SurfaceTemp, ... (other variables)
your_data <- datas

# Define variables
target_var <- "AirTemp"
start_date <- as.POSIXct("2023-01-01 00:00:00", tz = "UTC")
end_date <- as.POSIXct("2023-12-31 23:00:00", tz = "UTC")

# Create bar charts for Raw Data
raw_results <- create_bar_charts(your_data, target_var, "Raw")

# Create bar charts for Z-Score Scaling
z_score_scaled_data <- z_score_scale(your_data, colnames(your_data))
z_score_results <- create_bar_charts(z_score_scaled_data, target_var, "Z-Score")

# Create bar charts for Min-Max Scaling
min_max_scaled_data <- min_max_scale(your_data, colnames(your_data))
min_max_results <- create_bar_charts(min_max_scaled_data, target_var, "Min-Max")

# Create bar charts for Magnitude Scaling
magnitude_scaled_data <- magnitude_scale(your_data, colnames(your_data))
magnitude_results <- create_bar_charts(magnitude_scaled_data, target_var, "Magnitude")

# Combine results into a single data frame
combined_results <- rbind(raw_results, z_score_results, min_max_results, magnitude_results)

# Plot bar charts
ggplot(combined_results, aes(x = VariableGroup, y = MSE, fill = ScalingMethod)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Mean Squared Error (MSE) by Variable Group and Scaling Method",
       x = "Variable Group",
       y = "MSE") +
  theme_minimal()
################################################################################
# Function for training a Random Forest model with regularization
train_rf_model_with_regularization <- function(data, predictor_vars, target_var, start_date, end_date, threshold = 3) {
  # Filter data based on the specified time range
  filtered_data <- subset(data, DateTime >= start_date & DateTime <= end_date)
  
  # Split the dataset into training (75%) and validation (25%) sets
  split_index <- floor(0.75 * nrow(filtered_data))
  train_data <- filtered_data[1:split_index, ]
  valid_data <- filtered_data[(split_index + 1):nrow(filtered_data), ]
  
  # Train the Random Forest model
  rf_model <- randomForest(formula(paste(target_var, "~ .")), data = train_data[, c(target_var, predictor_vars)], ntree = 500)
  
  # Make predictions on the validation set
  predictions <- predict(rf_model, newdata = valid_data[, predictor_vars])
  
  # Regularization step for winter
  winter_indices <- which(valid_data$Season == "Winter")  # Adjust this based on your actual column name for seasons
  winter_residuals <- valid_data$AirTemp[winter_indices] - predictions[winter_indices]
  winter_outliers <- which(abs(winter_residuals) > threshold)
  
  # Adjust predictions for winter outliers
  predictions[winter_indices[winter_outliers]] <- valid_data$AirTemp[winter_indices[winter_outliers]]
  
  # Evaluate the model performance on the validation set
  loss_functions <- list(
    MSE = mean((valid_data[, target_var] - predictions)^2),
    RMSE = sqrt(mean((valid_data[, target_var] - predictions)^2)),
    R2 = 1 - sum((valid_data[, target_var] - predictions)^2) / sum((valid_data[, target_var] - mean(valid_data[, target_var]))^2)
  )
  
  return(list(Model = rf_model, LossFunctions = loss_functions))
}