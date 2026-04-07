# Function to calculate R-squared (R²)
r_squared <- function(y_actual, y_predicted) {
  ssr <- sum((y_predicted - y_actual)^2)
  sst <- sum((y_actual - mean(y_actual))^2)
  1 - (ssr / sst)
}

# Function to calculate RMSE
rmse <- function(y_actual, y_predicted) {
  sqrt(mean((y_actual - y_predicted)^2))
}

# Function to calculate MAE
mae <- function(y_actual, y_predicted) {
  mean(abs(y_actual - y_predicted))
}

# Function to calculate Mean Squared Logarithmic Error (MSLE)
mse <- function(y_actual, y_predicted) {
  mean((y_actual - y_predicted)^2)
}

# Function to calculate Mean Absolute Percentage Error (MAPE)
mape <- function(y_actual, y_predicted) {
  mean(abs((y_actual - y_predicted) / y_actual)) * 100
}

# Function to calculate Kullback-Leibler Divergence Loss
kl_divergence <- function(y_actual, y_predicted) {
  y_actual <- pmax(y_actual, 1e-10)  # Avoid zero or near-zero values
  y_predicted <- pmax(y_predicted, 1e-10)  # Avoid zero or near-zero values
  
  # Ensure valid probability values
  y_actual <- pmin(pmax(y_actual, 0), 1)
  y_predicted <- pmin(pmax(y_predicted, 0), 1)
  
  kl_values <- y_actual * log(y_actual / y_predicted)
  kl_values[is.na(kl_values) | is.infinite(kl_values)] <- 0  # Set NaN or infinite values to zero
  sum(kl_values)
}

# Function to calculate Skill Score (SS)
skill_score <- function(y_actual, y_predicted) {
  numerator <- sum((y_actual - mean(y_actual)) * (y_predicted - mean(y_predicted)))
  denominator <- sqrt(sum((y_actual - mean(y_actual))^2) * sum((y_predicted - mean(y_predicted))^2))
  numerator / denominator
}

# Function to calculate Quantile (Q)
quantile_loss <- function(y_actual, y_predicted, q) {
  sum(pmax(q * (y_actual - y_predicted), (1 - q) * (y_predicted - y_actual)))
}

# Function to calculate Cosine Similarity (CS)
cosine_similarity <- function(y_actual, y_predicted) {
  sum(y_actual * y_predicted) / (sqrt(sum(y_actual^2)) * sqrt(sum(y_predicted^2)))
}

# Function to calculate Huber Loss (Hδ (y, ˆy))
huber_loss <- function(y_actual, y_predicted, delta) {
  abs_diff <- abs(y_actual - y_predicted)
  loss <- ifelse(abs_diff <= delta, 0.5 * abs_diff^2, delta * (abs_diff - 0.5 * delta))
  mean(loss)
}

# Function to calculate Pseudo-Huber Loss (PH)
pseudo_huber_loss <- function(y_actual, y_predicted, delta) {
  delta_sq <- delta^2
  loss <- delta_sq * (sqrt(1 + ((y_actual - y_predicted) / delta)^2) - 1)
  mean(loss)
}

# Function to calculate Log-Cosh Loss (LC)
log_cosh_loss <- function(y_actual, y_predicted) {
  log_cosh <- log(cosh(y_actual - y_predicted))
  mean(log_cosh)
}

# Function to calculate Symmetric Mean Absolute Percentage Error (SMAPE)
smape <- function(y_actual, y_predicted) {
  200 * mean(abs(y_actual - y_predicted) / (abs(y_actual) + abs(y_predicted)))
}