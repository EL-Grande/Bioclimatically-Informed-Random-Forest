# Function to calculate Quantile Loss
quantile_loss <- function(y_actual, y_predicted, q) {
  m <- length(y_actual)
  loss <- numeric(m)
  
  for (i in 1:m) {
    if (y_actual[i] < y_predicted[i]) {
      loss[i] <- (q - 1) * (y_actual[i] - y_predicted[i])
    } else {
      loss[i] <- q * (y_actual[i] - y_predicted[i])
    }
  }
  
  mean(loss)
}

# Function to calculate Quantile (Q)
quantile_lossB <- function(y_actual, y_predicted, q) {
  sum(pmax(q * (y_actual - y_predicted), (1 - q) * (y_predicted - y_actual)))
}



# Example usage
y_actual <- c(1, 2, 3, 4, 5)
y_predicted <- c(5, 4, 3, 2, 1)
quantile_level <- 0.75

loss_value <- quantile_loss(y_actual, y_predicted, quantile_level)
cat("Quantile Loss:", loss_value, "\n")

loss_value2 <- quantile_lossB(y_actual, y_predicted, quantile_level)
loss_value2