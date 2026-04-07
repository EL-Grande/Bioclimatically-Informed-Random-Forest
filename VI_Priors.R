# Load required libraries
library(randomForest)

# Step 1: Generate some example data (replace this with your dataset)
set.seed(123)
n <- 100
train_data <- data.frame(
  X1 = rnorm(n),
  X2 = rnorm(n),
  X3 = rnorm(n),
  X4 = rnorm(n),
  X5 = rnorm(n),
  Y = rnorm(n)
)

# Step 2: Train Random Forest model
rf_model <- randomForest(Y ~ ., data = train_data)

# Step 3: Extract variable importance
importance_ranking <- importance(rf_model)

# Display top 5 important variables
top_5_variables <- rownames(importance_ranking)[1:5]
print(paste("Top 5 important variables:", paste(top_5_variables, collapse = ", ")))

# Step 4: Use top 5 variables for prediction in the second phase
# For demonstration purposes, we use the same training data in this example
second_phase_data <- train_data[, c(top_5_variables, "Y")]
