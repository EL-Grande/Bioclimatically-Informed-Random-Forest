
library(randomForest)
library(pdp)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate an artificial dataset
n <- 500
artificial_data <- data.frame(
  x1 = rnorm(n),
  x2 = rnorm(n),
  x3 = rnorm(n),
  x4 = rnorm(n),
  y = 2 * x1 + 3 * x2 - 1.5 * x3 + 0.5 * x4 + rnorm(n)
)

# Split the dataset into training and testing sets
train_indices <- sample(1:n, 0.8 * n)
df_train <- artificial_data[train_indices, ]
df_test <- artificial_data[-train_indices, ]

# Train a random forest model
rf_model <- randomForest(y ~ ., data = df_train, ntree = 500)

# Identify the top predictors
top_predictors <- topPredictors(rf_model, n = 2)
print(top_predictors)

# Construct partial dependence functions for top predictors
pd_plots <- NULL
for (i in top_predictors) {
  pd_plot <- partial(rf_model, pred.var = i)
  pd_plot <- pd_plot %>% 
    ggplot(aes_string(i, "y")) +
    geom_line() +
    geom_smooth() +
    labs(title = paste("Partial Dependence Plot for", i),
         x = i, y = "Response") +
    theme_bw()
  pd_plots <- c(pd_plots, list(pd_plot))
}

# Display the partial dependence plots
grid.arrange(grobs = pd_plots, ncol = 2)