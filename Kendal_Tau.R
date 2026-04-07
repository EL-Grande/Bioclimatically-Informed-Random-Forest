# Install and load the corrplot package
#install.packages("corrplot")
library(corrplot)

# Generate a sample dataset
set.seed(123)
data <- data.frame(
  X = rnorm(100),
  Y = rnorm(100),
  Z = rnorm(100)
)

# Compute Kendall Tau correlation matrix
cor_matrix <- cor(data, method = "kendall")

# Plot the correlation matrix using corrplot
corrplot(cor_matrix, method = "number", type = "upper", order = "hclust")