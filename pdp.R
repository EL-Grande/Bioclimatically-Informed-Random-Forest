library(ggplot2)
library(randomForest)

# Fit a random forest to the mtcars dataset
data(mtcars, package = "datasets")
set.seed(101)
#mtcars.rf <- randomForest(mpg ~ ., data = mtcars, mtry = 5, importance = TRUE)

# Topfour predictors
top4 <- topPredictors(mtcars.rf, n = 6)


## End(Not run)
