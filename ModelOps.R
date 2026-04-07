library(ggplot2)
library(randomForest)
library(tuneRanger)
library(ranger)
library(pdp,lattice)
library(caTools)
library(Metrics)
library(knitr)
library(caret)
library(h2o)
library(mlr)
library(vip)
library(OpenML)
library(rsq)
library(gridExtra)

Expe1<-function(){
  set.seed(2024)
  source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get the datasets
  Lista <- DataOps("2023-01-01", "2023-01-05") # Access the individual datasets from the list
  
  D1 <- Lista$dataset1
  D2 <- Lista$dataset2
  D3 <- Lista$dataset3
  D4 <- as.data.frame(Lista$dataset4)
  
  FinalDataSet <- D3
  SplitRatio = 0.95
  
#  FinalDataSet <- Default_WithR
  
  
  train_test_split <- function(df){
    sample = sample.split(df, SplitRatio)
    train = subset(df, sample == TRUE)
    test  = subset(df, sample == FALSE)
    return (list(train, test))
  }
  
  
  # New COVID-19 Positive Cases
  df_train <- train_test_split(FinalDataSet)[[1]]
  df_test <- train_test_split(FinalDataSet)[[2]]
  dim(df_train)
  dim(df_test)
  
  
  #set.seed(23)
  # Training the Random Forest regression model
  rf_model1 <- randomForest(Leaf.Wetness ~ ., data = df_train, ntree=500, mtry = 1, nodesie = 5, importance = TRUE) 
  
  str(rf_model1$oob.times)
  ########################################################
  
  ################################################################################
  # Step 5: Model Evaluation during validation phase
  # Making predictions on the test set
  predictions <- predict(rf_model1, newdata = df_test)
  
  #Initialise
  y_actual = df_test$Leaf.Wetness
  y_predicted = predictions
  
  
  # Function to calculate R-squared (R²)
  r_squared1 <- function(y_actual, y_predicted) {
    ssr <- sum((y_predicted - y_actual)^2)
    sst <- sum((y_actual - mean(y_actual))^2)
    1 - (ssr / sst)
  }
  
  
  # Function to calculate RMSE
  rmse1 <- function(y_actual, y_predicted) {
    sqrt(mean((y_actual - y_predicted)^2))
  }
  
  # Function to calculate MAE
  mae1 <- function(y_actual, y_predicted) {
    mean(abs(y_actual - y_predicted))
  }
  
  
  
  R_squared1 = round(r_squared1(y_actual, y_predicted), 1)
  RMSE1 = round(rmse1(y_actual, y_predicted), 1)
  MAE1 = round(mae1(y_actual, y_predicted), 1)
  
  # Calculate the data range
  x_range <- range(y_actual)
  y_range <- range(y_predicted)
  
  # Calculate coordinates for the annotation
  annotation_x <- max(x_range) - 0.7 * diff(x_range)
  annotation_y <- max(y_range) + 0.1 * diff(y_range)
  
  
  # Create a scatter plot with a line of best fit
  p1 <- ggplot(data = NULL, aes(x = y_actual, y = y_predicted)) +
    geom_point(shape = 8, color = "black", size = 1.5) +  # Use shape 8 for stars
    geom_smooth(method = "lm", color = "black") +  
    
    annotate(
      "text",
      x = annotation_x,  # Adjusted x-coordinate
      y = annotation_y,  # Adjusted y-coordinate
      label = sprintf("R^2 == %.2f", R_squared1),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = annotation_x + 0.18 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.01 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("RMSE == %.2f", RMSE1),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = annotation_x + 0.4 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.014 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("MAE == %.2f", MAE1),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    labs(
      x = "Actual Surface Temperature",  # Label for the X-axis
      y = "Predicted Surface Temperature",  # Label for the Y-axis
      title = "2020-02-17 to 2020-06-19"  # Title of the plot
    ) +
    theme_minimal()  # Remove gridlines and use a minimal theme
  
  p1
  
  # Create an empty list to store the figures
  figure_list <- list()
  
  ########################################################
 # vip1 <- vip(rf_model1, target = "Leaf.Wetness", aesthetics = list(color = "black", fill = "white"),size = 0.3)+
  #  ggtitle("B1: 2020-02-17 to 2020-06-19")  #
  
  #figure_list$vip1 <- vip1
  
  # Modify the size parameter to make bars thinner
  vip1 <- vip(rf_model1, target = "Leaf.Wetness", size = 0.1,
              aesthetics = list(color = "black", fill = "white"),
              geom = c("point")
              # Choose among: "point", "boxplot", "violin","col")
  ) +
    ggtitle("B1: 2020-02-17 to 2020-06-19") +
    geom_point(size = 2, shape = 3, color = "black") +  # Use shape 3 for crosses
    geom_text(aes(label = sprintf("%.2f", Importance)), vjust = -0.5, color = "black") +  # Add numerical labels
    theme(panel.grid = element_blank(),  # Remove grids
          axis.text = element_text(color = "black"),  # Set axis text color to black
          axis.title = element_text(color = "black"),  # Set axis title color to black
          plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"))  # Set panel background to white
  
  # Save the plot with reduced dimensions
  ggsave("vip_plot.png", vip1, width = 6, height = 4, units = "in")
  
  # Assuming 'figure_list' is a list to store your figures
  figure_list$vip1 <- vip1
  
  # Retrieve the Top6 predictors variables
  top4 <- topPredictors(rf_model1, n = 10); top4
  
  # Construct partial dependence functions for top 4 predictors
  pd1 <- NULL
  for (i in top4) {
    tmp <- partial(rf_model1, pred.var = i)
    names(tmp) <- c("x", "y")
    pd1 <- rbind(pd1,  cbind(tmp, predictor = i))
  }
  
  # Set the number of columns and rows for the facet grid
  num_cols <- 5  # Set the desired number of columns
  num_rows <- 2
  
  # Display partial dependence functions with dashed lines
  pd1 <- ggplot(pd1, aes(x, y)) +
    geom_line(linetype = "dashed") +  # Set linetype to dashed
    facet_wrap(~ predictor, scales = "free", ncol = num_cols, nrow = num_rows) +  # Set number of columns and rows
    geom_smooth() +
    ylab("Leaf Wetness") +
    xlab("Predictor Variables") +
    theme_bw() +
    ggtitle("2023-01-01 to 2023-06-19")
  
  # Print the plot
  print(pd1)
  
  
  
  figure_list$pd1 <- pd1
  
  figure_list$p1 <- p1
  
  
  return(figure_list)
  
}

# Call the function to generate the list of figures
figures <- Expe1()

# Access and display individual figures from the list
print(figures$p1)
print(figures$vip1)
print(figures$pd1)
