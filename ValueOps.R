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
library(dygraphs)
library(corrplot)
library(dplyr)



Expe1<-function(){
  set.seed(2024)
  source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get the datasets
  source("D:/PhD Material/DataBw/LossMetrics.R")# Call the function to get the datasets
  
  Lista <- DataOps("2023-01-01", "2023-03-05") # Access the individual datasets from the list
  
  D1A <- Lista$dataset1 # Raw data with  All priors
  D1B <- Lista$dataset1 # Raw data without priors
  D1C <- Lista$dataset1 # Raw data with  VI priors
  
  
  D2A <- Lista$dataset2 # Maginitude Scaling with  All priors
  D2B <- Lista$dataset2 # Maginitude Scaling without priors
  D2B <- Lista$dataset2 # Maginitude Scaling with VI priors
  
  D3A <- Lista$dataset3 # Min-Max Normalisation with  All priors
  D3B <- Lista$dataset3 # Min-Max Normalisation without priors
  D3C <- Lista$dataset3 # Min-Max Normalisation with VI priors
  
  
  D4A <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with  All priors
  D4B <- as.data.frame(Lista$dataset4) # Z-Score Normalisation without priors
  D4C <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with VI priors
   
  
  FinalDataSet <- D3A
  
################################################################################
 
################################################################################
  # Compute Kendall Tau correlation matrix
  df1 <- FinalDataSet %>% select(-DateTime)
  cor_matrix <- cor(df1, method = "kendall")
  cor_matrix[is.na(cor_matrix)] <- 0 # Replace missing values with zero
  
  # Plot the correlation matrix using corrplot
  # Omit colors (use black) and slant the labels
  #p0 <-corrplot(cor_matrix, method = "number",   tl.cex = 0.7, tl.srt = 45, type = "upper")
  p0 <- corrplot(cor_matrix, method = c("number"), col = "black", tl.col = "black",
                 bg = "white", number.cex = 0.7, tl.cex = 0.7, tl.srt = 45)

  
  #p0 <- corrplot(cor_matrix, method = "number", type = "upper", col = "black")
  p0
  ###############################################################
  
  SplitRatio = 0.75
  
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
  
  ###############################################################
  # Create an empty list to store the figures
  figure_list <- list()
  ###############################################################
  
  TopNpredictors <- topPredictors(rf_model1, n = 18)
  TopNpredictors
  
  # Function to calculate R-squared (R²)
  
  
  # Calculate loss metrics
  RMSE <- round(rmse(y_actual, y_predicted), 1)
  MAE <- round(mae(y_actual, y_predicted), 1)
  MAPE <- round(mape(y_actual, y_predicted), 2)
  R_squared <- round(r_squared(y_actual, y_predicted), 1)
  MSLE <- round(msle(y_actual, y_predicted), 4)
  KLD <- round(kl_divergence(y_actual, y_predicted), 4)
  SS <- round(skill_score(y_actual, y_predicted), 4)
  Q <- round(quantile_loss(y_actual, y_predicted, 0.5), 4)  # Example for the median quantile (0.5)
  CS <- round(cosine_similarity(y_actual, y_predicted), 4)
  H <- round(huber_loss(y_actual, y_predicted, 1.0), 4)  # Example with delta = 1.0
  PH <- round(pseudo_huber_loss(y_actual, y_predicted, 1.0), 4)  # Example with delta = 1.0
  LHC <- round(log_cosh_loss(y_actual, y_predicted), 4)
  SMAPE <- round(smape(y_actual, y_predicted), 4)
  
  # Example loss functions and values
  loss_functions <- c("RMSE","MAE", "Rsquared","MSLE", "MAPE", "KLD", 
                      "SS", "Q", "CS", "H", "PH","LHC", "SMAPE")
  loss_values <- c(RMSE,  MAE, R_squared,MSLE,MAPE,KLD,SS,Q,CS,H,PH,LHC,SMAPE)
  #loss_values
  # Create a data frame
  loss_data <- data.frame(Loss_Function = loss_functions, Value = loss_values)
  length(loss_data)
  Metrics <- as.data.frame(rbind(loss_functions,loss_values))
  # Print the tabulated format
  print(Metrics)
   
   # Create a data frame
   loss_dataL <- data.frame(Loss_Function = loss_functions, Value = loss_values)
   print(loss_dataL)
   
   
  ########################
  # Training Phase Scatter Plot
  train_predictions <- predict(rf_model1, newdata = df_train)
  y_train_actual <- df_train$Leaf.Wetness
  y_train_predicted <- train_predictions
  
  # Testing Phase Scatter Plot
  test_predictions <- predict(rf_model1, newdata = df_test)
  y_test_actual <- df_test$Leaf.Wetness
  y_test_predicted <- test_predictions
  
  # Function to create scatter plot
  create_scatter_plot <- function(y_actual, y_predicted, title) {
    ggplot(data = NULL, aes(x = y_actual, y = y_predicted)) +
      geom_point(shape = 8, color = "black", size = 1.5) +  # Use shape 8 for stars
      geom_smooth(method = "lm", color = "black") +
      labs(
        x = "Actual Leaf Wetness",
        y = "Predicted Leaf Wetness",
        title = title
      ) +
      theme_minimal()
  }
  
  # Create and display scatter plots
  train_scatter_plot <- create_scatter_plot(y_train_actual, y_train_predicted, "Training Phase")
  test_scatter_plot <- create_scatter_plot(y_test_actual, y_test_predicted, "Testing Phase")
  
  # Display the scatter plots
  # Arrange plots side by side
  grid.arrange(train_scatter_plot, test_scatter_plot,train_scatter_plot, test_scatter_plot, ncol = 4)
  ############################
  
  ##############################
  # Calculate residuals
  residuals <- y_actual - y_predicted
  
  # Create a box plot for residuals with larger stars
 # boxplot(residuals, main = "Residuals Box Plot", ylab = "Residuals", pch = "*", cex = 2)
  #################################################################
    
  ################################################################
  # Assume you have residuals_1, residuals_2, residuals_3, and residuals_4 for the four cases
  
  # Combine the residuals into a list
  residuals_list <- list(residuals, residuals, residuals, residuals)
  
  # Create a box plot with multiple boxes
  BoxP <- boxplot(residuals_list, names = c("Without Priors", "With VI Priors", "With All Priors", "With Tau Priors"),
          main = "Residuals Box Plots",
          ylab = "Residuals", pch = "*", cex = 2)
  ##########################################################################
  # Combine the actual and predicted values into a data frame
  data <- data.frame(Time = 1:length(y_actual), Actual = y_actual, Predicted = y_predicted)
  
  # Plot using dygraph
  ps <- dygraph(data, "Time") %>%
    dySeries("Actual", label = "Actual") %>%
    dySeries("Predicted", label = "Predicted") %>%
    dyOptions(strokePattern = "solid", drawPoints = TRUE) %>%
    dyRangeSelector() 
    ps
    
   ###############################################################
  
  ################################################################
  
 
  
  
  
  
  
  # Calculate the data range
  x_range <- range(y_actual)
  y_range <- range(y_predicted)
  
  # Calculate coordinates for the annotation
  annotation_x <- max(x_range) - 0.7 * diff(x_range)
  annotation_y <- max(y_range) + 0.1 * diff(y_range)
  
  
  # Create a scatter plot with a line of best fit
  # Create a scatter plot with a line of best fit
  p1 <- ggplot(data = NULL, aes(x = y_actual, y = y_predicted)) +
    geom_point(shape = 8, color = "black", size = 1.5) +  # Use shape 8 for stars
    geom_smooth(method = "lm", color = "black") +  
    
    annotate(
      "text",
      x = 0.125*annotation_x,  # Adjusted x-coordinate
      y = annotation_y,  # Adjusted y-coordinate
      label = sprintf("R^2 == %.2f", R_squared),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    
    annotate(
      "text",
      x = 0.125*annotation_x,  # Adjusted x-coordinate
      y = annotation_y,  # Adjusted y-coordinate
      label = sprintf("R^2 == %.2f", R_squared),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = 0.6*annotation_x,  #+ 0.1 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.01 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("RMSE == %.2f", RMSE),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = 1.14*annotation_x, # + 0.4 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.011 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("MAE == %.2f", MAE),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    annotate(
      "text",
      x = 1.7*annotation_x, # + 0.6 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.014 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("MAPE == %.2f", MAPE),  # Display MAPE2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    annotate(
      "text",
      x = 2.30*annotation_x, # + 0.4 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.011 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("LHC == %.2f", LHC),  # Display KL_divergence as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
   # labs(
   #   x = "Actual Surface Temperature",  # Label for the X-axis
   #   y = "Predicted Surface Temperature",  # Label for the Y-axis
   #   title = "2020-02-17 to 2020-06-19"  # Title of the plot
   # ) +
  #  theme_minimal()  # Remove gridlines and use a minimal theme
  
 
    
    labs(
      x = "Actual Surface Temperature",  # Label for the X-axis
      y = "Predicted Surface Temperature",  # Label for the Y-axis
      title = "2020-02-17 to 2020-06-19"  # Title of the plot
    ) +
    theme_minimal() +  # Remove gridlines and use a minimal theme
    theme(plot.title = element_text(size = rel(0.8)))  # Set title font size
  p1
  
  # Add the plot to the figure_list
  figure_list$p0 <- p0
  figure_list$ps <- ps
  figure_list$BoxP <- BoxP
  figure_list$loss_dataL <- loss_dataL
  figure_list$Metrics <- Metrics
  
  figure_list$p1 <- p1
   
  
  
  ########################################################
  # vip1 <- vip(rf_model1, target = "Leaf.Wetness", aesthetics = list(color = "black", fill = "white"),size = 0.3)+
  #  ggtitle("B1: 2020-02-17 to 2020-06-19")  #
  
   
  # Modify the size parameter to make bars thinner
  vip1 <- vip(rf_model1, target = "Leaf.Wetness", size = 0.1, num_features = 20,
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
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(size = rel(0.8)))  # Set title font size
  
  # Save the plot with reduced dimensions
  ggsave("vip_plot.png", vip1, width = 6, height = 4, units = "in")
    vip1
  
  # Assuming 'figure_list' is a list to store your figures
  figure_list$vip1 <- vip1
  
  # Retrieve the Top6 predictors variables
  top4 <- topPredictors(rf_model1, n = 12); top4
  
  # Construct partial dependence functions for top 4 predictors
  pd1 <- NULL
  for (i in top4) {
    tmp <- partial(rf_model1, pred.var = i)
    names(tmp) <- c("x", "y")
    pd1 <- rbind(pd1,  cbind(tmp, predictor = i))
  }
  
  pd1
   
  
  # Construct partial dependence functions for top 6 predictors
  pd12 <- NULL
  for (i in top4) {
    tmp <- partial(rf_model1, pred.var = i)
    names(tmp) <- c("x", "y")
    pd12 <- rbind(pd12,  cbind(tmp, predictor = i))
  }
  
  
  # Display partial dependence functions with thicker dashed lines
  # Calculate the number of rows and columns for facet_wrap
  num_cols <- 6
  num_rows <- 3
  
  # Display partial dependence functions with thicker dashed lines
  pd1 <- ggplot(pd1, aes(x, y)) +
    geom_line(linetype = "dashed", size = 0.75, color = "black") +  # Set linetype to dashed, increase size, and change color
    facet_wrap(~ predictor, scales = "free", ncol = num_cols, nrow = num_rows) +  # Adjust facet_wrap parameters
    geom_smooth(color = "black") +  # Change color of the smoothing line to black
    ylab("Leaf Wetness") +
    xlab("Predictor Variables") +
    theme_bw() +
    ggtitle("2020-02-17 to 2020-06-19") +
    theme(plot.title = element_text(size = rel(0.8)))  # Set title font size
  
  # Display the plot
  pd1
  
  figure_list$pd1 <- pd1
  
  
  return(figure_list)
  
}

# Call the function to generate the list of figures
figures <- Expe1()

# Access and display individual figures from the list
print(figures$ps)
print(figures$p0)
print(figures$p1)
print(figures$vip1)
print(figures$BoxP)
print(figures$pd1)
print(figures$Metrics)
print(figures$loss_dataL)



# Example loss functions and values
#loss_functions <- c("Mean Squared Error (MSE)", "Mean Absolute Error (MAE)", "Huber Loss", "Quantile Loss")
#loss_values <- c(1.154, 0.855, 1.023, 1.342)

# Create a data frame
#loss_data <- data.frame(Loss_Function = loss_functions, Value = loss_values)

# Print the tabulated format
#print(loss_values)
