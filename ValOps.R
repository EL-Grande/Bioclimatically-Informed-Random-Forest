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


Expe1<-function(FinalDataSet){
  set.seed(2024)
 # source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get the datasets
  source("D:/PhD Material/DataBw/LossMetrics.R")# Call the function to get the datasets
  source("D:/PhD Material/DataBw/create_scatter_plot.R")
  source("D:/PhD Material/DataBw/PlotDygraph.R")
  #source("D:/PhD Material/DataBw/myDataset.R")
  
 
 # Lista <- DataOps("2023-01-01", "2023-03-05") # Access the individual datasets from the list
 # FinalDataSet <- myDataset(Lista)
  ################################################################################
  

   
  ###############################################################
  
  SplitRatio = 0.75
  train_test_split <- function(df){
    sample = sample.split(df, SplitRatio)
    train = subset(df, sample == TRUE)
    test  = subset(df, sample == FALSE)
    return (list(train, test))
  }
  
  
  # New COVID-19 Positive Cases
  df_train <- train_test_split(FinalDataSet)[[1]]
  df_test <- train_test_split(FinalDataSet)[[2]]
   
  # Training the Random Forest regression model
  rf_model1 <- randomForest(LW ~ ., data = df_train, ntree=500, mtry = 1, nodesie = 5, importance = TRUE) 
  predictions <- predict(rf_model1, newdata = df_test)
   
  y_actual = df_test$LW
  y_predicted = predictions
  
  ###############################################################
  # Create an empty list to store the figures
  figure_list <- list()
  ###############################################################
  
  TopNpredictors <- topPredictors(rf_model1, n = 18)
    
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

  Metrics <- as.data.frame(rbind(loss_functions,loss_values))
   
  # Create a data frame
  loss_dataL <- data.frame(Loss_Function = loss_functions, Value = loss_values)
  #print(loss_dataL)
  
  ########################
  # Training Phase Scatter Plot
  train_predictions <- predict(rf_model1, newdata = df_train)
  y_train_actual <- df_train$LW
  y_train_predicted <- train_predictions
  
  # Testing Phase Scatter Plot
  test_predictions <- predict(rf_model1, newdata = df_test)
  y_test_actual <- df_test$LW
  y_test_predicted <- test_predictions
  
####################################################################################
  #   Return Train Errors
  # Calculate loss metrics
  RMSE2 <- round(rmse(y_train_actual, y_train_predicted), 1)
  MAE2 <- round(mae(y_train_actual, y_train_predicted), 1)
  MAPE2 <- round(mape(y_train_actual, y_train_predicted), 2)
  R_squared2 <- round(r_squared(y_train_actual, y_train_predicted), 1)
  MSLE2 <- round(msle(y_train_actual, y_train_predicted), 4)
  KLD2 <- round(kl_divergence(y_train_actual, y_train_predicted), 4)
  SS2 <- round(skill_score(y_train_actual, y_train_predicted), 4)
  Q2 <- round(quantile_loss(y_train_actual, y_train_predicted, 0.5), 4)  # Example for the median quantile (0.5)
  CS2 <- round(cosine_similarity(y_train_actual, y_train_predicted), 4)
  H2 <- round(huber_loss(y_train_actual, y_train_predicted, 1.0), 4)  # Example with delta = 1.0
  PH2 <- round(pseudo_huber_loss(y_train_actual, y_train_predicted, 1.0), 4)  # Example with delta = 1.0
  LHC2 <- round(log_cosh_loss(y_train_actual, y_train_predicted), 4)
  SMAPE2 <- round(smape(y_train_actual, y_train_predicted), 4)
  
  #################################################################################################  
  # Create and display scatter plots
  train_scatter_plot <- create_scatter_plot(y_train_actual, y_train_predicted, RMSE2,  MAE2, R_squared2,MSLE2,MAPE2,KLD2,SS2,Q2,CS2,H2,PH2,LHC2,SMAPE2, "Training Phase")
  test_scatter_plot <- create_scatter_plot(y_test_actual, y_test_predicted, RMSE,  MAE, R_squared,MSLE,MAPE,KLD,SS,Q,CS,H,PH,LHC,SMAPE, "Validation Phase")
  
  
  #ListaScat <- NULL
  #ListaScat$ScaterTrain <- train_scatter_plot
  #ListaScat$test_scatter_plot <- test_scatter_plot
  # Display the scatter plots
  # Arrange plots side by side
 # Lista_Scata <- grid.arrange(train_scatter_plot, test_scatter_plot, ncol = 2)
#################################################################################################  
  # Calculate residuals
  residuals <- y_actual - y_predicted
   
   
  ##########################################################################
  # Combine the actual and predicted values into a data frame
  #data <- data.frame(Time = 1:length(y_actual), Actual = y_actual, Predicted = y_predicted)
  
  # Plot using dygraph
 # dyG <- PlotDygraph(data)
  
  ###############################################################
  
  ################################################################
  
  
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
     
    
  labs(
    x = "Actual Leaf Wetness",  # Label for the X-axis
    y = "Predicted Leaf Wetness",  # Label for the Y-axis
    title = "2020-02-17 to 2020-03-31"  # Title of the plot
  ) +
    theme_minimal() +  # Remove gridlines and use a minimal theme
    theme(plot.title = element_text(size = rel(0.8)))  # Set title font size
   
  
  ########################################################
   
  
  # Modify the size parameter to make bars thinner
  vip1 <- vip(rf_model1, target = "LW", size = 0.1, num_features = 18,
              aesthetics = list(color = "black", fill = "white"),
              geom = c("point")
              # Choose among: "point", "boxplot", "violin","col")
  ) +
    #ggtitle("B1: 2020-02-17 to 2020-03-19") +
    geom_point(size = 2, shape = 3, color = "black") +  # Use shape 3 for crosses
    geom_text(aes(label = sprintf("%.1f", Importance)), vjust = -0.5, color = "black", size = 4) +  # Add numerical labels
    theme(panel.grid = element_blank(),  # Remove grids
          axis.text = element_text(color = "black"),  # Set axis text color to black
          axis.title = element_text(color = "black"),  # Set axis title color to black
          plot.background = element_rect(fill = "white"),  # Set plot background to white
          panel.background = element_rect(fill = "white"),
          plot.title = element_text(size = rel(0.8)))  # Set title font size
  
  # Save the plot with reduced dimensions
  ggsave("vip_plot.png", vip1, width = 4.5, height = 4, units = "in")
  vip1
  
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
  
  #pd1
  
   
  # Display partial dependence functions with thicker dashed lines
  # Calculate the number of rows and columns for facet_wrap
  num_cols <- 5
  num_rows <- 2
  
  # Display partial dependence functions with thicker dashed lines
  # Display partial dependence functions with dashed lines
  pd1 <- ggplot(pd1, aes(x, y)) +
    geom_line(linetype = "dashed") +  # Set linetype to dashed
    facet_wrap(~ predictor, scales = "free", ncol = num_cols, nrow = num_rows) +  # Set number of columns and rows
    geom_smooth() +
    ylab("Leaf Wetness") +
    xlab("Predictor Variables") +
    theme_bw() #+
    #ggtitle("2023-01-01 to 2023-02-31")
  
   
  # Add the plot to the figure_list
  #figure_list$p0 <- p0
  #figure_list$loss_dataL <- loss_dataL
  #figure_list$Metrics <- Metrics
 # figure_list$p1 <- p1
  #figure_list$dyG <- dyG
  figure_list$pd1 <- pd1
 # figure_list$residuals <- residuals
 # figure_list$train_scatter_plot <- train_scatter_plot
 # figure_list$test_scatter_plot <- test_scatter_plot
  
  
  
  return(figure_list)
  
}




