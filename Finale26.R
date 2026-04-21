library(ggplot2)
library(randomForest)
library(pdp,lattice)
library(caTools)
library(Metrics)
library(knitr)
library(ranger)
library(vip)
library(caret)
library(dygraphs)
library(corrplot)
library(dplyr)
library(rsq)
library(h2o)
library(OpenML)
library(gridExtra)
library(tuneRanger)
library(mlr)

  source("D:/PhD Material/DataBw/DataOps.R")# Call the function to get the datasets
  source("D:/PhD Material/DataBw/LossMetrics.R")# Call the function to invoke loss functions
  
  Lista <- DataOps("2023-01-01", "2023-03-31") # Access the individual datasets from the list
  drop_vars <- c("Twc","Tap","HI","ER","CP","HD","TE")
  
  D1A <- Lista$dataset1# Raw data with  All priors
  D1B <- Lista$dataset1[, !(names(Lista$dataset1) %in% drop_vars)] # Raw data without priors
  #D1C <- Lista$dataset1 # Raw data with  VI priors
  
  D2A <- Lista$dataset2 # Maginitude Scaling with  All priors
  D2B <- Lista$dataset2[, !(names(Lista$dataset2) %in% drop_vars)]  # Maginitude Scaling without priors
  #D2C <- Lista$dataset2 # Maginitude Scaling with VI priors
  
  D3A <- Lista$dataset3 # Min-Max Normalisation with  All priors
  D3B <- Lista$dataset3[, !(names(Lista$dataset3) %in% drop_vars)]  # Min-Max Normalisation without priors
  #D3C <- Lista$dataset3 # Min-Max Normalisation with VI priors
  
  
  D4A <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with  All priors
  D4B <- as.data.frame(Lista$dataset4[, !(names(Lista$dataset4) %in% drop_vars)] ) # Z-Score Normalisation without priors
  #D4C <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with VI priors
  
  
  FinalDataSet <- D1A
  set.seed(2024)
  names(FinalDataSet)
  ################################################################################
  
  ################################################################################
  # Compute Kendall Tau correlation matrix
  df1 <- FinalDataSet %>% select(-TS)
  ## cor_matrix <- cor(df1, method = "kendall")
  ## cor_matrix[is.na(cor_matrix)] <- 0 # Replace missing values with zero
  
  # Plot the correlation matrix using corrplot
  # Omit colors (use black) and slant the labels
  #p0 <-corrplot(cor_matrix, method = "number",   tl.cex = 0.7, tl.srt = 45, type = "upper")
  ## p0 <- corrplot(cor_matrix, method = c("number"), col = "black", tl.col = "black",
  ##                bg = "white", number.cex = 0.7, tl.cex = 0.7, tl.srt = 45)
  
  
  #p0 <- corrplot(cor_matrix, method = "number", type = "upper", col = "black")
  # p0
  ###############################################################
  
  SplitRatio = 0.75
  
  #  FinalDataSet <- Default_WithR
  
  
  train_test_split <- function(df){
    sample = sample.split(df, SplitRatio)
    train = subset(df, sample == TRUE)
    test  = subset(df, sample == FALSE)
    return (list(train, test))
  }
  
  
  #  
  df_train <- train_test_split(FinalDataSet)[[1]]
  df_test <- train_test_split(FinalDataSet)[[2]]
  dim(df_train)
  dim(df_test)
  
  dim(FinalDataSet)
 
  # Training the Random Forest regression model
  rf_model1 <- randomForest(LW ~ ., data = df_train, ntree=600, mtry = 6, nodesize = 5, importance = TRUE) 
  # -----------------------------
  # 2. Extract permutation importance (%IncMSE)
  # -----------------------------
  # -----------------------------
  # 1. Variable Importance (%IncMSE)
  # -----------------------------
  
  vip <- randomForest::importance(rf_model1, type = 1)
  
  vip_df <- data.frame(
    Variable = rownames(vip),
    IncMSE = vip[, 1]
  )
  
  vip_df <- vip_df[order(vip_df$IncMSE, decreasing = TRUE), ]
  
  
  # =============================
  # 5. Select Top Variables
  # =============================
  top_vars <- head(vip_df$Variable, 12)
  
  # =============================
  # 6. PDP Computation (SAFE)
  # =============================
  pd1 <- NULL
  
  for (i in top_vars) {
    
    tmp <- partial(rf_model1, pred.var = i, train = df_train)
    
    xcol <- names(tmp)[1]
    ycol <- "yhat"
    
    tmp <- tmp[order(tmp[[xcol]]), ]
    
    pd1 <- rbind(
      pd1,
      data.frame(
        x = tmp[[xcol]],
        y = tmp[[ycol]],
        predictor = i
      )
    )
  }
  
  pd1_plot <- ggplot(pd1, aes(x = x, y = y)) +
    geom_line(linetype = "dashed", linewidth = 0.7, color = "black") +
    facet_wrap(~ predictor, scales = "free", ncol = 4) +
    geom_smooth(color = "blue") +
    theme_bw() +
    labs(
      #title = "Partial Dependence Plots (2023-01-01 to 2023-03-31)",
      x = "Predictor Variables",
      y = "Leaf Wetness"
    )
  pd1_plot
  ggsave("D:/PhD Revision/Images/pd1_plot.png", plot = pd1_plot, width = 8, height = 6, dpi = 300)
  ###########################################################
  
  str(rf_model1$oob.times)
  
  
  ################################################################################
  # Step 5: Model Evaluation during validation phase
  # Making predictions on the test set
  predictions <- predict(rf_model1, newdata = df_test)
  
  #Initialise
  y_actual = df_test$LW
  y_predicted = predictions
  
  ########################
  # Training Phase Scatter Plot
  train_predictions <- predict(rf_model1, newdata = df_train)
  y_train_actual <- df_train$LW
  y_train_predicted <- train_predictions
  
  # Testing Phase Scatter Plot
  test_predictions <- predict(rf_model1, newdata = df_test)
  y_test_actual <- df_test$LW
  y_test_predicted <- test_predictions
  ########################################################
  ###############################################################
  
  compute_metrics <- function(actual, predicted) {
    list(
      RMSE = rmse(actual, predicted),
      MAE = mae(actual, predicted),
      R2 = r_squared(actual, predicted),
      MSE = mse(actual, predicted),
      MAPE = mape(actual, predicted),
      SMAPE = smape(actual, predicted),
      KLD = kl_divergence(actual, predicted),
      SS = skill_score(actual, predicted),
      Q5 = quantile_loss(actual, predicted, 0.5),
      CS = cosine_similarity(actual, predicted),
      H = huber_loss(actual, predicted, 1.0),
      PH = pseudo_huber_loss(actual, predicted, 1.0),
      LC = log_cosh_loss(actual, predicted)
    )
  }
  
  safe_round <- function(x, d = 3) {
    if (is.null(x) || !is.numeric(x) || is.na(x)) return(NA)
    round(x, d)
  }
  
  train_metrics <- compute_metrics(y_train_actual, y_train_predicted)
  test_metrics  <- compute_metrics(y_test_actual, y_test_predicted)
  
  train_df <- as.data.frame(train_metrics)
  test_df  <- as.data.frame(test_metrics)
  
  print("===== TRAIN METRICS =====")
  print(train_df)
  
  print("===== TEST METRICS =====")
  print(test_df)
  
#############################################################
  create_scatter_plot <- function(y_actual, y_predicted, title, metrics) {
    
    df <- data.frame(
      actual = y_actual,
      predicted = y_predicted
    )
    
    safe_round <- function(x, d = 3) {
      if (is.null(x) || !is.numeric(x) || is.na(x)) return(NA)
      round(x, d)
    }
    
    label_text <- paste(
      
      paste0("RMSE: ", safe_round(metrics$RMSE),
             "   |   MAE: ", safe_round(metrics$MAE)),
      
      paste0("R²: ", safe_round(metrics$R2),
             "   |   MSE: ", safe_round(metrics$MSE)),
      
      paste0("MAPE: ", safe_round(metrics$MAPE),
             "   |   SMAPE: ", safe_round(metrics$SMAPE)),
      
      paste0("KLD: ", safe_round(metrics$KLD),
             "   |   SS: ", safe_round(metrics$SS)),
      
      paste0("Q5: ", safe_round(metrics$Q5),
             "   |   CS: ", safe_round(metrics$CS)),
      
      paste0("H: ", safe_round(metrics$H),
             "   |   PH: ", safe_round(metrics$PH)),
      
      paste0("LC: ", safe_round(metrics$LC)),
      
      sep = "\n"
    )
    
    ggplot(df, aes(x = actual, y = predicted)) +
      geom_point(shape = 8, size = 1.5) +
      geom_smooth(method = "lm", se = FALSE, color = "black") +
      annotate(
        "text",
        x = min(df$actual, na.rm = TRUE),
        y = max(df$predicted, na.rm = TRUE),
        label = label_text,
        hjust = 0,
        vjust = 1,
        size = 4
      ) +
      labs(
        x = "Actual Leaf Wetness",
        y = "Predicted Leaf Wetness",
        title = title
      ) +
      theme_minimal()
  }
#########################################################
  train_scatter_plot <- create_scatter_plot(
    y_train_actual, y_train_predicted,
    "Training Phase: ZS Without Priors",
    train_metrics
  )
  
  test_scatter_plot <- create_scatter_plot(
    y_test_actual, y_test_predicted,
    "Testing Phase: ZS Without Priors",
    test_metrics
  )
  
  grid.arrange(train_scatter_plot, test_scatter_plot, ncol = 2)
  
  
  combined_plot <- grid.arrange(
    train_scatter_plot,
    test_scatter_plot,
    ncol = 2
  )
  
  ggsave("D:/PhD Revision/Images/ZS_Without_Priors.png", plot = combined_plot,
         width = 12, height = 5, dpi = 300)
  
  train_df <- as.data.frame(train_metrics)
  test_df  <- as.data.frame(test_metrics)
  
  print("===== TRAIN METRICS =====")
  print(t(train_df))
  
  print("===== TEST METRICS =====")
  print(t(test_df))
  
   
  vip_df
  