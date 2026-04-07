LossInvo <- function(y_actual, y_predicted){
  Loss_list <- list()
  
  # Calculate loss metrics
  Loss_list$RMSE <- round(rmse(y_actual, y_predicted), 1)
  Loss_list$MAE <- round(mae(y_actual, y_predicted), 1)
  Loss_list$MAPE <- round(mape(y_actual, y_predicted), 2)
  Loss_list$R_squared <- round(r_squared(y_actual, y_predicted), 1)
  Loss_list$MSLE <- round(msle(y_actual, y_predicted), 4)
  Loss_list$KLD <- round(kl_divergence(y_actual, y_predicted), 4)
  Loss_list$SS <- round(skill_score(y_actual, y_predicted), 4)
  Loss_list$Q <- round(quantile_loss(y_actual, y_predicted, 0.5), 4)  # Example for the median quantile (0.5)
  Loss_list$CS <- round(cosine_similarity(y_actual, y_predicted), 4)
  Loss_list$H <- round(huber_loss(y_actual, y_predicted, 1.0), 4)  # Example with delta = 1.0
  Loss_list$PH <- round(pseudo_huber_loss(y_actual, y_predicted, 1.0), 4)  # Example with delta = 1.0
  Loss_list$LHC <- round(log_cosh_loss(y_actual, y_predicted), 4)
  Loss_list$SMAPE <- round(smape(y_actual, y_predicted), 4)
  
  return(Loss_list)
 }