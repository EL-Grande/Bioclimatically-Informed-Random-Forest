

Exp1.pdp <- function(){
  source("D:/PhD Material/Pulpaso24/ML_Func.R")# Call the function to get the datasets
  miMmodel <- MLFUNC()
  
  
  
  top4 <- topPredictors(miMmodel, n = 4) # Topfour predictors
  
  # Construct partial dependence functions for top four predictors
  pd <- NULL
  for (i in top4) {
    tmp <- partial(miMmodel, pred.var = i)
    names(tmp) <- c("x", "y")
    pd <- rbind(pd,  cbind(tmp, predictor = i))
  }
  
  # Display partial dependence functions
  pdp1 <- ggplot(pd, aes(x, y)) +
    geom_line() +
    facet_wrap(~ predictor, scales = "free") +
    theme_bw() +
    ylab("mpg")
  
  pdp1
}
  
  ## End(Not run)
  