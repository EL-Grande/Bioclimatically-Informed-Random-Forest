MissingVI<- function(RData){
 
  # Function to replace missing values with mean for all columns
  replace_missing_with_mean <- function(df) {
    for (col_name in names(df)) {
      col_mean <- mean(df[[col_name]], na.rm = TRUE)
      df[[col_name]][is.na(df[[col_name]])] <- col_mean
    }
    return(df)
  }
  
  # Function to replace missing values using LOCF
  replace_missing_locf <- function(RData) {
    for (i in 2:ncol(RData)) {
      for (j in 1:nrow(RData)) {
        if (is.na(RData[j, i])) {
          RData[j, i] <- RData[j - 1, i]
        }
      }
    }
    return(RData)
  }
  
  
  
  # Replace missing values using LOCF
  MyData_filled <- replace_missing_locf(RData)
  
   
  
  # Replace missing values in all columns with mean
  #MyData_filled <- replace_missing_with_mean(RData)
  
  return(MyData_filled)
  
}
