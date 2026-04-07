check_missing_values <- function(data) {
  missing_values <- sum(is.na(data))
  
  if (missing_values > 0) {
    cat("There are", missing_values, "missing values in the dataset.\n")
    return(TRUE)
  } else {
    cat("No missing values found in the dataset.\n")
    return(FALSE)
  }
}