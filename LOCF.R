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
#MyData_filled <- replace_missing_locf(RData)