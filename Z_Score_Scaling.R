########################################################################################
# Optimized Z-score normalization function
zscore_normalize <- function(df) {
  # Identify numeric columns for normalization
  numeric_columns <- sapply(df, is.numeric)
  
  # Exclude non-numeric columns (like Date) from normalization
  numeric_data <- df[, numeric_columns, drop = FALSE]
  non_numeric_columns <- setdiff(names(df), names(numeric_data))
  
  # Calculate mean and standard deviation for each numeric column
  means <- colMeans(numeric_data, na.rm = TRUE)
  sds <- apply(numeric_data, 2, sd, na.rm = TRUE)
  
  # Z-score normalize numeric columns
  normalized_data <- scale(numeric_data, center = means, scale = sds)
  
  # Clip Z-scores to the range of [-3, 3]
  normalized_data <- pmax(pmin(normalized_data, 3), -3)
  
  # Combine normalized numeric columns with non-numeric columns
  normalized_df <- cbind(df[, non_numeric_columns, drop = FALSE], normalized_data)
  
  return(normalized_df)
}
########################################################################################