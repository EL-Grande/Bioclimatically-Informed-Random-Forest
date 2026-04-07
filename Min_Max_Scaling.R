########################################################################################
# Function to perform Min-Max scaling and scale to a custom range
Min_Max_Scaling <- function(df, min_range = -3, max_range = 3) {
  scaled_df <- df
  for (col_name in names(df)[-1]) {  # Exclude the first column (ID) from scaling
    min_val <- min(df[[col_name]])
    max_val <- max(df[[col_name]])
    
    # Check if the range is zero
    if (min_val == max_val) {
      # Handle the case where the range is zero (e.g., assign default value)
      scaled_values <- rep(0, length(df[[col_name]]))
    } else {
      # Min-Max scaling
      scaled_values <- (df[[col_name]] - min_val) / (max_val - min_val) * (max_range - min_range) + min_range
    }
    
    # Replace the original column with scaled values
    scaled_df[[col_name]] <- scaled_values
  }
  return(scaled_df)
}
########################################################################################