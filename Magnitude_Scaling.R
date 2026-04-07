########################################################################################
# Function to perform Magnitude scaling and scale to a custom range
Magnitude_Scaling <- function(df, min_magnitude = -3, max_magnitude = 3) {
  scaled_df <- df
  for (col_name in names(df)[-1]) {  # Exclude the first column (ID) from scaling
    magnitude <- sqrt(sum(df[[col_name]]^2))
    
    # Check if the magnitude is zero
    if (magnitude == 0) {
      # Handle the case where the magnitude is zero (e.g., assign default value)
      scaled_values <- rep(0, length(df[[col_name]]))
    } else {
      # Magnitude scaling
      scaled_values <- df[[col_name]] / magnitude * (max_magnitude - min_magnitude) + min_magnitude
    }
    
    # Replace the original column with scaled values
    scaled_df[[col_name]] <- scaled_values
  }
  return(scaled_df)
}
########################################################################################