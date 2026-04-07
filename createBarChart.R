# Function to create bar chart
createBarChart <- function(residuals_list, names_vector, title, ylab) {
  # Calculate mean and standard deviation for each group
  means <- sapply(residuals_list, mean)
  sds <- sapply(residuals_list, sd)
  
  # Create bar chart
  barplot(means, names.arg = names_vector, 
          main = title, ylab = ylab,
          ylim = c(min(means - sds), max(means + sds)),
          col = rainbow(length(names_vector)),
          beside = TRUE)
}