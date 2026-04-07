
# Function to create box plot
createBoxPlot <- function(residuals_list, names_vector, title, ylab) {
  boxplot(residuals_list, names = names_vector,
          main = title, ylab = ylab, pch = "*", cex = 2)
}
