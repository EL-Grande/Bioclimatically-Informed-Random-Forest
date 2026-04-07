Assuming you have a named vector of variable importance values
variable_importance <- c('Feature1' = 0.25, 'Feature2' = 0.18, 'Feature3' = 0.15, 'Feature4' = 0.12, 'Feature5' = 0.10)

# Convert the named vector to a data frame
df <- data.frame(Feature = names(variable_importance), Importance = as.numeric(variable_importance))

# Sort the data frame by importance values
df <- df[order(-df$Importance), ]

# Print the compact table using knitr::kable
cat(knitr::kable(df, format = "markdown", col.names = c("Feature", "Importance"), align = c("l", "r"), digits = 2), "\n")

# Plot the variable importance using horizontal bars
barplot(df$Importance, names.arg = df$Feature, horiz = TRUE, col = "skyblue", main = "Variable Importance", xlab = "Importance")