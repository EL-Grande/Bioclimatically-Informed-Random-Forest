# Function to create scatter plot
create_scatter_plot <- function(y_actual, y_predicted, RMSE,  MAE, R_squared,MSLE,MAPE,KLD,SS,Q,CS,H,PH,LHC,SMAPE, title) {
  # Calculate the data range
  x_range <- range(y_actual)
  y_range <- range(y_predicted)
  
  # Calculate coordinates for the annotation
  annotation_x <- max(x_range) - 0.7 * diff(x_range)
  annotation_y <- max(y_range) + 0.1 * diff(y_range)
  
  ggplot(data = NULL, aes(x = y_actual, y = y_predicted)) +
    geom_point(shape = 8, color = "black", size = 1.5) +  # Use shape 8 for stars
    geom_smooth(method = "lm", color = "red") +
    annotate(
      "text",
      x = 0.125*annotation_x,  # Adjusted x-coordinate
      y = annotation_y-0.2,  # Adjusted y-coordinate
      label = sprintf("R^2 == %.2f", R_squared),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    
    annotate(
      "text",
      x = 0.6*annotation_x,  #+ 0.1 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.01 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("MAE == %.2f", MAE),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    annotate(
      "text",
      x = 1.14*annotation_x, # + 0.4 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.011 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("CS == %.2f", CS),  # Display R^2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    annotate(
      "text",
      x = 1.7*annotation_x, # + 0.6 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.014 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("SS == %.2f", SS),  # Display MAPE2 as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    
    annotate(
      "text",
      x = 2.30*annotation_x, # + 0.4 * diff(x_range),  # Adjusted x-coordinate
      y = annotation_y - 0.011 * diff(y_range),  # Adjusted y-coordinate
      label = sprintf("KLD == %.2f", KLD),  # Display KL_divergence as an equation
      parse = TRUE,
      color = "black",
      size = 3.5
    ) +
    
    labs(
      x = "Actual Leaf Wetness",
      y = "Predicted Leaf Wetness",
      title = title
    ) +
    
    theme_minimal() # +  # Remove gridlines and use a minimal theme
  #  theme(plot.title = element_text(size = rel(0.8)))  # Set title font size
  }



################################################################



########################################################

