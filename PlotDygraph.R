PlotDygraph <- function(data){
  ps <- dygraph(data, "Time") %>%
    dySeries("Actual", label = "Actual") %>%
    dySeries("Predicted", label = "Predicted") %>%
    dyOptions(strokePattern = "solid", drawPoints = TRUE) #%>%
  
}