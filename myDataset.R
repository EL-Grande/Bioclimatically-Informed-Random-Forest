myDataset <- function(Lista){
  # Lista <- DataOps("2023-01-01", "2023-03-05") # Access the individual datasets from the list
  
  # Call the function to generate the list of figures
  Datasetlist <- NULL
  D1A <- Lista$dataset1 # Raw data with  All priors
  
   
  D2A <- Lista$dataset2 # Maginitude Scaling with  All priors
   
  D3A <- Lista$dataset3 # Min-Max Normalisation with  All priors
   
  
  D4A <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with  All priors
   
 
  
  FinalDataSet <- D3A
  
  return(FinalDataSet)
}