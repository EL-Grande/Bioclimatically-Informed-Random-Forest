myDataset <- function(Lista){
  # Lista <- DataOps("2023-01-01", "2023-03-05") # Access the individual datasets from the list
  
  # Call the function to generate the list of figures
  
  D1A <- Lista$dataset1 # Raw data with  All priors
  D1B <- Lista$dataset1 # Raw data without priors
  D1C <- Lista$dataset1 # Raw data with  VI priors
  
  
  D2A <- Lista$dataset2 # Maginitude Scaling with  All priors
  D2B <- Lista$dataset2 # Maginitude Scaling without priors
  D2B <- Lista$dataset2 # Maginitude Scaling with VI priors
  
  D3A <- Lista$dataset3 # Min-Max Normalisation with  All priors
  D3B <- Lista$dataset3 # Min-Max Normalisation without priors
  D3C <- Lista$dataset3 # Min-Max Normalisation with VI priors
  
  
  D4A <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with  All priors
  D4B <- as.data.frame(Lista$dataset4) # Z-Score Normalisation without priors
  D4C <- as.data.frame(Lista$dataset4) # Z-Score Normalisation with VI priors
  
  
  FinalDataSet <- D3A
  
  return(FinalDataSet)
}