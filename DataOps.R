DataOps <- function(D,E) {
  
#source("D:/PhD Material/DataBw/Data_Set_Gen_CodeA.R")
source("D:/PhD Material/DataBw/PaEso.R")
MiData <- DataSetGen(D,E) # Extract the data
 

# Replace missing values using LOCF
source("D:/PhD Material/DataBw/LOCF.R")
Data_Filled <- replace_missing_locf(MiData)

#source("D:/PhD Material/DataBw/CheckMV.R")

# Omit the second column (column B)
#modified_data <- subset(datos, select = c(-2,-3))

#check_missing_values(MiData)  # Check missing values
  
#source("D:/PhD Material/DataBw/MVI.R")
#Data_Filled <- MissingVI(MiData)

#check_missing_values(Data_Filled)  # Check missing values


source("D:/PhD Material/DataBw/Derive_New_Variables.R")
FinalData <- Derive_Add_Variables(Data_Filled)

source("D:/PhD Material/DataBw/Magnitude_Scaling.R")
Magnitude_s <- Magnitude_Scaling(FinalData, min_magnitude = -1, max_magnitude = 1)

source("D:/PhD Material/DataBw/Min_Max_Scaling.R")
min_max_s <- Min_Max_Scaling(FinalData, min_range = 0, max_range = 1)

source("D:/PhD Material/DataBw/Z_Score_Scaling.R")
Z_score_s <- zscore_normalize(FinalData)

#numerical_values <- Z_Score_Scaling(FinalData, range_center = 0, range_scale = 3)

# Extract only the numerical values without variable names
#z_score_s <- as.matrix(numerical_values[, sapply(numerical_values, is.numeric)])
#names(z_score_s) <- names(min_max_s)

# Create a list to store the datasets
datasets_list <- list(dataset1 = FinalData, dataset2 = Magnitude_s, 
                      dataset3 = min_max_s, dataset4 = Z_score_s)

# Return the list of datasets
return(datasets_list)
 
}

