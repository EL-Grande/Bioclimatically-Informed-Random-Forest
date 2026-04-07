 
source("D:/PhD Material/DataBw/Data_Set_Gen_CodeA.R")#Retrieve   CSV file generate a raw data
source("D:/PhD Material/DataBw/MVI.R") #For Imputation of missing values


MyDataA<-DataSetGen("01-03") #Generate the dataset of raw values
Data_filled<-MissingVI(MyDataA) # Impute the missing values

View(Data_filled)

###############################################################################
# Function to add new variables to the original dataset
add_new_variables <- function(Jan_1_filled) {
  Jan_1_filled$Twc <- 13.12 + 0.6215 * Jan_1_filled$AirTemp - 11.37 * Jan_1_filled$WindSpeed^0.16 + 0.3965 * Jan_1_filled$AirTemp * Jan_1_filled$WindSpeed^0.16
  Jan_1_filled$Tap <- Jan_1_filled$AirTemp + 0.348 * Jan_1_filled$Barom.Press + 0.7 * Jan_1_filled$WindSpeed + 0.7 * Jan_1_filled$Solar.Rad * (Jan_1_filled$WindSpeed - 10)^(-1) - 4.25
  Jan_1_filled$HI <- -42.379 + 2.04901523 * Jan_1_filled$AirTemp - 6.83783e-3 * Jan_1_filled$AirTemp^2 + Jan_1_filled$Humidity * (10.14333127 + 0.22475541 * Jan_1_filled$AirTemp + 1.22874e-3 * Jan_1_filled$AirTemp^2) + Jan_1_filled$Humidity^2 * (8.5282e-4 + 1.99e-6 * Jan_1_filled$AirTemp^2)
  Jan_1_filled$ER <- 0.1396 + -3.019e-3 * Jan_1_filled$Humidity + -1.2109e-3 * Jan_1_filled$AirTemp + 1.626e-5 * Jan_1_filled$Humidity^2 + 8.224e-5 * Jan_1_filled$AirTemp^2 + 0.1842 * Jan_1_filled$Solar.Rad + 0.5 * Jan_1_filled$Solar.Rad * (Jan_1_filled$Humidity * -1.095e-3 + Jan_1_filled$AirTemp * 3.655e-3) + -4.442e-3 * Jan_1_filled$Solar.Rad^2
  Jan_1_filled$CP <- 20.52 * Jan_1_filled$WindSpeed^0.42 * (36.5 - Jan_1_filled$AirTemp)
  Jan_1_filled$HD <- Jan_1_filled$AirTemp + 0.5555 * (6.11 * exp(5417.753 * (273.16)^(-1) * (273.16 + Jan_1_filled$Dew.Point)^(-1)) - 10)
  Jan_1_filled$TE <- 37 - ((0.68 - 0.0014 * Jan_1_filled$Humidity + (1.76 + 1.4 * Jan_1_filled$WindSpeed^0.75)^(-1))^(-1) * (37 - Jan_1_filled$AirTemp) - 0.29 * Jan_1_filled$AirTemp * (1 - 0.01 * Jan_1_filled$Humidity))
  return(Jan_1_filled)
}

# Apply the function to add new variables to the original dataset
original_data_with_new_vars <- add_new_variables(Jan_1_filled)
dim(original_data_with_new_vars)

str(original_data_with_new_vars)
