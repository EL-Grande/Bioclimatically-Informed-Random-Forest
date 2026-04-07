DataSetGen <- function(start_date, end_date) {
  # Generate sequence of dates within the specified range (rounded to the nearest hour)
  date_seq <- seq(
    from = as.POSIXct(paste0("2023-", start_date, " 00:00:00")),
    to = as.POSIXct(paste0("2023-", end_date, " 23:00:00")),
    by = "hour"
  )
  
  # Initialize an empty data frame
  MyData <- data.frame(DateTime = date_seq)
  
  # Given variables
  Hd <- "D:/PhD Material/DataBw/DailyCSV/HOURLY-SUMMARY-Tubu (Okavango Delta)-"
  Tl <- ".csv"
  
  for (current_date in seq_along(date_seq)) {
    # Format the current date to match the file naming convention
    formatted_date <- format(date_seq[current_date], "%m-%d")
    
    # Concatenate strings to create DataExtS without spaces
    DataExtS <- paste0(Hd, formatted_date, Tl)
    
    # Read data from the CSV file
    Buf <- readr::read_csv(DataExtS)
    
    # Omit the second column (column B)
    Buf <- subset(Buf, select = c(-2, -3))
    
    # New variable names
    new_names <- c(
      "AirTemp", "SurfaceTemp", "Rain", "WindSpeed", "WindDirect", "WindSpeed.Max",
      "WindSpeed.Min", "Humidity", "Barom.Press", "Solar.Rad", "Solar.Rad.Energy",
      "Leaf.Wetness", "Dew.Point", "Wet.Bulb", "Sun.Hours", "BatteryVoltage"
    )
    
    # Assign the current date to the DateTime column
    Buf$DateTime <- date_seq[current_date]
    
    # Reorder the columns
    Buf <- Buf[, c("DateTime", new_names)]
    
    # Append the data for the current day to the overall data frame
    MyData <- dplyr::bind_rows(MyData, Buf)
  }
  
  return(MyData)
}

# Example usage: retrieve data for a range of dates (e.g., January 1 to January 3)
result <- DataSetGen(start_date = "01-01", end_date = "01-03")