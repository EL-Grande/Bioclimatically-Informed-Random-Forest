DataSetGen <- function(Month_Days) {
  # Number of days and hours
  num_days <- length(Month_Days)
  hours_per_day <- 24
  Year <- "2023-"
  TimeS <- "00:00:00"
  
  # Initialize an empty data frame
  MyData <- data.frame()
  
  for (Month_Day in Month_Days) {
    Fecha <- paste0(Year, Month_Day)  # Concatenate strings
    DateTimeS <- paste(Fecha, TimeS)  # Concatenate strings
    
    # Given variables
    Hd <- "D:/PhD Material/DataBw/DailyCSV/HOURLY-SUMMARY-Tubu (Okavango Delta)-"
    Tl <- ".csv"
    
    # Concatenate strings to create DataExtS without spaces
    DataExtS <- paste0(Hd, Fecha, Tl)
    
    # Generate sequence of dates and times for each day (rounded to the nearest hour)
    date_seq <- seq(
      from = as.POSIXct(DateTimeS),
      by = "hour",
      length.out = hours_per_day
    )
    
    Buf <- readr::read_csv(DataExtS)  # Extract data from the saved CSV files
    
    # Create a data frame for the current day
    day_data <- data.frame(
      DateTime = date_seq,
      Buf
    )
    
    # Omit the second column (column B)
    day_data <- subset(day_data, select = c(-2, -3))
    
    # New variable names
    new_names <- c(
      "DateTime", "AirTemp", "SurfaceTemp", "Rain", "WindSpeed", "WindDirect", "WindSpeed.Max",
      "WindSpeed.Min", "Humidity", "Barom.Press", "Solar.Rad", "Solar.Rad.Energy", "Leaf.Wetness",
      "Dew.Point", "Wet.Bulb", "Sun.Hours", "BatteryVoltage"
    )
    names(day_data) <- new_names
    
    # Append the data for the current day to the overall data frame
    MyData <- dplyr::bind_rows(MyData, day_data)
  }
  
  return(MyData)
}

# Example usage: retrieve data for January 1st and 2nd
result <- DataSetGen(c("01-01", "01-02"))