DataSetGen <- function(start_date, end_date) {
  # Generate a sequence of dates within the specified range
  date_range <- seq(
    from = as.Date(start_date),
    to = as.Date(end_date),
    by = "days"
  )
  
  # Convert the date range to the desired day strings
  month_days <- format(date_range, "%m-%d")
  
  # Number of days and hours
  hours_per_day <- 24
  Year <- "2023-"
  TimeS <- "00:00:00"
  
  # Initialize an empty data frame
  MyData <- data.frame()
  
  for (Month_Day in month_days) {
    Fecha <- paste0(Year, Month_Day)  # Concatenate strings
    DateTimeS <- paste(Fecha, TimeS)  # Concatenate strings
    
    # Given variables
    Hd <- "D:/PhD Material/DataBw/DailyCSV/HOURLY-SUMMARY-Tubu (Okavango Delta)-"
    Tl <- ".csv"
    
    # Concatenate strings to create DataExtS without spaces
    DataExtS <- paste0(Hd, Fecha, Tl)
    
    # Generate sequence of dates and times for each day (rounded to the nearest hour)
    date_seq <- seq(
      from = as.POSIXct(DateTimeS, tz = "UTC", origin = "1970-01-01"),
      by = "hour",
      length.out = hours_per_day
    )
    
    Buf <- readr::read_csv(DataExtS)  # Extract data from the saved CSV files
    
    # Check if Buf is numeric
    if (is.numeric(Buf)) {
      Buf <- data.frame(Buf)
    }
    
    # Create a data frame for the current day
    day_data <- data.frame(
      DateTime = date_seq,
      Buf
    )
    
    # Omit the second column (column B)
    day_data <- subset(day_data, select = c(-2, -3))
    
    # New variable names
    new_names <- c(
      "TS", "AT", "ST", "Rain", "WS", "WD", "WSX",
      "WSm", "RH", "BP", "SR", "SRE", "LW",
      "DP", "WB", "SH", "BV"
    )
    names(day_data) <- new_names
    
    # Append the data for the current day to the overall data frame
    MyData <- dplyr::bind_rows(MyData, day_data)
  }
  
  return(MyData)
}

# Example usage: retrieve data for January 1st to January 10th
#result <- DataSetGen("2023-01-01", "2023-01-10")