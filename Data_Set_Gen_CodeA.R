DataSetGen<- function(Month_Day){
  # Number of days and hours
  num_days <- 1  # Only consider January 1 2023
  hours_per_day <- 24
  #Month_Day<-"01-03"
  Year = "2023-"
  TimeS <- "00:00:00"
  
  Fecha <- paste0(Year, Month_Day) # Concatenate strings
  DateTimeS <- paste(Fecha, TimeS) # Concatenate strings
  
  # Given variables
  Hd <- "D:/PhD Material/DataBw/DailyCSV/HOURLY-SUMMARY-Tubu (Okavango Delta)-"
  Tl <- ".csv"
  
  # Concatenate strings to create DataExtS without spaces
  DataExtS <- paste0(Hd, Fecha, Tl)
  
  # Generate sequence of dates and times for January 1 2023 (rounded to the nearest hour)
  date_seq <- seq(
    from = as.POSIXct(DateTimeS),
    by = "hour",
    length.out = num_days * hours_per_day
  )
  
  Buf <- read_csv(DataExtS) # Extract data from the saved CSV files
  
  # Create a data frame
  datos <- data.frame(
    DateTime = date_seq,
    Buf
  )
  
  # Omit the second column (column B)
  MyData <- subset(datos, select = c(-2,-3))
  # New variable names
  new_names <- c("DateTime" ,"AirTemp","SurfaceTemp","Rain","WindSpeed","WindDirect","WindSpeed.Max",
                 "WindSpeed.Min","Humidity","Barom.Press","Solar.Rad","Solar.Rad.Energy","Leaf.Wetness",
                 "Dew.Point","Wet.Bulb","Sun.Hours","BatteryVoltage")
  names(MyData) <- new_names
  
  return(MyData)
  
}

DataSetGen("01-01")



