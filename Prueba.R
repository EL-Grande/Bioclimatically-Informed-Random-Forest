# Set seed for reproducibility
set.seed(123)

# Number of observations in the dataset
n <- 100

# Generate artificial data for air temperature (Ta), wind speed (V), and dew point temperature (Tdew)
air_temperature <- rnorm(n, mean = 20, sd = 5)
wind_speed <- runif(n, min = 1, max = 10)
dew_point_temperature <- rnorm(n, mean = 15, sd = 3)

# Calculate Wind Chill Temperature (Twc) using the provided formula
wind_chill_temperature <- 13.12 + 0.6215 * air_temperature - 11.37 * wind_speed^0.16 + 0.3965 * air_temperature * wind_speed^0.16

# Calculate Potential Evapotranspiration (Ep) using the Hargreaves equation
potential_evapotranspiration <- 0.0023 * sqrt(air_temperature + 17.8) * (air_temperature - dew_point_temperature)^0.5 * (air_temperature - 30)

# Coefficients for the Heat Index equation
c <- c(-42.379, 2.04901523, -6.83783e-03, 10.14333127, -0.22475541, 1.22874e-03, -0.05481717, 8.5282e-04, -1.99e-06)

# Calculate Heat Index (HI) using the provided equation
heat_index <- c[1] + c[2] * air_temperature + c[3] * air_temperature^2 +
  runif(n, min = 0, max = 100) * (c[4] + c[5] * air_temperature + c[6] * air_temperature^2) +
  runif(n, min = 0, max = 100)^2 * (c[7] + c[8] * air_temperature + c[9] * air_temperature^2)

# Calculate Apparent Temperature (AT) using the provided equation
apparent_temperature <- air_temperature + 0.33 * runif(n, min = 0, max = 10) + 0.7 * runif(n, min = 1, max = 5) - 4.0

# Calculate Cooling Power (CP) using the provided equation
cooling_power <- 20.52 * runif(n, min = 1, max = 5)^0.42 * (36.5 - air_temperature)

# Calculate Humidex (HD) using the provided equation
humidex <- air_temperature + 0.5555 * (6.11 * exp(5417.753 * (1 / 273.16) * (1 / (273.16 + dew_point_temperature)) ) - 10)

# Calculate Effective Temperature (TE) using the provided equation
effective_temperature <- 37 - (37 - air_temperature) / (0.68 - 0.0014 * runif(n, min = 0, max = 100) + (1.76 + 1.4 * runif(n, min = 1, max = 5)^0.75)^-1) - 0.29 * air_temperature * (1 - 0.01 * runif(n, min = 0, max = 100))

# Generate artificial data for Actual Sensation Vote (ASV)
# (Replace this with the correct formula if available)
actual_sensation_vote <- runif(n, min = 0, max = 100)

# Create the raw dataset
raw_dataset <- data.frame(
  Air_Temperature = air_temperature,
  Wind_Speed = wind_speed,
  Dew_Point_Temperature = dew_point_temperature,
  Wind_Chill_Temperature = wind_chill_temperature,
  Potential_Evapotranspiration = potential_evapotranspiration,
  Heat_Index = heat_index,
  Apparent_Temperature = apparent_temperature,
  Cooling_Power = cooling_power,
  Humidex = humidex,
  Effective_Temperature = effective_temperature,
  Actual_Sensation_Vote = actual_sensation_vote
)

# Print the first few rows of the raw dataset
print("Raw Dataset:")
head(raw_dataset)

# Min-Max Normalization
min_max_normalized <- as.data.frame(scale(raw_dataset, center = FALSE, scale = apply(raw_dataset, 2, function(x) (x - min(x))/(max(x) - min(x)))))

# Magnitude Scaling
magnitude_scaled <- as.data.frame(scale(raw_dataset, center = FALSE, scale = apply(raw_dataset, 2, function(x) x/sqrt(sum(x^2))))))

# Z-Score Normalization
z_score_normalized <- as.data.frame(scale(raw_dataset))

# Print the first few rows of the normalized datasets
print("Min-Max Normalized Dataset:")
head(min_max_normalized)

print("Magnitude Scaled Dataset:")
head(magnitude_scaled)

print("Z-Score Normalized Dataset:")
head(z_score_normalized)