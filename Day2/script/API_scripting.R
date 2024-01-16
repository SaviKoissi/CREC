# API scripting
# Replace 'YOUR_API_KEY' with your actual API key
api_key <- 'YOUR_API_KEY'

# Function to get weather data for a specific date and location
get_weather_data <- function(lat, lon, date) {
  unix_timestamp <- as.numeric(as.POSIXct(date))
  url <- sprintf("https://api.openweathermap.org/data/2.5/onecall/timemachine?lat=%f&lon=%f&dt=%s&appid=%s",
                 lat, lon, unix_timestamp, api_key)
  response <- GET(url)
  content(response, "parsed")
}

# Example usage
lat <- 55.7522
long <- 37.6156
date <- "2024-01-20"
weather_data <- get_weather_data(lat, long, date)
weather_data

