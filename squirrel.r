# Load the stringr library
library(stringr)

# Read the csv file into a dataframe
hectare <- read.csv("2018_Central_Park_Squirrel_Census_-_Hectare_Data.csv")

# Convert all values in the "Sighter.Observed.Weather.Data" column to lowercase
weather <- tolower(hectare$Sighter.Observed.Weather.Data)

# Split the lowercase weather data by ","

unique_weather <- unique(unlist(weather_split))

unique_weather

weather_split <- str_split(weather, ",")

# Check the results of the split
weather_split

# Check if the string "foggy" is present in each split weather string
temp <- sapply(weather_split, function(x) any(str_detect(x, "foggy")))

# Convert the results to a logical vector and add it as a new column to the dataframe
hectare$temperature <- as.logical(temp)

# Write the modified dataframe to a new csv file
write.csv(hectare, "hectare_temperature.csv")



