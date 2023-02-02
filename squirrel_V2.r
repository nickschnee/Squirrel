# Load the stringr library
library(stringr)

# Read the csv files into dataframes
hectare <- read.csv("2018_Central_Park_Squirrel_Census_-_Hectare_Data.csv")
squirrels <- read.csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

# Convert to lowercase
weather <- tolower(hectare$Sighter.Observed.Weather.Data)


# temperature
# temperature
# temperature
# temperature
# temperature
# temperature

fahrenheit_to_celsius <- function(x) {
  if(!is.na(x)){
    (x - 32) * 5/9
  } else {
    NA
  }
}


# get all temperatures using this function and regex
temp_fahrenheit <- as.numeric(str_extract(weather, "\\d+"))
temp_fahrenheit

# convert to celsius, if is not NA and then round to one decimal
temp_celsius <- round(sapply(temp_fahrenheit, fahrenheit_to_celsius), 1)
temp_celsius

hectare$temperature_fahrenheit <- temp_fahrenheit
hectare$temperature_celsius <- temp_celsius
write.csv(hectare, "hectare_temperature.csv")

# temperature END
# temperature END
# temperature END
# temperature END
# temperature END
# temperature END


# weather conditions extraction
# weather conditions extraction
# weather conditions extraction

# make a list of all different weather conditions
weather_split <- str_split(weather, ",")
str_split(weather, ",", simplify = TRUE)


# find all unique weather conditions
unique_weather <- str_trim(unique(unlist(weather_split)))
unique_weather

# make a table with all unique values, without values with numbers (temperatures)
weather_no_numbers <- str_trim(unlist(weather_split), side = "both")[!str_detect(str_trim(unlist(weather_split), side = "both"), "\\d")]
sort(table(weather_no_numbers), ascending = TRUE)

# get unique values where sun appears
# table(unique_weather[grepl(pattern = "sun", x = unique_weather)])

# END weather conditions extraction
# END weather conditions extraction
# END weather conditions extraction


# START making new columns for groups of weather conditions
# START making new columns for groups of weather conditions
# START making new columns for groups of weather conditions
# START making new columns for groups of weather conditions


# sun:      sun(ny), clear, bright, blue, pleasant, perfect, not a cloud, nice, fair, crisp
# clouds:   overcast, cloud(y), dreary (trostlos), gray, !not a cloud

# drizzle:  light rain, drizzle, drizzling, drizzly, sprink(ling), lite rain, couple drops of rain, a few raindrops
# rain:     rain, rainy, wet, showers, !lite rain, !light rain drops, !couple drops of rain, !a few raindrops

# wind:     wind(y), breez(e), brisk, gusty, !no wind

# fog:      foggy, mist(y)
# humid:    humid, muggy, damp, moist

# temp:     cool, chilly, cold, warm, dewy, dewpoint, mild
# other:    artsy lighting, got dark very suddenly, shady, sun starting to go down
# no-wind:  calm, no wind

# Split the lowercase weather data by ","
weather_split <- str_split(weather, ",")

# Check the results of the split
weather_split

#words_temp <- c("cool", "chilly", "cold", "warm", "dewy", "dewpoint", "mild")
#words_other <- c("artsy lighting", "got dark very suddenly", "shady", "sun starting to go down")
#words_nowind <- c("calm", "no wind")

detect_weather <- function(x, words_positive, words_negative = character(0)) {
  detect <- any(str_detect(x, paste(words_positive, collapse = "|")))
  if (detect == TRUE && length(words_negative) > 0) {
    not_detect <- any(str_detect(x, paste(words_negative, collapse = "|")))
    if (not_detect == TRUE) {
      detect <- FALSE
    }
  }
  detect
}

# sun
words_sun <- c("sun", "clear", "bright", "blue", "pleasant", "perfect", "not a cloud", "nice", "fair", "crisp")
words_not_sun <- c("sonnelein")

sunny <- sapply(weather_split, function(x) detect_weather(x, words_sun, words_not_sun))
sunny

# rain
words_rain <- c("rain", "wet", "shower")
words_not_rain <- c("lite rain", "drop", "light rain")

rainy <- sapply(weather_split, function(x) detect_weather(x, words_rain, words_not_rain))
rainy

# clouds
words_clouds <- c("overcast", "cloud", "drear", "gray")
words_not_clouds <- ("not a cloud")

cloudy <- sapply(weather_split, function(x) detect_weather(x, words_clouds, words_not_clouds))
cloudy

# wind
words_wind <- c("wind", "breez", "brisk", "gusty")
windy <- sapply(weather_split, function(x) detect_weather(x, words_wind))
windy

# drizzle
words_drizzle <- c("light rain", "drizzle", "drizzling", "drizzly", "sprink", "lite rain", "couple drops of rain", "a few raindrops")
# !!! this will remove light rain and lite rain, drops of rain etc.
words_not_drizzle <- c("rain", "wet", "showers")
drizzly <- sapply(weather_split, function(x) detect_weather(x, words_drizzle, words_not_drizzle))
drizzly

# fog
words_fog <- c("fog", "mist")
foggy <- sapply(weather_split, function(x) detect_weather(x, words_fog))
foggy

# humid
words_humid <- c("humid", "muggy", "damp", "moist")
humid <- sapply(weather_split, function(x) detect_weather(x, words_humid))
humid

hectare$sunny <- as.logical(sunny)
hectare$rainy <- as.logical(rainy)
hectare$cloudy <- as.logical(cloudy)
hectare$windy <- as.logical(windy)
hectare$drizzly <- as.logical(drizzly)
hectare$foggy <- as.logical(foggy)
hectare$humid <- as.logical(humid)

# END making new columns for groups of weather conditions
# END making new columns for groups of weather conditions
# END making new columns for groups of weather conditions
# END making new columns for groups of weather conditions



# Write the modified dataframe to a new csv file
write.csv(hectare, "hectare_temperature.csv")

# Join datasets based on hectare, time of day and date
joint <- merge(squirrels, hectare, by=c("Hectare", "Shift", "Date"))

# write the joint dataframe to a new csv file
write.csv(joint, "joint_squirrels.csv")



write.csv(hectare, "hectare_weather.csv")

###########################
# DESCRIPTIVE STATISTICS
###########################

summary(joint)

# Number of squirrel sightings per hectare
plot(squirrels$Hectare.Squirrel.Number ~ squirrels$Hectare, main = "Squirrel Sightings per Hectare", ylab = "squirrels", xlab = "hectare")

# 


#distance between terms
#adist("partly sunny", "sunny")

# regex look for temp
#grepl(pattern = "[0-9]+° F", weather)

