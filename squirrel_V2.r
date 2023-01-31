# Load the stringr library
library(stringr)

# Read the csv file into a dataframe
hectare <- read.csv("2018_Central_Park_Squirrel_Census_-_Hectare_Data.csv")

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
table(unique_weather[grepl(pattern = "sun", x = unique_weather)])

# sun:      sun(ny), clear, bright, blue, pleasant, perfect, not a cloud, nice, fair, crisp
# clouds:   overcast, cloud(y), dreary (trostlos), gray, !not a cloud

# drizzle:  light rain, drizzle, drizzling, drizzly, sprink(ling), lite rain, couple drops of rain, a few raindrops
# rain:     rain, rainy, wet, showers, !lite rain, !light rain drops, !couple drops of rain, !a few raindrops

# wind:     wind(y), breez(e), brisk, gusty, !no wind
# no-wind:  calm, no wind

# fog:      foggy, mist(y)
# humid:    humid, muggy, damp, moist

# temp:     cool, chilly, cold, warm, dewy, dewpoint, mild
# other:    artsy lighting, got dark very suddenly, shady, sun starting to go down

# Split the lowercase weather data by ","
weather_split <- str_split(weather, ",")

# Check the results of the split
weather_split

# Check if the string "foggy" is present in each split weather string
temp <- sapply(weather_split, function(x) any(str_detect(x, "foggy")))

# Convert the results to a logical vector and add it as a new column to the dataframe
hectare$sunny <- as.logical(temp)

# Write the modified dataframe to a new csv file
write.csv(hectare, "hectare_temperature.csv")




#distance between terms
#adist("partly sunny", "sunny")

# regex look for temp
#grepl(pattern = "[0-9]+° F", weather)