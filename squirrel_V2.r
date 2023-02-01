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

# fog:      foggy, mist(y)
# humid:    humid, muggy, damp, moist

# temp:     cool, chilly, cold, warm, dewy, dewpoint, mild
# other:    artsy lighting, got dark very suddenly, shady, sun starting to go down
# no-wind:  calm, no wind

# Split the lowercase weather data by ","
weather_split <- str_split(weather, ",")

# Check the results of the split
weather_split


# TODO: do this for everything else

# check words sunny and make column true false
# check words sunny and make column true false
# check words sunny and make column true false
# check words sunny and make column true false

words_sun <- c("sun", "clear", "bright", "blue", "pleasant", "perfect", "not a cloud", "nice", "fair", "crisp")
words_not_sun <- c("sunny")

sunny <- sapply(weather_split, function(x) {
  
  sun_detect <- any(str_detect(x, paste(words_sun, collapse = "|")))
  if (sun_detect == TRUE) {
    not_sun_detect <- any(str_detect(x, paste(words_not_sun, collapse = "|")))
    if (not_sun_detect == TRUE) {
      sun_detect <- FALSE
    }
  }
  
  sun_detect
  
})

sunny


# Convert the results to a logical vector and add it as a new column to the dataframe
hectare$sunny <- as.logical(sunny)


# Write the modified dataframe to a new csv file
write.csv(hectare, "hectare_temperature.csv")

# Join datasets based on hectare, time of day and date
joint <- merge(squirrels, hectare, by=c("Hectare", "Shift", "Date"))

# Write the modified dataframe to a new csv file
write.csv(joint, "joint_squirrels.csv")



#distance between terms
#adist("partly sunny", "sunny")

# regex look for temp
#grepl(pattern = "[0-9]+° F", weather)

