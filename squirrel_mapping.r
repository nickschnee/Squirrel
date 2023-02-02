library(osmdata)
library(sf)
library(ggmap)


squirrels <- read.csv("2018_Central_Park_Squirrel_Census_-_Squirrel_Data.csv")

# first five features
head(available_features())

longitude_min <- min(squirrels$X) - 0.005
latitude_min <- min(squirrels$Y) - 0.001
longitude_max <- max(squirrels$X) + 0.005
latitude_max <- max(squirrels$Y) + 0.001


# Background map
mad_map <- get_map(location = c(left = longitude_min, bottom = latitude_min, right = longitude_max, top = latitude_max),
                   source = "stamen",
                   maptype = "toner-background",
                   zoom = 15,
                   cache = TRUE)


# Some points
d.stations <- data.frame(Latitude = squirrels$Y, 
                         Longitude = squirrels$X)

# Plot map
ggmap(mad_map) +
  geom_sf(data = cinema$osm_points,
          inherit.aes = FALSE,
          colour = "#238443",
          fill = "#004529",
          alpha = .5,
          size = 10,
          shape = 21) +
  geom_point(data = d.stations,
             aes(x = Longitude, y = Latitude),
             color = "blue") +
  labs(x = "Longitude", y = "Latitude")



















