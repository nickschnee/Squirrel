library(osmdata)
library(sf)
library(ggmap)


squirrels <- read.csv("joint_squirrels.csv")

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
  geom_point(data = squirrels,
             aes(x = X, y = Y, color = sunny),
             ) +
  theme(legend.position = "none") +
  labs(x = "Longitude", y = "Latitude")


# get number of distinct values
library(dplyr)
n_distinct(squirrels$Hectare)

















