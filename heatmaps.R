# Load required packages
library(ggplot2)


weather <- read.csv("hectare_weather.csv")
squirrels <- read.csv("joint_squirrels.csv")
hectare <- read.csv("2018_Central_Park_Squirrel_Census_-_Hectare_Grid.csv")


#############################


# Scatterplot
plot(weather$Hectare, weather$Number.of.Squirrels, 
     xlab="Hectare ID", 
     ylab="Number of Squirrel Sightings",
     main="Squirrel Sightings per Hectare in Central Park")

#############################

# not sure ob dieser graph stimmt...

# Plot histogram of squirrel sightings
ggplot(data = weather, aes(x = Number.of.Squirrels)) +
  geom_histogram(fill = "gray", color = "black") +
  xlab("Squirrel Sightings") +
  ylab("Frequency") +
  ggtitle("Histogram of Squirrel Sightings Per Hectare")



#############################

# Heatmap with squirrels per hectare in central park
ggplot(data = weather, cache = TRUE, aes(x = as.numeric(substr(Hectare, 1, 2)), 
                           y = as.factor(substr(Hectare, 3, 3)), 
                           fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "darkgreen") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel density per hectare") +
  theme(legend.position = "bottom")



#########################

# Heatmaps per weather condition: sunny

# Subset data for each weather condition
weather_sunny <- subset(weather, sunny == TRUE)
weather_cloudy <- subset(weather, cloudy == TRUE)
weather_windy <- subset(weather, windy == TRUE)
weather_rainy <- subset(weather, rainy == TRUE)
weather_humid <- subset(weather, humid == TRUE)
weather_foggy <- subset(weather, foggy == TRUE)
weather_drizzly <- subset(weather, drizzly == TRUE)

# Plot each subset in a separate plot: heatmap for squirrel sightings when sunny
p_sunny <- ggplot(data = weather_sunny, aes(x = as.numeric(substr(Hectare, 1, 2)), 
                                            y = as.factor(substr(Hectare, 3, 3)), 
                                            fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "gold") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Sunny)") +
  theme(legend.position = "bottom")

p_sunny


# Plot each subset in a separate plot: heatmap for squirrel sightings when rainy
p_rainy <- ggplot(data = weather_rainy, 
                  aes(x = as.numeric(substr(Hectare, 1, 2)), 
                                            y = as.factor(substr(Hectare, 3, 3)), 
                                            fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "darkblue") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Rainy)") +
  theme(legend.position = "bottom")

p_rainy


# Plot each subset in a separate plot: heatmap for squirrel sightings when cloudy
p_cloudy <- ggplot(data = weather_cloudy, 
                  aes(x = as.numeric(substr(Hectare, 1, 2)), 
                      y = as.factor(substr(Hectare, 3, 3)), 
                      fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "darkgrey") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Cloudy)") +
  theme(legend.position = "bottom")

p_cloudy


# Plot each subset in a separate plot: heatmap for squirrel sightings when humid
p_humid <- ggplot(data = weather_humid, 
                   aes(x = as.numeric(substr(Hectare, 1, 2)), 
                       y = as.factor(substr(Hectare, 3, 3)), 
                       fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "violet") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Humid)") +
  theme(legend.position = "bottom")

p_humid


# Plot each subset in a separate plot: heatmap for squirrel sightings when drizzly
p_drizzly <- ggplot(data = weather_drizzly, 
                  aes(x = as.numeric(substr(Hectare, 1, 2)), 
                      y = as.factor(substr(Hectare, 3, 3)), 
                      fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "blue") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Drizzly)") +
  theme(legend.position = "bottom")

p_drizzly


# Plot each subset in a separate plot: heatmap for squirrel sightings when foggy
p_foggy <- ggplot(data = weather_foggy, 
                    aes(x = as.numeric(substr(Hectare, 1, 2)), 
                        y = as.factor(substr(Hectare, 3, 3)), 
                        fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "brown") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Foggy)") +
  theme(legend.position = "bottom")

p_foggy



# Plot each subset in a separate plot: heatmap for squirrel sightings when windy
p_windy <- ggplot(data = weather_windy, 
                    aes(x = as.numeric(substr(Hectare, 1, 2)), 
                        y = as.factor(substr(Hectare, 3, 3)), 
                        fill = Number.of.Squirrels)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = "red") +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Windy)") +
  theme(legend.position = "bottom")

p_windy

############################################

# Combined heatmaps: sunny and rainy

weather$fill_alpha <- ifelse(weather$sunny & weather$rainy, 0.75, 
                             ifelse(weather$sunny, 1, 0.5))

ggplot(data = weather, aes(x = as.numeric(substr(Hectare, 1, 2)), 
                           y = as.factor(substr(Hectare, 3, 3)), 
                           fill = ifelse(sunny, 
                                         Number.of.Squirrels * (1-rainy), 
                                         Number.of.Squirrels * rainy),
                           alpha = fill_alpha)) +
  geom_tile() +
  geom_text(aes(label = Hectare), color = "darkgrey", size = 3, vjust = 0.5) +
  scale_fill_gradient(low = "white", high = c("yellow", "blue"),
                      breaks = c(0, 1)) +
  scale_x_continuous(limits = c(0, 43), expand = c(0, 0)) +
  scale_y_discrete(limits = c("A", "B", "C", "D", "E", "F", "G", "H", "I"), expand = c(0, 0)) +
  labs(x = "Grid Column (1-42)", y = "Grid Row (A-I)", fill = "Number of Squirrels") +
  theme_classic() +
  ggtitle("Squirrel Density per Hectare (Sunny and Rainy)") +
  theme(legend.position = "bottom")



