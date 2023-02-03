squirrels <- read.csv("joint_squirrels.csv")
weather <- read.csv("hectare_weather.csv")


'''
GLM with date and shift

'''

# Load the necessary packages
library(tidyverse)

# Fit the GLM model
model <- glm(Number.of.Squirrels ~ Date + Shift,
             data = weather,
             family = poisson())


# Summarize the model
summary(model)



'''
oVERVIEW OVER THE NUMBER OF OCCURRENCES OF THE DIFFERENT WEATHER CONDIITONS (AND OTHER THINGS)
'''
table(weather$Litter)
table(weather$Date)
table(weather$Shift)

table(weather$sunny)
table(weather$rainy)
table(weather$cloudy)
table(weather$foggy)
table(weather$humid)
table(weather$drizzly)





''' 
USEFUL:  Generalized Linear Model (GLM) with a Poisson distribution

The results obtained from the Poisson regression model show the relationship between the number of squirrels and the weather conditions.

The model estimates the expected log count of squirrels as a linear combination of the predictor variables. The estimated coefficients indicate the effect of each predictor variable on the response variable while holding all other predictors constant. The standard errors of the coefficients indicate the uncertainty in the estimates.
'''
# Fit the GLM model
model <- glm(Number.of.Squirrels ~ sunny + drizzly + cloudy + foggy + rainy + humid,
             data = weather,
             family = poisson())

summary(model)

''' 
According to the results, the presence of rainy weather significantly decreases the expected number of squirrels, while the presence of drizzly and humid weather significantly increases the expected number of squirrels. The presence of sunny weather has a small, but not significant, negative effect on the expected number of squirrels. The presence of cloudy and foggy weather does not have a significant effect on the expected number of squirrels.

The residual deviance of 2066.5 on 693 degrees of freedom indicates that the model fits the data well, as the residual deviance is smaller than the null deviance. The AIC of 4069.7 also suggests that the model is a good fit to the data.

Overall, these results suggest that the number of squirrels observed in Central Park may be influenced by the weather conditions, particularly the presence of rainy, drizzly, and humid weather.
'''




'''
LINEAR REGRESSION WITH WEATHER CONDITIONS, SHIFT, HECTARE CONDITIONS AND TEMPERATURE
'''
# Fit the GLM model
model <- glm(Number.of.Squirrels ~ sunny + drizzly + cloudy + foggy + rainy + humid + Shift + Hectare.Conditions + temperature_celsius,
             data = weather,
             family = poisson())

# Summarize the model
summary(model)




'''
REGRESSION WITH TIME and TEMP (with interaction term)
'''
reg <- glm(Number.of.Squirrels ~ temperature_celsius * Shift, data = weather, family = poisson())
summary(reg)




