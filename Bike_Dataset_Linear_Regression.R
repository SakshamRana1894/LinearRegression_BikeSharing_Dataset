#### Linear Regression on Bike Sharing Dataset ##
#### Perform simple linear regression to study relation between bike rented and temperature
### setup library
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(caTools)

### set working directory
#working_directory <- readline(prompt = "Enter working directory path: ")
setwd("D:/Material/Simple_Linear_Regression/Simple_Linear_Regression/Bike-Sharing-Dataset")

### import data
bike_data <- read_csv("hour.csv", progress = show_progress())


### explore data
str(bike_data)
summary(bike_data)

## data cleaning and checking for outliers
# check for NAs
# if any outliers replace with mean or median
sapply(bike_data, function(x)(sum(is.na(x))))
# here we see not NAs in data

# check for normality in cnt variable
cnt_distribution <- ggplot(bike_data, aes(x = cnt)) +
  geom_histogram(position = "identity", color = "black", fill = "cyan", alpha = 0.3) + 
  geom_vline(aes(xintercept = mean(cnt)), colour = "red", linetype = "dotted", size = 1)
ggplotly(cnt_distribution)
# it is observed that the data is normally distributed and validates the assumption of regression

# check for normality in temp variable
temp_distribution <- ggplot(bike_data, aes(x = temp)) + 
  geom_histogram(color = "black", fill = "red", alpha = 0.3) + 
  geom_vline(aes(xintercept = mean(temp), colour = "red", size = 1))
ggplotly(temp_distribution)
# distribution is skewed implying that assumption of linear regression is not validated; 
# may result in a faulty/inaccurate model

# check correlation of "cnt" with "temp"
cor(bike_data$temp, bike_data$cnt)
# it is observed that there is little positive linear relationship between "cnt" and "temp". 
# We will observe scatter plot will have high heteroskedasticity in data

scatter_cnt_temp <- ggplot(bike_data, aes(x = temp, y = cnt)) + 
  geom_point(aes(color = temp), alpha = 0.4) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + 
  ggtitle(label = "cnt vs temp")
scatter_cnt_temp
# it's observed that as temperature increases the number of bikes rented increases, however relation
# is not strong

cor(bike_data$hum, bike_data$cnt)
scatter_cnt_hum <- ggplot(bike_data, aes(x = hum, y =  cnt)) + 
  geom_point(aes(color = hum), alpha = 0.4) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") +
  ggtitle(label = "cnt vs hum")
scatter_cnt_hum
# it is observed there is negative weak linear relation between humidity and count; indicationg linear 
# regression is not the right way to go for prediction

cor(bike_data$windspeed, bike_data$cnt)
scatter_cnt_windspeed <- ggplot(bike_data, aes(x = windspeed, y =  cnt)) + 
  geom_point(aes(color = windspeed), alpha = 0.4) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") +
  ggtitle(label = "cnt vs windspeed")
scatter_cnt_windspeed
# it is observed that as windspeed increases number of rentals decline. Again there is little linear
# relationship to support prediction

# checking how rentals are affected throughout the year
bike_data$dteday <- as.POSIXct(bike_data$dteday)

cnt_dteday <- ggplot(bike_data, aes(x = dteday, y = cnt)) + 
  geom_point(aes(color = temp), alpha = 0.4) + 
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69")
ggplotly(cnt_dteday)
# it is observed that bike rentals increase during summers as compared. Also overall the bike rentals 
# have increased.

scatter_hr_cnt_wd <- ggplot(bike_data[bike_data$workingday == 1, ], aes(x = hr, y = cnt)) + 
  geom_point(aes(color = temp), alpha = 0.4, position = position_jitter(w = 1, h = 0)) + 
  ggtitle(label = "Bikes Rented on Working Days")
scatter_hr_cnt_wd
# it is observed that bike rentals on working days tend to peak during 8A.M. and 5 P.M.. This maybe as 
# people hire bikes to for exercise purposes or transit to office.

scatter_hr_cnt_nwd <- ggplot(bike_data[bike_data$workingday == 0, ], aes(x = hr, y = cnt)) + 
  geom_point(aes(color = temp), position = position_jitter(w = 1, h = 0)) + 
  ggtitle(label = "Bikes Rented on Non Working Days")
scatter_hr_cnt_nwd
# it is observed that bike rental activity escalates during noon 

# Split into training and test sets
set.seed(123)
# split_vector <- sample.split(bike_data$cnt, SplitRatio = 0.8)
# training_set <- subset(bike_data, split_vector == TRUE)
# test_set <- subset(bike_data, split_vector == FALSE)

training_set <- subset(bike_data, as.numeric(format(as.Date(bike_data$dteday), "%d")) <= 20) 
test_set <- subset(bike_data, as.numeric(format(as.Date(bike_data$dteday), "%d")) > 20) 

# Feature Scaling # Not needed taken care of by "lm" function
# scaling should only be applied on columns that are numeric
# check if all columns are numeric; exclude any columns that are not numeric
sapply(bike_data, is.numeric)

#trainging_set <- scale(training_set[, -2])
#test_set <- scale(test_set[, -2])

# Model 
model <- lm(cnt ~ temp, training_set)
summary(model)
# we can observe that R Square value is very low which is evident as stated above where we see variables
# do not have a strong linear relation with bikes rented
model$coefficients
model$residuals # shows difference between actual values and predicted values in training set
model$fitted.values # these are actual values or predicted values

# Predict
y_pred <- predict(model, newdata = test_set)

# Graphical representation on Training Set
ggplot() +
  geom_point(aes(x = training_set$temp, y = training_set$cnt),
             colour = 'red') +
  geom_line(aes(x = training_set$temp, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  geom_smooth(aes(x = training_set$temp, y = training_set$cnt)) +
  ggtitle('Bikes Rented vs Temperature (Training set)') +
  xlab('Bikes Rented') +
  ylab('Temperature')

# Graphical representation on Test Set
ggplot() +
  geom_point(aes(x = test_set$temp, y = test_set$cnt),
             colour = 'red') +
  geom_line(aes(x = training_set$temp, y = predict(model, newdata = training_set)),
            colour = 'blue') +
  ggtitle('Bikes Rented vs Temperature (Test set)') +
  xlab('Bikes Rented') +
  ylab('Temperature')

# Residual Analysis
# Residual plot should show constant variance/spread. Y axis = residue; X axis = temp
# there should be random spread in the graph that indicated constant variance
residual_plot <- 
  ggplot() + 
  geom_point(data = training_set, aes(x = training_set$temp, y = resid(model)), colour = "red") + 
  geom_hline(aes(yintercept = mean(resid(model))), linetype = "dashed", size = 1) + 
  geom_smooth(aes(x = training_set$temp, y = resid(model)))
residual_plot

# it is observed that the residuals increase as temperture increases. This means that variance in 
# predicted values and actual values increases. This indicates that the model does not have constant 
# variance. Also this phenomenon is known as heteroskedasticity.

# residuals here show skewness in normal distribution
ggplot() + 
  geom_histogram(aes(x = model$residuals), color = "red")

# plot model to sum up residuals
plot(model)

# Conclusion
# 1. Temperature has weak linear relationship with bikes rented.
# 2. Coefficient of Determination is very low, thereby supporting that model does not explain 
#    variation in temp with variation in y.
# 3. Residual plots also do not show constant variance, there is increase in error


