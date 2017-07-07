###############################
# Benchmark Methods Exercises #
###############################

# packages used
library(forecast)
library(fpp2)

# data used
AirPassengers

# 1. Partition your data into a training set that includes 1949-1959 and a test 
#    set that includes all months in 1960.
train <- window(AirPassengers, start = c(1949, 1), end = c(1959, 12))
test <- window(AirPassengers, start = c(1960, 1), end = c(1960, 12))

# 2. Examine time plots of the training data set.
autoplot(train)
ggseasonplot(train)
ggseasonplot(train, polar = TRUE)
ggsubseriesplot(train)
ggAcf(train)
gglagplot(train)

# 3. Perform a naive and seasonal naive model on the training data.
fc_naive <- naive(train, 12)
summary(fc_naive)

fc_snaive <- snaive(train, 12)
summary(fc_snaive)

# 4. Assess the residuals of these models.  Do they meet the required assumptions?  
#    Do they appear to be white noise or can you still see a pattern in the residuals?
checkresiduals(fc_naive)
checkresiduals(fc_snaive)

# 5. Compare the forecasting accuracy of these models to the values in the test 
#    data.  Which model provides greater forecasting accuracy?
accuracy(fc_naive, test)
accuracy(fc_snaive, test)

# 6. Using the entire `AirPassengers` data set, perform a time series cross validation that:
#    - Uses the best performing model in exercise 5.
#    - Computes and compares the MSE for different forecast horizons (1-8).
#    - Which time horizon produces the lowest MSE?

MSE <- vector("numeric", 8)
for(h in 1:8) {
  errors <- tsCV(AirPassengers, forecastfunction = snaive, h = h)
  MSE[h] <- mean(errors^2, na.rm = TRUE)
}
MSE



