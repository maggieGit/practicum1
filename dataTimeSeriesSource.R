# Practicum #1 - Maggie Sleziak
# #3: Look at the data in a time series
# Data: 
# Daily Summary from the Station at the Denver International Airport. 
# Data-range: January 1, 2010 to August 26, 2018. 
# These are daily measurements of precipitation in inches, average wind speed in miles, 
# snowfall in inches, snow depth in inches, maximum/ minimum temperature in FH
# National Centers for Environmental Information, & NCEI. Climate Data Online. 
# Retrieved from https://www.ncdc.noaa.gov/cdo-web/

# 1.- Read the downloaded data
# Run line: command, enter. Run all: alt, command, r
dev.off()
rm(list = ls(all=TRUE))
# detach("package:MTS", unload = TRUE)
library(ggplot2)
library(dplyr)
library(e1071)
library(lattice)
library(corrplot)
library(tseries) 
library(vars)
library(astsa)
library(readr)
setwd('/Users/maggie/Documents/dataScience/practicum1/project/data')
# Read the file: 
dailyData <- read_csv(file='dailyClean.csv')
head(dailyData,10)
names(dailyData)


# Check time series and if stationary: 
# Time series is stationary if its mean level and variance stay steady over time
# Maximum Temperature
tmaxTS<- ts(dailyData[,5], frequency=365, start=c(2010, 1))
plot.ts(tmaxTS, main="Daily Maximum Daily Temperature at DIA")
tmaxTSdec <- decompose(tmaxTS)
plot(tmaxTSdec)
# Check for stationarity
# http://www.statosphere.com.au/check-time-series-stationary-r/
# This data is stationary. 
require(graphics)
# Partial Autocorrelation Function - ACF: 
# Gives the partial correlation of a time series with its own lagged values, 
# controlling for the values of the time series at all shorter lags.
# The residuals are uncorrelated (Acf = 0)
# The residuals follow a normal distribution, with zero mean (unbiased) and constant variance
# It contrasts with the autocorrelation function, which does not control for other lags.
pacf(tmaxTS)
# Autocorrelation
# Plot the autocorrelation function
# Plots lags on the horizontal and the correlations on vertical axis.
acf(tmaxTS)
# If P less than 0.05 then stationary: 
Box.test(tmaxTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(tmaxTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(tmaxTS)

# Minimum Temperature
tminTS<- ts(dailyData[,6], frequency=365, start=c(2010, 1)) 
plot.ts(tminTS, main="Daily Minimum Daily Temperature at DIA")
tminTSdec <- decompose(tminTS)
plot(tminTSdec)
# This data is stationary. 
pacf(tminTS)
acf(tminTS)
# If P less than 0.05 then stationary: 
Box.test(tminTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(tminTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(tminTS)

# Snow
snowTS<- ts(dailyData[,4], frequency=365, start=c(2010, 1))
plot.ts(snowTS, main="Daily Snow Average at DIA")
snowTSdec <- decompose(snowTS)
plot(snowTSdec)
# This data is stationary. 
pacf(snowTS)
acf(snowTS)
# If P less than 0.05 then stationary: 
Box.test(snowTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(snowTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(snowTS)


# Rain
rainTS<- ts(dailyData[,3], frequency=365, start=c(2010, 1))
plot.ts(rainTS, main="Daily Precipitation Average at DIA")
rainTSdec <- decompose(rainTS)
plot(rainTSdec)
# This data is stationary. 
pacf(rainTS)
acf(rainTS)
# If P less than 0.05 then stationary: 
Box.test(rainTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(rainTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(rainTS)

# T average
tavgTS<- ts(dailyData[,7], frequency=365, start=c(2010, 1))
plot.ts(tavgTS, main="Daily Humidity Average at DIA")
tavgTSdec <- decompose(tavgTS)
plot(tavgTSdec)

# This data is stationary. 
pacf(tavgTS)
acf(tavgTS)
# If P less than 0.05 then stationary: 
Box.test(tavgTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(tavgTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(tavgTS)

# Humidity
humTS<- ts(dailyData[,8], frequency=365, start=c(2010, 1))
plot.ts(humTS, main="Daily Humidity Average at DIA")
humTSdec <- decompose(humTS)
plot(humTSdec)
# This data is stationary. 
pacf(humTS)
acf(humTS)
# If P less than 0.05 then stationary: 
Box.test(humTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(humTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(humTS)


# Dew Point
dewTS<- ts(dailyData[,9], frequency=365, start=c(2010, 1))
plot.ts(dewTS, main="Daily Dew Point Average at DIA")
dewTSdec <- decompose(dewTS)
plot(dewTSdec)
# This data is stationary. 
pacf(dewTS)
acf(dewTS)
# If P less than 0.05 then stationary: 
Box.test(dewTS,lag = 20, type = "Ljung-Box")
# If P less than 0.05 then it is stationary: 
adf.test(dewTS, alternative = "stationary")
# If P greater than 0.05 then it is stationary: 
kpss.test(dewTS)

# Wind
windTS<- ts(dailyData[,2], frequency=365, start=c(2010, 1))
plot.ts(windTS, main="Daily Wind Speed Average at DIA")
windTSdec <- decompose(windTS)
plot(windTSdec)

# VAR: 
# Endogenous variables - Variables influenced by each other - there is correlation
# Exogeneous variables - Variables not affected by another variable - no correlation
# From: https://onlinecourses.science.psu.edu/stat510/node/79/
x = cbind(tavgTS, humTS, dewTS)
plot.ts(x , main = "", xlab = "")

# We can include the trend directly in the VAR model  
fitvar1=VAR(x, p=1, type="both")
summary(fitvar1)
# The highest R-squared is: tavgTS = tavgTS.l1 + humTS.l1 + dewTS.l1 + const + trend = 
# Adjusted R-squared: 0.8535 

# The residuals should be uncorrelated (Acf = 0) - This is false for all VARs

# It is important now to test for serial correlation (or autocorrelation) in the model  
# Apply a portmanteau test to check the hypothesis that residuals are uncorrelated 
# The null hypothesis of no autocorrelation is rejected since the p-value < 2.2e-16 
# is lower than the significance level of 0.05. 
# Therefore there is autocorrelation
serial.test(fitvar1, lags.pt=10, type="PT.asymptotic")

# With lag of 2 days: 
fitvar2 = VAR(x, p=2, type="both")
summary(fitvar2)
# The highest R-squared is: tavgTS = tavgTS.l1 + humTS.l1 + dewTS.l1 + tavgTS.l2 + humTS.l2 + dewTS.l2 + const + trend  = 
# Adjusted R-squared (Coefficient of determination): 0.8535  
serial.test(fitvar2, lags.pt=10, type="PT.asymptotic")

# With lag of 3 days: 
fitvar3 = VAR(x, p=3, type="both")
summary(fitvar3)
serial.test(fitvar3, lags.pt=10, type="PT.asymptotic")

# The highest R-squared is: tavgTS = tavgTS.l1 + humTS.l1 + dewTS.l1 + tavgTS.l2 + humTS.l2 + dewTS.l2 + tavgTS.l3 + humTS.l3 + dewTS.l3 + const + trend   = 
# Adjusted R-squared: 0.8603 
# So with the var, the highest we can get right now buy estimating tavgTS with dew, hum with 3 lag (days) measuremems is: 
# Residual standard error: 6.897
# The residual standard error is the square root of the residual sum of squares divided by the residual degrees of freedom. 
# The mean square error is the mean of the sum of squared residuals, i.e. it measures the average of the squares of the errors. Adjusted R-squared: 0.8603 

# Look at the residuals: Looks good a little more white noise 
# (The spike at the beginning is the unimportant lag 0 correlation.)
acf(residuals(fitvar3)[,1]) # TAVG
acf(residuals(fitvar3)[,2]) # HUM
acf(residuals(fitvar3)[,3]) # DEWP

pacf(residuals(fitvar3)[,1]) # TAVG
pacf(residuals(fitvar3)[,2]) # HUM
pacf(residuals(fitvar3)[,3]) # DEWP
summary(fitvar3, equation="tavgTS")


plot(fitvar3)

# https://stats.stackexchange.com/questions/110999/r-confused-on-residual-terminology
# # Mean squared error
# The mean squared error (MSE) is the mean of the square of the residuals:
tavg_mse <- mean(residuals(fitvar3)[,1]^2)
tavg_mse

# Root mean squared error (RMSE) is then the square root of MSE:
tavg_rmse <- sqrt(tavg_mse)
tavg_rmse

# Residual sum of squares (RSS) is the sum of the squared residuals:
tavg_rss <- sum(residuals(fitvar3)[,1]^2)
tavg_rss

# Residual standard error (RSE) is the square root of residual sum of squares (RSS / degrees of freedom):
# rse <- sqrt(sum(residuals(fitvar3)[,1]^2) / fitvar3$df.residual ) 
tavg_rse <- sqrt(sum(residuals(fitvar3)[,1]^2) / 3146 ) # Regression degrees of freedom
tavg_rse


# Experimenting 
# Forecast for further time periods: (Here H is one day)
library("forecast")
fitvar3Extended <- forecast(fitvar3, h=20)
plot(forecast(fitvar3, h=50), include = 1000)
fitvar3Extended
