# Practicum #1 - Maggie Sleziak
# #3: Prep the data for running algorithms in Python - Turn to supervised problem
# Data: 
# Daily Summary from the Station at the Denver International Airport. 
# Data-range: January 1, 2010 to August 26, 2018. 
# These are daily measurements of precipitation in inches, average wind speed in miles, 
# snowfall in inches, snow depth in inches, maximum/ minimum temperature in FH
# National Centers for Environmental Information, & NCEI. Climate Data Online. 
# Retrieved from https://www.ncdc.noaa.gov/cdo-web/

# 1.- Read the downloaded data
# Run line: command, enter. Run all: alt, command, r

rm(list = ls(all=TRUE))
library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(GGally)
setwd('/Users/maggie/Documents/dataScience/practicum1/project/data')
# Read the file: 
dailyData <- read_csv(file='dailyClean.csv')
head(dailyData,10)
names(dailyData)

# Let's take the average temperature, humidity and dew point. 
# Let's predict current temperature based on measurements from previous day(s)
dailyLag <- dailyData[, c('DATE', 'TAVG', 'HUM', 'DEWP')]
dailyLag <- dplyr::rename(dailyLag, date = 'DATE', yTavg = 'TAVG', hum = 'HUM', dewp = 'DEWP')
write_csv(dailyLag, 'dailyTemp.csv')
dailyLag$tavg1 <- c(NA, dailyLag$yTavg[1:{nrow(dailyLag) - 1}])
dailyLag$tavg2 <- c(NA, NA, dailyLag$yTavg[1:{nrow(dailyLag) - 2}])
dailyLag$tavg3 <- c(NA, NA, NA, dailyLag$yTavg[1:{nrow(dailyLag) - 3}])
head(dailyLag,10)

dailyLag$hum1 <- c(NA, dailyLag$hum[1:{nrow(dailyLag) - 1}])
dailyLag$hum2 <- c(NA, NA, dailyLag$hum[1:{nrow(dailyLag) - 2}])
dailyLag$hum3 <- c(NA, NA, NA, dailyLag$hum[1:{nrow(dailyLag) - 3}])

dailyLag$dewp1 <- c(NA, dailyLag$dewp[1:{nrow(dailyLag) - 1}])
dailyLag$dewp2 <- c(NA, NA, dailyLag$dewp[1:{nrow(dailyLag) - 2}])
dailyLag$dewp3 <- c(NA, NA, NA, dailyLag$dewp[1:{nrow(dailyLag) - 3}])
head(dailyLag,10)
# Lag one day: 
dailyLag1 <- dailyLag[, c('hum1', 'dewp1', 'tavg1','yTavg' )]
head(dailyLag1,10)
# Remove the 1st row of NAs: 
dailyLag1<- dailyLag1[-c(1), ]
head(dailyLag1,10)

# Lag 3 days of information for the current temp
dailyLag3 <- dailyLag[, c('hum3','hum2','hum1', 'dewp3','dewp2','dewp1','tavg3', 'tavg2','tavg1','yTavg' )]
head(dailyLag3,10)
# Remove the 1st row of NAs: 
dailyLag3<- dailyLag3[-c(1,2,3), ]
head(dailyLag3,10)


correlations <- cor(dailyLag3)
print(correlations)

# Values above/below zero show more positive or negative correlation
# Values above +/- 0.75 show high correlation - values of +/1 show full positive/negative correlation. 
# Correlation plot: 
corrplot(correlations, method="circle")


# +++++++++++++++++++++++++++++ Write to file for data descritions +++++++++++++++++++++++++++++++
write_csv(dailyLag1, 'dailyLag1.csv')
write_csv(dailyLag3, 'dailyLag3.csv')

