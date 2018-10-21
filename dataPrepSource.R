# Practicum #1 - Maggie Sleziak
# #1: Read data and get the needed columns
# Data: 
# The First file is the Daily Summary from the Station at the Denver International Airport. 
# Data-range: January 1, 2010 to August 26, 2018. 
# 
# These are daily measurements of precipitation in inches, average wind speed in miles, 
# snowfall in inches, snow depth in inches, maximum/minimum temperature in FH

# The Second file is the Local Climatological Data for the Same Station (Denver International Airport)
# Data Retrieved from: 
# National Center for Environmental Information, & NCEI. Climate Data Online. 
# Retrieved from https://www.ncdc.noaa.gov/cdo-web/

# 1.- Read the downloaded data
# Run line: command, enter. Run all: alt, command, r
rm(list = ls(all=TRUE))
library(dplyr)
library(mice)
library(lubridate)
library(ggplot2)
library(readr)
setwd('/Users/maggie/Documents/dataScience/practicum1/project/data')
list.files()

# Read the file: 
dailyRaw <- read_csv(file='diaDaily_raw.csv')
# Number of variables
length(dailyRaw)
# Types of data and of variables
# 3160 obs. of  36 variables:
str(dailyRaw)
# Names of the variables
names(dailyRaw)
# Statistical summary of the variables
summary(dailyRaw)
# Print first 10 rows
head(dailyRaw,10)
# Make sure these are all records from the Denver International Airport
dailyRaw <- dplyr::filter(dailyRaw, NAME == 'DENVER INTERNATIONAL AIRPORT, CO US')
# 3160 obs. of  36 variables
str(dailyRaw)
# Get the variables we are interested in  
dailyNew <- dailyRaw[, c('DATE', 'AWND', 'PRCP', 'SNOW', 'TMAX', 'TMIN')]
names(dailyNew)
head(dailyNew,10)
# Summary showing 3 NAs for AWND
summary(dailyNew)
# Take a look at the NAs - where there are in the data set. 
tempNA = dailyNew[is.na(dailyNew$AWND),] 
tempNA
# I do not want to remove these 3 days since I still need to add pressure and dew point from a different data set for these days.

# https://www.statmethods.net/input/missingdata.html
# Let's look at the statistics
summary(dailyNew$AWND) 
mean(dailyNew$AWND, na.rm=T)
# Try to replace the missing NA with the mean:  
dailyNew$AWND.imp.mean<-ifelse(is.na(dailyNew$AWND)==T, mean(dailyNew$AWND, na.rm=T), dailyNew$AWND)
# Same result for mean and median (central tendency) as with removing the NA, but now we keep the time structure. 
mean(dailyNew$AWND.imp.mean)
median(dailyNew$AWND.imp.mean)
median(dailyNew$AWND, na.rm=T)
# The variance is lower 
var(dailyNew$AWND, na.rm=T)
var(dailyNew$AWND.imp.mean)
# Let's if we can improve the difference in the variance by imputing with the mice library
imp<-mice(data = dailyNew[,c('DATE', 'AWND', 'PRCP', 'SNOW', 'TMAX', 'TMIN')], seed = 22)
dailyImp<-complete(imp)
# Checking - same # of obs, 
str(dailyNew)
str(dailyImp)
# Same median and mean
summary(dailyNew)
summary(dailyImp)
# A little closer with the variance with removing the NAs. - Will use this as need the time structure. 
var(dailyNew$AWND, na.rm=T)
var(dailyImp$AWND)

# Since there is no temperature average in the data set - get the mean from the TMAX and TMIN
dailyImp$TAVG = (dailyImp$TMIN + dailyImp$TMAX) / 2
head(dailyImp,10)

# Get year, month and day columns as this will be needed to get data from second dataset 

dailyImp <- mutate(dailyImp, year = lubridate::year(DATE), month = lubridate::month(DATE), 
                day = lubridate::day(DATE), week = lubridate::wday(DATE, label = TRUE))
head(dailyImp,10)

# +++++++++++++++++++++ ADD VARIABLES FROM SECOND DATA SET ++++++++++++++++++++++++++++++++++++

climateRaw <- read_csv('diaClimate_raw.csv')
climateRaw <- dplyr::filter(climateRaw, STATION_NAME == 'DENVER INTERNATIONAL AIRPORT CO US')
summary(climateRaw)
climate <- dplyr::select(climateRaw, DATE, DAILYAverageRelativeHumidity, DAILYAverageDewPointTemp)
climate <- mutate(climate, year = lubridate::year(DATE), month = lubridate::month(DATE), 
                    day = lubridate::day(DATE), week = lubridate::wday(DATE, label = TRUE))

names(climate)
head(climate,10)
# The DAILYAverageRelativeHumidity, DAILYAverageDewPointTemp values are once a day, the climate data is hourly, thus there are NA values for 23 hours of each day. 
# I am removing all the NA values so then we only have one value per day
climate <- climate[complete.cases(climate), ]
head(climate,10)
# 2562 obs - a few less than the 3160 obs
str(climate)

# Merge the variables from climate into dailyImp based on Year, month and day
dailyImp <- merge(dailyImp, climate[, c('year', 'month', 'day', 'DAILYAverageRelativeHumidity', 'DAILYAverageDewPointTemp')], by = c('year','month', 'day'), 
               all.x = TRUE)
head(dailyImp,10)

# Drop month day week - these were needed for the merge. 
dailyImp <- dailyImp[, c('DATE', 'AWND', 'PRCP', 'SNOW', 'TMAX', 'TMIN', 'TAVG', 'DAILYAverageRelativeHumidity', 'DAILYAverageDewPointTemp', 'year')]
head(dailyImp,10)

# Rename the long variables
dailyImp <- dplyr::rename(dailyImp, HUM = 'DAILYAverageRelativeHumidity', DEWP = 'DAILYAverageDewPointTemp')
head(dailyImp,10)

# Look for NAs (598 for both HUM and DEWP)
summary(dailyImp,10)

# See if we can impute with MICE as I do not want to get rid of these records. We can see how it is only HUM and DEWP
md.pattern(dailyImp[,c('DATE', 'AWND', 'PRCP', 'SNOW', 'TMAX', 'TMIN', 'TAVG', 'HUM', 'DEWP', 'year')])

imp2<-mice(data = dailyImp[,c('DATE', 'AWND', 'PRCP', 'SNOW', 'TMAX', 'TMIN', 'TAVG', 'HUM', 'DEWP', 'year')], seed = 22)
print(imp2)
dailyImp2<-complete(imp2)
# Checking - same # of obs, 
str(dailyImp)
str(dailyImp2)
# median and mean
summary(dailyImp)
summary(dailyImp2)
# VAR 
# Tried to replace the missing NA with the mean - The MICE is better, leaving the MICE
dailyImp$HUM.imp.mean<-ifelse(is.na(dailyImp$HUM)==T, mean(dailyImp$HUM, na.rm=T), dailyImp$HUM)
var(dailyImp$HUM, na.rm=T)
var(dailyImp$HUM.imp.mean)
var(dailyImp2$HUM)

dailyImp$DEWP.imp.mean<-ifelse(is.na(dailyImp$DEWP)==T, mean(dailyImp$DEWP, na.rm=T), dailyImp$DEWP)
var(dailyImp$DEWP, na.rm=T)
var(dailyImp$DEWP.imp.mean)
var(dailyImp2$DEWP)

head(dailyImp2,10)

# +++++++++++++++++++++++++++++ Write to file for data prep +++++++++++++++++++++++++++++++
write_csv(dailyImp2, 'dailyClean.csv')
