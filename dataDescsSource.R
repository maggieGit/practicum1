# Practicum #1 - Maggie Sleziak
# #2: Exploratory Data Analysis

# 1.- Read the downloaded data
# Run line: command, enter. Run all: alt, command, r
rm(list = ls(all=TRUE))
library(ggplot2)
library(dplyr)
library(e1071)
library(lattice)
library(corrplot)
library(GGally)
library(readr)
setwd('/Users/maggie/Documents/dataScience/practicum1/project/data')
# Read the file: 
dailyData <- read_csv(file='dailyClean.csv')
head(dailyData,10)
# Dimensions of the data: 
dim(dailyData)
# Looking at the mean and standard deviation
summary(dailyData)

# Distribution: 
## skew per each non date variable: 
# The larger the skew value if it is away from zero. Negative skew if less than zero, positive skew if more than zero. 
# TMIN,TMAX, TAVG have little negative skew. DEWP also small negative skew. AWND (Wind) and HUM slight postive skew, 
# PRCP and SNOW have high postive skew most likely since it does not rain as much, 
# and snows only during some months of the year. 
sapply(dailyData[,2:9], skewness)


# Let's look at the distribution of the numerial variables with histograms: 
dev.off()
hist(dailyData$TMAX, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(0 , 110), col = "orange", main = "Histogram: DIA - Maximum Temperature - Day", xlab = "Whole Degrees Fahrenheit")
hist(dailyData$TMIN, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(-20 , 80), col = "blue", main = "Histogram:DIA - Minimum Temperature - Day", xlab = "Whole Degrees Fahrenheit")
hist(dailyData$TAVG, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(-10 , 90), col = "brown", main = "Histogram:DIA - Average Temperature - Day", xlab = "Degrees Fahrenheit")
hist(dailyData$PRCP, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(0 , 2), col = "green", main = "Histogram:DIA - Precipitation - Day", xlab = "Inches")
hist(dailyData$SNOW, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(0 , 13), col = "grey", main = "Histogram:DIA - Snowfall -  Day", xlab = "Inches")
hist(dailyData$HUM, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(10 , 100), col = "navy", main = "Histogram:DIA - Average Relative Humidity -Day", xlab = "Nearest Whole Percentage")
hist(dailyData$DEWP, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(-14 , 65), col = "yellow", main = "Histogram:DIA - Dew Point Temp - Day", xlab = "Whole Degrees Fahrenheit")
hist(dailyData$AWND, freq=NULL, density=NULL, breaks=200, prob = T, xlim = c(3 , 28), col = "red", main = "Histogram:DIA - Average Wind Speed - Day", xlab = "Miles")


# Box plot to look at the distribution of the data - Also look for any data outliers: 
# Box 50% of the data, line is the median,wiskers show the reasonable extend of the data. 
# Dots outside the wiskers are good candidates for outliers. 

# Temp, Humidity and Dew Point have little outliers. Wind much more and Rain and Snow lots of outliers. 


# Temp Avg
p <- ggplot(dailyData, aes(x=year, y=TAVG)) + 
  geom_boxplot(fill="brown") + 
  labs(title="Boxplot - Daily Average Temperature - DIA",x="Year", y = "Temp AVG (FH)")
p

# Temp Max
p <- ggplot(dailyData, aes(x=year, y=TMAX)) + 
  geom_boxplot(fill="orange") + 
  labs(title="Boxplot - Daily Maximum Average - DIA",x="Year", y = "Temp MAX (FH)")
p

# Temp Min
p <- ggplot(dailyData, aes(x=year, y=TMIN)) + 
  geom_boxplot(fill="blue") + 
  labs(title="Boxplot - Daily Minimum - DIA",x="Year", y = "Temp MIN (FH)")
p

# Wind
p <- ggplot(dailyData, aes(x=year, y=AWND)) + 
  geom_boxplot(fill="red") + 
  labs(title="Boxplot - Daily Wind Speed Average - DIA",x="Year", y = "Average Wind Speed (Miles)")
p

# Snow
p <- ggplot(dailyData, aes(x=year, y=SNOW)) + 
  geom_boxplot() + 
  labs(title="Boxplot - Daily Snow Fall - DIA",x="Year", y = "Snow Fall (Inches)")
p

# Rain
p <- ggplot(dailyData, aes(x=year, y=PRCP)) + 
  geom_boxplot() + 
  labs(title="Boxplot - Daily Precipitation Average - DIA",x="Year", y = "Precipitation Average (Inches)")
p

# Humidity
p <- ggplot(dailyData, aes(x=year, y=HUM)) + 
  geom_boxplot(fill="blue") + 
  labs(title="Boxplot - Daily Humidity - DIA",x="Year", y = "Humidity (%)")
p

# Dew Point
p <- ggplot(dailyData, aes(x=year, y=DEWP)) + 
  geom_boxplot(fill="yellow") + 
  labs(title="Boxplot - Daily Dew Point - DIA",x="Year", y = "Dew Point (FH)")
p


# Create a scatter plot: showing also density and correlation: 
# We have a gaussian with a slightly bimodal (double-pleaked) ditribution for TMAX, TMIN and TAVG (lower in TAVG) and DEWP
# These peaks are mostly related to the number of observations found at the two local maxima (lower and higher FH tems)
# They have two local maxima and the valley between represents the local minimum.
# We also have gaussian (normal) distribution with Humidity. 
# The Temps are skewed to the left, and Hum and Dew Point are skewed to the right. 
# Wind Prec and Snow have exponential distribution as well. 

# http://ggobi.github.io/ggally/#references_1
scatterPlot <- ggpairs(
  dailyData[,2:9],
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box"),
  lower = list(continuous = wrap("points", alpha = 0.3, col = "blue", size=0.1), 
               combo = wrap("dot", alpha = 0.4, col = "blue", size=0.2) ),
  title = "DIA Daily Weather Summary 2010 - 2018"
)
scatterPlot

# Correlation: 
# How attributes relate to each other 
# The closest to 1 are TMAX, TMIN and TAVG - We may consider taking out TMIN, TMAX, but let's leave these for a while
# DEWP and HUM have correlation with the temps. 
correlations <- cor(dailyData[,2:9])
print(correlations)

# Values above/below zero show more positive or negative correlation
# Values above +/- 0.75 show high correlation - values of +/1 show full positive/negative correlation. 
# Correlation plot: 
library(corrplot)
corrplot(correlations, method="circle")

# Looking at data patterns throughout the years for some variables: 
scatter <- ggplot(data=dailyData, aes(x = year, y = TMAX))
scatter +  geom_point(aes(color=TMAX)) + xlab("Year") +  ylab("FH") + ggtitle("Yearly Patterns: DIA: Maximum Temperature")

# Looking at data patterns throughout the years: 
scatter <- ggplot(data=dailyData, aes(x = year, y = HUM))
scatter +  geom_point(aes(color=HUM)) + xlab("Year") +  ylab("%") + ggtitle("Yearly Patterns: DIA: HUMIDITY")

# Looking at data patterns throughout the years: 
scatter <- ggplot(data=dailyData, aes(x = year, y = DEWP))
scatter +  geom_point(aes(color=DEWP)) + xlab("Year") +  ylab("FH") + ggtitle("Yearly Patterns: DIA: DEW POINT")

# Looking at data patterns throughout the years: 
scatter <- ggplot(data=dailyData, aes(x = year, y = SNOW))
scatter +  geom_point(aes(color=SNOW))  + xlab("Year") +  ylab("Inches") + ggtitle("Yearly Patterns: DIA: SNOW")

# Looking at data patterns throughout the years: 
scatter <- ggplot(data=dailyData, aes(x = year, y = PRCP))
scatter +  geom_point(aes(color=PRCP)) + xlab("Year") +  ylab("Inches") + ggtitle("Yearly Patterns: DIA: PRECIPITATION")

# Looking at data patterns throughout the years: 
scatter <- ggplot(data=dailyData, aes(x = year, y = AWND))
scatter +  geom_point(aes(color=AWND)) + xlab("Year") +  ylab("Miles") + ggtitle("Yearly Patterns: DIA: WIND SPEED")

# Looking at the values together, Average temperature and Humidity: There two variables did converge from the begining 
# and are growing appart as time goes by - Humidity seems steady but avg temp is getting higher with time
library(reshape2)
dailySmall <- dailyData[, c('year', 'TAVG', 'HUM')]
dailySmall <- melt(dailySmall, id="year")
# Plot with geom_smooth(method="lm")  - Linear model fit
ggplot(dailySmall, aes(x=year, y=value, colour=variable, group=variable)) +  geom_point(aes(y=value, colour=variable)) + geom_smooth(method="lm") + ylab("Humidity and Temperature Average") + ggtitle("Humidity and Temperature Average - Linear Model ")

# Looking at the values together, Average temperature and DewPoint: Do not converge but both increasing over time
dailySmall <- dailyData[, c('year', 'TAVG', 'DEWP')]
dailySmall <- melt(dailySmall, id="year")
# Plot with geom_smooth(method="lm")  - Linear model fit
ggplot(dailySmall, aes(x=year, y=value, colour=variable, group=variable)) +  geom_point(aes(y=value, colour=variable)) + geom_smooth(method="lm") + ylab("Humidity and Minimum Temperature") + ggtitle("Humidity and Minimum Temperature - Linear Model ")

# Looking at the values together, Average humidity and DewPoint: Do not converge, and follow same pattern over time. 
dailySmall <- dailyData[, c('year', 'HUM', 'DEWP')]
dailySmall <- melt(dailySmall, id="year")

# Plot with geom_smooth(method="loess")  - Smooth Local Regression
ggplot(dailySmall, aes(x=year, y=value, colour=variable, group=variable)) +  geom_point(aes(y=value, colour=variable)) + geom_smooth(method="loess") + ylab("Humidity and Minimum Temperature") + ggtitle("Humidity and Minimum Temperature - Smooth Local Regression")

# Plot with geom_smooth(method="lm")  - Linear model fit
ggplot(dailySmall, aes(x=year, y=value, colour=variable, group=variable)) +  geom_point(aes(y=value, colour=variable)) + geom_smooth(method="lm") + ylab("Humidity and Minimum Temperature") + ggtitle("Humidity and Minimum Temperature - Linear Model ")


