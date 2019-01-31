
####################################################################################
# Housekeeping
####################################################################################
# Clear all variables from R
rm(list = ls())
# Set working directory
getwd()
#setwd()
dir()

####################################################################################
## Install and load packages
####################################################################################
install.packages("tidyr")
install.packages("dplyr")
install.packages("RMySQL")
install.packages("ggplot")
install.packages("lubridate")
install.packages("plotly")
require(lubridate) #
require(dplyr)  
require(tidyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(lubridate)
library(dplyr)
library(RMySQL)

####################################################################################
# Load dataset 
####################################################################################
newDFF <- read.csv("/Users/Vijendhar/Desktop/Education/Data Analytics/Course3/household_power_consumption.txt", sep = ";", stringsAsFactors = FALSE, header = T)
class(newDFF)
str(newDFF)

####################################################################################
# Evaluate data
####################################################################################
#str(newDF)  
summary(newDFF)
head(newDFF)
tail(newDFF)
names(newDFF)
#attributes(newDF)

####################################################################################
# Pre-process DS 
####################################################################################

#------Create a DateTime col by using unite() in tidyr-------------#

# as.Date() is an R method [R - Date Class - as.Date]
# If use as.Date, will lose any time stamp; (time less than a day)
# as.POSIXct will preserve time stamp; [R - Date-Time - POSIX classes]
# as.POSIXct stores both a date and time with an associated time zone. 
# Default is tz of your computer.
# Be sure to keep the date format the same (e.g.,"%d/%m/%Y %H:%M:%S")

# combine Date and Time using unite in tidyr
ForecastDF <- newDFF%>% unite(DateTime, Date, Time, sep = " ", remove = FALSE)
# Could remove Date and Time by remove = TRUE and could add back using spread()
# convert DateTime to POSIXct
ForecastDF$DateTime <- as.POSIXct(ForecastDF$DateTime,
                               format = "%d/%m/%Y %H:%M:%S",
                               tz = "America/New_York")
class(ForecastDF$DateTime) #[1] "POSIXct" "POSIXt" 
tz(ForecastDF$DateTime) # "America/New_York"

# convert Date to as.Date
ForecastDF$Date <- as.Date(ForecastDF$Date, "%d/%m/%Y")
str(ForecastDF)


####################################################################################
##------- Change data types---------##
# Note: Understand the difference between as.numeric(as.character()) and as.numeric()
####################################################################################
ForecastDF$Global_active_power <- as.numeric(as.character(newDFF$Global_active_power))
ForecastDF$Global_reactive_power <- as.numeric(as.character(newDFF$Global_reactive_power))
ForecastDF$Voltage <- as.numeric(as.character(newDFF$Voltage))
ForecastDF$Global_intensity <- as.numeric(as.character(newDFF$Global_intensity))
ForecastDF$Sub_metering_1 <- as.numeric(as.character(newDFF$Sub_metering_1))
ForecastDF$Sub_metering_2 <- as.numeric(as.character(newDFF$Sub_metering_2))
str(ForecastDF)

####################################################################################
# Preprocess # handle missing values## ------ Evaluate NA values ----------##
####################################################################################
any(is.na(ForecastDF)) 
# Count the number of values = NA
sum(is.na(ForecastDF$Sub_metering_1)) # Review any metadata with dataset
## -------- Save pre-processed dataset --------##
# after ALL preprocessing, save a new version of the dataset
write.csv(ForecastDF, file = "/Users/vijendhar/Desktop/Education/Data Analytics/Course3/ForecastDFforecast.csv")
#read.csv("/Users/vijendhar/Desktop/Education/Data Analytics/Course3/ForecastDF.csv")

####################################################################################
###- 1. Create a subset that shows the total kWh per year for 
#submeter 3 for the years 2007-09. Forecast for 2010 and 2011
####################################################################################

totalkwhsm3 <- ForecastDF %>%
  #mutate(Year = year(DateTime), Month = month(DateTime)) %>%  
  mutate(Year = year(DateTime)) %>%  
  filter(Date >= "2007-01-01" & Date <= "2009-12-31") %>%
  group_by(Year) %>%  # Group data by Year
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3), # Total kWh per hour
            DateTime = first(DateTime))%>%   # To verify date of first instance
filter(!is.na(Year)) 
totalkwhsm3
# A tibble: 3 x 3
# Year   SM3 DateTime           
# <dbl> <dbl> <dttm>             
#   1  2007 3023. 2007-01-01 00:00:00
# 2  2008 3179. 2008-01-01 00:00:00
# 3  2009 3557. 2009-01-01 00:00:00
summary(totalkwhsm3)
## Create TS object with SM3
tsSM3_070809yearly<- ts(totalkwhsm3$SM3, frequency=1, start=c(2007))
tsSM3_070809yearly
# Time Series:
#   Start = 2007 
# End = 2009 
# Frequency = 1 
# [1] 3022.840 3179.059 3556.816
## Apply time series linear regression to the sub-meter 3 ts object and use summary to obtain R2 and RMSE from the model you built
install.packages("forecast")
library(forecast)
fitSM3 <- tslm(tsSM3_070809yearly ~ trend ) 
fitSM3
summary(fitSM3)
# Call:
#   tslm(formula = tsSM3_070809yearly ~ trend)
# 
# Residuals:
#   Time Series:
#   Start = 2007 
# End = 2009 
# Frequency = 1 
# 1      2      3 
# 36.92 -73.85  36.92 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)  2718.93     138.15  19.681   0.0323 *
#   trend         266.99      63.95   4.175   0.1497  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 90.44 on 1 degrees of freedom
# Multiple R-squared:  0.9457,	Adjusted R-squared:  0.8915 
# F-statistic: 17.43 on 1 and 1 DF,  p-value: 0.1497

####################################################################################
#####Forecasting
####################################################################################
forecastfitSM3 <- forecast(fitSM3, h=2)
## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3cannual <- forecast(fitSM3, h=2, level=c(80,90))
forecastfitSM3cannual
# Point Forecast    Lo 80    Hi 80    Lo 90    Hi 90
# 2010       3786.881 3278.680 4295.082 2744.325 4829.437
# 2011       4053.869 3381.582 4726.156 2674.697 5433.041

####################################################################################
## Plots
####################################################################################
install.packages("ggplot2")
install.packages("ggfortify")
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_070809yearly)
autoplot(tsSM3_070809yearly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
plot.ts(tsSM3_070809yearly)
## Plot sub-meter 3 forecast, limit y and add labels
plot(forecastfitSM3cannual, ylim = c(0, 10000), ylab= "Watt-Hours", xlab="Time")
plot(forecastfitSM3cannual)


####################################################################################
# - 2. Create a subset that shows the total kWh per month for submeter 3 for the months Jan-07 
# through Oct-10. Forecast for Nov-10 through Dec-11.Note: Be sure to make the adjustment 
# depending on if the ts is seasonal. Also, be sure to record the summary metrics and know how to
# interpret the output; specifically, R-squared, Adjusted R-squared, F-stat, and p-value. Also, 
#understand how the p-value relates to the null hypothesis regarding the statistics (i.e., slope coefficients). 
#For an additional resource for learning about regression output, I suggest Brandon Foltz's tutorials on YouTube for statistics/regression. 
####################################################################################
totalkwhsm3monthly <- ForecastDF %>%
  mutate(Year = year(DateTime), Month = month(DateTime)) %>%  
  filter(Date >= "2007-01-01" & Date <= "2010-10-31") %>%
  group_by(Year,Month) %>%  # Group data by Year
  summarize(SM3 = round(sum(Sub_metering_3 / 1000, na.rm = TRUE),3), # Total kWh per hour
            DateTime = first(DateTime))%>%   # To verify date of first instance
  filter(!is.na(Month)) 
totalkwhsm3monthly
# A tibble: 46 x 4
# Groups:   Year [4]
# Year Month   SM3 DateTime           
# <dbl> <dbl> <dbl> <dttm>             
#   1  2007     1  330. 2007-01-01 00:00:00
# 2  2007     2  270. 2007-02-01 00:00:00
# 3  2007     3  290. 2007-03-01 00:00:00
# 4  2007     4  190. 2007-04-01 00:00:00
# 5  2007     5  229. 2007-05-01 00:00:00
# 6  2007     6  189. 2007-06-01 00:00:00
# 7  2007     7  155. 2007-07-01 00:00:00
# 8  2007     8  225. 2007-08-01 00:00:00
# 9  2007     9  226. 2007-09-01 00:00:00
# 10  2007    10  256. 2007-10-01 00:00:00
# # ... with 36 more rows
summary(totalkwhsm3monthly)
## Create TS object with SM3
tsSM3_07080910monthly<- ts(totalkwhsm3monthly$SM3, frequency=12, start=c(2007,1))
tsSM3_07080910monthly
# Jan     Feb     Mar     Apr     May     Jun     Jul     Aug     Sep     Oct     Nov     Dec
# 2007 329.578 270.274 290.361 189.503 229.448 188.851 154.815 225.442 226.375 256.080 299.690 362.423
# 2008 312.175 255.918 279.542 295.678 290.620 290.103 227.228  79.665 284.282 275.887 280.615 307.346
# 2009 329.606 296.166 328.697 307.840 311.048 259.969 187.936 192.064 296.547 327.505 335.529 383.909
# 2010 395.913 411.714 324.312 336.091 364.625 306.934 192.912 160.189 257.861 315.669   
####################################################################################
#####Forecasting
####################################################################################
library(forecast)
fitSM3monthly <- tslm(tsSM3_07080910monthly ~ trend + season) 
fitSM3monthly
# Call:
#   tslm(formula = tsSM3_07080910monthly ~ trend + season)
# 
# Coefficients:
#   (Intercept)        trend      season2      season3      season4      season5      season6  
# 305.4980       1.9116     -35.2116     -39.9132     -65.2747     -50.5291     -89.9117  
# season7      season8      season9     season10     season11     season12  
# -162.5647    -190.8591     -90.8444     -65.2370     -44.1863      -0.1499   
summary(fitSM3monthly)
# Call:
#   tslm(formula = tsSM3_07080910monthly ~ trend + season)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -73.206 -22.304  -0.428  19.314  95.510 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  305.4980    20.6848  14.769 4.22e-16 ***
#   trend          1.9116     0.4308   4.437 9.59e-05 ***
#   season2      -35.2116    26.8680  -1.311  0.19906    
# season3      -39.9132    26.8783  -1.485  0.14705    
# season4      -65.2747    26.8956  -2.427  0.02085 *  
#   season5      -50.5291    26.9197  -1.877  0.06938 .  
# season6      -89.9117    26.9507  -3.336  0.00211 ** 
#   season7     -162.5647    26.9886  -6.023 9.00e-07 ***
#   season8     -190.8591    27.0333  -7.060 4.42e-08 ***
#   season9      -90.8444    27.0847  -3.354  0.00201 ** 
#   season10     -65.2370    27.1429  -2.403  0.02202 *  
#   season11     -44.1863    29.0681  -1.520  0.13801    
# season12      -0.1499    29.0968  -0.005  0.99592    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 37.99 on 33 degrees of freedom
# Multiple R-squared:  0.7666,	Adjusted R-squared:  0.6817 
# F-statistic:  9.03 on 12 and 33 DF,  p-value: 2.522e-07

## Create the forecast for sub-meter 3. Forecast ahead xx time periods 
forecastfitSM3 <- forecast(fitSM3monthly, h=14)
forecastfitSM3
# #        Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# Nov 2010       351.1559 292.2140 410.0978 259.4569 442.8550
# Dec 2010       397.1039 338.1620 456.0458 305.4049 488.8030
# Jan 2011       399.1654 341.1026 457.2283 308.8340 489.4969
# Feb 2011       365.8654 307.8026 423.9283 275.5340 456.1969
# Mar 2011       363.0754 305.0126 421.1383 272.7440 453.4069
# Apr 2011       339.6254 281.5626 397.6883 249.2940 429.9569
# May 2011       356.2827 298.2198 414.3455 265.9513 446.6141
# Jun 2011       318.8117 260.7488 376.8745 228.4803 409.1431
# Jul 2011       248.0702 190.0073 306.1330 157.7388 338.4016
# Aug 2011       221.6874 163.6246 279.7503 131.3560 312.0189
# Sep 2011       323.6137 265.5508 381.6765 233.2823 413.9451
# Oct 2011       351.1327 293.0698 409.1955 260.8013 441.4641
# Nov 2011       374.0949 313.2450 434.9448 279.4275 468.7623
# Dec 2011       420.0429 359.1930 480.8928 325.3755 514.7103

## Create sub-meter 3 forecast with confidence levels 80 and 90
forecastfitSM3c <- forecast(fitSM3monthly, h=14, level=c(80,90))
forecastfitSM3c
# Point Forecast    Lo 80    Hi 80    Lo 90    Hi 90
# Nov 2010       351.1559 292.2140 410.0978 274.8784 427.4335
# Dec 2010       397.1039 338.1620 456.0458 320.8264 473.3815
# Jan 2011       399.1654 341.1026 457.2283 324.0255 474.3054
# Feb 2011       365.8654 307.8026 423.9283 290.7255 441.0054
# Mar 2011       363.0754 305.0126 421.1383 287.9355 438.2154
# Apr 2011       339.6254 281.5626 397.6883 264.4855 414.7654
# May 2011       356.2827 298.2198 414.3455 281.1428 431.4226
# Jun 2011       318.8117 260.7488 376.8745 243.6718 393.9516
# Jul 2011       248.0702 190.0073 306.1330 172.9303 323.2101
# Aug 2011       221.6874 163.6246 279.7503 146.5475 296.8274
# Sep 2011       323.6137 265.5508 381.6765 248.4738 398.7536
# Oct 2011       351.1327 293.0698 409.1955 275.9928 426.2726
# Nov 2011       374.0949 313.2450 434.9448 295.3482 452.8416
# Dec 2011       420.0429 359.1930 480.8928 341.2962 498.7896
####################################################################################
## Plots
####################################################################################
## Plot sub-meter 3 with autoplot (you may need to install these packages)
library(ggplot2)
library(ggfortify)
autoplot(tsSM3_07080910monthly)
autoplot(tsSM3_07080910monthly, ts.colour = 'red', xlab = "Time", ylab = "Watt Hours", main = "Sub-meter 3")
plot.ts(tsSM3_07080910monthly)
plot(forecastfitSM3c, ylim = c(0, 500), ylab= "Watt-Hours", xlab="Time",main = "Consumption Forecast for 2010-2011")
plot(forecastfitSM3c)


####################################################################################
## Decompose and Holtwinters Section
####################################################################################

## Decompose Sub-meter 3 into trend, seasonal and remainder
components070809SM3monthly <- decompose(tsSM3_07080910monthly)
## Plot decomposed sub-meter 3 
plot(components070809SM3monthly)
## Check summary statistics for decomposed sub-meter 3 
summary(components070809SM3monthly)
## Seasonal adjusting sub-meter 3 by subtracting the seasonal component & plot
tsSM3_07080910Adjusted <- tsSM3_07080910monthly - components070809SM3monthly$seasonal
autoplot(tsSM3_07080910Adjusted)
tsSM3_07080910Adjusted
## Test Seasonal Adjustment by running Decompose again. Note the very, very small scale for Seasonal
plot(decompose(tsSM3_07080910Adjusted))
## Holt Winters Exponential Smoothing & Plot
tsSM3_HW07080910 <- HoltWinters(tsSM3_07080910Adjusted, beta=FALSE, gamma=FALSE)
tsSM3_HW07080910
plot(tsSM3_HW07080910, ylim = c(0, 500))
## HoltWinters forecast & plot
tsSM3_HW07080910for <- forecast(tsSM3_HW07080910, h=15)
plot(tsSM3_HW07080910for, ylim = c(0, 500), ylab= "Watt-Hours", xlab="Time" ,main = "HoltWinters Forecast for Sub-meter 3")
# Forecast HoltWinters with diminished confidence levels
tsSM3_HW07080910forC <- forecast(tsSM3_HW07080910, h=15, level=c(10,75))
## Plot only the forecasted area
plot(tsSM3_HW07080910forC, ylim = c(0, 500), ylab= "Watt-Hours", xlab="Time - Sub-meter 3", start(2010))

