# Read in raw data from local computer
setwd("C:/Users/Tony Truong/Desktop/Coursera R/Reproducable Research")
ActData<-read.csv(file = "activity.csv", header = TRUE, sep = ",")

#Convert date from factor to date
ActData$date<-as.Date(ActData$date, "%Y-%m-%d")

# Create cDate and cDateInt columns for later processing
ActData$cDate<-as.POSIXct(ActData$date)
ActData$cDateInt<-(ActData$cDate+ActData$interval*60)

# Run crosstab to get total steps, with NA's omitted

DailySteps<-xtabs(steps~cDate, data = ActData, exclude = NA)


# Plot Total Daily Steps
plot(DailySteps, type ="h", main="Total Daily Steps, NAs excluded")

# Calculate Mean & Median Daily Steps
MeanDailySteps<-mean(DailySteps)
MedianDailySteps<-median(DailySteps)


# Run crosstab with aggregate to create xtab with means (of intervals over all dates) as summary


AvgDSteps<-xtabs(steps~interval, aggregate(steps~interval,ActData,mean, exclue =NA ))
plot(AvgDSteps, type = "l", ylab="Total Steps ", main="Total Steps per Interval")

AvgDSteps<-data.frame(AvgDSteps)

MaxInterval<-AvgDSteps[which.max(AvgDSteps[,2]),1]
MaxInterval

# Find the number of rows with missing values (NA's)
colSums(is.na(ActData))

# Substituting NA's with Interval Average Steps

# First make a copy of data

ActDataMod<- ActData

# Outer loop goes through all the rows in ActData looking for NA's
# Inner loop test for NA and lookup interval value in ActData 
# then the corresponding average interval steps is used to substitute for the NA 

for (i in 1:nrow(ActData)){

        if(is.na(ActData$steps[i])){
                ActDataMod$steps[i] <- subset(AvgDSteps, interval == ActData[i,3] , select = Freq))
        }
}

ActDataMod$steps<-as.integer(ActDataMod$steps)

# Recalculate daily total steps with modified data
DailyStepsMod<-xtabs(steps~cDate, data = ActDataMod, exclude = NA)
plot(DailyStepsMod, type ="h", xlab= "Date", ylab="Total Daily Steps", main= "NA's replaced with Interval Avg")

MeanDStepsMod<-mean(DailyStepsMod)
MedianDStepsMod<-median(DailyStepsMod)


# Adding column for Weekdays vs Weekend Activities
WDaysList <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
ActData$WDays<-factor((weekdays(ActData$date) %in% WDaysList), levels=c(FALSE, TRUE), labels=c('weekend','weekday'))

# Weekday vs Weekend Activities

WeekdaysActAvg<-data.frame(xtabs(steps~interval + WDays, aggregate(steps~interval + WDays, ActDataMod, mean)))

WeekdaysActAvg$interval<-as.integer(WeekdaysActAvg$interval)

g<-ggplot(WeekdaysActAvg, aes(x=interval, y=Freq))
g + facet_grid(WDays~.)+ geom_line()
