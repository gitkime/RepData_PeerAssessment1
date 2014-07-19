    
#########################
# 
# COUSERA 
# PeerAssement 1
# 
# 19 July 2014
#
#########################
# read rawdata
    rdata <- read.csv("./rawdata/activity.csv",sep=",")
#names 
    names(rdata)
# show header ( 3 rows)
    head(rdata,n=3)
# summary rdata
    summary(rdata)
# convert date in R convertible Date-format
    tidy_data <- as.data.frame(rdata)
    tidy_data$date <- as.Date(as.character(tidy_data$date))
#
## as date and time
#
    tidy_date1 <- as.character(paste(tidy_data$date,sprintf("%04d",tidy_data$interval)))
    xdate <- strftime(tidy_date1,format="%y-%m-%d %H:%M")
    xdate <- as.POSIXct(xdate)

#plot histogramm
    sum_pday <- aggregate(cbind(steps) ~ date, tidy_data, sum,na.rm=TRUE)
    head(sum_pday,n=3)
    
#install packages  ggplot2
#load lib ggplot2
#install.packages(ggplot2)
    library(ggplot2)
# Output png 
    png(file="./figures/hist_01.png")
    a <- ggplot(sum_pday, aes(x = date, y = steps))
    b <- a + geom_bar(stat = "identity", position = "stack",colour="lightgreen")
    c <- b + ggtitle("Histogram of total Number of steps per day")
    print(c)
# close plot
    dev.off()
    print(c)
# mean per day
    mean_pday <- aggregate(cbind(steps) ~ date, tidy_data, mean,na.rm=F)
    head(mean_pday,n=3)
# median**
    median_alldays <- median(mean_pday$steps)
    median_alldays
    
# mean per interval
    mean_pint <- aggregate(cbind(steps) ~ interval, tidy_data, mean,na.rm=F)
    head(mean_pint,n=3)
## plot mean per interval
# Output png 
    png(file="./figures/mean_pint_02.png")
    plot(mean_pint,type="l",main="Mean per 5 minutes interval")
# close plot
    dev.off()
    plot(mean_pint,type="l",main="Mean per 5 minutes interval")
#The maximun of steps in 5 minute inteval
# max step in all steps
    xmax <-grep(max(tidy_data$steps, na.rm = T), tidy_data$steps)
    tidy_data[xmax:xmax,]
#
# calculate max NA
# max na in dataset
    sum(is.na(tidy_data$steps))
#
## new dataset filled NA with mean value
#
    
    tidy_data_nNa <- tidy_data
    tidy_data_nNa$steps[is.na(tidy_data_nNa$steps)] <- mean(tidy_data_nNa$steps, na.rm = TRUE)
# str 
    str(tidy_data_nNa)
# summary ne data.frame tidy_data_nNa
    summary(tidy_data_nNa)
# show head of tidy_data_nNa
    head(tidy_data_nNa)
#
## calculate sum per day in dataset with no NA
    sum_pday_nNa <- aggregate(cbind(tidy_data_nNa$steps)~ tidy_data_nNa$date,tidy_data_nNa, sum,na.rm=TRUE)
    head(sum_pday_nNa,n=3)
    str(sum_pday_nNa)
    summary(sum_pday_nNa)
#**histigram not Na in dataset**
#plot histogramm
#load lib ggplot2
    library(ggplot2)
# Output png 
    png(file="./figures/hist_02.png")
    a <- ggplot(sum_pday_nNa, aes(x = tidy_data_nNa$interval, y = tidy_data_nNa$steps))
    b <- a + geom_bar(stat = "identity", position = "stack",colour="lightgreen")
    c <- b + ggtitle("Histogram of total Number of steps per day in not Na dataset")
    d <-  c + xlab("Interval")
    e <-  d + ylab("Steps")
    print(e) 
    
# close plot
    dev.off()
    print(e)
    
#
##**mean of all step with no NA**
#
    
    mean_pday_nNa <- aggregate(cbind(steps) ~ date, tidy_data_nNa, mean,na.rm=F)
    head(mean_pday_nNa)
    
    
## **median of all step with no NA**
    
    median_alldays_nNa <- median(mean_pday_nNa$steps)
    median_alldays_nNa
    
#  weekday and weekend
# new factor variable in the dataset
# with two levels -- "weekday" and "weekend"
    week <- as.factor(weekdays(as.Date(as.character(xdate))) %in% c('Sunday','Saturday'))
    table(week)
    levels ( week ) <- c ( "weekday" , "weekend" )
    mean_pweek <- aggregate(cbind(steps) ~ week, tidy_data_nNa, mean,na.rm=F)
    mean_pweek
#
## xyplot for weeday and weekend
#
##** sum per weekday and weekend **
    
    sum_pweek <- aggregate(cbind(steps) ~ week, tidy_data_nNa, sum,na.rm=F)
    sum_pweek
    
##** xyplot type line steps per weekdays and weekend **
# write plot to dir figures
    png(file="./figures/xyplot_01.png")
##library(lattice)
    library(lattice)
#plot weekday and weekend
    xyplot(tidy_data_nNa$steps ~ tidy_data$date | week,type="l",xlab="Dates",ylab="Steps",main="Steps on weekday and weekend",layout=c(2,1))
# close plot
    dev.off()
    xyplot(tidy_data_nNa$steps ~ tidy_data$date | week,type="l",xlab="Dates",ylab="Steps",main="Steps on weekday and weekend",layout=c(2,1))
