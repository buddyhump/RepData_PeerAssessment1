## Loading and preprocessing the data
## First set work directory with setwd("your working directory path here")
library(dplyr)
df1 <- read.csv("activity/activity.csv")  ## ajust path as needed to "activity.csv" file location
      ## df1$date <- as.Date(df1$date)
## What is mean total number of steps taken per day?

df2 <-
  df1 %>%
  group_by(date) %>%
  filter(!steps == "NA", !steps == 0) %>%
  summarise(Total = sum(steps))

hist(df2$Total, main = "Histogram of the Total Steps w/ NA's", xlab = "Total Steps per Day")

#barplot(df2$total, main = "Total Steps", xlab = "Date (10/1/12 thru 11/30/12)", ylab = "Steps")

df3 <-
  df1 %>%
  group_by(date) %>%
  filter(!steps == "NA", !steps == 0) 
print (paste("Mean of Total Steps =",round(mean(df3$steps),1)))
print (paste("Median of Total Steps =",round(median(df3$steps),1)))

## What is the average daily activity pattern?
df4 <-
  df1 %>%
  group_by(interval) %>%
  filter(!steps == "NA") %>%
  summarise(Mean = mean(steps))

plot(df4$interval, df4$Mean, type = "l", main = "Time Series Plot of Fitness Data", xlab ="5 Minute Intervals"
     , ylab = "Mean of Steps (for 61 Days)")

Max <- df4[which.max(df4$Mean),1]

print (paste("Time Interval with max steps on average is ", Max))

## Imputing missing values
      ## 1. Number of rows with "NA"
print (paste("Total rows containing NA = ", sum(is.na(df1$steps))))  
      ## 2. & 3.  Fill in NA with mean for that day; create new dataset
df5 <- df1
for (i in 1:nrow(df5))
  {
  if (is.na(df5[i,1]))
    {
    interval <- df5[i,3]
    for (j in 1:nrow(df4))
      {
      if (interval == df4[j,1])
        {
        value <- df4[j,2]
        df5[i,1] <- value
        }
      }
    }
  }
              ## sum(is.na(df5$steps)) ## check number of NA after function
      ## 4a. Make histogram of new dataset
df6 <-
  df5 %>%
  group_by(date) %>%
  filter(!steps == "NA",!steps == 0) %>%
  summarise(Total = sum(steps))

hist(df6$Total, main = "Histogram of the Total Steps w/o NA's", xlab = "Total Steps per Day")
    ## 4b. Report mean & med1an of new dataset
df7 <-
  df5 %>%
  group_by(date) %>%
  filter(!steps == "NA", !steps == 0) 
print (paste("Mean of Total Steps =",round(mean(df7$steps),1)))
print (paste("Median of Total Steps =",round(median(df7$steps),1)))
    ## 4c. Calculate the differences between datasets with/wihout NA's
print (paste("Difference in Mean from earlier estimate =", (round((mean(df3$steps))-(mean(df7$steps))))))
print (paste("Difference in Median from earlier estimate =", (round((median(df3$steps))-(median(df7$steps))))))
    ## 4d. Calculate the differences between total daily steps with/wihout NA's
print (paste("Difference in total daily steps from earlier estimate =", (round((sum(df3$steps))-(sum(df7$steps))))))

## Are there differences in activity patterns between weekdays and weekends?
## USED dataset df7 which replaced NA's 
    ## create new factor column for "weekend" and "weekday"
df8 <- df7
df8$date <- as.Date(df8$date)

df8 <-
  df8 %>%
  mutate(Day = weekdays(date)) %>%
    mutate(Weektype = ifelse(Day %in% c("Saturday", "Sunday"), "weekend", "weekday")) 
df8$Day <- as.factor(df8$Day)        
df8$Weektype <- as.factor(df8$Weektype) 
    ## plot intervals vs average steps for weekend and weekday
df9 <-
  df8 %>%
    select(steps, interval, Weektype) %>%
      group_by(Weektype, interval) %>%
        summarise(avgsteps = mean(steps)) 
    ## use lattice to make plot
library(lattice)
xyplot(avgsteps ~ interval | Weektype, df9,
       type = "l",
       layout =c (1,2),
       ylab = "Number of steps",
       xlab = "Interval",
        )


