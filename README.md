# Course_5_Assignment

---
title: "Assignment"
author: "Sagar Patel"
date: "26 June 2018"
output: html_document
---

** Assignment Introduction**

This is an R Markdown Assignment in HTML. This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The data for this assignment can be downloaded from this link: <https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip>.

**1. Code for reading in the dataset and processing the data**
```{r library data, message=FALSE}
# Loading Libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Loading Data
amd <- read.csv('../../R Data/C5W2A1/rep data activity/activity.csv', header = TRUE)

# Converting date and creating data frame
amd$date <- as.Date(amd$date, '%Y-%m-%d')
complete_amd <- data.frame(steps = numeric(), date = character(), interval = numeric())
options(scipen=999)
```
  
**2. Histogram of the total number of steps taken each day**
```{r histo steps}

# Group by total steps taken in a day
date_amd <- amd %>%
                drop_na() %>%
                group_by(date) %>%
                summarise(total_steps = sum(steps))

# Plot a histogram by date
with(date_amd, plot(date, total_steps, type = 'h', xlab = 'Date', ylab = 'Total Steps per day', main = 'Histogram of total steps for each day', lwd = 7))

```
  
**3. Mean and median number of steps taken each day**
```{r mean and median}
# Mean and median of steps
mean_steps <- round(mean(date_amd$total_steps), 2)
median_steps <- median(date_amd$total_steps)

```


Mean for number of steps taken each day is `r mean_steps` and Median is `r median_steps`  
  
**4. Time series plot of the average number of steps taken**
```{r average steps}

# Group by average steps taken in a five minutes interval
int_amd <- amd %>%
                drop_na() %>%
                group_by(interval) %>%
                summarise(avg_steps = round(mean(steps), 2))

# Plot a graph by five minutes interval
with(int_amd, plot(interval, avg_steps, type = 'l', xlab = 'Five Minutes Interval', ylab = 'Mean Steps by interval', main = 'Average steps taken every five minutes'))

```

**5. The 5-minute interval that, on average, contains the maximum number of steps**  

Five minute interval where average steps are highest is at `r int_amd[,1][which.max(int_amd$avg_steps),]`.  
  
**6. Code to describe and show a strategy for imputing missing data**   

Total number of rows with missing values are `r sum(is.na(amd))`.  
  
  
```{r missing value}
# Extract first and last date of data
first_date <- head(amd$date, n = 1)
last_date <- tail(amd$date, n = 1)

# Loop through data by date in increment order
while(first_date <= last_date) {
    
    # Select data for that paticular date
    select_amd <- amd[amd$date == first_date,]
    
    # Condition to check if data have NA values for that particular date
    if(sum(is.na(select_amd$steps)) > 0){
        
            # Replace NA values for that date with Average steps for each interval
            select_amd$steps <- int_amd$avg_steps
    }
    
    # Using new data frame and append data for each date
    complete_amd <- rbind(complete_amd, select_amd)
    
    # Increase date by one
    first_date <- first_date + 1
}
```

**7. Histogram of the total number of steps taken each day after missing values are imputed**
```{r histo new steps}

# NEW complete data group, by total steps taken in a day
new_date_amd <- complete_amd %>%
                    group_by(date) %>%
                    summarise(new_total_steps = round(sum(steps), 2))

# NEW Histogram after replacing NA values
with(new_date_amd, plot(date, new_total_steps, type = 'h', xlab = 'Date', ylab = 'Total Steps per day', main = 'New Histogram of total steps for each day', lwd = 7))

```         

```{r new mean and new median}
# NEW mean and median of complete data
new_mean_steps <- round(mean(new_date_amd$new_total_steps), 2)
new_median_steps <- median(new_date_amd$new_total_steps)

```

Mean with NA values : `r mean_steps`   
Mean replacing NA values : `r new_mean_steps`    
Median with NA values : `r median_steps`   
Median replacing NA values : `r new_median_steps`  

**8. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**
```{r weekdays and weekend}

# Add a new column day of the week to complete data
complete_amd$dayofweek <- weekdays(complete_amd$date)

# Replacing day of the week with Weekday or Weekend
complete_amd[4][complete_amd$dayofweek == 'Monday',]    <- 'Weekday'
complete_amd[4][complete_amd$dayofweek == 'Tuesday',]   <- 'Weekday'
complete_amd[4][complete_amd$dayofweek == 'Wednesday',] <- 'Weekday'
complete_amd[4][complete_amd$dayofweek == 'Thursday',]  <- 'Weekday'
complete_amd[4][complete_amd$dayofweek == 'Friday',]    <- 'Weekday'
complete_amd[4][complete_amd$dayofweek == 'Saturday',]  <- 'Weekend'
complete_amd[4][complete_amd$dayofweek == 'Sunday',]    <- 'Weekend'

# Group by average steps taken every five minute interval and day type 
dayofweek_amd <- complete_amd %>%
                    group_by(dayofweek, interval) %>%
                    summarise(day_avg_steps = mean(steps))

# Graph of average steps taken every five minute interval seperated by weekdays and weekend
gp <- ggplot(dayofweek_amd, aes(x = interval, y = day_avg_steps)) +
        geom_line() +
        facet_grid(dayofweek~.) +
        labs(title = 'Average steps taken every five minutes interval by weekdays and weekend') +
        ylab('Average number of steps') + 
        xlab("Five minutes interval")

print(gp)
```
