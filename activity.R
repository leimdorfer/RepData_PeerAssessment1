

Q1 <- function(){
      
      activity <- read.csv("./activity.csv") 
      
      library(dplyr)
      
      by_date <- group_by(activity, date)
      
      per_day <- summarise(by_date,
            total_steps = sum(steps, na.rm = TRUE))
      
      hist(per_day$total_steps)
      
      steps_mean <- mean(per_day$total_steps)
      steps_median <- median(per_day$total_steps)
      
      print(steps_mean)
      print(steps_median)
            
      
}

Q2 <- function(){
      
      activity <- read.csv("./activity.csv") 
      
      library(dplyr)
      
      by_interval <- group_by(activity, interval)
      
      steps_per_interval <- summarise(by_interval,
            mean_steps = mean(steps, na.rm = TRUE))
      
      View(steps_per_interval)
      
      plot(steps_per_interval$interval, steps_per_interval$mean_steps, 
           type="l", 
           xlab= "Interval",
           ylab= "Average number of steps taken")
      
      most_steps <- max(steps_per_interval$mean_steps)
      
      interval_with_most_ateps <- subset(steps_per_interval, mean_steps == most_steps)
  
      print(most_steps)
      print(interval_with_most_ateps)
        
}

replaceNA <- function(x){
      

      
      
}

f <- function(x, output) {
      wellName <- x[1]
      plateName <- x[2]
      wellID <- 1
      print(paste(wellID, x[3], x[4], sep=","))
      cat(paste(wellID, x[3], x[4], sep=","), file= output, append = T, fill = T)
}

returnIntervalMean<-function(activity, i){
      
      library(dplyr)
      
      by_interval <- group_by(activity, interval)
      
      steps_per_interval <- summarise(by_interval,
            mean_steps = mean(steps, na.rm = TRUE))
      
      this_interval_mean <- subset(steps_per_interval, interval == i)
      
      return(this_interval_mean[2])
      
}

loop <- function(){
      
      activity <- read.csv("./activity.csv") 
      
      print(sum(is.na(activity)))
      
      print(nrow(activity))
      activity_no_na <- activity

      print(head(activity_no_na, n=10))
      
      
      #for (n in 1:nrow(activity)){
            
      for (n in 1:10){
            
            if(is.na(activity[n,1])){
                  
                  meanForThisInterval <- returnIntervalMean(activity,activity[n,3])
                  
                  activity_no_na[n,1] <- as.numeric(meanForThisInterval)
            }
            
            
      }
      
      #print(head(activity_no_na, n=10))
      
      Q4(activity_no_na)
      
}


Q4 <- function(){
      
      d <- read.csv("./activity.csv") 
      
      summary(d)
      
      weekend<- vector(length=0) #mode="boolean", 
      
      library(lubridate)
      
      print(head(d, n=10))
      
      
      for (n in 1:nrow(d)){
      
      #for (n in 1:10){
            
            thisDay <- as.POSIXlt(d[n,2])$wday -1
            
            if(thisDay<5){ # 0-4 = Mon-Fri. 5=Sat, 6=Sun
                  
                  weekend[n]=FALSE
                  
            }else{
                  
                  weekend[n]=TRUE
                  
            }      
      }
      
      d <- cbind(d, weekend)
      
      print(summary(d))
            
     
}
