

init <- function(){
      
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
      
      #ggplot(data=steps_by_day, aes(steps_by_day$tot_steps)) + geom_histogram()
      
      #
      
      #mean_steps = mean(steps, na.rm = TRUE))
      #head(steps_by_day, n=10)
      
      
      
      
}
