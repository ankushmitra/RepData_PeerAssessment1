# Open activity.zip 
# CONVERT DATE AS DATE OBJECT
activity <- read.csv(unz("activity.zip","activity.csv"),
                     colClasses =c("numeric","Date","numeric"))

# Make histogram of total steps per day

# auxiallary function for calculating total steps per day
steps_per_day <- function(date, df)
{
        cat(date,"\n")
        total_steps <- sum(df[df$date == date,]$steps,na.rm=TRUE)
        cat(total_steps,"\n")
        if (is.na(total_steps)) {
                cat("total_steps is NA \n")
                return
        }
        cat("Next line\n")
        date_seq <- seq(as.Date(date), by=0, len=total_steps)
        date_seq
}
 
date_factor <- factor(activity$date)
steps_freq <- sapply(levels(date_factor), steps_per_day, df=activity)


# Convert NA ... to do

