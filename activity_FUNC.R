##################
#Input is a data frame. Returns a vector of the minimum of the differences between 
# the dates or "max_week" 
##################
get_week_gaps <- function(log, max_week=7){
  nn <- dim(log)[1]
  diff <- as.numeric(c(log$Date[2 : nn], today(tzone="UTC")) - log$Date)
  return(pmin(diff, max_week))
}

################
#S plits all week summary data in a activity log over the 
# time following the summary until the next summary or "max_week", whichever comes first.
# The function splits numeric values in columns 5,6,7,9, replicates columns 1,3,4 and 
# increments the date in column 2.
# New week data is recombined with non-week data and sorted by date.
################
split_week_data <- function(log, max_week=7){
  log_non_week <- log %>% filter(Week_total==0)
  log <- log %>% filter(Week_total==1)
  gaps <- get_week_gaps(log, max_week)
  nn <- dim(log)[1]
  locs <- c(1, cumsum(gaps) + 1)
  week_df <- log[1 : sum(gaps), c(1:7, 9)] #create empty df of correct dimensions
  for (i in 1 : nn){
    for (j in 1 : gaps[i]){
      week_df[locs[i] + j - 1, ] <- 
        c(1, log[i, 2] + j - 1, log[i, 3:4], log[i, c(5:7, 9)] / gaps[i])
    }
  }
  return(bind_rows(log_non_week, week_df) %>% arrange(Date) %>% rename(Week_data=Week_total))
}

###################
#Replaces all NAs with zeros.
# Not used now.
###################
na_to_zero <- function (x) {
  x[is.na(x)] <- 0
  return(x)
}

###################
#Moving average for a vector.
#http://www.markhneedham.com/blog/2014/09/13/r-calculating-rolling-or-moving-averages/
#filter() returns a time series object but this just returns the vector part.
###################
move_ave <- function(x,n=7){
  return(as.numeric(stats::filter(x, rep(1/n,n), sides=1)))
}

