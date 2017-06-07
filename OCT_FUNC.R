##############
#Convert a date to a period
##############
date_to_time <- function(date){
  if(is.POSIXct(date)){
    start <- floor_date(date, unit="days")
    time <- as.period(interval(start, date))
  }else{
    time <- date
  }
  return(time)
}

##############
#Converts a decimal number to a period in HMS
##############
decimal_to_period <- function(x){
  hours <- floor(x)
  minutes <- floor((60 * x) %% 60)
  seconds <- floor((3600 * x) %% 60)
  return(period(hours=hours, minutes=minutes, seconds=seconds))
}

##############
#Converts a decimal number to a period in HM
##############
decimal_to_period <- function(x){
  hours <- floor(x)
  minutes <- round((60 * x) %% 60, 0)
  #return(period(c(hours, minutes), c("hour", "minute")))
  return(period(hours=hours, minutes=minutes))
}

##############
#Converts a decimal number to a string in H:M format
##############
decimal_to_time <- function(x){
  hours <- floor(x)
  minutes <- round((60 * x) %% 60, 0)
  return(paste0(sprintf("%01d", hours), ":", sprintf("%02d", minutes)))
}