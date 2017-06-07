
if(file.exists("C:/Users/edwards/Dropbox/Mine/Personal/Activity Record.xlsx")){
  file_name <- "C:/Users/edwards/Dropbox/Mine/Personal/Activity Record.xlsx" #new laptop
}else{
  file_name <- "E:/Dropbox/Mine/Personal/Activity Record.xlsx" #old laptop
}

library(readxl)
library(dplyr)
library(tidyr) #for replace_na()
library(lubridate)

source("activity_record_FUNC.R")
#readxl has two functions
excel_sheets(file_name)
log_master <- read_excel(file_name, sheet="Log", skip=11)

#trim rows with no entry
log <- filter(log_master, !is.na(Type))
#Rename cols
colnames(log)
colnames(log) <- c("Week_total", "Date", "Type", "Sub-type", "Time", "Distance", "Ascent", "Notes", "Terrain", 
                   "Tempo_pace", "5k10k_pace", "Sub_5k_pace", "Hill_sprints", "Strides", "Drills", 
                   "Total_time", "Year", "Month", "Week")
log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)
#replace NAs
#apply(log, 2, FUN=function(x){sum(is.na(x))}) #returns number of NAs in each column
#log <- log %>% replace_na(replace=list(Strides=0))
#log <- log %>% mutate(colname = ifelse(is.na(colname),0,colname))
#or see mutate_each()

log <- log %>% mutate_each(funs(na_to_zero), c(1, 5:7, 9:15)) %>%
  mutate(Week_total=ifelse(Week_total=="week", 1, 0)) %>%
  mutate(Total_time=ifelse(is.na(Total_time), Time, Total_time))

#explore
#table(log$Type)
#print(log)
#glimpse(log)
#print(log, n=20)

#New dfs separated by activity type (loses day, week, month columns)
log_R <- log %>% filter(Type=="R") %>%
  select(c(1:16))
log_B <- log %>% filter(Type=="B") %>%
  select(c(1:8, 16))
log_F <- log %>% filter(Type=="F") %>%
  select(c(1:9, 16))

#Split weeks (loses terrain)
log_B_split <- split_week_data(log_B, max_week=5) #Note 5 day week for cycling
log_F_split <- log_F %>% select(-Terrain) %>%
  split_week_data(max_week=7)

#Checks
log_B_split %>% summarise_if(is.numeric, sum)
log_B %>% summarise_if(is.numeric, sum)

log_F_split %>% summarise_if(is.numeric, sum)
log_F %>% summarise_if(is.numeric, sum)

#Combine R,B,F into single df
log_new <- log_R %>% rename(Week_data=Week_total) %>% select(c(1:8, 16)) %>%
  bind_rows(log_B_split)
log_new %<>% bind_rows(log_F_split) %>% arrange(Date)

#Create temp df with zero entries for each days and type
start <- min(log_new$Date)
ndays <- as.numeric(max(log_new$Date) - start) + 1
nn <- 3 * ndays
temp_df <- log_new[1: nn, ]
temp_df$Date <- rep(start + (1 : ndays) - 1, 3)
temp_df$Type <- rep(c("B", "F", "R"), 1, each=ndays)
temp_df[, 5:7] <- 0

#Create new df with daily totals for each activity type (uses temp_df to add zero 
# entries on days where there is no activity for a given type)
totals <- log_new %>% bind_rows(temp_df) %>% 
  select(c(2:3, 5:7)) %>%
  group_by(Type, Date) %>% summarise_each(funs(sum))
totals %<>% ungroup() %>% mutate(Day=as.numeric(Date - min(Date) + 1)) %>% arrange(Date)

rm(temp_df)

#checks
if(F){
  totals %>% group_by(Type) %>% summarise_each(funs(sum))
  sum(1:ndays)
  temp_df %>% arrange(Date)
  totals %>% group_by(Date) %>% tally
  
  #checks
  log_new %>% select(Date, Type) %>% unique %>% tally
  length(unique(totals$Day))
  glimpse(totals)
}

