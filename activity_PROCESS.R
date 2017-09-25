#############
# Imports activity data from my spreadsheet. Activities are recorded on the "log" sheet of the spreadsheet.
# See "activity_EXPLORE.R" for summary of dfs created.
#############
if(file.exists("C:/Users/edwards/Dropbox/Mine/Personal/Activity Record.xlsx")){
  file_name <- "C:/Users/edwards/Dropbox/Mine/Personal/Activity Record.xlsx" #new laptop
}else{
  file_name <- "E:/Dropbox/Mine/Personal/Activity Record.xlsx" #old laptop
}

# file.exists("../../../Dropbox/Mine/Personal/Activity Record.xlsx") gets to the spreadsheet on my new laptop.

library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)

source("activity_FUNC.R")
#readxl has two functions
excel_sheets(file_name)
log_master <- read_excel(file_name, sheet="Log", skip=12)
#glimpse(log_master)
count(log_master, is.na(Type))
#trim rows with no entry and remove blank column
log <- filter(log_master, !is.na(Type)) %>%
  select(-16)

#Rename cols
colnames(log)
colnames(log) <- c("Week_total", "Date", "Type", "Sub-type", "Time", "Distance", "Ascent", "Notes", "Terrain", 
                   "Tempo_pace", "5k10k_pace", "Sub_5k_pace", "Hill_sprints", "Strides", "Drills", 
                   "Total_time", "Year", "Month", "Week")

#glimpse(log)
log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)

# List number of NAs and replace
log %>% summarise_all(funs(sum(is.na(.)))) %>% unlist
log <- log %>% mutate_at(vars(c(1, 5:7, 9:15)), funs(ifelse(is.na(.), 0, .))) %>%
  mutate(Week_total=ifelse(Week_total=="week", 1, 0)) %>% # Convert week total to binary
  mutate(Total_time=ifelse(is.na(Total_time), Time, Total_time)) %>%
  mutate_at(vars(3:19), funs(ifelse(is.na(.), "none", .))) # For Sub-type and notes

#explore
#table(log$Type)
#count(log, Type)
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

#Split week totals (loses terrain)
#Note 5 day week for cycling
# NAs created in Notes column by split_week_data function
log_B_split <- split_week_data(log_B, max_week=5) %>%
  mutate_at(vars(Notes), funs(ifelse(is.na(.), "none", .)))
log_F_split <- log_F %>% select(-Terrain) %>%
  split_week_data(max_week=7) %>%
  mutate_at(vars(Notes), funs(ifelse(is.na(.), "none", .)))

#Checks
if (F){
  log_B_split %>% summarise_if(is.numeric, sum)
  log_B %>% summarise_if(is.numeric, sum)
  
  log_F_split %>% summarise_if(is.numeric, sum)
  log_F %>% summarise_if(is.numeric, sum)
  
  log_F_split %>% summarise_all(funs(sum(is.na(.)))) %>% unlist
  log_F_split %>% summarise_all(funs(sum(.=="none"))) %>% unlist
}

#Combine R,B,F into single df
log_new <- log_R %>% rename(Week_data=Week_total) %>% select(c(1:8, 16)) %>%
  bind_rows(log_B_split)
log_new <- log_new %>% bind_rows(log_F_split) %>% arrange(Date)

#Create temp df with zero entries for all days and types
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
  group_by(Type, Date) %>% summarise_all(funs(sum))
totals <- totals %>% ungroup() %>% 
  mutate(Day=as.numeric(Date - min(Date) + 1)) %>%
  arrange(Date)

rm(temp_df)

#checks
if(F){
  totals %>% group_by(Type) %>% summarise_each(funs(sum))
  sum(1:ndays)
  temp_df %>% arrange(Date)
  totals %>% group_by(Date) %>% tally
  
  log_new %>% select(Date, Type) %>% unique %>% tally
  length(unique(totals$Day))
  glimpse(totals)
}

