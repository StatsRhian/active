#############
#This reads results from a pdf and converts to a csv. 
#############
#NOTES on sources
#----------------
#Some columns have missing entries in some results:
# Age categories in ColdaleHS2017 - shouldn't be entirely numbers (race no is after club)
# Age categories in HuttonRoof2015 - should be either L, Ldd, Vdd or Udd. So either contains
# number or is exactly "L"
# Clubs in ColdaleHS2016 - detect by contains numbers (time is after club)
# 
#HuttonRoof2015. This is a text file but reads in as a vector of lines.
# 244 runners, all finish. Two header lines.

library(pdftools)
library(stringr)
source("race_FUNC.R")

if(dir.exists("C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/")){
  path_root <- "C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/" #new laptop
}else{
  path_root <- "E:/Dropbox/Mine/Personal/Running/Races&events/" #old laptop
}

settings <- read_csv("to_csv_settings.csv")
settings
n_files <- dim(settings)[1]

for (i in c(1 : n_files)){
  cat(i)
  vars <- settings %>% slice(i)
  source("race_to_csv_DO.R")
}


