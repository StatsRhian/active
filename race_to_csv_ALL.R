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
library(tidyverse)
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

#########
# KWL
#########
files_kwl <- list.files(str_c(path_root, "KWL_results"))
files_kwl <- files_kwl[!str_detect(files_kwl, "Championship")]

for (race in files_kwl){
  cat(race, "\n")
  txt <- pdf_text(str_c(path_root, "KWL_results/", race))
  
  lines <- str_split(txt,"\n") %>% unlist
  u17_line <- which(str_detect(lines, "Under 17 Race"))[1]
  lines <- lines[1 : (u17_line - 1)]
  keep <- vapply(lines, FUN=is_numeric_first, FUN.VALUE=logical(1))
  lines <- lines[keep]
  
  n <- length(lines)
  fr_entries <- character(n)
  for (i in 1 : n){
    fr_entries[i] <- convert_entry(lines[i], name_loc=c(3, 4), cat_loc=5, club_start=7, kwl=T) %>%
      str_c(collapse=",")
  }
  #fr_entries
  
  # Save as CSV, adding a line for headers 
  files_root <- str_sub(race, 1, -5) %>% unlist #remove ".pdf" etc.
  csv_name <- str_c(path_root, "Results_CSV/", files_root, ".csv")
  headings <- "name,club,category,time"
  cat(headings, fr_entries, file=csv_name, sep="\n")
}
