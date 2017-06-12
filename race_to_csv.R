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

files_raw <- list.files(str_c(path_root, "Results_raw"))
files_raw
#t <- as.matrix(files_raw, ncol=1)

files_root <- str_sub(files_raw, 1, -5) #remove ".pdf" etc.
choose <- 1
file_name <- str_c(path_root, "Results_raw/", files_raw[choose])
# Read pdf
txt <- pdf_text(file_name)
#cat(txt)
#cat(txt[[1]])

# Save header text for later if needed
# headers <- str_split(txt,"\n")[[1]][3] %>%
#   str_split(boundary("word")) %>% 
#   unlist
# headers

# Extract entries as character vector with one entry per runner
# Trims irrelevant lines
lines <- str_split(txt,"\n") %>% 
  lapply(FUN='[', -c(1 : 2)) %>%
  lapply(FUN=function(x) x[-length(x)]) %>%
  unlist %>%
  .[-1]
lines
head(lines)
# Process entries to match fellrunner data form 
n <- length(lines)
fr_entries <- character(n)
for (i in 1 : n){
  convert_entry(lines[i], name_loc=4:5, cat_loc=6, club_start=7)
  fr_entries[i] <- c(convert_entry(lines[i], name_loc=4:5, cat_loc=6, club_start=7)) %>%
    str_c(collapse=",")
}
fr_entries

# Save as CSV, adding a line for headers 
csv_name <- str_c(path_root, "Results_CSV/", files_root[choose], ".csv")
headings <- "name,club,category,time"
cat(headings, fr_entries, file=csv_name, sep="\n")
# cat seems to automatically add a \r to each newline when saving so using sep='\n' results in 
# a \r\n after each entry which is what I want.
