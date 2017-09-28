################
#Does some cleaning on 3 files:
# HuttonRoof2015.txt - adds in missing category & position entries. Saved to Results_raw.
# ColedaleHS2016.csv and WartonCrag2016.csv - processes to match FRA csv format. Saved 
# to Results_CSV.
################

library(tidyverse)
library(stringr)
source("race_FUNC.R")

if(dir.exists("C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/")){
  path_root <- "C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/" #new laptop
}else{
  path_root <- "E:/Dropbox/Mine/Personal/Running/Races&events/" #old laptop
}

files_raw <- list.files(str_c(path_root, "Results_raw"))
files_raw

#############
# HuttonRoof2015.txt
#############
file_name <- str_c(path_root, "Results_raw/HuttonRoof2015.txt")
#Load
txt <- readLines(file_name)
txt

#Find location of age category and position
test <- txt[4]
test
str_locate_all(test, "\\d")
str_sub(test, 37, 39) #category starts at 37
str_sub(test, 42, 43) #position starts at 42 or 43 depending on number of digits

#Replace blanks
pos <- 1
for (i in 3 : 246){
  if (str_sub(txt[i], 37, 37)==" "){
    txt[i] <- replace_char(txt[i], "M", 37)
    txt[i] <- replace_char(txt[i], pos, 43 - (pos >= 10))
    pos <- pos + 1
  }
}
#Save
cat(txt, file=file_name, sep="\n")

###############
# ColedaleHS2016.csv
###############
file_name <- str_c(path_root, "Results_raw/ColedaleHS2016raw.csv")
res <- read_csv(file_name)
# create new column "name" 
temp <- res %>% transmute(name=str_c(forename, " ", str_to_title(surname)))

# replace missing values
res_new <- res %>% replace_na(replace=list(club="UA")) %>% 
  select(c(-1,-2))
res_new <- bind_cols(temp, res_new)
#Save
file_name <- str_c(path_root, "Results_csv/ColedaleHS2016.csv")
write.csv(res_new, file=file_name, row.names=F)

###############
# WartonCrag2016.csv
###############

#############
#Inputs a string time written as mm.ss and returns string as hh:mm:ss.
#Note the input may be in form "mm" which means mm mins and 0 seconds or
# "mm.d" which means mm mins and d0 seconds.
#############
correct_time_format <- function(time){
  if (!str_detect(time, "\\d")){return(time)}
  time <- as.numeric(time)
  mins <- floor(time)
  secs <- round(100 * (time - mins), 2)
  return(str_c(c(str_pad(mins%/%60, 2, pad = "0"), str_pad(mins%%60, 2, pad = "0"), 
               str_pad(secs, 2, pad="0")), collapse=":"))
}

file_name <- str_c(path_root, "Results_raw/WartonCrag2016raw.csv")
res <- read_csv(file_name)
res

res_new <- res %>% mutate(name=str_c(Forename, " ", Surname)) %>%
  select(name, club=Club, cat=`Vet Category`, time=Time) %>%
  rowwise %>%
  mutate(time=correct_time_format(time)) %>%
  # mutate(category=cat) %>%
  select(name, club, category=cat, time)

file_name <- str_c(path_root, "Results_csv/WartonCrag2016.csv")
write.csv(res_new, file=file_name, row.names=F)
