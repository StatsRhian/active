################
#Does some cleaning on 3 files:
# HuttonRoof2015.txt - adds in missing category & position entries. Saved to Results_raw.
# ColedaleHS2016.csv and WartonCrag2016.csv - processes to match FRA csv format. Saved 
# to Results_CSV.
################

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

choose <- 8
file_name <- str_c(path_root, "Results_raw/", files_raw[choose])

#############
# HuttonRoof2015.txt
#############
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

