
convert_entry <- function(entry, name_loc, cat_loc, club_start, club_end=0, cat_missing=F){
  words <- str_split(entry, boundary("word")) %>% unlist
  if(words[1]=="WD"){words <- c("RET", words)} #for arnside ret runner
  n <- length(words)
  club_end <- n - club_end
  name <- str_c(words[name_loc], collapse=" ")
#  if(time_loc < 0){time_loc <- length(words) + 1 + time_loc}
  time <- extract_time(entry)
  club <- str_c(words[club_start : club_end], collapse=" ")
  if (str_detect(club, "\\d")){club <- "UA"} #club missing if it contains a digit
  categ <- words[cat_loc]
  if (cat_missing & !check_category(categ)){categ <- "M"}
  return(c(name, club, categ, time))
}

#############
#Takes a string as input and outputs a string in form "dd:dd:dd" where d are digits.
#The output is formed from extracting a time from the input string.
# The time can take several forms:
# mm.ss where mm could be >60, 
# mm:ss or h:mm:ss or 0h:mm:ss.
#############
extract_time <- function(entry){
  if (str_count(entry, ":")==0){
    time <- str_extract(entry, "\\d\\d\\.\\d\\d")
    if (is.na(time)){return("RET")}
    mins <- as.numeric(str_sub(time, 1, 2))
    return(c(str_pad(mins%/%60, 2, pad = "0"), str_pad(mins%%60, 2, pad = "0"), 
             str_sub(time, -2, -1)) %>%
             str_c(collapse=":"))
  }
  if (str_count(entry, ":")==1){
    return(str_c("00:", str_extract(entry, "\\d\\d:\\d\\d")))
  }else{
    return(str_c("0", str_extract(entry, "\\d:\\d\\d:\\d\\d")))
  }
}

#############
# Checks if category is wrong which will happen when it is missing. 
# Returns TRUE if category is ok.
# Currently designed only to pickup known issues in results where category is missing:
#
# Age categories in ColdaleHS2017 - shouldn't be entirely numbers (race no is after club)
# Age categories in HuttonRoof2015 - should be either L, Ldd, Vdd or Udd. So either contains
# number or is exactly "L"
# 
# So it checks if contains both numbers and digits OR is exactly "L".
############
check_category <- function(category){
  has_digit <- str_detect(category, "\\d")
  has_letter <- str_detect(category, "M|L|V|U")
  return((has_digit & has_letter) | category=="L")
}


