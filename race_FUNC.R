
################
# Takes a string race entry, processes it and outputs a character vector matching the FRA
# format of: name, club, category, time.
################
convert_entry <- function(entry, name_loc, cat_loc, club_start, 
                          after_club=0, cat_missing=F, kwl=F){
  time <- extract_time(entry)
  # Various cleaning of initial text
  entry <- entry %>% str_replace_all(":", ".") %>% #so that the time is treated as single "word"
    str_replace_all("\\&", " and ") %>%
    str_replace_all("/", "") %>%
    str_replace_all("L OPEN", "L") %>%
    str_replace_all("Male Under 23", "MU23") %>%
    str_replace_all("Ladies Under 23", "LU23") %>%
    str_replace_all(" \\?", "Unknown") %>% #gigg2017 has ?????? in place of a name
    str_replace_all("-", "_") #handle hypehnated names
  words <- str_split(entry, boundary("word")) %>% unlist
  n <- length(words)
  categ <- if(cat_loc>0){words[cat_loc]}else{words[length(words) + cat_loc + 1]}
  this_cat_missing <- cat_missing & !check_category(categ)
  after_club <- n - after_club + this_cat_missing
  name <- words[name_loc]  %>% str_replace_all("_", "-") %>%
    str_to_title %>% str_c(collapse=" ") %>% apostrophe_name_title
  club <- str_c(words[club_start : after_club], collapse=" ")
  if (is.na(club)){club <- "Unattached"}
  if (str_detect(club, "\\d")){
    if (kwl){
      loc <- str_locate(club, "\\d")
      club <- str_sub(club, 1, loc[1, 1] - 2)
    }else{
      club <- "Missing" #club missing if it contains a digit
    } 
  }
  # Inserts "None" as category if missing
  if (cat_missing & !check_category(categ)){
    categ <- "None"
  }
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

################
# Inputs: two strings ("txt" and "string") and an integer "start".
# Outputs txt with the characters starting at "start" replaced by "string".
# The length of "txt" is unchanged.
################
replace_char <- function(txt, string, start){
  keep1 <- str_sub(txt, start=1, end=(start - 1))
  keep2 <- str_sub(txt, start=(start + str_length(string)), end=-1)
  return(str_c(c(keep1, string, keep2) , collapse=""))
}

##############
# Returns TRUE if "string" begins with a digit.
##############
is_numeric_first <- function(string){
  words <- str_split(string, boundary("word")) %>% unlist
  if (is.na(str_detect(words, "^\\d")[1])){return(FALSE)}
  return(str_detect(words, "^\\d")[1])
}

##############
# Returns TRUE if "string" ends with ".pdf". 
##############
is_pdf <- function(string){
  return(str_sub(string, -4, -1) %>% identical(".pdf"))
}

##############
#Capatilises the letter after ' in a string.
##############
apostrophe_name_title <- function(name){
  if (!str_detect(name, "'")){return(name)}
  return(name %>% str_split("'") %>% unlist %>% str_to_title %>% str_c(collapse="'"))
}
