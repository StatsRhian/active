###########################################
# FUNCTIONS FOR DATA READING AND PROCESSING 
###########################################

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
    str_replace_all("-", "_") %>% #handle hypehnated names
    str_replace_all(" De ", " De1") %>% #Arnside2016
    str_replace_all("DE CURTIS", "De1CURTIS") %>% #Coldale2017
    str_replace_all(" C\\?| D ", " ") %>% #Weasdale
    str_replace_all("James L", "James") #Weasdale
  words <- str_split(entry, boundary("word")) %>% unlist
  n <- length(words)
  categ <- if(cat_loc>0){words[cat_loc]}else{words[length(words) + cat_loc + 1]}
  this_cat_missing <- cat_missing & !check_category(categ)
  after_club <- n - after_club + this_cat_missing
  name <- words[name_loc]  %>% str_replace_all("_", "-") %>% str_replace("De1", "De ") %>%
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

##############
#Converts string in format "HH:MM:SS" to a numeric equal to the number of seconds.
##############
string_to_time <- function(time_string){
  h <- as.numeric(str_sub(time_string, 1, 2))
  m <- as.numeric(str_sub(time_string, 4, 5))
  s <- as.numeric(str_sub(time_string, 7, 8))
#  return(duration(hour=h, mins=m, second=s))
  return(3600 * h + 60 * m + s)
}

#################
#Used to correct differing strings used for the same club so that a single string is used for a single club.
#################
standardise_club <- function(club){
  if(is.na(club)){return("ua")}
  club <- str_to_lower(club)
  pattern <- " (ac|runners|club|running|fell|fr|harriers|road|rmi|rc|a\\.c|a c|f\\.r|rr)"
  club <- club %>% str_replace_all(pattern, "") %>%
    str_replace_all(" and ", " & ") %>%
    str_replace_all("-|_|- ", " ") %>%
    str_replace_all("u/a|u\a|un attached|unattached", "ua") %>%
    str_replace_all("cfr", "cumberland")
    return(club)
}

#################
#Converts category to a standard form. I have chosen to use:
# Man, Lady, MVxx, LVxx, MUxx, LUxx.
#
#There are various options used: 5 race use MSEN, 4 use M, KWL& whernside use Man, 
# others use OPEN, MOpen, MO, M Sen. Women similar but variations on W and F as well as L.
#################
standardise_category <- function(categ){
  if (is.na(categ)){return("Man")}
  if (str_detect(categ, "\\d\\d")){
    age <- str_extract(categ, "\\d\\d")
    lady <- str_detect(categ, "L|W|F")
    under <- ifelse(age < 30, "U","V")
    if (lady){
      return(str_c("L", under, age))
    }else{
      return(str_c("M", under, age))
    }
  }else{
    if (str_detect(categ, "^M|^O|SM")){categ <- "Man"}
    if (str_detect(categ, "^F|^L|^W|SF")){categ <- "Lady"}
  }
  return(categ)
}

####################
# ANALYSIS FUNCTIONS
####################

#############
#Inputs: names of two runners.
#Returns a data frame with one row for each race both runners did and a comparison of times.
#############
compare_runners <- function(data, run1, run2) {
  two <- data %>% filter(name %in% c(run2)) %>% select(raceID, time2=time, seconds2=seconds, place2=place)
  compare <- data %>% filter(name==run1) %>% 
    select(raceID, year, time1=time, seconds1=seconds, place1=place) %>% 
    inner_join(two, by="raceID") %>% 
    mutate(diff_place=place2 - place1) %>%
    mutate(gap=seconds2 - seconds1) %>%
    mutate(multiple=seconds2 / seconds1)
  return(compare)
}

############
# Input a runner's name and a range (vector of length two).
# Returns a dataframe of results where the time multiple compared to the time of the "runner"
# was within the supplied range.
# Output df adds two variables: my_time - time of "runner", and multiple - time / my_time.
# So multiple < 1 indicates "runner" was slower. 
############
raced_with <- function(data, runner, range=c(-Inf, Inf)) {
  low <- range[1]
  high <- range[2]
  by_race <- data %>% group_by(raceID) %>% nest()
  return(by_race %>% mutate(my_time=unlist(map(data, runner_time, runner))) %>% 
           filter(!is.na(my_time)) %>% unnest(data) %>%
           mutate(multiple=seconds / my_time) %>%
           filter((multiple >= low) & (multiple <= high)) %>%
           filter(name!=runner))
}

#################
# A helper function for raced_with(). Returns a vector of times (seconds) for the runner
# named in the argument.
#################
runner_time <- function(data, runner){
  ran <- any(data$name==runner)
  if (!ran){return(NA)}
  return(filter(data, name==runner)["seconds"])
}

###########
# Helper function for rivals list. Given an input of a vector of categories returns a single category which 
# is the "oldest".
###########
oldest_cat <- function(cat_vec){
  cat_order <- c('Amanda', 'Cocks', 'Scott', 'Kampen', 'None','Girl', 'V', 'LU18', 'LU20', 'LU21', 'LU23', 'LU25', 'Lady', 'LV40', 'LV45', 'LV50', 'LV55', 'LV60', 'LV65', 'LV70', 
                 'MU18', 'MU20', 'MU21', 'MU23', 'MU25', 'Man', 'MV40', 'MV41', 'MV45', 'MV50', 'MV55', 'MV60', 'MV65', 'MV70', 'MV75')
  matches <- which(cat_order %in% cat_vec)
  return(cat_order[max(matches)])
}

#############
# Helper function for rivals list. Given an input of a vector of genders returns a single gender which 
# is "M" or "F" if contained in the vector or "Unknown" otherwise.
#############
check_gender <- function(gender_vec){
  if(any(gender_vec == "L")){return("L")}
  if(any(gender_vec == "M")){return("M")}
  return("Unknown")
}

#############
# Helper function for rivals list. Input is a vector of categories. Returns "Y" for veteran categories i.e. >=35 
# age, "N" otherwise.
#############
check_vet <- function(cat_vec){
  vet <- ifelse(str_extract(cat_vec, "\\d\\d") >= 35, "Y", "N")
  return(ifelse(is.na(vet), "N", vet))
}

###############
# Filters the rivals_full df according to the arguments.
# gender_filter takes either NA for all (default) or "M" or "L".
# vet_filter is either NA (for all), "Y" or "N".
# arrange_by and desc control sorting.
###############
filter_rivals <- function(rivals_full, min_multiple=-Inf, max_multiple=Inf, min_races=2, gender_filter=NA, vet_filter=NA, 
                          arrange_by="avg_multiple", desc=F, min_prop_faster=0){
  rivals <- rivals_full %>% 
    filter(avg_multiple <= max_multiple)  %>% 
    filter(avg_multiple >= min_multiple)  %>%
    filter(races >= min_races) %>%
    filter(prop_faster >= min_prop_faster)
  if(!is.na(gender_filter)){rivals <- filter(rivals, gender==gender_filter)}
  if(!is.na(vet_filter)){rivals <- filter(rivals, vet==vet_filter)}
  if (desc){arrange_by=paste0("desc(", arrange_by, ")")}
  rivals <- arrange_(rivals, arrange_by)
  
  return(rivals)
}