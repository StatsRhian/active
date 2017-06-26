################
# Imports all the csvs in "Results_CSV" folder and combines in a single tibble.
# A new variable is added (race time in seconds) and retired runners are removed into a separate
# table ("ret"). The main tibble is called "data".
# Option to do other cleaning here too.
################
library(tidyverse)
library(lubridate)
library(stringr)

if(dir.exists("C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Results_CSV/")){
  path_root <- "C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Results_CSV/" #new laptop
}else{
  path_root <- "E:/Dropbox/Mine/Personal/Running/Races&events/Results_CSV/" #old laptop
}

files <- list.files(path_root)
files
race <- files[1]
race_id <- str_sub(files, 1, -5)
race_names <- str_sub(files, 1, -9)
race_years <- as.numeric(str_sub(files, -8, -5))
n_races <- length(files)
results <- vector('list', n_races)
for (i in 1 : n_races){
  cat(files[i], "\n")
  path <- str_c(path_root, files[i])
  results[[i]] <- read_csv(path, col_types = "cccc")
  results[[i]] <- mutate(results[[i]], raceID=race_id[i])
}

data <- bind_rows(results)

# Remove runners that retire
ret <- data %>% filter(!str_detect(time, "\\d"))
data <- data %>% filter(str_detect(time, "\\d"))
# Add numerical time column (in seconds) and correct name case
data <- data %>% rowwise %>% mutate(seconds=string_to_time(time)) %>%
  mutate(name=str_to_title(name)) %>% rowwise %>% mutate(name=apostrophe_name_title(name))

#data <- na_replace(data, replace=list(club=unknown))
# data %>% filter(str_detect(name, "'"))
# data %>% filter(str_detect(name, "[A-Z][A-Z]")) %>% group_by(raceID) %>% tally

############
# Club processing
############
if(F){
  data %>% group_by(club) %>% tally %>% arrange(desc(n)) %>% print(n=20) #696 unique
  unique_clubs <- data %>% select(club) %>% distinct %>% unlist
  data %>% mutate(club=str_replace_all(club, " AC", "")) %>%
    mutate(club=str_to_title(club)) %>%
    group_by(club) %>% tally %>% arrange(desc(n)) %>% print(n=50)
  data %>% rowwise %>% mutate(club=standardise_club(club)) %>%
    group_by(club) %>% tally %>% arrange(desc(n)) %>% print(n=50) #487 unique
  data %>% select(club) %>% mutate(club=str_to_lower(club)) %>% 
    filter(str_detect(club, "runners")) %>% distinct %>% View
  
}
#################
#
#################
standardise_club <- function(club){
  club <- str_to_lower(club)
  pattern <- " (ac|runners|club|running|fell|fr|harriers|road|rmi|rc|a\\.c|a c|f\\.r|rr)"
  club <- club %>% str_replace_all(pattern, "") %>%
    str_replace_all(" and ", " & ") %>%
    str_replace_all("-|_|- ", " ") %>%
    str_replace_all("u/a|u\a|un attached|unattached", "ua")
  return(club)
}

standardise_club("broad road runners")

str_replace_all("ddd ac harriers team fell runners", " (ac|fell|harriers)" , "")

str_replace_all("clayton-le-moors", "-|_|- ", " ")
str_replace_all("u\a", "u\a", "ua")
