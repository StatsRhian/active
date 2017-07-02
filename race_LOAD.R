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
  results[[i]] <- read_csv(path, col_types = "cccc") #could use "ccct" here but will convert RET to NAs
  results[[i]] <- mutate(results[[i]], raceID=race_id[i])
}

data <- bind_rows(results)

# Remove runners that retire
ret <- data %>% filter(!str_detect(time, "\\d"))
data <- data %>% filter(str_detect(time, "\\d"))  #could convert "time" to a time with "%>% type_convert"
# Add numerical time column (in seconds) and correct name case
data <- data %>% rowwise %>% mutate(seconds=string_to_time(time)) %>%
  mutate(name=str_to_title(name)) %>% rowwise %>% mutate(name=apostrophe_name_title(name))

#data <- na_replace(data, replace=list(club=unknown))
# data %>% filter(str_detect(name, "'"))
# data %>% filter(str_detect(name, "[A-Z][A-Z]")) %>% group_by(raceID) %>% tally

############
# Club and category processing exploration).
############
if(F){
  data %>% group_by(club) %>% tally %>% arrange(desc(n)) %>% print(n=20) #696 unique
  unique_clubs <- data %>% select(club) %>% distinct %>% unlist
  data %>% rowwise %>% mutate(club=standardise_club(club)) %>%
    group_by(club) %>% tally %>% arrange(desc(n)) %>% print(n=50) #487 unique
  data %>% select(club) %>% mutate(club=str_to_lower(club)) %>% 
    filter(str_detect(club, "runners")) %>% distinct %>% View
  #explore categories
  data %>% select(category) %>% distinct #123 unique
  data %>% select(category) %>% filter(!str_detect(category, "\\d")) %>% distinct %>% print(n=Inf)
  data %>% filter(category=="Open") %>% group_by(raceID) %>% tally
  data %>% select(category) %>% rowwise %>% 
    mutate(category=standardise_category(category)) %>%
    group_by(category) %>% tally %>% print(n=Inf) #36 unique
  
}

#standardise categories
data <- data %>%  rowwise %>% mutate(category=standardise_category(category))
#Add placings
data <- data %>% group_by(raceID) %>% mutate(place=min_rank(seconds))
#Category placings
data <- data %>% group_by(raceID, category) %>% mutate(cat_place=min_rank(seconds))
#Add % of winner's time
data <- data %>% group_by(raceID) %>% mutate(perc_winner=seconds / min(seconds))
#Add % of cat winner's time
data <- data %>% group_by(raceID, category) %>% mutate(perc_cat_winner=seconds / min(seconds))
#Add gender
#Add year of race
data <- data %>% mutate(year = str_sub(raceID, -4, -1)) 
data %>% ungroup %>% arrange(year, raceID, place)
