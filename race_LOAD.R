################
# Imports all the csvs in "Results_CSV" folder and combines in a single tibble.
# A new variable is added (race time in seconds) and retired runners are removed into a separate
# table ("ret"). The main tibble is called "data".
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
  cat(files[i])
  path <- str_c(path_root, files[i])
  results[[i]] <- read_csv(path, col_types = "cccc")
  results[[i]] <- mutate(results[[i]], raceID=race_id[i])
}

data <- bind_rows(results)

# Remove runners that retire
ret <- data %>% filter(!str_detect(time, "\\d"))
data <- data %>% filter(str_detect(time, "\\d"))
# Add numerical time column (in seconds)
data <- data %>% rowwise %>% mutate(seconds=string_to_time(time))

