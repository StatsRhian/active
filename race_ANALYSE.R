############
# For analysing the race results data.
# Either load from the csvs or from rdata file.
############

if(T){
  library(tidyverse)
  library(lubridate)
  library(stringr)
  library(purrr)
  source("race_FUNC.R")
  data <- readRDS("race_data_processed.RDATA")
}else{
  source("race_LOAD.R")
}

if (F){
  count(data, raceID) %>% print(n=Inf)
  count(data, name) %>% arrange(desc(n)) %>% print(n=20)
  data %>% filter(str_detect(club, "Lonsdale")) %>% count(name) %>% arrange(desc(n))
}

# Exploration for cleaning ---------------
if (F){
  data %>% filter(!str_detect(name, "[[:lower:]]")) %>% group_by(raceID) %>% tally
  data %>% filter(!str_detect(name, "[a-z]")) %>% group_by(raceID) %>% tally
  data %>% filter(!str_detect(name, "[a-z]")) %>% 
    filter(!str_detect(raceID, "Arnside")) %>% group_by(raceID) %>% tally
  
  # Club NAs and no category
  data %>% filter(is.na(club)) %>% group_by(raceID) %>% tally
  data %>% filter(category=="None") %>% group_by(raceID) %>% tally
  
  # Category summaries
  data %>%  group_by(category) %>% tally %>% top_n(20, n)
  data %>%  group_by(category) %>% tally %>% View
  
  # Detect triple names
  data %>% filter(str_detect(name, " [A-z] | [A-z]$| [A-z][A-z]$| [A-z][A-z] "))
}



#Race winners ----------------
data %>% group_by(raceID) %>% summarise(fastest=min(time)) %>% print(n=Inf)
#These would work with character time I think
winners <- data %>% group_by(raceID) %>% top_n(1, desc(seconds))
cat_winners <- data %>% group_by(raceID, category) %>% top_n(1, desc(seconds))
#Or using placings
winners <- filter(data, place==1)
cat_winners <- filter(data, cat_place==1)


#Single runner info --------------------
runner = "James Edwards"
Jed <- data %>% filter(name==runner) %>% select(-club) %>% print(n=Inf)
plot(Jed$perc_winner)
Jed <- data %>% filter(name==runner) 
Rhi <- data %>% filter(name=="Rhian Davies") 


# Compare a pair of runners --------------------
# It'd be useful to have this work with vectors of names. Output will be a list.
run2 <- "James Edwards"
run1 <- "Tim Cowin"
run1 <- "Heidi Dent"
run1 <- "Jos Addison"
run1 <- "James Edwards"
run1 <- "May Crawford"
run1 <- "Wendy Dodds"
run2 <- "Rhian Davies"
run1 <- "Jon Rylance"
run1 <- "Richard Mellon"
run1 <- "Phil Davies"
run1 <- "Josh Jardine"
run1 <- "Harvey Lord"
run2 <- "Sharon Taylor"
run1 <- "Jo Matthew"
run1 <- "May Crawford"
(compare <- compare_runners(data, run1, run2) %>% select(-c(seconds1, seconds2)))
ggplot(data=compare) +
  geom_bar(aes(x=factor(raceID, levels=raceID), y=multiple), stat="identity") +
  coord_flip(ylim=c(min(compare$multiple), max(compare$multiple)))


# Analyse rivals --------------------
runner <- "James Edwards"
runner <- "Rhian Davies"

# Creates df of people/race combinations who have raced with "runner".
all_raced_with <- raced_with(data, runner)

# Creates df summary of all runners who have raced with "runner"
rivals_full <- all_raced_with %>% group_by(name) %>% 
  summarise(races=n(), races_faster=sum(multiple < 1), 
            prop_faster=mean(multiple < 1), avg_multiple=mean(multiple), 
            oldest_cat=oldest_cat(category), gender=check_gender(gender)) %>%
  mutate(vet=check_vet(oldest_cat)) %>%
  arrange(desc(races)) 
rivals_full

# Various filters of rivals full
filter_rivals(rivals_full, max_multiple=1.08, min_races=2, desc=T)
filter_rivals(rivals_full, max_multiple=1.1, min_races=2, gender_filter="L")
filter_rivals(rivals_full, min_multiple=0.9, max_multiple=1.05, min_races=2)
(rivals_vets <- filter_rivals(rivals_full, max_multiple=1.1, min_races=2, vet_filter="Y"))
filter_rivals(rivals_full, min_prop_faster = 1, min_races=3) #never beaten

