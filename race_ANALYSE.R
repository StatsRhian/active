############
# For analysing the race results data.
# Either load from the csvs or from rdata file.
############

if(T){
  library(tidyverse)
  library(lubridate)
  library(stringr)
  source("race_FUNC.R")
  data <- readRDS("race_data_processed.RDATA")
}else{
  source("race_LOAD.R")
}


ret %>% group_by(time) %>% tally
ret %>% group_by(name) %>% count

data %>% group_by(raceID) %>% tally %>% print(n=Inf)

data %>% group_by(name) %>% tally %>% arrange(desc(n)) %>% print(n=20)



data %>% filter(str_detect(club, "Lonsdale")) %>% group_by(name) %>% tally %>% arrange(desc(n))
data %>% filter(!str_detect(name, "[[:lower:]]")) %>% group_by(raceID) %>% tally
data %>% filter(!str_detect(name, "[a-z]")) %>% group_by(raceID) %>% tally

data %>% filter(!str_detect(name, "[a-z]")) %>% 
  filter(!str_detect(raceID, "Arnside")) %>% group_by(raceID) %>% tally


data %>% filter(is.na(club)) %>% group_by(raceID) %>% tally
data %>% filter(category=="None") %>% group_by(raceID) %>% tally
                
data %>%  group_by(category) %>% tally %>% top_n(20, n)
data %>%  group_by(category) %>% tally %>% View

#Issues with triple names
data %>% filter(str_detect(name, " [A-z] | [A-z]$| [A-z][A-z]$| [A-z][A-z] "))

##########
#Race winners

data %>% group_by(raceID) %>% summarise(fastest=min(time)) %>% print(n=Inf)
#These would work with character time I think
winners <- data %>% group_by(raceID) %>% top_n(1, desc(seconds))
cat_winners <- data %>% group_by(raceID, category) %>% top_n(1, desc(seconds))
#Or using placings
winners <- filter(data, place==1)
cat_winners <- filter(data, cat_place==1)

#########
#Single runner info
runner = "James Edwards"
Jed <- data %>% filter(name==runner) %>% select(-club) %>% print(n=Inf)
plot(Jed$perc_winner)
Jed <- data %>% filter(name==runner) 
Rhi <- data %>% filter(name=="Rhian Davies") 

########
# Compare a pair of runners
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
(compare <- compare_runners(data, run1, run2) %>% select(-c(seconds1, seconds2)))
ggplot(data=compare) +
  geom_bar(aes(x=factor(raceID, levels=raceID), y=multiple), stat="identity") +
  coord_flip(ylim=c(min(compare$multiple), max(compare$multiple)))

########
# Analyse rivals
runner <- "James Edwards"
range <- c(0.95, 1)
range <- c(-Inf, Inf)
raced_with <- raced_with(data, runner, range)
raced_with %>% group_by(name) %>% tally %>% arrange(desc(n)) %>% print(n=30)
raced_with %>% group_by(name) %>% summarise(avg=mean(multiple), n=n()) %>%
  filter(n>1) %>% arrange(desc(n))

rivals_full <- raced_with %>% group_by(name) %>% nest

