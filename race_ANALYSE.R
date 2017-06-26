
source("race_FUNC.R")
source("race_LOAD.R")

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
