
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

##########
#Race winners

data %>% group_by(raceID) %>% summarise(fastest=min(time)) %>% print(n=Inf)
#These would work with character time I think
winners <- data %>% group_by(raceID) %>% top_n(1, desc(seconds))
cat_winners <- data %>% group_by(raceID, category) %>% top_n(1, desc(seconds))
#Or using palcings
winners <- filter(data, place==1)
cat_winners <- filter(data, cat_place==1)

#########
#Single runner info
runner = "James Edwards"
Jed <- data %>% filter(name==runner) %>% print(n=Inf)
Jed <- data %>% filter(name==runner) 
Rhi <- data %>% filter(name=="Rhian Davies") 



