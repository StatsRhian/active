
source("race_FUNC.R")
source("race_LOAD.R")

ret %>% group_by(time) %>% tally
ret %>% group_by(name) %>% count

data %>% group_by(raceID) %>% tally %>% print(n=Inf)

data %>% group_by(name) %>% tally %>% arrange(desc(n)) %>% print(n=20)

data %>% filter(!is.na(cat))
