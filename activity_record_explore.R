
source("activity_record_process.R")
#log_master - the original data import
#log - the original data with blank rows removed and some other cleaning
#log_B, log_R, log_F - the data for types B,F,R with appropriate variables
#log_B_split, log_F_split - B and F data with week data split up over following week
#log_new - R combined with B and F splits. 9 variables so some running specific ones lost
#totals - Daily totals for each of R,B,F (Time, Distance, Ascent)

#Other queries
print(log_new, n=15)
log_new %>% group_by(Type, Week_data) %>% tally(Distance)


shoe_summary <- log_R %>% group_by(`Sub-type`) %>%
  summarise_each(funs(sum), Time, Distance, Ascent) %>%
  arrange(desc(Distance))
shoe_summary

log %>% filter(Type %in% c("R", "B", "F")) %>%
  filter(Week_total==0) %>%
  group_by(Type) %>%
  select(c(2:8, 16)) %>%
  top_n(n=5, wt=Distance) %>%
  arrange(Type, desc(Distance))

#TO DO
#Replicate excel sheets with functions:
#Shoes
#Longest rides/runs - fucntion similar to above but with options
#Number rides/runs over given lenght etc.
#...?
#Rolling sum - I think create a new df for each #days I want in the rolling sum with all
# the cols I need i.e. date & time/dist/ascent for each of BFR & foot combined & time combined.
#...or date, type, time, dist, ascent cols with type R,B,F,RF,RFB then filter when needed.
#Maybe make own function - use cumsum then lag? Use dplyr?
library(zoo)
temp <- totals %>% filter(Type=="R") %>% transmute(temp=rollsum(Distance, 28, alig="right", fill=NA))
plot(temp$temp, typ='l')



