##############
#File for processing OCT results. See OCT_analysis.R
##############
if(file.exists("C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Old Counties Tops/Results_OCT.xlsx")){
  file_name <- "C:/Users/edwards/Dropbox/Mine/Personal/Running/Races&events/Old Counties Tops/Results_OCT.xlsx" #new laptop
}else{
  file_name <- "E:/Dropbox/Mine/Personal/Running/Races&events/Old Counties Tops/Results_OCT.xlsx" #old laptop
}
#assign(paste0("raw_", sheetnames[num]),
#       read_excel("Results_OCT.xlsx", sheet=sheetnames[num]))
#raw <- read_excel("Results_OCT.xlsx", col_types = rep("text", 20))
num=1
raw <- read_excel(file_name, sheet=num) #gives warnings - is ok

# There are 20 cols. Position, team, team no, club, category.
# Cols 6-20 are checkpoint times and placings (pairs for 6-19 then final time without placing).
#The positions, team no, category cols only have one entry per pair (and NAs for other runner).
#The times give leg then cumulative times. Clubs may be different for each runner.

#col names
checkp_names <- names(raw)[seq(6, 19, by=2)]
checkp_names
names(raw)[seq(7, 19, by=2)] <- paste0("Pos_", checkp_names)


#Transform data to two dfs. First with no NAs but all cols and
# the second with just the names and times


main <- raw %>% na.omit %>% mutate_each(funs(date_to_time))
times <- raw %>% select(c(2, seq(6, 20, by=2))) %>% na.omit %>%
  mutate_each(funs(date_to_time))

#str(main)
#str(times)
#split team members into two vectors
runners <- times$Team
n_runners <- length(runners)
runner1 <- runners[seq(1, n_runners, by=2)]
runner2 <- runners[seq(2, n_runners, by=2)]
identical(runner1, main$Team) #should be TRUE

#Just keep leg times
#times <- times %>% slice(seq(2, n_runners, by=2)) #strangely not correct
times <- times[seq(2, n_runners, by=2), ] %>% select(-1)
names(times) <- c(paste0("Leg_", checkp_names), "Leg_finish")
head(times)

# times[c(2,4), ]
# times %>% slice(c(2,4))
main <- main %>% mutate(Team2=runner2)
#Merge
tidy_data <- bind_cols(main, times)
#str(tidy_data)
#names(tidy_data)

#Order columns
indices <- c(rbind(seq(6, 19, by=2), 22:28, seq(7, 19, by=2)))
tidy_data <- tidy_data %>% select(c(1, 2, 21, 3:5, indices, 20, 29))
names(tidy_data)[c(seq(7, 25, by=3), 28)] <- c(paste0("Time_", checkp_names), "Time_finish")
#str(times)
#glimpse(tidy_data)

#Get leg times as %age of total
# First need to add a Total time column to "times" df. 
# Due to missing data I just got it from "main".
#times <- times %>% mutate(Total=rowSums(.)) #doesn't work for times
#times <- times %>% mutate(Total=Reduce(`+`, .)) #not correct as there is missing data
temp <- main %>% select(20)
times <- bind_cols(times, temp)
rm(temp)
names(times)[9] <- "Total_time"

dur_times <- times %>% mutate_each(funs(as.duration(.)) )

perc_times <- dur_times %>% mutate_each(funs(. / Total_time))

# 
# 
# str(perc_times)
# str(dur_times)
# str(times)