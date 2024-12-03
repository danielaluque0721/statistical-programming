# Data management for Wistia Data
# Daniela Luque-Sanchez
# 06/05/2024

#Goal is to calculate the total number of Wistia sessions per study week

#### Load data and packages ####
library(tidyverse)
wistia <- read.csv("C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/DataRaw/Wistia/movefield_wistia_merged_deid.csv")

#Filter by if the session was viewed for >= 80%
filtered_wistia<-wistia %>% filter(Percent.Viewed >= 80)

#Extract Dates & Times
date<-as.Date(filtered_wistia$Session.Start.Time)
time<-format(as.POSIXct(filtered_wistia$Session.Start.Time), format= "%H:%M")

#Create new column with extracted dates and times
filtered_wistia<-filtered_wistia%>% mutate(Date = date, Time=time)

#Drop Unwanted columns
filtered_wistia <- filtered_wistia %>% select(Record_ID,Date,Time,Percent.Viewed)

#Filter by dates between 4/17/23 & 7/9/23
filtered_wistia<- filtered_wistia%>% filter(between(Date, as.Date('2023-04-17'), as.Date('2023-07-09')))
#Group into individual weeks
filtered_wistia <- filtered_wistia %>% mutate(Wistia_wk = case_when(between(Date, as.Date('2023-04-17'), as.Date('2023-04-23')) ~ "Wistia_wk1",
                                                                         between(Date, as.Date('2023-04-24'), as.Date('2023-04-30')) ~ "Wistia_wk2",
                                                                         between(Date, as.Date('2023-05-01'), as.Date('2023-05-07')) ~ "Wistia_wk3",
                                                                         between(Date, as.Date('2023-05-08'), as.Date('2023-05-14')) ~ "Wistia_wk4",
                                                                         between(Date, as.Date('2023-05-15'), as.Date('2023-05-21')) ~ "Wistia_wk5",
                                                                         between(Date, as.Date('2023-05-22'), as.Date('2023-05-28')) ~ "Wistia_wk6",
                                                                         between(Date, as.Date('2023-05-29'), as.Date('2023-06-04')) ~ "Wistia_wk7",
                                                                         between(Date, as.Date('2023-06-05'), as.Date('2023-06-11')) ~ "Wistia_wk8",
                                                                         between(Date, as.Date('2023-06-12'), as.Date('2023-06-18')) ~ "Wistia_wk9",
                                                                         between(Date, as.Date('2023-06-19'), as.Date('2023-06-25')) ~ "Wistia_wk10",
                                                                         between(Date, as.Date('2023-06-26'), as.Date('2023-07-02')) ~ "Wistia_wk11",
                                                                         between(Date, as.Date('2023-07-03'), as.Date('2023-07-09')) ~ "Wistia_wk12",))
#Challenge Goal Dataset
challenge <-filtered_wistia %>% select(Record_ID,Time)%>%mutate(time_of_day = case_when(Time>="05:00" & Time<= "11:59" ~ "AM",
                                                               Time>="12:00" & Time<= "16:59" ~ "Afternoon",
                                                               Time>="17:00" & Time<= "20:59" ~ "Evening",
                                                               Time>="21:00" | Time<= "04:59" ~ "Night"))

wide_challenge_wistia <- challenge %>% group_by(Record_ID, time_of_day) %>% summarise(Session_by_tod = length(time_of_day),  
                                                                               .groups = 'drop') %>% 
  pivot_wider(names_from =time_of_day, values_from = Session_by_tod, values_fill = list(Session_by_tod = 0))
                                       
#Summarize total Wistia Sessions per study week for each participant 
wide_wistia <- filtered_wistia %>% group_by(Record_ID, Wistia_wk) %>% summarise(Total_Sessions = length(Percent.Viewed),                                                                                   .groups = 'drop') %>% 
pivot_wider(names_from = Wistia_wk, values_from = Total_Sessions, values_fill = list(Total_Sessions = 0))

# Calculate average number of sessions for weeks 11 and 12 
wide_wistia_wk11_wk12 <- wide_wistia %>% mutate(Average_Wk11_12 = (Wistia_wk11 + Wistia_wk12) / 2)

#Order columns by week
ordered_columns <- c("Record_ID", paste0("Wistia_wk", 1:12))
wide_wistia <- wide_wistia %>% select(all_of(ordered_columns))

# Create long-format dataset
long_wistia <- filtered_wistia %>% group_by(Record_ID, Wistia_wk) %>% summarise(Total_Sessions = length(Percent.Viewed),
                                                                                    .groups = 'drop') %>% rename(week = Wistia_wk, Wistia = Total_Sessions)
#Exporting Data-sets
write.csv(wide_challenge_wistia, "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/wide_challenge_wistia.csv", row.names = FALSE)
write.csv(long_wistia, "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/long_wistia.csv", row.names = FALSE)
write.csv(wide_wistia, "C:/Users/luquesad/OneDrive - The University of Colorado Denver/Luque-Sanchez_Ostendorf_CoSIBSProject/Reports/wide_wistia.csv", row.names = FALSE)
