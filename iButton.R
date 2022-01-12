#Script to process raw iButton temperature data to get number of days under snow

#CLEAN DATA FIRST:======
#Many files contained spaces. Replace with "_"
library(filesstrings) #https://cran.r-project.org/web/packages/filesstrings/vignettes/files.html
#Set Working Directory to where raw iButton files are:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/RawData2Bprocessed/SoilTemperature_ibuttons/2020")
remove_filename_spaces(replacement = "_") #22 files required renaming and this was done successfully
#Check manually if all looks good. Sometimes space was missing or typo existed.
files <- list.files()
files


#FUNCTIONS=========
library(data.table)
library(tidyverse)
#Create a function to run over data in all csv files
#WEB HELP = https://dplyr.tidyverse.org/articles/programming.html

#Specify path to folder where iButtons spreadsheets are and list them all:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/RawData2Bprocessed/SoilTemperature_ibuttons/2020")
#Include full path:
files <- list.files(path = "C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/RawData2Bprocessed/SoilTemperature_ibuttons/2020",
                    pattern = "*.csv", full.names = T)
files

#Create function to export data from data rows:
Read_iButton <- function(x) read.csv (file = x,
                                      skip = 19)

#Export iButton data from all files into one dataset:
iButtonData2020 <- sapply(files, Read_iButton, simplify=FALSE) %>%
  bind_rows(.id = "id1")
  
dim(iButtonData2020)#41974     4
str(iButtonData2020)#NICE! Value is numeric.

#SnowDayNumber in 2020======
names(iButtonData2020)

iButtonData2020_processed <- iButtonData2020 %>%
  
  mutate(SampleID1 = str_sub (id1, 126,-1)) %>%    #Cut first 126 characters (pathway) out off "id" column
  mutate(SampleID = str_sub (SampleID1, 1,-5)) %>% #Cut the ".csv" out to produce SampleID we can use to match other datasets with

  mutate(snow_roll_av = frollmean(Value, 4, align = "center", fill = NA)) %>% #Computes rolling average over 24h
 
  separate( Date.Time,  c("date", "time"), sep = " ", remove = F) %>% #Separates date from time
  
  group_by(date, SampleID) %>% #aggregate by date and plot (SampleID)
  summarise(AV = mean(Value),  #compute mean temp per day (4 measuerments)
            AV_rolling =mean(snow_roll_av)) %>% #compute mean of rolling mean over 4 values
  
  filter(AV_rolling < 1 ) %>%  # leave (Assuming that you can't get above 1C under snow)
  
  group_by(SampleID) %>% #aggregate by plot (SampleID)
  summarise(SnowDayNumber = length(SampleID)) %>%  #compute SnowDay number in 2020
  mutate(year = 2020)

View(iButtonData2020_processed)
#write.csv(iButtonData2020_processed, file = "SnowDays2020.csv", row.names = F)


#PROOF of AV per Day ~ AV_rolling=========
#Below is proof that group_by Date works as well as rolling average of 4 rows
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/RawData2Bprocessed/SoilTemperature_ibuttons")
files <- list.files()
files

f1 <- read.csv("BHP_Jim_G1_SE.csv", skip = 19 ) #Skip 19 rows where no-temperature info sits
head(f1,8)
f1$snow_roll_av <- frollmean(f1$Value, 4, align = "center", fill = NA) %>%
  separate( Date.Time, c("date", "time"), sep = " ")
f1_processed <- f1 %>%
  mutate(snow_roll_av = frollmean(Value, 4, align = "center", fill = NA)) %>% #Computes rolling average over 24h
  separate( Date.Time, c("date", "time"), sep = " ", remove = F) %>% #Separates date from time
  group_by(date)%>% #aggregate by day
  summarise(AV = mean(Value), #compute mean per day
            AV_rolling =mean(snow_roll_av)) %>% #compute rolling mean over 4 values
  filter(AV_rolling < 1 )

head(f1_processed)



#Raw script from Sonya Rita Geange <Sonya.Geange@uib.no>=========
#Hi Susanna and Pawel, Hmm, I had one that I made myself for my OTC paper a while ago.
#But it was just a self-made function, for detecting mean temperatures above or below 0, for a minimum period of days, in this case 4 days.
#I think it was something like this

library(zoo) #seems not existing anymore
library(data.table)
library(dplyr)
library(tidyverse)

#For each Treatment, Block and Date in the study calculate the mean temperature.

block_temp <- ddply(
  .data = full_ib,
  .variables = c("Treatment","Block","Date"),
  .fun=summarize,
  mean_temp = mean(Temperature, na.rm = TRUE),
  .parallel = F)

# Calculate when snow persists, with rolling mean temp of 4 days
# Then search for Date index where the first day with snow_roll_av = 0

block_temp$snow_roll_av <- rollmean(block_temp$mean_temp, 4, align = "center", fill = NA)

#But this was many, many years ago, so not sure if it still works these days.
#I’m sure there’s a much more efficient way to do it as well, though off the top of my head don’t have an R package for it.

