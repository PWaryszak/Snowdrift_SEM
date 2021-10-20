#Load these packages into your working environment
library("readxl")
library("tidyverse")
library("vegan")

#CLEAN VEG DATA =========

# PAWEL WENT  BACK TO RAW DATA AND CREATE SpeciesID AND ChECK FOR DUPLICATES ON THEM
#Removed special character :dots and (((((())))))-s, fill with zeros empty cells.
#LOTS OF SPACES between Genus and species were found.

#Then check for duplicates again:
##https://stackoverflow.com/questions/30664012/extract-column-name-to-vector-by-calling-the-column-name

#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM/FloristicsRawCleaned")
files <- list.files()
files

veg2 <- read.csv('1Bundarashrubdata_CLEAN.csv') %>% select(-Species, -species, -Genus)
names(veg2)
v2_cols <- as.data.frame(colnames(veg2)) 
colnames(v2_cols)[1] <- "ColumnName"
dim(v2_cols)#9


veg3 <- read.csv("1JimAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg3)
v3_cols <- as.data.frame(colnames(veg3)) 
colnames(v3_cols)[1] <- "ColumnName"
dim(v3_cols)#23

veg4 <- read.csv("1KnollAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg4)
v4_cols <- as.data.frame(colnames(veg4)) 
colnames(v4_cols)[1] <- "ColumnName"
dim(v4_cols)#27

veg5 <- read.csv("1MarumAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg5)
v5_cols <- as.data.frame(colnames(veg5)) 
colnames(v5_cols)[1] <- "ColumnName"
dim(v5_cols)#7

veg6 <- read.csv("1MarumShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg6)
v6_cols <- as.data.frame(colnames(veg6)) 
colnames(v6_cols)[1] <- "ColumnName"
dim(v6_cols)#5

veg7 <- read.csv("1Rolling1PhebShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg7)
v7_cols <- as.data.frame(colnames(veg7)) 
colnames(v7_cols)[1] <- "ColumnName"
dim(v7_cols)#13

veg8 <- read.csv("1Rolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg8)
v8_cols <- as.data.frame(colnames(veg8)) 
colnames(v8_cols)[1] <- "ColumnName"
dim(v8_cols)#13

veg9 <- read.csv("1RuinedAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg9)
v9_cols <- as.data.frame(colnames(veg9)) 
colnames(v9_cols)[1] <- "ColumnName"
dim(v9_cols)#25

veg10 <- read.csv("1SaddlePShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg10)
v10_cols <- as.data.frame(colnames(veg10)) 
colnames(v10_cols)[1] <- "ColumnName"
dim(v10_cols)#13

veg11 <- read.csv("1SS21ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg11)
v11_cols <- as.data.frame(colnames(veg11)) 
colnames(v11_cols)[1] <- "ColumnName"
dim(v11_cols)#13

veg12 <- read.csv("1SS2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg12)
v12_cols <- as.data.frame(colnames(veg12)) 
colnames(v12_cols)[1] <- "ColumnName"
dim(v12_cols)#13

veg13 <- read.csv("1SS3ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg13)
v13_cols <- as.data.frame(colnames(veg13)) 
colnames(v13_cols)[1] <- "ColumnName"
dim(v13_cols)#13

veg14 <- read.csv("1ThredboRolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg14)
v14_cols <- as.data.frame(colnames(veg14)) 
colnames(v14_cols)[1] <- "ColumnName"
dim(v14_cols)#15

veg15 <- read.csv("1ThredboShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg15)
v15_cols <- as.data.frame(colnames(veg15)) 
colnames(v15_cols)[1] <- "ColumnName"
dim(v15_cols)#15

veg16 <- read.csv("1Watchbed1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg16)
v16_cols <- as.data.frame(colnames(veg16)) 
colnames(v16_cols)[1] <- "ColumnName"
dim(v16_cols)#9

veg17 <- read.csv("1Watchbed2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg17)
v17_cols <- as.data.frame(colnames(veg17)) 
colnames(v17_cols)[1] <- "ColumnName"
dim(v17_cols)#9

veg1 <- read.csv("1BHPshrubdata_CLEAN.csv") %>% select(-Species, -species, -Genus)
names(veg1)
#Veg1 one seems like aggregation of many sites. Remove that are duplicated:
vbind_nov1 <- rbind(v2_cols,v3_cols,v4_cols,v5_cols,v6_cols,v7_cols,v8_cols,v9_cols,v10_cols,v11_cols,v12_cols,v13_cols,v14_cols,v15_cols,v16_cols,v17_cols)
str(vbind_nov1)
vbind_nov1_noSp <- filter(vbind_nov1,  ColumnName != "SpecID") #remove specID
duplVeg1<- vbind_nov1_noSp [duplicated(vbind_nov1_noSp$ColumnName),] #Check for duplicates. Tick! Now, get veg1 missing sites:
duplVeg1 #ERO! YAY!
veg1_original <- veg1[ , - which ( colnames(veg1) %in% vbind_nov1_noSp$ColumnName)] #keep columns that original sits not listed in " vbind_nov1_noSp" 
str(veg1_original) #29 sites were original in this file.

names(veg1_original)
v1_cols_orig <- as.data.frame(colnames(veg1_original))
colnames(v1_cols_orig)[1] <- "ColumnName"
v1_cols_orig <- v1_cols_orig[-1,] #Remove first row tat contains ColumnName entry
v1_cols_orig

#Combine all sites together and check for duplicates
vbind <- rbind(v1_cols_orig,v2_cols,v3_cols,v4_cols,v5_cols,v6_cols,v7_cols,v8_cols,v9_cols,v10_cols,v11_cols,v12_cols,v13_cols,v14_cols,v15_cols,v16_cols,v17_cols)
dim(vbind)#223
unique(vbind$ColumnName)

#Extra check for duplicates
duplicateCols <- as.factor(vbind[duplicated(vbind$ColumnName),]) #checking if any columns, apart from SpecId,  are duplicated. NONE! YAY!
duplicateSiteCols <-  factor(duplicateCols[duplicateCols != "SpecID"]) #keep only site names
duplicateSiteCols #YAY!

#Combine all sites together and heck for duplicates
vbind <- rbind(v1_cols_orig,v2_cols,v3_cols,v4_cols,v5_cols,v6_cols,v7_cols,v8_cols,v9_cols,v10_cols,v11_cols,v12_cols,v13_cols,v14_cols,v15_cols,v16_cols,v17_cols)
unique(vbind$ColumnName)

#Produce TRIMMED file of veg one to work on:
View(veg1_original)
#write.csv(veg1_original, "1BHPshrubdata_CLEAN_TRIMMED.csv", row.names = F)
