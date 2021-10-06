#Load these packages into your working environment
library("readxl")
library("tidyverse")
library("vegan")

#SNOW DATA ==========
SnowData <- read.csv("alpine_data.csv", header = TRUE)#
names(SnowData) #all names until column 22 are NW-related
unique(SnowData$site_site_no)
# "SE_snow_days_1_aug" = snow day since 1 aug showing more snow on leeward (1_aug marks end of winter)
# NW_snow_days_1_aug = snow day since 1 aug showing more snow on windward
#Wind data from both sides. SE Wind / NW Wind = ratio to be found by Suze.

unique(SnowData$year)#2015 2016 2017 2018 - for years of snow measurements and veg survey was done in 2019
unique(SnowData$site_site_no)# [1] "Bundara", "Jim","Knoll","MarumPoint","Ruined.Castle1", "Saddle.Track1",  "Watchbed1"      "Watchbed2"      " Giddyup"      
# "Horse.camp1", "Horse.camp2", "Horse.camp3","Horsey", "Rolling1","Rolling2" , "Snowy.Slopes2", "Snowy.Slopes3"  "Thredbo"     

#Seperate SE columns from NW columns and create "aspect" variable (SE vs NW)
data_nw <- SnowData %>%
  #skip all SE variables
  select(-SE_snow_days, -SE_snow_days_1_aug, 
         -early_depth_se,-late_depth_se,-early_density_se,-late_density_se) %>%
  rename(early_depth_cm     = early_depth_nw,
         early_density_gcm3 = early_density_nw,
         late_depth_cm      = late_depth_nw,
         late_density_gcm3 =late_density_nw,
         snow_days         = NW_snow_days,
         snow_days_1_aug   = NW_snow_days_1_aug) %>%
  mutate(aspect = "NW") #adding new variable of aspect
  

data_se <- SnowData %>%
  select(-NW_snow_days, -NW_snow_days_1_aug,    #skip all NW variables
         -early_depth_nw,-late_depth_nw,-early_density_nw,-late_density_nw) %>%
  rename(early_depth_cm     = early_depth_se,
         early_density_gcm3 = early_density_se,
         late_depth_cm      = late_depth_se,
         late_density_gcm3 =late_density_se,
         snow_days         = SE_snow_days,
         snow_days_1_aug   = SE_snow_days_1_aug) %>%
  mutate(aspect = "SE") #adding new aspect variable

SnowData_nwse <- rbind(data_nw, data_se) %>%
  #Create SampleID variable by which to join multiple datasets:
  unite("SampleID", c("region", "site_site_no", "shrub_code", "aspect"), sep = "_" ,remove = F )
  
unique(SnowData_nwse$SampleID)               

#VEG DATA =========
#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
veg1 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 1)# sheet = "KNP_ALL")
names(veg1)
veg2 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 2)# sheet = "KNP_ALL")
names(veg2)
veg3 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 3)# sheet = "KNP_ALL")
names(veg3)
veg4 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 4)# sheet = "KNP_ALL")
names(veg4)
veg5 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 5)# sheet = "KNP_ALL")
names(veg5)
veg6 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 6)# sheet = "KNP_ALL")
names(veg6)
veg7 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 7)# sheet = "KNP_ALL")
names(veg7)
veg8 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 8)# sheet = "KNP_ALL")
names(veg8)
veg9 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 9)# sheet = "KNP_ALL")
names(veg9)
veg10 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 10)# sheet = "KNP_ALL")
names(veg10)
veg11 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 11)# sheet = "KNP_ALL")
names(veg11)
veg12 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 12)# sheet = "KNP_ALL")
names(veg12)
veg13 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 13)# sheet = "KNP_ALL")
names(veg13)
veg14 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 14)# sheet = "KNP_ALL")
names(veg14)
veg15 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 15)# sheet = "KNP_ALL")
names(veg15)
veg16 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 16)# sheet = "KNP_ALL")
names(veg16)
veg17 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 17)# sheet = "KNP_ALL")
names(veg17)
veg18 <- read_excel("AlpineShrubFloristicsCombined_MASTER_RAW.xlsx", sheet= 18)# sheet = "KNP_ALL")
names(veg18)

#Create MASTER SPECIES LIST:
sp1 <- select (veg1, Species)
sp2 <- select (veg2, Species)
sp3 <- select (veg3, Species)
sp4 <- select (veg4, Species)
sp5 <- select (veg5, Species)
sp6 <- select (veg6, Species)
sp7 <- select (veg7, Species)
sp8 <- select (veg8, Species)
sp9 <- select (veg9, Species)
sp10 <- select (veg10, Species)
sp11 <- select (veg11, Species)
sp12 <- select (veg12, Species)
sp13 <- select (veg13, Species)
sp14 <- select (veg14, Species)
sp15 <- select (veg15, Species)
sp16 <- select (veg16, Species)
sp17 <- select (veg17, Species)
sp18 <- select (veg18, Species)


#Create Master Species list: First bind all data:
sp <- as.data.frame(rbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,sp16,sp17,sp18))
# Then remove duplicates:
   SpeciesList <- gather(sp, variable, Species) %>% #convert to long format
  group_by(Species) %>%
  summarise(nDuplicated = n())
View(SpeciesList)
#write.csv(SpeciesList, file = "SpeciesList_SEM2.csv", row.names = F)


#FIX THAT VEG DATA many sites are mising=========
#Join the veg datasets to include SpeciesList and create a Master Veg file:
v1<- left_join(SpeciesList, veg1, by = "Species" ) 
v2<- left_join(v1, veg2, by = "Species" )

v3<- left_join(SpeciesList, veg2, by = "Species", suffix = c("", ".y") ) 
v4<- left_join(v3, veg2, by = "Species", suffix = c("", ".y") ) 
v5<- left_join(v4, veg2, by = "Species" , suffix = c("", ".y")) 
v6<- left_join(v5, veg2, by = "Species" , suffix = c("", ".y")) 
v7<- left_join(v6, veg2, by = "Species", suffix = c("", ".y") ) 
v8<- left_join(v7, veg2, by = "Species" , suffix = c("", ".y")) 
v9<- left_join(v8, veg2, by = "Species" , suffix = c("", ".y")) 
v10<- left_join(v9, veg2, by = "Species" , suffix = c("", ".y")) 
v11<- left_join(v10, veg2, by = "Species", suffix = c("", ".y") ) 
v12<- left_join(v11, veg2, by = "Species" , suffix = c("", ".y")) 
v13<- left_join(v12, veg2, by = "Species" , suffix = c("", ".y")) 
v14<- left_join(v13, veg2, by = "Species" , suffix = c("", ".y")) 
v15<- left_join(v14, veg2, by = "Species" , suffix = c("", ".y")) 
v16<- left_join(v15, veg2, by = "Species" , suffix = c("", ".y")) 
v17<- left_join(v16, veg2, by = "Species" , suffix = c("", ".y")) 
View(v17)

names(v17) #Many columns were duplicated ans x/y suffixes were adeed. Remove these columns 
#Remove duplicated columns if any: https://stackoverflow.com/questions/24142942/how-to-remove-duplicated-column-names-in-r

VegMaster_knp_bhp <- v17 %>%  select_at(vars(-ends_with(".y"))) %>% select(-nDuplicated)
names(VegMaster_knp_bhp)
unique(VegMaster_knp_bhp $ Species) # 190 Species


#Turn Sites into rows and species into columns to compute plant indices:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, SampleID, Cover, -Species)
#VegMaster_knp_bhp_long$Cover <- ifelse(is.na(VegMaster_knp_bhp_long$Cover)==TRUE,0,VegMaster_knp_bhp_long$Cover) #replace NA with 0
names(VegMaster_knp_bhp_long) #Cover needs to be numeric:
VegMaster_knp_bhp_long$Cover <- as.numeric(as.character(VegMaster_knp_bhp_long$Cover))

#Create veg matrix and compute plant species indices:
veg_matrix <- spread(VegMaster_knp_bhp_long, Species, value = Cover, fill=0) 

soil <- veg_matrix %>%
  select(SampleID, rock, litter, bare_ground) #keep rock, litter and bare_ground as these are none-plant variables

vege <- veg_matrix %>%
    select(-rock, -litter, -bare_ground) #remove rock, litter and bare_ground as these are none-plant variables

dim(vege)#150 101
vege$Richness  <- specnumber(vege[,2:101])
vege$Cover   <- rowSums(vege[,2:101])
vege$Diversity <- diversity(vege[,2:101])

plot(vege$Richness)
plot(vege$Cover)
plot(vege$Diversity) 
  
#WIND DATA=========
wind <- read_excel("WindData_SEM.xlsx", sheet = "WindData")
str(wind)

#Join  vege DATA (vege + soil) with SNOW DATA and WIND DATA (alpine.data.csv)======
names(vege)
names(SnowData_nwse)
unique(vege$SampleID)

VegSnowData <- left_join(SnowData_nwse, vege, by = "SampleID")
VegSnowSoilData <- left_join(VegSnowData, soil, by = "SampleID")
VegSnowSoilWindData <- left_join(VegSnowSoilData, wind, by = "SampleID")
str(VegSnowSoilWindData)
write.csv(VegSnowSoilWindData, file = "VegSnowSoilWindData_SEM.csv", row.names = F)

