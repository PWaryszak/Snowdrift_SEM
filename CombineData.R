#Load these packages into your working environment
library("readxl")
library("tidyverse")
library("vegan")

#SNOW DATA ==========
SnowData <- read.csv("alpine_data.csv", header = TRUE)#
names(SnowData) #all names until column 22 are NW-related

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
                  

#VEG DATA========
#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
veg_knp <- read_excel("AlpineShrubFloristicsCombined_MASTER.xlsx", sheet = "KNP_ALL")
veg_knp[is.na(veg_knp)] <- 0 #replace NA with zero to form veg matrix

veg_bhp <- read_xlsx("AlpineShrubFloristicsCombined_MASTER.xlsx", sheet = "BHP_ALL")
veg_bhp[is.na(veg_bhp)] <- 0 #replace NA with zero to form veg matrix

#Create Master Species list:
sp <- as.data.frame(rbind(veg_knp$Species, veg_bhp$Species))
speciesID <- gather(sp, variable, Species) %>% #convert to long format
  group_by(Species) %>%
  summarise(nDuplicated = n())

#Get a list of all species as a separate data frame:
SpeciesList <- as.data.frame(speciesID$Species) 
colnames(SpeciesList)[1]<-"Species"  #Rename column to Species to allign with veg_knp and veg_bhp
View(SpeciesList)
#write.csv(SpeciesList, file = "SpeciesList_SEM.csv", row.names = F)

#Join the two site datasets to include SpeciesList:
VegMaster_knp <- left_join(SpeciesList, veg_knp, by = "Species" ) ##Master File of all species and sites:
VegMaster_knp_bhp <- left_join(VegMaster_knp, veg_bhp, by = "Species")
unique(VegMaster_knp_bhp $ Species) # 107 Species

#Turn Sites into rows and species into columns to compute plant indices:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, SampleID, Cover, -Species)
VegMaster_knp_bhp_long$Cover <- ifelse(is.na(VegMaster_knp_bhp_long$Cover)==TRUE,0,VegMaster_knp_bhp_long$Cover) #replace NA with 0
names(VegMaster_knp_bhp_long)

#Createveg matrix and compute plant species indices:
veg_matrix <- spread(VegMaster_knp_bhp_long, Species, value = Cover) 
soil <- veg_matrix %>%
  select(SampleID, rock, litter, bare_ground) #keep rock, litter and bare_ground as these are none-plant variables

veg <- veg_matrix %>%
    select(-rock, -litter, -bare_ground) #remove rock, litter and bare_ground as these are none-plant variables

dim(veg)#150 105
veg$Richness  <- specnumber(veg[,2:105])
veg$Cover   <- rowSums(veg[,2:105])
veg$Diversity <- diversity(veg[,2:105])

plot(veg$Richness)
plot(veg$Cover)
plot(veg$Diversity) 
  
#WIND DATA=========
wind <- read_excel("WindData_SEM.xlsx", sheet = "WindData")
str(wind)

#Join  VEG DATA (veg + soil) with SNOW DATA and WIND DATA (alpine.data.csv)======
names(veg)
names(SnowData_nwse)
unique(veg$SampleID)

VegSnowData <- left_join(SnowData_nwse, veg, by = "SampleID")
VegSnowSoilData <- left_join(VegSnowData, soil, by = "SampleID")
VegSnowSoilWindData <- left_join(VegSnowSoilData, wind, by = "SampleID")
str(VegSnowSoilWindData)
write.csv(VegSnowSoilWindData, file = "VegSnowSoilWindData_SEM.csv", row.names = F)

