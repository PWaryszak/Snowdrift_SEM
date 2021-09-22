#Load these packages into your working environment
library("readxl")
library("tidyverse")


#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
veg_knp <- read_excel("AlpineShrubFloristicsCombined_MASTER.xlsx", sheet = "KNP_ALL")
veg_knp[is.na(veg_knp)] <- 0 #replace NA with zero to form veg matrix

veg_bhp <- read_xlsx("AlpineShrubFloristicsCombined_MASTER.xlsx", sheet = "BHP_ALL")
veg_bhp[is.na(veg_bhp)] <- 0 #replace NA with zero to form veg matrix

#Form Master Species list:
sp <- as.data.frame(rbind(veg_knp$Species, veg_bhp$Species))
speciesID <- gather(sp, variable, Species) %>% #convert to long format
  group_by(Species) %>%
  summarise(nDuplicated = n())

#Get a list of all species as a separate data frame:
SpeciesList <- as.data.frame(speciesID$Species) 
colnames(SpeciesList)[1]<-"Species"  #Rename column to Species to allign with veg_knp and veg_bhp
View(SpeciesList)

#Join the two site datasets to combine SpeciesList:
VegMaster_knp <- left_join(SpeciesList, veg_knp, by = "Species" ) ##Master File of all species and sites:
VegMaster_knp_bhp <- left_join(VegMaster_knp, veg_bhp, by = "Species")
unique(VegMaster_knp_bhp $ Species) # 107 Species

#Turn Sites into rows and species into columns to compute plant indices:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, Site, Cover, -Species)
VegMaster_knp_bhp_long$Cover <- ifelse(is.na(VegMaster_knp_bhp_long$Cover)==TRUE,0,VegMaster_knp_bhp_long$Cover) #replace NA with 0
View(VegMaster_knp_bhp_long)

veg <- spread(VegMaster_knp_bhp_long, Species, value = Cover)
View(veg)

#Prepare veg data for joining alpine.data.csv for SEM======
names(veg)
unique(veg$Site)

#Split Site and figurout how to join with alpine.data (possibly separata block) with these columns as NA:
DATA <- DATA1 %>%  #selet variables that we used in SEM so far:
  select(height_cm, area_cm3,leaf.area.index,
         early_depth_se,early_density_se,early_depth_nw,early_density_nw,
         late_depth_se, late_density_se,late_depth_nw, late_density_nw)


#Filter out bare ground, litter and rock
ground <- select(veg, bare_ground, litter, rock)
