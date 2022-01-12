#Load these packages into your working environment
library("readxl")
library("tidyverse")
library("vegan")

#SNOW DATA ==========
SnowData <- read.csv("alpine_data.csv", header = TRUE)#
names(SnowData) #all names until column 22 are NW-related
unique(SnowData$site_site_no) #These are Samapling Units (SampleI)
unique(SnowData$year)#2015 2016 2017 2018 - for years of snow measurements and veg survey was done in 2019

#NOTES:
# "SE_snow_days_1_aug" = snow day since 1 aug showing more snow on leeward (1_aug marks end of winter)
# NW_snow_days_1_aug = snow day since 1 aug showing more snow on windward
#Wind data from both sides. SE Wind / NW Wind = ratio to be found by Suze.


#Seperate SE columns from NW columns and create "aspect" variable (SE vs NW)
data_nw <- SnowData %>%
  #skip all SE variables
  select(-SE_snow_days, -SE_snow_days_1_aug, 
         -early_depth_se,-late_depth_se,-early_density_se,-late_density_se) %>%
  mutate(aspect = "NW") %>%  #adding new variable of aspect
  #To merge with data and avoid repeated measures, take average across years of all SEM  variables:
  group_by(region, site_site_no, shrub_code, aspect) %>%  #thas is components of SampleID
  summarise (early_depth_cm = mean(early_depth_nw,na.rm = T),
         early_density_gcm3 = mean(early_density_nw,na.rm = T),
         late_depth_cm      = mean(late_depth_nw,na.rm = T),
         late_density_gcm3  =  mean(late_density_nw,na.rm = T),
         snow_days          =  mean(NW_snow_days,na.rm = T),
         snow_days_1_aug    =  mean(NW_snow_days_1_aug,na.rm = T),
         height_cm          = mean (height_cm ,na.rm = T),
         area_cm3           = mean (area_cm3 ,na.rm = T),
         leaf.area.index    = mean(leaf.area.index, na.rm=T))

data_se <- SnowData %>%
  select(-NW_snow_days, -NW_snow_days_1_aug,    #skip all NW variables
         -early_depth_nw,-late_depth_nw,-early_density_nw,-late_density_nw) %>%
  mutate(aspect = "SE") %>% #adding new aspect variable
  #To merge with data and avoid repeated measures, take average across years of all SEM  variables:
  group_by(region, site_site_no, shrub_code, aspect) %>% #thas is components of SampleID
  summarise(early_depth_cm     =  mean(early_depth_se, na.rm = T),
         early_density_gcm3    = mean(early_density_se,na.rm = T),
         late_depth_cm         = mean(late_depth_se,na.rm = T),
         late_density_gcm3     = mean(late_density_se,na.rm = T),
         snow_days             = mean(SE_snow_days,na.rm = T),
         snow_days_1_aug    =  mean(SE_snow_days_1_aug,na.rm = T),
         height_cm          = mean (height_cm ,na.rm = T),
         area_cm3           = mean (area_cm3 ,na.rm = T),
         leaf.area.index    = mean(leaf.area.index, na.rm=T))



#BIND NW and SE together:
SnowData_nwse <- rbind(data_nw, data_se) %>%
  #Create SampleID variable by which to join multiple datasets:
  unite("SampleID", c("region", "site_site_no", "shrub_code", "aspect"), sep = "_" ,remove = F ) 

str(SnowData_nwse) #It is teurned to tibble. reformat bacl to data.frame
SnowData_nwse <- as.data.frame(SnowData_nwse)
unique(SnowData_nwse$SampleID)#374             
duplicateSnow <- as.factor(SnowData_nwse[duplicated(SnowData_nwse$SampleID),]) #checking if any columns, apart from SpecId,  are duplicated. NONE! YAY!
duplicateSnow
#VEG DATA =========
#PAWEL WENT  BACK TO RAW DATA AND CREATE SpeciesID AND ChECK FOR DUPLICATES ON THEM
#Remove dots and (((((())))))-s, fill with zeros empty cells.
#LOTS OF SPACES between Genus and species!!!!

#Then check for duplicates again:
##https://stackoverflow.com/questions/30664012/extract-column-name-to-vector-by-calling-the-column-name

#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM/FloristicsRawCleaned")
files <- list.files()
files

#The Veg Data was cleaned and duplicats removes as per Duplicates.R file :
veg1 <- read.csv("1BHPshrubdata_CLEAN_TRIMMED.csv")
names(veg1)
veg2 <- read.csv('1Bundarashrubdata_CLEAN.csv') %>% select(-Species, -species, -Genus)
names(veg2)
veg3 <- read.csv("1JimAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg3)
veg4 <- read.csv("1KnollAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg4)
veg5 <- read.csv("1MarumAngeShrubData_CLEAN.csv" )%>% select(-Species, -species, -Genus)
names(veg5)
veg6 <- read.csv("1MarumShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg6)
veg7 <- read.csv("1Rolling1PhebShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg7)
veg8 <- read.csv("1Rolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
veg9 <- read.csv("1RuinedAngeShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg9)
veg10 <- read.csv("1SaddlePShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg10)
veg11 <- read.csv("1SS21ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg11)
veg12 <- read.csv("1SS2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg12)
veg13 <- read.csv("1SS3ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg13)
veg14 <- read.csv("1ThredboRolling1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
veg15 <- read.csv("1ThredboShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg15)
veg16 <- read.csv("1Watchbed1ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg16)
veg17 <- read.csv("1Watchbed2ShrubData_CLEAN.csv")%>% select(-Species, -species, -Genus)
names(veg17)

#Check if any no sites are missing from the timmed file of original veg1:
veg1_orig <- read.csv("1BHPshrubdata_CLEAN.csv") %>% select(-Species, -species, -Genus)
colnames(veg1_orig) #94
colnames(v17) #234 SampleID +SpeciesID
#Check if none left behind:
veg1_check <- veg1_orig[ , which ( colnames(veg1_orig) %in% colnames(veg17))]
colnames(veg1_check)#None left behind.

#Create MASTER SPECIES LIST:
sp1 <- select (veg1, SpecID)
sp2 <- select (veg2, SpecID)
sp3 <- select (veg3, SpecID)
sp4 <- select (veg4, SpecID)
sp5 <- select (veg5, SpecID)
sp6 <- select (veg6, SpecID)
sp7 <- select (veg7, SpecID)
sp8 <- select (veg8, SpecID)
sp9 <- select (veg9, SpecID)
sp10 <- select (veg10, SpecID)
sp11 <- select (veg11, SpecID)
sp12 <- select (veg12, SpecID)
sp13 <- select (veg13, SpecID)
sp14 <- select (veg14, SpecID)
sp15 <- select (veg15, SpecID)
sp16 <- select (veg16, SpecID)
sp17 <- select (veg17, SpecID)

#Create Master Species list: First bind all data:
sp <- as.data.frame(rbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,sp16,sp17))
# Then remove duplicates:
  SpeciesList <- gather(sp, variable, SpecID) %>% #convert to long format
  group_by(SpecID) %>%
  summarise(nDuplicated = n())
  
SpeciesList<-SpeciesList[-1,] #remove empty cell
SpeciesList[duplicated(SpeciesList$SpecID),]
unique(SpeciesList$SpecID)#124 plant species
#write.csv(SpeciesList, file = "SpeciesList_SEM4.csv", row.names = F)


#JOIN VEG setss=========
#Join all veg datasets to include SpeciesList and create a Master Veg file:
#Use SpecID to bind data by species!!!
v1<- left_join(SpeciesList, veg1, by = "SpecID" ) 
v2<- left_join(v1, veg2, by = "SpecID" )
v3<- left_join(v2, veg3, by = "SpecID", suffix = c("", ".y") ) 
v4<- left_join(v3, veg4, by = "SpecID", suffix = c("", ".y") )
v5<- left_join(v4, veg5, by = "SpecID" , suffix = c("", ".y")) 
v6<- left_join(v5, veg6, by = "SpecID" , suffix = c("", ".y")) 
v7<- left_join(v6, veg7, by = "SpecID", suffix = c("", ".y") ) 
v8<- left_join(v7, veg8, by = "SpecID" , suffix = c("", ".y")) 
v9<- left_join(v8, veg9, by = "SpecID" , suffix = c("", ".y")) 
v10<- left_join(v9, veg10, by = "SpecID" , suffix = c("", ".y")) 
v11<- left_join(v10, veg11, by = "SpecID", suffix = c("", ".y") ) 
v12<- left_join(v11, veg12, by = "SpecID" , suffix = c("", ".y")) 
v13<- left_join(v12, veg13, by = "SpecID" , suffix = c("", ".y")) 
v14<- left_join(v13, veg14, by = "SpecID" , suffix = c("", ".y")) 
v15<- left_join(v14, veg15, by = "SpecID" , suffix = c("", ".y")) 
v16<- left_join(v15, veg16, by = "SpecID" , suffix = c("", ".y")) 
v17<- left_join(v16, veg17, by = "SpecID" , suffix = c("", ".y")) 

names(v17) #Many columns were duplicated and x/y suffixes were adeed. Remove .y columns 
#Remove duplicated columns if any: https://stackoverflow.com/questions/24142942/how-to-remove-duplicated-column-names-in-r
#Check which ones are duplicated:
dupli <- v17 %>%  select_at(vars(ends_with(".y")))  
dupli #NONE YAY! I cleaned all the raw files off dulicates and triplicates

VegMaster_knp_bhp <- v17 %>%  
  select_at(vars(-ends_with(".y"))) %>%
  select(-nDuplicated)
dim(VegMaster_knp_bhp)#235 distinct_columns (observational units)
unique(VegMaster_knp_bhp $ SpecID) # 124 Species
VegMaster_knp_bhp <- as.data.frame(VegMaster_knp_bhp)
View(VegMaster_knp_bhp)


#Turn Sites into rows and species into columns to compute plant indices:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, SampleID, Cover, -SpecID)%>%
  unite("ID", SpecID:SampleID, sep = "_", remove = F) %>% #Creating new ID to pull a mean on duplicated records
  group_by(ID, SpecID, SampleID)%>% #Way to remove duplicated entries
  summarise(GoodCover = mean(Cover, na.rm=T)) #pulling a mean cover aka GoodCover

duplicateID <- VegMaster_knp_bhp_long[duplicated(VegMaster_knp_bhp_long$ID),] #checking if any columns are duplicated
duplicateID #No duplicates. if they keep on popping up. Let's remove them with group by "ID" as per above

View(VegMaster_knp_bhp_long)
unique(VegMaster_knp_bhp_long$SampleID)
dim(VegMaster_knp_bhp_long)# 29016     4
VegMaster_knp_bhp_long <- as.data.frame(VegMaster_knp_bhp_long) #Change from tibble (problematic for spread and gather function) to data.frame

#Create veg matrix and compute plant species indices:
VegMaster_knp_bhp_long_NoID <- select (VegMaster_knp_bhp_long, -ID) #Remoe ID which we used to remover duplicates
  
veg_matrix <- spread(VegMaster_knp_bhp_long_NoID,  SpecID, value = GoodCover, fill=0)
dim(veg_matrix)#234 125 - each sampling unit has its own row now.
#write.csv(veg_matrix, file = "Veg_matrix_SEM2.csv", row.names = F)

#VEG INDICES:========
#Separate soil data from veg data:
soil <- veg_matrix %>%
  select(SampleID, rock, litt, bare) #keep rock, litter and bare ground as these are none-plant variables

vege <- veg_matrix %>%
    select(-rock, -litt, -bare) #remove rock, litter and bare_ground as these are none-plant variables

names(vege)#122
dim(vege)# 234 122
vege$Richness  <- specnumber(vege[,2:122])
vege$Cover   <- rowSums(vege[,2:122])
vege$Diversity <- diversity(vege[,2:122])
vege$SampleID_FromVegData <- vege$SampleID

par(mfrow=c(1,1))
plot(vege$Richness, main = "Alpine Plant Richness")
plot(vege$Cover, main = "Alpine Plant Cover")
plot(vege$Diversity) 


#WIND DATA=========
#Go back to project directory:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
wind <- read_excel("WindData_SEM.xlsx", sheet = "WindData")
str(wind)

#Join  vege DATA (vege + soil) with SNOW DATA and WIND DATA (alpine.data.csv)======
names(SnowData_nwse)
unique(vege$SampleID)
unique(SnowData_nwse$SampleID)

#Check how well snow and vege data combine:
#anti_join(x, y, by = "ID") gives rows in x that was not found in y using the ID.

FromSnow <- anti_join(SnowData_nwse, vege, by = "SampleID")
FromSnow$SampleID
SnowSitesNotFoundInVege <- as.data.frame(FromSnow$SampleID)
#write.csv(SnowSitesNotFoundInVege , file = "SnowSitesNotFoundInVege.csv", row.names = F)

FromVege <- anti_join(vege, SnowData_nwse, by = "SampleID")
FromVege$SampleID
VegSitesNotFoundInSnow <- as.data.frame(FromVege$SampleID)
write.csv(VegSitesNotFoundInSnow, file = "VegSitesNotFoundInSnow.csv", row.names = F)

#Combine Snow, Wind and Veg data===========
VegSnowData <- left_join(SnowData_nwse, vege, by = "SampleID")
VegSnowSoilData <- left_join(VegSnowData, soil, by = "SampleID")
VegSnowSoilWindData <- left_join(VegSnowSoilData, wind, by = "SampleID")
str(VegSnowSoilWindData)#data.frame':	374 obs. of  162 variables:'
names(VegSnowSoilWindData)
write.csv(VegSnowSoilWindData, file = "VegSnowSoilWindData_SEM2.csv", row.names = F)


#Run Regressions========
dat <- read.csv("VegSnowSoilWindData_SEM2.csv")
names(dat)
summary(lm(Wind_Ave ~ aspect, data = dat)) #YES = significant, P =1.71e-11 ***
summary(lm(litt ~ aspect, data = dat)) #NO
summary(lm(bare ~ aspect, data = dat)) #No
summary(lm(rock ~ aspect, data = dat)) #No
summary(lm(Richness ~ aspect, data = dat)) #YES! P = 0.036 *

summary(lm(snow_days ~ aspect, data = dat)) #No 
summary(lm(snow_days_1_aug ~ aspect, data = dat)) #No 
summary(lm(late_density_gcm3 ~ aspect, data = dat)) #No 
summary(lm(early_density_gcm3 ~ aspect, data = dat)) #YES, P=3.22e-08 ***
qplot(dat$aspect,dat$early_density_gcm3, geom = "boxplot")

summary(lm(early_depth_cm ~ aspect, data = dat)) #YES, P= 3.01e-05 ***
qplot(dat$aspect,dat$early_depth_cm, geom = "boxplot")
plot(lm(early_depth_cm ~ aspect, data = dat)) 

summary(lm(late_depth_cm ~ aspect, data = dat)) #No


[1] "SampleID"           "region"             "site_site_no"      
[4] "shrub_code"         "aspect"             "early_depth_cm"    
[7] "early_density_gcm3" "late_depth_cm"      "late_density_gcm3" 
[10] "snow_days"          "snow_days_1_aug"    "height_cm"    

#Create Path Analysis=======
#Apriori Model:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
veg_snow<- read.csv("VegSnowSoilWindData_SEM2.csv") #Joined Data
names(veg_snow)
unique(veg_snow$aspect)#"NW" "SE"

model_snow <- '

#latent variable definition:
    Drift        =~ LAI  + Wind  # Define the latent variable for Conditions for snow drift accumulation
    Snow =~ DepthLATE + DensLATE + DepthLATE +DensLATE
    Veg   =~  Rich + Cover + Litt + Bare
    
#regressions:
              Snow ~  Drift
              Drift ~  Veg  + Drift
 
 # residual covariances:   



#Plot Combined data======
snow <- read.csv("VegSnowSoilWindData_SEM2.csv") #Joined Data


names(snow)
#Plot variables in relation to aspect from combined data:
snow_long <- snow %>%
  select(aspect, early_depth_cm,
  #early_density_gcm3,late_depth_cm,late_density_gcm3, 
    #snow_days,snow_days_1_aug,height_cm,
  Wind_Max, Wind_Ave,
   leaf.area.index, bare, rock, litt) %>%
  gather(Treatment,Value, -aspect)

head(snow_long )
ggplot(snow_long, aes(x=aspect, y=Value,color=aspect)) +
  geom_boxplot() +
  geom_jitter() +                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  facet_grid(.~Treatment)+
  labs(y="Respective Value") +
  facet_grid(.~Treatment) +theme(legend.position = "none")
 

#Plot variables in relation to aspect from original data:
SnowData <- read.csv("alpine_data.csv", header = TRUE)#
names(SnowData)

snow_long <- SnowData %>%
  select(early_depth_nw, early_depth_se,
         late_depth_nw,late_depth_se,
         NW_snow_days_1_aug, SE_snow_days_1_aug, year) %>%
  gather(Treatment,Value, -year)

head(snow_long )
ggplot(snow_long, aes(x=Treatment, y=Value, color=year)) +
  geom_boxplot() +
  geom_jitter() +                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  facet_grid(.~Treatment)+
  labs(y="Respective Value", x="")+
  facet_grid(.~year)+theme(legend.position = "none",
                           axis.text.x=element_text(vjust=0.5,size=12, angle =90))
