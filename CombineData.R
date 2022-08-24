#Load these packages into your working environment:
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require("readxl")) install.packages("readxl"); library("readxl")
if (!require("vegan")) install.packages("vegan"); library("vegan")

library("readxl")
library("tidyverse")
library("vegan")
library(lme4)
library(lmerTest)


#SNOW DATA ==========
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
SnowData <- read.csv("alpine_data.csv", header = TRUE)#
View(SnowData)
names(SnowData) #all names until column 22 are NW-related
<<<<<<< HEAD
unique(SnowData$site_site_no) #These are Sites
unique(SnowData$year)#2015 2016 2017 2018 - for years of snow measurements and veg survey was done in 2019
=======
unique(SnowData$site_site_no) #These are Samapling Units (SampleI)
unique(SnowData$year)#2015 2016 2017 2018 - for years of snow measurements and veg survey was done in 2019

#NOTES:
# "SE_snow_days_1_aug" = snow day since 1 aug showing more snow on leeward (1_aug marks end of winter)
# NW_snow_days_1_aug = snow day since 1 aug showing more snow on windward
#Wind data from both sides. SE Wind / NW Wind = ratio to be found by Suze.

>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44


#NOTES:
# "SE_snow_days_1_aug" = snow day since 1 aug showing more snow on leeward (1_aug marks end of winter)
# "NW_snow_days_1_aug" = snow day since 1 aug showing more snow on windward


#Ratio in snow days:
SnowData$SE2NW_SnowDays <- SnowData$SE_snow_days_1_aug / SnowData$NW_snow_days_1_aug

#Plot SE of snow days by year:
ggplot(SnowData, aes(x = height_cm, y = SE_snow_days_1_aug)) +
  geom_point()+
  scale_y_continuous(limits = c(0,100))+
  facet_grid(.~year) +
  ggtitle("SE (leeward)")+
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.text.x = element_text(size = 12, angle=90, color = "black"),
        axis.title.y=element_text(size=14),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

#ggsave("SE_snow_days_1_aug.jpg", height = 6.3, width = 8)


#Plot NW of snow days by year:
ggplot(SnowData, aes(x = height_cm, y = NW_snow_days_1_aug)) +
  geom_point()+
  facet_grid(.~year)+
  ggtitle("NW (windward)")+
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.text.x = element_text(size = 12, angle=90, color = "black"),
        axis.title.y=element_text(size=14),
        panel.grid.minor.x = element_blank(),
        plot.title = element_text(color = "black", size = 18, face = "bold"),
        plot.subtitle = element_text(color = "red"),
        strip.text=element_text(size=20))

#ggsave("NW_snow_days_1_aug.jpg", height = 6.3, width = 8)

#Remove heath and grass  (no veg surveys for Closed.heath, no aspects in both) :
SnowData2 <- SnowData %>%
  filter(! shrub =="Open.grassy") %>%  #Exclude grassy areas as these had no aspect accounted for
  filter(! shrub =="Closed.heath")      #Exclude heathy areas as these had no aspect accounted for


leeward_snow <- SnowData2$SE_snow_days
windward_snow <- SnowData2$NW_snow_days
boxplot(leeward_snow-windward_snow, main = "SE_snow_days - NW_snow_days", ylab= "Difference in snow days")

leeward_snow <- SnowData2$SE_snow_days_1_aug
windward_snow <- SnowData2$NW_snow_days_1_aug
boxplot(leeward_snow-windward_snow, main = "SE_snow_days - NW_snow_days", ylab= "Difference in snow days past Aug01")

leeward_snow <- SnowData2$SE_snow_days_1_aug
windward_snow <- SnowData2$NW_snow_days_1_aug
boxplot(leeward_snow/windward_snow, main = "SE_snow_days / NW_snow_days", ylab= "SE/NW Ratio of snow days past Aug01")
#Exported as"Snow_1aug_SE2NW_Ratio_AlpineData.jpg", height = 6.3, width = 8)

#Find site with max ratio:
SnowData2 [ which.max(SnowData2$SE2NW_SnowDays), ]




#Seperate SE columns from NW columns and create "aspect" column (SE vs NW)
data_nw <- SnowData %>%
  #skip all SE variables
  select(-SE_snow_days, -SE_snow_days_1_aug, 
         -early_depth_se,-late_depth_se,-early_density_se,-late_density_se) %>%
  mutate(aspect = "NW") %>%  #adding new variable of aspect
  #To merge with data and avoid repeated measures, take average across years of all SEM  variables:
<<<<<<< HEAD
  group_by(region, site_site_no, shrub_code,shrub, aspect) %>%  #that is components of SampleID
=======
  group_by(region, site_site_no, shrub_code, aspect) %>%  #thas is components of SampleID
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
  summarise (early_depth_cm = mean(early_depth_nw,na.rm = T),
         early_density_gcm3 = mean(early_density_nw,na.rm = T),
         late_depth_cm      = mean(late_depth_nw,na.rm = T),
         late_density_gcm3  =  mean(late_density_nw,na.rm = T),
<<<<<<< HEAD
         
         late_snow_depth_ratio  =  mean(late_depth_ratio,na.rm = T),
         late_snow_density_ratio = mean(late_density_ratio,na.rm = T),
         
=======
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
         snow_days          =  mean(NW_snow_days,na.rm = T),
         snow_days_1_aug    =  mean(NW_snow_days_1_aug,na.rm = T),
         height_cm          = mean (height_cm ,na.rm = T),
         area_cm3           = mean (area_cm3 ,na.rm = T),
<<<<<<< HEAD
         LAI    = mean(leaf.area.index, na.rm=T))

#View(data_nw)
=======
         leaf.area.index    = mean(leaf.area.index, na.rm=T))
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44

data_se <- SnowData %>%
  select(-NW_snow_days, -NW_snow_days_1_aug,    #skip all NW variables
         -early_depth_nw,-late_depth_nw,-early_density_nw,-late_density_nw) %>%
  mutate(aspect = "SE") %>% #adding new aspect variable
  #To merge with data and avoid repeated measures, take average across years of all SEM  variables:
<<<<<<< HEAD
  group_by(region, site_site_no, shrub_code,shrub, aspect) %>% #thas is components of SampleID
=======
  group_by(region, site_site_no, shrub_code, aspect) %>% #thas is components of SampleID
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
  summarise(early_depth_cm     =  mean(early_depth_se, na.rm = T),
         early_density_gcm3    = mean(early_density_se,na.rm = T),
         late_depth_cm         = mean(late_depth_se,na.rm = T),
         late_density_gcm3     = mean(late_density_se,na.rm = T),
<<<<<<< HEAD
         
         
         late_snow_depth_ratio  =  mean(late_depth_ratio,na.rm = T),
         late_snow_density_ratio = mean(late_density_ratio,na.rm = T),
         
         
         
=======
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
         snow_days             = mean(SE_snow_days,na.rm = T),
         snow_days_1_aug    =  mean(SE_snow_days_1_aug,na.rm = T),
         height_cm          = mean (height_cm ,na.rm = T),
         area_cm3           = mean (area_cm3 ,na.rm = T),
<<<<<<< HEAD
         LAI    = mean(leaf.area.index, na.rm=T))


data_short <- SnowData %>%
unite( "PreSampleID", c('region', 'site_site_no', 'shrub_code'))
unique(data_short$PreSampleID) #[187] sites
187*2 # = 374 which is exactly the plot number of  SnowData_nwse. It looks like data for SE and NW  is fully alligned.
=======
         leaf.area.index    = mean(leaf.area.index, na.rm=T))


>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44

#BIND NW and SE together:
SnowData_nwse <- rbind(data_nw, data_se) %>%
  #Create SampleID variable by which to join multiple datasets:
  unite("SampleID", c("region", "site_site_no", "shrub_code", "aspect"), sep = "_" ,remove = F ) 
<<<<<<< HEAD

class(SnowData_nwse) #It is turned to tibble. reformat bacl to data.frame
SnowData_nwse <- as.data.frame(SnowData_nwse)
unique(SnowData_nwse$SampleID)#374  observational units of snow data (I will call it SampledID and use to merge with Veg and Winda data)

#write.csv(SnowData_nwse, file = "SnowDataPlots.csv", row.names = F)
duplicateSnow <- as.factor(SnowData_nwse[duplicated(SnowData_nwse$SampleID),]) #checking if any columns, apart from SpecId,  are duplicated. NONE! YAY!
duplicateSnow #NONE!


#LMER in snow data:=======
#snow_days_1_aug_model============
SnowData <- read.csv("SnowDataPlots.csv")
head(SnowData)
snow_days_1_aug_model <- lmer(snow_days_1_aug ~ aspect + height_cm + (1|region), data = SnowData_nwse)
summary(snow_days_1_aug_model)#No effect
#Fixed effects:
#Estimate Std. Error        df t value Pr(>|t|)    
#(Intercept)    64.31301    3.58789  35.52152  17.925   <2e-16 ***
#aspectSE        0.72085    1.05099 279.51242   0.686   0.4934    
#height_cm       0.07514    0.03776 179.94969   1.990   0.0481 *  


=======
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44

str(SnowData_nwse) #It is teurned to tibble. reformat bacl to data.frame
SnowData_nwse <- as.data.frame(SnowData_nwse)
unique(SnowData_nwse$SampleID)#374             
duplicateSnow <- as.factor(SnowData_nwse[duplicated(SnowData_nwse$SampleID),]) #checking if any columns, apart from SpecId,  are duplicated. NONE! YAY!
duplicateSnow
#VEG DATA =========
<<<<<<< HEAD
#PAWEL WENT  BACK TO RAW DATA AND CREATE SpecID column for merging thse files +CHECKED FOR DUPLICATED FILE
=======
#PAWEL WENT  BACK TO RAW DATA AND CREATE SpeciesID AND ChECK FOR DUPLICATES ON THEM
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
#Remove dots and (((((())))))-s, fill with zeros empty cells.
#LOTS OF SPACES between Genus and species!!!!

#Then check for duplicates again:
##https://stackoverflow.com/questions/30664012/extract-column-name-to-vector-by-calling-the-column-name

<<<<<<< HEAD
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM/FloristicsRawCleaned")
files <- list.files()
files #19 csv files

#The Veg Data was cleaned and duplicates removed as per Duplicates.R file :
=======
#Load DAta from Master file (Previously called: "Alpine Shrub Floristics combined_MASTER.csv")
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM/FloristicsRawCleaned")
files <- list.files()
files

#The Veg Data was cleaned and duplicats removes as per Duplicates.R file :
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
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
<<<<<<< HEAD
names(veg8)
=======
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
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
<<<<<<< HEAD
veg18 <- read.csv("BHP_OpenGrassyData2022_CLEAN.csv")%>% select(-Species, -species, -Genus) #Our OG
# OG plots were recorded once and subsequently duplicated to match NW-SE design from all plots before.
names(veg18) #25

#Check if any no sites are missing from the trimmed file of original veg1 (many duplicated records were there before):
veg1_orig <- read.csv("1BHPshrubdata_CLEAN.csv") %>% select(-Species, -species, -Genus)
colnames(veg1_orig) #94
colnames(veg17) #236 SampleID +SpeciesID
#Check if none left behind:
veg1_check <- veg1_orig[ , which ( colnames(veg1_orig) %in% colnames(veg17))]
colnames(veg1_check)#NULL has been left behind = GOOD NEWS!

#Create MASTER SPECIES LIST (long format):
=======

#Check if any no sites are missing from the timmed file of original veg1:
veg1_orig <- read.csv("1BHPshrubdata_CLEAN.csv") %>% select(-Species, -species, -Genus)
colnames(veg1_orig) #94
colnames(v17) #234 SampleID +SpeciesID
#Check if none left behind:
veg1_check <- veg1_orig[ , which ( colnames(veg1_orig) %in% colnames(veg17))]
colnames(veg1_check)#None left behind.

#Create MASTER SPECIES LIST:
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
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
<<<<<<< HEAD
sp18 <- select (veg18, SpecID)

#Create Master Species list: First bind all data:
sp <- as.data.frame(rbind(sp1,sp2,sp3,sp4,sp5,sp6,sp7,sp8,sp9,sp10,sp11,sp12,sp13,sp14,sp15,sp16,sp17,sp18))
=======

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
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44

# Then create wide format and  remove duplicates:
SpeciesList <- gather(sp, variable, SpecID) %>% #convert to long format
               group_by(SpecID) %>%
               summarise(nDuplicated = n())
head(SpeciesList)  
SpeciesList[duplicated(SpeciesList$SpecID),] #Duplicate-Check. ZERO duplicated! YAY!
unique(SpeciesList$SpecID)#116 plant species
#write.csv(SpeciesList, file = "SpeciesList_SEM6.csv", row.names = F)

#JOIN VEG setss=========
#Join all veg datasets to include SpeciesList and create a Master Veg file:
#Use SpecID to bind data by species!!!
<<<<<<< HEAD
v1<- left_join(SpeciesList, veg1, by = "SpecID" ,fill=0) 
=======
v1<- left_join(SpeciesList, veg1, by = "SpecID" ) 
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
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
<<<<<<< HEAD
v18<- left_join(v17, veg18, by = "SpecID" , suffix = c("", ".y")) 

names(v18) #260 Columns. Many columns were duplicated and x/y suffixes were added. Remove .y columns 
#Remove duplicated columns if any: https://stackoverflow.com/questions/24142942/how-to-remove-duplicated-column-names-in-r
#Check which ones are duplicated:
dupli <- v18 %>%  select_at(vars(ends_with(".y")))  
dupli #NONE NOW. YAY! I cleaned all the raw files off duplicates and triplicates

v18[is.na(v18)] <-0 #Replace NA with 0:


VegMaster_knp_bhp <- v18 %>%  
  select_at(vars(-ends_with(".y"))) %>% #Remove duplicated columns
  select(-nDuplicated) #Remove nDuplicated column

names(VegMaster_knp_bhp)#258 observational units + SpecID column

unique(VegMaster_knp_bhp $ SpecID) # 116 Species including litt, rock and bare!
VegMaster_knp_bhp <- as.data.frame(VegMaster_knp_bhp)# reformat from tibble to data.frame
#View(VegMaster_knp_bhp)


#Turn Sites into rows and species into columns to compute plant cover:
VegMaster_knp_bhp_long <- gather(VegMaster_knp_bhp, SampleID, Cover, -SpecID)%>%
  unite("ID", SpecID:SampleID, sep = "_", remove = F) %>% #Creating new ID to pull a mean on duplicated records
  group_by(ID, SpecID, SampleID)%>% #Way to remove duplicated entries
  summarise(GoodCover = mean(Cover, na.rm=T)) #pulling a mean cover aka GoodCover

duplicateID <- VegMaster_knp_bhp_long[duplicated(VegMaster_knp_bhp_long$ID),] #checking if any columns are duplicated
duplicateID #No duplicates. if they keep on popping up. Let's remove them with group by "ID" as per above

View(VegMaster_knp_bhp_long)
unique(VegMaster_knp_bhp_long$SampleID) #258 observational units in veg data.
dim(VegMaster_knp_bhp_long)#29928     4
VegMaster_knp_bhp_long <- as.data.frame(VegMaster_knp_bhp_long) #Change from tibble (problematic for spread and gather function) to data.frame


#Join LIFE Forms & Compute % Contributions ======= 
#form data and compute % contributions of each groth form:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
LifeFormData <- read.csv("SpecID_LifeForms.csv")
names(LifeFormData)#Columns: SpecID","Species","ReconcileDuplicated" "Propose.New.SpecID"  "LifeFormShort", "LifeForm"  

VegMaster_knp_bhp_long_forms <- left_join(VegMaster_knp_bhp_long, LifeFormData , by = "SpecID" )
#View(VegMaster_knp_bhp_long_forms)

#Compute % Contribution for each SampleID:
VegMaster_LifeForm <- VegMaster_knp_bhp_long_forms %>%
                         group_by(SampleID, LifeForm) %>%
                        summarise(LifeFormSum = sum(GoodCover))  %>% na.omit()

#View(VegMaster_LifeForm)

VegMaster_LifeForm_wide <-  spread(VegMaster_LifeForm, LifeForm, LifeFormSum)
names(VegMaster_LifeForm_wide)

VegMaster_LifeForm_wide_perc <- VegMaster_LifeForm_wide %>%
  mutate( TotalCover = Bare+Grass+Herb+ Litter+ Moss+Rock+Shrub) %>% #Get total Cover 
  mutate( Bare_perc = round ( Bare/TotalCover*100,1)) %>%
  mutate( Grass_perc = round ( Grass/TotalCover*100,1)) %>%
  mutate( Herb_perc = round ( Herb/TotalCover*100,1)) %>%
  mutate( Litter_perc = round ( Litter/TotalCover*100,1)) %>%
  mutate( Moss_perc = round ( Moss/TotalCover*100,1))%>%
  mutate( Rock_perc = round ( Rock/TotalCover*100,1))%>%
  mutate( Shrub_perc = round ( Shrub/TotalCover*100,1))

View( VegMaster_LifeForm_wide_perc )

#Wrangle for ggploting:
VegMaster_LifeForm_wide_perc2 <- VegMaster_LifeForm_wide_perc  %>%
  select(SampleID, Bare_perc,Grass_perc,Herb_perc, Litter_perc,  Moss_perc,   Rock_perc , Shrub_perc ) %>%
  gather(key = "Form", value =  "Percent", -SampleID) %>%
  separate(SampleID, into = c("region", "site", "shrub_code", "Aspect"), remove = F) %>%
  filter(Aspect == "NW" | Aspect =="SE") %>%
  filter(Form != "Moss_perc") %>%
  mutate (OpenGrass =ifelse(grepl('OG',shrub_code), 'Yes', 'No')) %>%
  filter(OpenGrass == "No")

head(VegMaster_LifeForm_wide_perc2)
unique(VegMaster_LifeForm_wide_perc2$Form)# "Bare_perc"   "Grass_perc"  "Herb_perc"   "Litter_perc" "Rock_perc"   "Shrub_perc" 
VegMaster_LifeForm_wide_perc2$Form <- factor(VegMaster_LifeForm_wide_perc2$Form, levels = c("Shrub_perc","Herb_perc","Grass_perc","Bare_perc" ,  "Rock_perc" ,"Litter_perc" ))

ggplot(data = VegMaster_LifeForm_wide_perc2 , aes(x= Aspect, y=Percent))+
  geom_boxplot(outlier.shape = NA) +geom_jitter(aes(color = Aspect)) +
  facet_wrap(.~Form)+
  #ggtitle("Richness is significantly higher at high shrubs (P = 0.0118)",
  # subtitle = "open_grass <0,1cm>, low_shrubs <1,50cm>, high_shrubs <50,110cm>")+
  labs(x = "Aspect", y = "Percent Contribution (%)", color = "Height (cm)")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 12, color= "black"),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = "none",               #c(0.2, 0.5)
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#Shrub contribution higher in NW
summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Shrub_perc",]))
#Estimate Std. Error      df t value Pr(>|t|)  
#(Intercept)   16.478      7.888   1.056   2.089   0.2737  
#AspectSE      -5.109      2.295 205.254  -2.226   0.0271 *
  
  
#No effect of aspect on Herb%:
summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Herb_perc",]))

#No effect of aspect on Grass %
summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Grass_perc",]))


summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Bare_perc",]))

summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Rock_perc",]))

summary (lmer(Percent ~ Aspect + (1|region)+(1|shrub_code),
              data = VegMaster_LifeForm_wide_perc2[VegMaster_LifeForm_wide_perc2$Form=="Litter_perc",]))



#Create veg matrix and compute plant species indices:
VegMaster_knp_bhp_long_NoID <- select (VegMaster_knp_bhp_long, -ID) #Remove ID which we used to remove duplicates
  
veg_matrix <- spread(VegMaster_knp_bhp_long_NoID,  SpecID, value = GoodCover, fill=0)
dim(veg_matrix)#258 117 - each sampling unit has its own row now.
#write.csv(veg_matrix, file = "Veg_matrix_SEM6.csv", row.names = F)
setwd( "C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
veg_check <- read.csv("Veg_matrix_SEM6.csv")
unique(veg_check$SampleID) #258
=======

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
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44

#VEG INDICES:========
#Separate soil data from veg data:
soil <- veg_matrix %>%
  select(SampleID, rock, litt, bare) #keep rock, litter and bare ground as these are none-plant variables

vege <- veg_matrix %>%
    select(-rock, -litt, -bare) #remove rock, litter and bare_ground as these are none-plant variables

<<<<<<< HEAD
names(vege)#114
dim(vege)# 258 114
vege$Richness  <- specnumber(vege[,2:114])
vege$Cover   <- rowSums(vege[,2:114])
vege$Diversity <- diversity(vege[,2:114])
vege$SampleID_FromVegData <- vege$SampleID


#Plot to see raw data:
=======
names(vege)#122
dim(vege)# 234 122
vege$Richness  <- specnumber(vege[,2:122])
vege$Cover   <- rowSums(vege[,2:122])
vege$Diversity <- diversity(vege[,2:122])
vege$SampleID_FromVegData <- vege$SampleID

>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
par(mfrow=c(1,1))
plot(vege$Richness, main = "Alpine Plant Richness")
plot(vege$Cover, main = "Alpine Plant Cover")
plot(vege$Diversity) 

<<<<<<< HEAD
#Join vege data with percent data:
vege2 <- left_join(vege,VegMaster_LifeForm_wide_perc, by = "SampleID" )
names(vege2)


#WIND DATA=========
#Go back to project directory:
#if At Work comp:
#setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
#if at Private come:
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
=======

#WIND DATA=========
#Go back to project directory:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
wind <- read_excel("WindData_SEM.xlsx", sheet = "WindData")
str(wind)# tibble [84 x 11]

#Join  vege DATA (vege + soil) with SNOW DATA and WIND DATA (alpine.data.csv)======
names(SnowData_nwse)
<<<<<<< HEAD
unique(vege$SampleID) #258
unique(SnowData_nwse$SampleID) #374 - these data have more sampling units


#Find Plots missing in Snow and Veg data=========
#Checking how well snow and vege data combine with snow data:
#anti_join(x, y, by = "ID") gives rows in x that was not found in y using the ID.

FromSnow <- anti_join(SnowData_nwse, vege, by = "SampleID")
FromSnow$SampleID #132 sampling units in snow data do not match  veg data
SnowSitesNotFoundInVege <- as.data.frame(FromSnow$SampleID)
SnowSitesNotFoundInVege
=======
unique(vege$SampleID)
unique(SnowData_nwse$SampleID)

#Check how well snow and vege data combine:
#anti_join(x, y, by = "ID") gives rows in x that was not found in y using the ID.

FromSnow <- anti_join(SnowData_nwse, vege, by = "SampleID")
FromSnow$SampleID
SnowSitesNotFoundInVege <- as.data.frame(FromSnow$SampleID)
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
#write.csv(SnowSitesNotFoundInVege , file = "SnowSitesNotFoundInVege.csv", row.names = F)

FromVege <- anti_join(vege, SnowData_nwse, by = "SampleID")
FromVege$SampleID
VegSitesNotFoundInSnow <- as.data.frame(FromVege$SampleID)
<<<<<<< HEAD
VegSitesNotFoundInSnow
#write.csv(VegSitesNotFoundInSnow, file = "VegSitesNotFoundInSnow.csv", row.names = F)

#Combine Snow, Wind and Veg data===========
VegSnowData <- left_join(SnowData_nwse, vege2, by = "SampleID")
=======
write.csv(VegSitesNotFoundInSnow, file = "VegSitesNotFoundInSnow.csv", row.names = F)

#Combine Snow, Wind and Veg data===========
VegSnowData <- left_join(SnowData_nwse, vege, by = "SampleID")
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
VegSnowSoilData <- left_join(VegSnowData, soil, by = "SampleID")
VegSnowSoilWindData <- left_join(VegSnowSoilData, wind, by = "SampleID")
str(VegSnowSoilWindData)#data.frame':	374 obs. of  162 variables:'
names(VegSnowSoilWindData)
<<<<<<< HEAD


#Update snow_days data (processed in Excel by Pawel, May 2022)=========
setwd("C:/Users/poles/Documents/00Deakin_Docs/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
dat <- VegSnowSoilWindData
names(dat)
unique(dat$SampleID)#374

#Join dat with newly processed snow_days data (By Pawel on 01-June-2022):
new_snow_data <- read.csv("ToMerge_NewSnowDays_DoneManuallyInExcle_ByPawel.csv")
head(new_snow_data)

#Select snow variables and average by plot:
dat_new <- new_snow_data %>%
  select(SampleID, Year, snow_days_NEW, snow_days_1_aug_NEW) %>%
  group_by(SampleID) %>%
  summarise(snow_days_NEW2 = mean(snow_days_NEW),
            snow_days_1_aug_NEW2 = mean(snow_days_1_aug_NEW))



dat2 <- left_join(dat, dat_new, by = "SampleID")
View(dat2)

#Check if these variables were recorded before
dat3 <- dat2 %>%
  select(SampleID, snow_days_1_aug, snow_days_1_aug_NEW2,snow_days,snow_days_NEW2)
View(dat3)

#Yes, most of the snow records were there from other years. Take mean and join with Master "dat" data:
#snow_days_1_aug
x<- dat3 %>%
  select(snow_days_1_aug, snow_days_1_aug_NEW2)
dat3$snow_days_1_aug_average <- rowMeans(x, na.rm = T) 

#snow_days
y<- dat3 %>%
  select(snow_days, snow_days_NEW2)
dat3$snow_days_average <- rowMeans(y, na.rm = T) 

View(dat3)

dat4 <- dat3 %>%
  select(SampleID, snow_days_average, snow_days_1_aug_average)
#Join average snow data and use in lm:
dat_new2 <- left_join(dat, dat4, by = "SampleID")
dim(dat_new2)# 374 175

#MASTER Snowdrift data file======
#write.csv(dat_new2, file = "VegSnowSoilWindData_SEM7.csv", row.names = F) #THIS IS OUR MASTER DATA FILE!
=======
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
>>>>>>> ddb99dfe33e49a32453ade7f6c746ef8fb458a44
