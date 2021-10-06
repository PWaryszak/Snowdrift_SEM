#LOAD snow DATA and LIBRARIES:=========
library(lavaan)
library(qgraph)
library(semPlot)
library(tidyverse)

#Load snow prepared and c rued in Combinesnow.R file:
snow <- read.csv("VegSnowSoilWindsnow_SEM.csv")
names(snow)

#See Values:
par(mfrow=c(2,2))
plot(snow$height_cm)
plot(snow$area_cm3)
plot(snow$leaf.area.index)
plot(snow$early_depth_cm)
plot(snow$early_density_gcm3)
plot(snow$early_depth_cm )
plot(snow$early_density_gcm)
plot(snow$Richness)
plot(snow$bare_ground)

#Snow_mean=========
snow <- read.csv("VegSnowSoilWindsnow_SEM.csv")

snow <- snow %>%  mutate(aspect01 = ifelse(aspect=="SE", 1,0)) #SE leeward, NW = Windward,  turn aspect numeric to enable regressions

snow_av <- snow %>%
    select(SampleID, aspect01, height_cm, area_cm3, leaf.area.index,
           early_depth_cm,early_density_gcm3,late_depth_cm,late_density_gcm3,
           Richness, Diversity, bare_ground, rock, litter, Cover, Wind_Ave) %>%
  group_by(SampleID) %>%
  summarise_at(vars(aspect01:Wind_Ave), mean,na.rm=T)

View(snow_av)
snow_av_Cov <- select (snow_av, - SampleID)
cov(snow_av_Cov)

#Get means of variables by year because collected on different years but merged to all years (for example Richness, Wind_Ave):
#Standarize all numeric variables to compare effect sizes between them in SEM:
snow_av$Height <- (snow_av$height_cm - mean(snow_av$height_cm, na.rm=TRUE)) / sd(snow_av$height_cm, na.rm=TRUE)
snow_av$Area <- (snow_av$area_cm3 - mean(snow_av$area_cm3, na.rm=TRUE)) / sd(snow_av$area_cm3, na.rm=TRUE)
snow_av$LAI <- (snow_av$leaf.area.index - mean(snow_av$leaf.area.index, na.rm=TRUE)) / sd(snow_av$leaf.area.index, na.rm=TRUE)

#Early snow_av depth and density"
snow_av$DepthEARLY <- (snow_av$early_depth_cm - mean(snow_av$early_depth_cm, na.rm=TRUE)) / sd(snow_av$early_depth_cm, na.rm=TRUE)
snow_av$DensEARLY <- (snow_av$early_density_gcm3 - mean(snow_av$early_density_gcm3, na.rm=TRUE)) / sd(snow_av$early_density_gcm3, na.rm=TRUE)

#Late depth and density"
snow_av$DepthLATE <- (snow_av$late_depth_cm - mean(snow_av$late_depth_cm, na.rm=TRUE)) / sd(snow_av$late_depth_cm, na.rm=TRUE)
snow_av$DensLATE <- (snow_av$late_density_gcm3 - mean(snow_av$late_density_gcm3, na.rm=TRUE)) / sd(snow_av$late_density_gcm3, na.rm=TRUE)

#Plant, cover, wind
snow_av$Rich <- (snow_av$Richness - mean(snow_av$Richness, na.rm=TRUE)) / sd(snow_av$Richness, na.rm=TRUE)
snow_av$Div <- (snow_av$Diversity - mean(snow_av$Diversity, na.rm=TRUE)) / sd(snow_av$Diversity, na.rm=TRUE)
snow_av$Bare<- (snow_av$bare_ground - mean(snow_av$bare_ground, na.rm=TRUE)) / sd(snow_av$bare_ground, na.rm=TRUE)
snow_av$Rock<- (snow_av$rock - mean(snow_av$rock, na.rm=TRUE)) / sd(snow_av$rock, na.rm=TRUE)
snow_av$Litt<- (snow_av$litter - mean(snow_av$litter, na.rm=TRUE)) / sd(snow_av$litter, na.rm=TRUE)
snow_av$Cove<- (snow_av$Cover - mean(snow_av$Cover, na.rm=TRUE)) / sd(snow_av$Cover, na.rm=TRUE)
snow_av$Wind<- (snow_av$Wind_Ave - mean(snow_av$Wind_Ave, na.rm=TRUE)) / sd(snow_av$Wind_Ave, na.rm=TRUE)


#Construct SEM:===========
model_snow <- '

#latent variable definition:
    Drift        =~ LAI  + Wind  # Define the latent variable for Conditions for snow drift accumulation
    Snow         =~ DepthLATE + DensLATE + DepthEARLY +DensEARLY +aspect01
    Veg          =~  Rich + Cove 
    
#regressions:
              Snow ~  Drift
              Drift ~  Veg  + Drift
 
 # residual covariances:   

    DepthEARLY~~DepthEARLY # making sure variance of depth and density are estimated
    DensEARLY~~DensEARLY
    DepthLATE~~DepthLATE # making sure variance of depth and density are estimated
    DensLATE~~DensLATE
    DepthLATE~~aspect01

'
fit1 <- sem(model_snow, data = snow, std.lv=T)
summary (fit1)
par(mfrow=c(1,1)) #Create two columns in plotting grid

semPaths(fit1,"est", intercepts = F, fade = F, 
         sizeMan = 11,nCharNodes=12, title = T, edge.label.cex = 1.3)
title("Alpine Shurbs as Snow Engineers", line = 3)




#snow=======
#Raw data used:

#Standarize all numeric variables to compare effect sizes between them in SEM:
snow$Height <- (snow$height_cm - mean(snow$height_cm, na.rm=TRUE)) / sd(snow$height_cm, na.rm=TRUE)
snow$Area <- (snow$area_cm3 - mean(snow$area_cm3, na.rm=TRUE)) / sd(snow$area_cm3, na.rm=TRUE)
snow$LAI <- (snow$leaf.area.index - mean(snow$leaf.area.index, na.rm=TRUE)) / sd(snow$leaf.area.index, na.rm=TRUE)

#Early snow depth and density"
snow$DepthEARLY <- (snow$early_depth_cm - mean(snow$early_depth_cm, na.rm=TRUE)) / sd(snow$early_depth_cm, na.rm=TRUE)
snow$DensEARLY <- (snow$early_density_gcm3 - mean(snow$early_density_gcm3, na.rm=TRUE)) / sd(snow$early_density_gcm3, na.rm=TRUE)

#Late depth and density"
snow$DepthLATE <- (snow$late_depth_cm - mean(snow$late_depth_cm, na.rm=TRUE)) / sd(snow$late_depth_cm, na.rm=TRUE)
snow$DensLATE <- (snow$late_density_gcm3 - mean(snow$late_density_gcm3, na.rm=TRUE)) / sd(snow$late_density_gcm3, na.rm=TRUE)

#Plant, cover, wind
snow$Rich <- (snow$Richness - mean(snow$Richness, na.rm=TRUE)) / sd(snow$Richness, na.rm=TRUE)
snow$Div <- (snow$Diversity - mean(snow$Diversity, na.rm=TRUE)) / sd(snow$Diversity, na.rm=TRUE)
snow$Bare<- (snow$bare_ground - mean(snow$bare_ground, na.rm=TRUE)) / sd(snow$bare_ground, na.rm=TRUE)
snow$Rock<- (snow$rock - mean(snow$rock, na.rm=TRUE)) / sd(snow$rock, na.rm=TRUE)
snow$Litt<- (snow$litter - mean(snow$litter, na.rm=TRUE)) / sd(snow$litter, na.rm=TRUE)
snow$Cove<- (snow$Cover - mean(snow$Cover, na.rm=TRUE)) / sd(snow$Cover, na.rm=TRUE)
snow$Wind<- (snow$Wind_Ave - mean(snow$Wind_Ave, na.rm=TRUE)) / sd(snow$Wind_Ave, na.rm=TRUE)


#Construct SEM:===========
model_snow <- '

#latent variable definition:
    Drift        =~ LAI  + Wind  # Define the latent variable for Conditions for snow drift accumulation
    Snow =~ DepthLATE + DensLATE + DepthLATE +DensLATE
    Veg   =~  Rich + Cover + Litt + Bare
    
#regressions:
              Snow ~  Drift
              Drift ~  Veg  + Drift
 
 # residual covariances:   

    DepthEARLY~~DepthEARLY # making sure variance of depth and density are estimated
    DensEARLY~~DensEARLY
    DepthLATE~~DepthLATE # making sure variance of depth and density are estimated
    DensLATE~~DensLATE
    
    DepthEARLY~~ LAI  # making sure variance of depth and density are estimated
    DensEARLY~~ LAI 
    DepthLATE~~ LAI  # making sure variance of depth and density are estimated
    DensLATE~~ LAI 


'
composite1 <- sem(model_snow, data = snow, std.lv=T)
summary (composite1)
par(mfrow=c(1,1)) #Create two columns in plotting grid

semPaths(composite1,"est", intercepts = F, fade = F, 
         sizeMan = 11,nCharNodes=12, title = T, edge.label.cex = 1.3)
title("SE wind", line = 3)

