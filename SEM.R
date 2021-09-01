#DATA and LIBRARIES:
library(lavaan)
library(qgraph)
library(semPlot)
library(tidyverse)


DATA1 <- read.csv("alpine_data.csv", header = TRUE)#
names(DATA1)
#[1] "year"                "region"              "site_site_no"        "shrub"               "shrub_genus"        
#[6] "shrub_species"       "shrub_code"          "latitude"            "longitude"           "leaf.area.index"    
#[11] "height_cm"           "area_cm3"            "early_depth_se"      "early_depth_nw"      "early_depth_ratio"  
#[16] "early_density_se"    "early_density_nw"    "early_density_ratio" "late_depth_se"       "late_depth_nw"      
#[21] "late_depth_ratio"    "late_density_se"     "late_density_nw"     "late_density_ratio"  "NW_snow_days"       
#[26] "NW_snow_days_1_aug"  "SE_snow_days"        "SE_snow_days_1_aug" 

DATA <- DATA1 %>%
    #selet variables that we need
    select(height_cm, area_cm3,leaf.area.index,
           early_depth_se,early_density_se,early_depth_nw,early_density_nw,
           late_depth_se, late_density_se,late_depth_nw, late_density_nw)
names(DATA)

#Standarize all variables to compare effect sizes between them in SEM:

DATA$Height <- (DATA$height_cm - mean(DATA$height_cm, na.rm=TRUE)) / sd(DATA$height_cm, na.rm=TRUE)
DATA$Area <- (DATA$area_cm3 - mean(DATA$area_cm3, na.rm=TRUE)) / sd(DATA$area_cm3, na.rm=TRUE)
DATA$LAI <- (DATA$leaf.area.index - mean(DATA$leaf.area.index, na.rm=TRUE)) / sd(DATA$leaf.area.index, na.rm=TRUE)

#Early snow characteristics SE and NW depth and density"
DATA$SEdepthEARLY <- (DATA$early_depth_se - mean(DATA$early_depth_se, na.rm=TRUE)) / sd(DATA$early_depth_se, na.rm=TRUE)
DATA$SEdensEARLY <- (DATA$early_density_se - mean(DATA$early_density_se, na.rm=TRUE)) / sd(DATA$early_density_se, na.rm=TRUE)
DATA$NWdepthEARLY <- (DATA$early_depth_nw - mean(DATA$early_depth_nw, na.rm=TRUE)) / sd(DATA$early_depth_nw, na.rm=TRUE)
DATA$NWdensEARLY <- (DATA$early_density_nw - mean(DATA$early_density_nw, na.rm=TRUE)) / sd(DATA$early_density_nw, na.rm=TRUE)

#Late snow characteristics SE and NW depth and density"
DATA$SEdepthLATE <- (DATA$late_depth_se - mean(DATA$late_depth_se, na.rm=TRUE)) / sd(DATA$late_depth_se, na.rm=TRUE)
DATA$SEdensLATE <- (DATA$late_density_se - mean(DATA$late_density_se, na.rm=TRUE)) / sd(DATA$late_density_se, na.rm=TRUE)
DATA$NWdepthLATE <- (DATA$late_depth_nw - mean(DATA$late_depth_nw, na.rm=TRUE)) / sd(DATA$late_depth_nw, na.rm=TRUE)
DATA$NWdensLATE <- (DATA$late_density_nw - mean(DATA$late_density_nw, na.rm=TRUE)) / sd(DATA$late_density_nw, na.rm=TRUE)

DATA_CLEAN <- na.omit(DATA)

model_SE <- '
    Canopy ~ 1*Height + LAI  # Define the composite variable and scale to height
    Canopy ~~ 0*Canopy # specifying zero error variance
    Canopy =~ SEdepthEARLY + SEdensEARLY + SEdepthLATE+ SEdensLATE  # showing that depth and density are indicators of canopy structure
    
    #Covariances:
    SEdepthEARLY~~SEdepthEARLY # making sure variance of depth and density are estimated
    SEdensEARLY~~SEdensEARLY
    SEdepthLATE~~SEdepthLATE # making sure variance of depth and density are estimated
    SEdensLATE~~SEdensLATE
'
composite1 <- sem(model_SE, data = DATA_CLEAN, std.lv=T)
summary (composite1)

par(mfrow=c(1,2)) #Create two columns in plotting grid

semPaths(composite1,"est", intercepts = F, fade = F, 
         sizeMan = 11,nCharNodes=6, title = T, edge.label.cex = 1.3)
title("SE wind", line = 3)

#Snowdrift from NW wind

model_NW <- '
    Canopy ~ 1*Height + LAI  # Define the composite variable and scale to height
    Canopy ~~ 0*Canopy # specifying zero error variance
    Canopy =~ NWdepthEARLY + NWdensEARLY + NWdepthLATE+ NWdensLATE  # showing that depth and density are indicators of canopy structure
    
    #Covariances:
    NWdepthEARLY~~NWdepthEARLY # making sure variance of depth and density are estimated
    NWdensEARLY~~NWdensEARLY
    NWdepthLATE~~NWdepthLATE # making sure variance of depth and density are estimated
    NWdensLATE~~NWdensLATE
'
composite2 <- sem(model_NW, data = DATA_CLEAN, std.lv=T)

semPaths(composite2,"est", intercepts = F, fade = F, 
         sizeMan = 11,nCharNodes=6, edge.label.cex = 1.3)
title("NW wind", line = 3)


#SNOW SEM ==========
model_snow <- '
    Canopy ~ Height + LAI + 1*Area
    SnowCover ~ 1*SEdensEARLY + SEdensEARLY
    Canopy ~~ 0*Canopy
    SnowCover ~~ 0* Snow cover
    Canopy =~ Snow cover
    SnowCover=~NWdepthLATE + SEdensLATE
    
    #Covariances:
    NWdepthLATE ~~ NWdepthLATE
    SEdensLATE ~~ SEdensLATE
'
snow_sem = sem(model_snow, data = DATA_CLEAN, std.lv=T)
semPaths(snow_sem,"est", intercepts = F, fade = F, 
         sizeMan = 11,nCharNodes=7)
summary(snow_sem, standardized = TRUE)
standardizedsolution(composite)
inspect(composite)
semCors(composite)
