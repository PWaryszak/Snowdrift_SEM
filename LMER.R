#Not enough data for SEM (Power issue, too many variables, not enough data). Let's go with glmer
#LOAD snow DATA and LIBRARIES:=========
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom)
library(sjPlot)

#Load snow prepared and cured in CombineData.R file:
setwd("C:/Users/BlueCarbon/Documents/00DeakinUni/R/SusannaVenn/SEM/Shrub-snowdrift/Structural Equation Model/Emily/shrubs snowdrift/Snowdrift_SEM")
snow <- read.csv("VegSnowSoilWindData_SEM2.csv") #Joined Data

#Explore/Wrangle Data:
dim(snow)#374 152
names(snow)

snow$EngineerPlant<- str_sub (snow$shrub_code,1 ,-2) #shorten all OG1-OG4 to OG only
unique(snow$EngineerPlant)#Plant Engineers All: "C"   "G"   "H"   "O"   "CH"  "OG"  "E"   "OZO" "P"  
og <- snow[snow$EngineerPlant =="OG",] #open grass only
dim(og) #22 records on open grass

#NA-s
dim(snow[is.na(snow$SampleID_FromVegData),]) #154 NA-s as snow data did not match veg data
snow1 <- snow [ ! is.na(snow$SampleID_FromVegData) |  snow$EngineerPlant == "OG" ,] #REMOVE NA-s that are not OG (Open Grass)
#Open grass has hegiht set to 0 cm and is an important control for the snow_model
dim(snow1)#242 153

#RANK Height_cm:
summary(snow1$height_cm)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#0.00   25.00   41.00   42.34   60.00   90.00       4 

snow2 <- snow1 %>% mutate(HeightRank_ = ifelse(height_cm < 1, "open_grass",
         ifelse(height_cm > 1 & height_cm < 50, "low_shrub","high_shrub"))) %>%
  filter(!is.na(HeightRank_))

table(snow2$HeightRank_)
unique(snow2$HeightRank_)
snow2$HeightRank_ <- factor(snow2$HeightRank_, levels = c("open_grass","low_shrub","high_shrub"))

###########################################
### Snow~Veg+aspect models tested below: ##

#early_depth_cm_model============
early_depth_cm_model <- lmer(early_depth_cm ~ aspect + HeightRank_ +(1|EngineerPlant), data=snow2)
summary(early_depth_cm_model)#No effect

# plot fixed effects:
plot_model(early_depth_cm_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="Early snow depth (cm)", title = "Predicted effect of shrub height and aspect")
  
#early_density_gcm3_model============
early_density_gcm3_model <- lmer(early_density_gcm3 ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(early_density_gcm3_model)#No effect

# plot fixed effects:
plot_model(early_density_gcm3_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="early_density_gcm3", title = "Predicted effect")

#late_depth_cm_model============
late_depth_cm_model <- lmer(late_depth_cm ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(late_depth_cm_model)#No effect

# plot fixed effects:
plot_model(late_depth_cm_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="late_depth_cm_", title = "Predicted effect")

#snow_days_1_aug_model============
snow_days_1_aug_model <- lmer(snow_days_1_aug ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(snow_days_1_aug_model)#No effect

# plot fixed effects:
plot_model(snow_days_1_aug_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="snow_days_1_aug", title = "Predicted effect")


#snow_days_model============
snow_days_model <- lmer(snow_days ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(snow_days_model)#No effect

# plot fixed effects:
plot_model(snow_days_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="snow_days", title = "Predicted effect")

#Cover_model============
Cover_model <- lmer(Cover ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(Cover_model)#No effect

# plot fixed effects:
plot_model(Cover_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="Cover", title = "Predicted effect")


#Richness_model============
Richness_model <- lmer(Richness ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(Richness_model) # YES EFFECT!
#                       Estimate Std. Error  df t value Pr(>|t|)    
#Intercept)             9.2584     0.9141   5.5343  10.128 8.86e-05 ***
#aspectSE                1.0926     0.4782 209.0096   2.285   0.0233 *  
#HeightRank_high_shrub   1.6321     0.6413 182.8777   2.545   0.0118 *  

# plot fixed effects:
plot_model(Richness_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="Richness", title = "Predicted effect")

#Diagnostics:
qqnorm(resid(Richness_model))
qqline(resid(Richness_model))

#Plot significant effects (Richness)==========
ggplot(data = snow2, aes(x= aspect, y=Richness, fill = HeightRank_))+
  geom_boxplot() +geom_jitter(aes(color = height_cm)) +
  facet_wrap(.~HeightRank_)+
  ggtitle("Richness is significantly higher at high shrubs (P = 0.0118)",
          subtitle = "open_grass <0,1cm>, low_shrubs <1,50cm>, high_shrubs <50,110cm>")+
  labs(x = "Aspect", fill = "Height Rank", color = "Height (cm)")+
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        legend.position = c(0.2, 0.5),
        legend.text = element_text(size = 16),
        strip.text=element_text(size=16),
        plot.title = element_text(size = 17, face = "bold", vjust = 0.5))


#bare_model============
bare_model <- lmer(bare ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(bare_model)#No effect

# plot fixed effects:
plot_model(bare_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="bare", title = "Predicted effect")


#litt_model============
litt_model <- lmer(litt ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(litt_model)
#                 Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)             9.9212     1.6888   8.5731   5.875 0.000285 ***
#  aspectSE              0.3796     1.2924 209.3833   0.294 0.769253    
#HeightRank_high_shrub  -3.2361     1.6342  90.0740  -1.980 0.050722 .  

# plot fixed effects:
set_theme(
  base = theme_classic(), 
  legend.title.face = "italic", # title font face
  legend.inside = TRUE,         # legend inside plot
  legend.pos = c(0.5,0.5),  # legend position inside plot
  axis.title.size = 1.9,
  axis.textsize.x = 1.4,
  axis.textsize.y = 1.4,
  legend.size = 1.7,
  legend.title.size = 2.8,
  geom.label.size = 5,
  geom.outline.size = 2,
)
plot_model(litt_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="litt", title = "Predicted effect of aspect and shrub height on litter (%):")

#rock_model============
rock_model <- lmer(rock ~ aspect + HeightRank_ + (1|EngineerPlant), data = snow2)
summary(rock_model)#No effect

# plot fixed effects:
plot_model(rock_model, type = "pred", terms = c("aspect", "HeightRank_")) +
  labs(x="Aspect",y="rock", title = "Predicted effect")

