#Load DATA and these packages=======
library("readxl")
library("tidyverse")
library("vegan")

#WEB on NMDS: https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
veg_matrix <- read.csv("Veg_matrix_SEM2.csv") #As prduced in CombineData.R file

#CLEAN VEG data========
community_matrix <- veg_matrix %>%  select(-rock, -litt, -bare, -SampleID)
alpine_NMDS <-  metaMDS(community_matrix,k=2,trymax=100,distance = "bray")
#PLOT:
plot(alpine_NMDS)
stressplot(alpine_NMDS)
orditorp(alpine_NMDS,display="species",cex=1.25,air=0.01)#TOO DENSE


#Remove Singletons:
occur.cols <- apply(community_matrix,2,sum)#sum species occurances in each column
table(occur.cols) #4 instances of zero abundance
zero_species <- as.data.frame(occur.cols) %>% filter(occur.cols == 0)
zero_species 
#These species were absent:
#Aust_velu          0
#Epac_hear          0 #found in raw data and replaced with epac_micr
#Exoc_nana          0
#NotT_Ryti          0

#Turn matrix into presence absence as this create a clarer composition patterns:
veg_matrix01 <- community_matrix
veg_matrix01[veg_matrix01>0] <- 1 #Turning entire matrix to 1 if >0

good.matrix <- veg_matrix01 [ , !occur.cols <= 1  ] #removing all 0 or 1-sum columns
dim(good.matrix) #234 119
dim(community_matrix) #234 135

env <- as.data.frame(veg_matrix %>% 
                       select(SampleID) %>%
                       separate(SampleID, c("region", "site", "shrub_code", "aspect"), sep="_"))


veg.env<-cbind(good.matrix,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(good.matrix)
range(veg.env$RowSum)#3 21
#veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows if any.

#Run NMDS: ========
MDS <- metaMDS(veg.env[ , 1:119], distance = "bray", try = 250)#computing distances in veg matrix
MDS$stress # Stress =  0.2326458

coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds<-as.data.frame(cbind(coordinates, veg.env))
#write.csv (veg.nmds.counts.spr12, file = "vegNMDS_Spring2012_OnCounts250runs.csv", row.names = F)
#veg.nmds <- read.csv ("vegNMDS_Spring2012_250runs.csv")




#REGION NMDS PLOT:=====
region_data <- filter(veg.nmds, region == "KNP" |region == "BHP")

#Draw a hull:
grp.a <- region_data[region_data$region == "KNP", ][chull(region_data[region_data$region =="KNP", c("MDS1", "MDS2")]), ]
grp.b <- region_data[region_data$region == "BHP", ][chull(region_data[region_data$region =="BHP", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

reg_plot <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=region, group=region),alpha=0.30) + # add the convex hulls
  geom_point(data=region_data ,aes(x=MDS1,y=MDS2,shape=region,colour=region),size=4)+ # add the point markers
  scale_colour_manual(values=c("KNP" = "red", "BHP" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

reg_plot


#Aspect NMDS PLOT:=====
aspect_data <- select(veg.nmds, MDS1, MDS2, aspect) %>%
               filter(aspect == "SE" |aspect == "NW")

#Draw a hull:
grp.a <- aspect_data[aspect_data$aspect == "SE", ][chull(aspect_data[aspect_data$aspect =="SE", c("MDS1", "MDS2")]), ]
grp.b <- aspect_data[aspect_data$aspect == "NW", ][chull(aspect_data[aspect_data$aspect =="NW", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

aspect_plot <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2, fill=aspect, group=aspect),alpha=0.30) + # add the convex hulls
  geom_point(data=aspect_data ,aes(x=MDS1,y=MDS2,shape=aspect,colour=aspect),size=4)+ # add the point markers
  scale_colour_manual(values=c("SE" = "green", "NW" = "black")) +
  scale_fill_manual(values=c("SE" = "green", "NW" = "black")) +
  
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

aspect_plot