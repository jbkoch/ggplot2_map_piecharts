# Make a pie-chart Map
# PNW climate change project
# 29 January 2019

# Set Working Directory
setwd("/Users/jonathankoch/Google Drive/PacNWCommunityStudy/Analysis")
list.files()

# data
df <-read.csv("pnw_sr.csv", header = TRUE)
names(df)
attach(df)
np.df <-read.csv("othernp.csv", header = TRUE)
names(np.df)
attach(np.df)

# relative abundance business (15 spp.)
df$bifa <- df$Abundance - df$bifarius # bifarius
df$appa <- df$Abundance - df$appositus # appositus
df$cala <- df$Abundance - df$californicus # californicus
df$caga <- df$Abundance - df$caliginosus # caliginosus
df$flaa <- df$Abundance - df$flavifrons # flavifrons
df$gria <- df$Abundance - df$griseocollis # grisocollis
df$mela <- df$Abundance - df$melanopygus # melanopygus
df$mixa <- df$Abundance - df$mixtus #mixtus
df$neva <- df$Abundance - df$nevadensis # nevadensis
df$occa <- df$Abundance - df$occidentalis # occidentlis
df$rufa <- df$Abundance - df$rufocinctus # rufocinctus
df$sita <- df$Abundance - df$sitkensis # sitkensis
df$syla <- df$Abundance - df$sylvicola #sylvicola
df$vana <- df$Abundance - df$vandykei # vandykei
df$vosa <- df$Abundance - df$vosnesenskii #vosnesenskii


# libraries
library(maps)
library(mapdata)
library(maptools)
library(scales)
library(scatterpie)
library(dplyr)
library(ggplot2)
library(ggpubr)

# read some files
nps <-readShapePoly("/Users/jonathankoch/Google Drive/PacNWCommunityStudy/Analysis/ne_10m_parks_and_protected_lands/ne_10m_parks_and_protected_lands_area.shp")
nps.fort <- fortify(nps)
states <- map_data("state")
west_coast <- subset(states, region %in% c("oregon", "washington"))
df$Other <- df$Abundance - df$bifarius
ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  # geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
  #           fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("bifarius","bifa"))+
  # geom_point(data = df, mapping = aes(x = Longitude, y = Latitude), 
  #           color = "darkblue", alpha=0.5, size=Abundance/5)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Experimental\nCondition",
                    breaks=c("bifa", "bifarius"),
                    labels=c("Control", "Treatment 1"))+
  # geom_scatterpie_legend(radius=log(df$Abundance/100), x=-125, y=45.5,n=3)+
  # geom_text(data=df, aes(label=Row.Labels,x = Longitude, y = Latitude),hjust=0, vjust=0)+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))

# pie chart business
# appositus
tiff(filename = "appositus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
a <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("appositus","appa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("appositus","appa"),
                    labels=c(expression(italic("B. appositus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.7, 49,
           label="italic(B.)~ italic(appositus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(a,.11,.3,symbol=1)
dev.off()

# bifarius
tiff(filename = "bifarius.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
b <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("bifarius","bifa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("bifarius","bifa"),
                    labels=c(expression(italic("B. bifarius")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(bifarius)~ relative~ abundance"
  annotate('text', -124, 49,
           label="italic(B.)~ italic(bifarius)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(b,.11,.3,symbol=1)
dev.off()


# californicus
tiff(filename = "californicus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
c <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("californicus","cala"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("californicus","cala"),
                    labels=c(expression(italic("B. californicus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.5, 49,
           label="italic(B.)~ italic(californicus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(c,.11,.3,symbol=1)
dev.off()

# caliginosus
tiff(filename = "caliginosus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
d <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("caliginosus","caga"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("caliginosus","caga"),
                    labels=c(expression(italic("B. caliginosus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(caliginosus)~ relative~ abundance"
  annotate('text', -123.5, 49,
           label="italic(B.)~ italic(caliginosus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(d,.11,.3,symbol=1)
dev.off()

# flavifrons
tiff(filename = "flavifrons.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
e <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("flavifrons","flaa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("flavifrons","flaa"),
                    labels=c(expression(italic("B. flavifrons")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.8, 49,
           label="italic(B.)~ italic(flavifrons)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(e,.11,.3,symbol=1)
dev.off()

# griseocollis
tiff(filename = "griseocollis.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
f <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("griseocollis","gria"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("griseocollis","gria"),
                    labels=c(expression(italic("B. griseocollis")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.5, 49,
           label="italic(B.)~ italic(griseocollis)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(f,.11,.3,symbol=1)
dev.off()

# melanopygus
tiff(filename = "melanopygus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
g <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("melanopygus","mela"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("melanopygus","mela"),
                    labels=c(expression(italic("B. melanopygus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.2, 49,
           label="italic(B.)~ italic(melanopygus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(g,.11,.3,symbol=1)
dev.off()

# mixtus
tiff(filename = "mixtus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
h <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("mixtus","mixa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("mixtus","mixa"),
                    labels=c(expression(italic("B. mixtus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -124, 49,
           label="italic(B.)~ italic(mixtus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(h,.11,.3,symbol=1)
dev.off()

# nevadensis
tiff(filename = "nevadensis.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
i <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("nevadensis","neva"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("nevadensis","neva"),
                    labels=c(expression(italic("B. nevadensis")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.5, 49,
           label="italic(B.)~ italic(nevadensis)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(i,.11,.3,symbol=1)
dev.off()

# occidentalis
tiff(filename = "occidentalis.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
j <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("occidentalis","occa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("occidentalis","occa"),
                    labels=c(expression(italic("B. occidentalis")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.45, 49,
           label="italic(B.)~ italic(occidentalis)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(j,.11,.3,symbol=1)
dev.off()

# rufocinctus
tiff(filename = "rufocinctus.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
k <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("rufocinctus","rufa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("rufocinctus","rufa"),
                    labels=c(expression(italic("B. rufocinctus")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.6, 49,
           label="italic(B.)~ italic(rufocinctus)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(k,.11,.3,symbol=1)
dev.off()

# sitkensis
tiff(filename = "sitkensis.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
l <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("sitkensis","sita"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("sitkensis","sita"),
                    labels=c(expression(italic("B. sitkensis")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.75, 49,
           label="italic(B.)~ italic(sitkensis)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(l,.11,.3,symbol=1)
dev.off()

# sylvicola
tiff(filename = "sylvicola.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
m <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("sylvicola","syla"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("sylvicola","syla"),
                    labels=c(expression(italic("B. sylvicola")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.75, 49,
           label="italic(B.)~ italic(sylvicola)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(m,.11,.3,symbol=1)
dev.off()

# vandykei
tiff(filename = "vandykei.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
n <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("vandykei","vana"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("vandykei","vana"),
                    labels=c(expression(italic("B. vandykei")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.75, 49,
           label="italic(B.)~ italic(vandykei)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(n,.11,.3,symbol=1)
dev.off()

# vosnesenskii
tiff(filename = "vosnesenskii.tiff",width = 2000, height = 2000, 
     units = "px", pointsize =12, res = 300)
o <- ggplot(west_coast, aes(long,lat))+
  geom_map(map=west_coast, aes(map_id=region),fill="grey87",color="black")+
  geom_polygon(data=nps.fort, mapping=aes(x=long,y=lat,group=group),fill="darkgreen",color="black",alpha=0.5)+
  geom_point(data=np.df, mapping = aes(x=Longitude, y = Latitude),
             fill="darkgreen", colour = "black", shape=22, size=2)+
  geom_scatterpie(aes(x=Longitude,y=Latitude,r=0.10), data=df,
                  cols=c("vosnesenskii","vosa"), alpha=0.75)+
  xlab("Longitude")+
  ylab("Latitude")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  scale_fill_manual(values=c("#999999", "#E69F00"), 
                    name="Relative\nAbundance",
                    breaks=c("vosnesenskii","vosa"),
                    labels=c(expression(italic("B. vosnesenskii")), substitute(paste("Other ", italic('Bombus')))))+
  theme(legend.text.align = 0)+
  # annoying ~ is an escape characater
  # future reference: label="italic(B.)~ italic(appositus)~ relative~ abundance"
  annotate('text', -123.33, 49,
           label="italic(B.)~ italic(vosnesenskii)", parse=TRUE, 
           hjust=1, size=5)+
  annotate('text', -123.4, 47.70,
           label="OLYM", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.8, 45.60,
           label="FOVA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -124, 46.1,
           label="LEWI", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -123.28, 48.7,
           label="SAJH", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -122.85, 48.22,
           label="EBLA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.5, 47.15,
           label="MORA", parse=TRUE, 
           hjust=1, size=3)+
  annotate('text', -121.65, 48.85,
           label="NOCA", parse=TRUE, 
           hjust=1, size=3)+
  scalebar(west_coast, height = 0.009,
           st.size = 3,
           anchor = c(x = -120.15,y = 45.2), dist = 50, 
           dd2km = TRUE, model = 'WGS84')+
  coord_fixed(xlim = c(-125, -120.0),  
              ylim = c(45, 49))
north2(o,.11,.3,symbol=1)
dev.off()

# group two plots
tiff(filename = "test.tiff",width = 2000, height = 3000, 
     units = "px", pointsize =12, res = 300)
ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 1, nrow = 2, hjust = 0)
dev.off()
