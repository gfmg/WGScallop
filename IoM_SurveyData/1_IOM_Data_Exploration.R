# File: 1_IOM_Data_Exploration.R
# Author: Guillermo Martin 
# Template Created: Tue May 25 12:19:37 2021
# ---------------------------------------------------------------------------
# Description:
# Exploration script for the Isle of Man survey data with the idea of the
# development of an standardized survey index
# ---------------------------------------------------------------------------
rm(list = ls())

library(openxlsx)
library(ggplot2);theme_set(theme_bw())
library(sp)
library(gstat)
library(raster)
library(rgdal)


dataDir<-file.path("./Data")

spatialdir<-file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/",
                      "Common_R/geoData")

#Common proj
projWGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Load Isle of Man data
iom<-read.xlsx(xlsxFile = file.path(dataDir,
                                    "IOM_Datasheet2019_Abund_ICES_TORC_V1.xlsx"),
               sheet = 1,
               colNames=TRUE,
               detectDates = TRUE)
iom_coords<-read.csv(file=file.path(dataDir,
                                    "IoM_StationCoordinates.csv"),
                     stringsAsFactors = FALSE)

# Load Shapefiles
IRL<-readOGR(dsn = spatialdir,
             layer = "Ireland",
             encoding = "utf8")
IRL <- spTransform(IRL, CRSobj=projWGS84)

UK<-readOGR(dsn = spatialdir,
            layer = "greatbritain",
            encoding = "utf8")
UK <- spTransform(UK, CRSobj=projWGS84)

# Match tows with coordinates ---------------------------------------------
iom$lat<-iom_coords$Latitude..dd[match(iom$Site,iom_coords$Station)]
iom$lon<-iom_coords$Longitude..dd.[match(iom$Site,iom_coords$Station)]

#Sites without coordinates?: 
table(iom$Site[is.na(iom$lat) & is.na(iom$lon)])

#Subset for now
iom<-subset(iom,!is.na(lon) & !is.na(lat))

#Adding UTM coords
cord.dec<-iom[,c("lon","lat")]
coordinates(cord.dec)<-c("lon","lat")
proj4string(cord.dec) <- CRS("+proj=longlat +datum=WGS84")
cord.UTM <- spTransform(cord.dec, CRS("+proj=utm +zone=30 +units=km"))

iom$xkm<-cord.UTM@coords[,1]
iom$ykm<-cord.UTM@coords[,2]

# Station Locations
ggplot(iom,aes(x=lon,y=lat))+
  geom_point()+
  geom_polygon(data = IRL,aes(x=long,y=lat,group=group),
               fill='grey', colour="black")+
  geom_polygon(data = UK,aes(x=long,y=lat,group=group),
               fill='grey', colour="black")+
  coord_cartesian(xlim = c(min(iom$lon),max(iom$lon)),
                  ylim=c(min(iom$lat),max(iom$lat)))


# Format columns ----------------------------------------------------------
str(iom)

#Unique ID columns for plotting
iom$ID<-1:nrow(iom)
iom$ICESRECT_n<-paste0(substr(iom$ICESRECT, 1, 2),"E",5) # Modify ICES rectangles format

#Add CPUE columns
iom$CPUE_n<-iom$Total.Number/iom$Tow_area # Number/m2
iom$CPUE_b<-iom$Total_ScallopBiomass/iom$Tow_area # Kg/m2 
# SPiCT only deals with continuous? 

#Year column as factor
iom$fYear<-as.factor(as.character(iom$Year))

# Data exploration --------------------------------------------------------

#Plot CPUE_b and CPUE_n
ggplot(iom,aes(x=Year,y=CPUE_b))+geom_point()+geom_smooth(method = "gam")
ggplot(iom,aes(x=Year,y=CPUE_n))+geom_point()+geom_smooth(method = "gam")
# Quite different distribution of point before and after ~2008? Why? 


# Function to plot continuos variables: 
MyDotplot.ggp2 <- function(Z, varx,varz, Ncol = 5, TextSize = 15, PointSize = 1,
                           DropLabels = FALSE,Outliers=FALSE) {
  library(ggplot2)	
  K     <- length(varx)
  MyData <- data.frame(Y = rep(1:nrow(Z), K),
                       X = as.vector(as.matrix(Z[, varx])),
                       Var = rep(varx, each = nrow(Z))) 
  p <- ggplot(MyData, aes(y = Y, x = X))
  p <- p + 
    geom_point(size= PointSize) + 
    ylab("Order of the data") + xlab("Range of the data")+
    theme_bw()
  if (length(varz) !=0){
    K     <- length(varx)
    MyData <- data.frame(Y = rep(1:nrow(Z), K),
                         X = as.vector(as.matrix(Z[, varx])),
                         Var = rep(varx, each = nrow(Z)),
                         fillz=as.vector(as.matrix(Z[, varz]))) 
    p <- ggplot(MyData, aes(y = Y, x = X,colour=fillz))
    p <- p + 
      geom_point(size= PointSize) + 
      ylab("Order of the data") + xlab("Range of the data")+
      theme_bw()
  }
  if (DropLabels) { 
    p <- p + theme_bw()+ 
      theme(axis.text = element_blank(),
            strip.text = element_text(size=TextSize),
            axis.ticks = element_blank() )} else {
              p <- p + theme(strip.text = element_text(size=TextSize))
            }
  p <- p + facet_wrap(~ Var, scales = "free_x", ncol=Ncol)
  print(p)	
}

x11()
plotCols<-c("Total_ScallopBiomass","CPUE_b","Total.Number","CPUE_n","Tow_area")
fill<-"Vessel"
MyDotplot.ggp2(iom,plotCols,fill,Ncol=round(length(plotCols)/2))

# Range of CPUE is very limited (.0-.03). No clear outliers. 
# Tow area though. Some points above 6000? Reason? 


#Histogram of CPUE
hist(iom$CPUE_b,breaks = 200)
hist(log(iom$CPUE_b),breaks = 200)

# Proportion of 0's in the data: 
length(which(iom$CPUE_b==0))/dim(iom)[[1]]
# 0.0142 % of 0's in the data. Very low. 
# Simplest would be to remove and apply gamma distribution. Strictly positive


# Relationship response vs covariates

#Catches vs effort
ggplot(iom,aes(x=Tow_area,y=Total_ScallopBiomass))+
  geom_point()+geom_smooth(method = "lm")+
  geom_abline(slope = 1,colour="red")

summary(lm(Total_ScallopBiomass~Tow_area,data = iom)) #Slope so much smaller than 1

#Catches vs dredge type
ggplot(iom,aes(x=dredge_type,y=CPUE_b))+
  geom_boxplot()

#Catches vs boat
ggplot(iom,aes(x=Vessel,y=CPUE_n))+
  geom_boxplot()

#Are the catches by different vessels different? 
iom$Vessel<-factor(iom$Vessel)
iom$LogSA<-log(iom$Tow_area)

glm1<-glm(Total_ScallopBiomass~Vessel+offset(LogSA),
          data=subset(iom,Total_ScallopBiomass>0),family = Gamma(link="log"))
summary(glm1)


#Biomass over time
ggplot(iom,aes(x=factor(Year),y=CPUE_b))+geom_boxplot()



# Spatial dependency?  ----------------------------------------------------
# Compute semivariograms for each year
years=unique(levels(factor(iom$Year)))
Vario.all<-data.frame()

for (i in 1:length(years)) {
  ind_y=which(iom$Year==years[i])
  iom_Year1=iom[ind_y,]
  
  iom_Year1<-subset(iom_Year1,!is.na(xkm) & !is.na(ykm))
  
  mydata<-data.frame(Total_ScallopBiomass=iom_Year1$Total_ScallopBiomass,
                     xkm=iom_Year1$xkm,
                     ykm=iom_Year1$ykm)
  coordinates(mydata)<-c("xkm","ykm")
  Vario<-variogram(object = Total_ScallopBiomass ~ 1,
                   data = mydata,
                   cressie=TRUE,
                   cutoff=60, # Defining a a distance limit of 80Km for semivariance calculation
                   width=4)
  
  Vario$fYear<-years[i]
  
  if(i==1){
    Vario.all=Vario 
  }
  if(i>1){
    Vario.all=rbind(Vario.all,Vario) 
  }
}

p<-ggplot(data=Vario.all,aes(x=dist,y=gamma))+
  geom_point()+
  geom_smooth(method = "gam",formula = y ~ s(x,bs="cs",k=5),colour="black")+
  xlab("Distance (Km)")+
  ylab("Sample variogram")+
  facet_wrap(.~fYear)+
  theme(aspect.ratio = 1)+
  scale_y_continuous(limits = c(0,10))
p



# Nominal CPUE ------------------------------------------------------------
#Just a very first plot to see how it looks
CPUE_nom_mu<-setNames(aggregate(CPUE_b~Year,iom,FUN=mean),
                      c("Year","mu")) #Biomass
CPUE_nom_sd<-setNames(aggregate(CPUE_b~Year,iom,FUN=sd),
                      c("Year","sd"))
CPUE_nom<-merge(CPUE_nom_mu,CPUE_nom_sd)

ggplot(data=CPUE_nom,aes(x=Year,y=mu,group=1)) +
  geom_point()+
  geom_line()+
  geom_ribbon(aes(ymin=mu-sd,ymax=mu+sd),alpha=.3)


