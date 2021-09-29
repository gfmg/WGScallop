# File: IoM_Post2009.R
# Author: Guillermo Martin 
# Template Created: Mon Aug 23 11:59:41 2021
# ---------------------------------------------------------------------------
# Description:
# Analysing the survey data for the IoM survey post 2008
# ---------------------------------------------------------------------------


rm(list = ls())

library(dplyr)
library(openxlsx)
library(ggplot2);theme_set(theme_bw())
library(ggpubr)
library(sp)
library(gstat)
library(raster)
library(rgdal)
library(VAST)
library(splines)  # Used to include basis-splines
library(effects)  # Used to visualize covariate effects
library(colorRamps)
library(stringr)

dataDir<-file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/Stock_Assessment/",
                   "SCA/WGScallop_IrishSeaKing/Data")

spatialdir<-file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/",
                      "Common_R/geoData")
gisDir<-file.path(dataDir,"GIS")

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

UKeez<-readOGR(dsn = spatialdir,
            layer = "UK_eez_12nm",
            encoding = "utf8")
UKeez <- spTransform(UKeez, CRSobj=projWGS84)

Depth<-  raster(file.path(gisDir,"IoM_bathymetry.asc"))
crs(Depth)<-projWGS84

#Load survey region for VAST
region <- readRDS(file = file.path(dataDir,"GIS",'IoM_survey_region.rds'))

# Post 2009 ---------------------------------------------------------------
# It was decided to analyse survey data only from 2009 onwards
iom09<-subset(iom,Year >= 2009)

# Match tows with coordinates ---------------------------------------------
iom09$lat<-iom_coords$Latitude..dd[match(iom09$Site,iom_coords$Station)]
iom09$lon<-iom_coords$Longitude..dd.[match(iom09$Site,iom_coords$Station)]

#Sites without coordinates?: 
table(iom09$Site[is.na(iom09$lat) & is.na(iom09$lon)])

#Remove that station. Not available
iom09<-subset(iom09,!is.na(lon) & !is.na(lat))

#Adding UTM coords
cord.dec<-iom09[,c("lon","lat")]
coordinates(cord.dec)<-c("lon","lat")
proj4string(cord.dec) <- projWGS84

### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(cord.dec)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=km +no_defs "))
cord.UTM <- spTransform(cord.dec, crs_UTM)

iom09$xkm<-cord.UTM@coords[,1]
iom09$ykm<-cord.UTM@coords[,2]

#Getting bathymetry on stations
coordinates(iom09)<- ~ lon+lat
proj4string(iom09)<- projWGS84

iom09$Depth<-raster::extract(Depth,iom09)

#Transform back to data.frame
iom09<-data.frame(iom09)


#Format column types before VAST
iom09$Ivessel<-as.integer(as.factor(iom09$Vessel)) #Vessel as integer. Required in VAST
iom09$Tow_area_Km2<-iom09$Tow_area/1000 #Tow area in Km2 (as the grid in VAST)
iom09$dredge_type<-as.factor(iom09$dredge_type) #Not sure how to check differences in factors?

#Rename columns so standard notation: 
names(iom09)[names(iom09) == 'lat'] <- 'Lat'
names(iom09)[names(iom09) == 'lon'] <- 'Lon'

#There is 4 dredges for each tow, two queen and two king! We either aggregate or include a 
#factor for dredge number (a random factor). Something to ask Isobel and MyDAS
iom09<- iom09 %>%
  group_by(across(-c(Dredge_No,Total_ScallopBiomass,Total.Number,Tow_length_m,Tow_area_Km2))) %>%
  summarise(Total_ScallopBiomass=sum(Total_ScallopBiomass),
            Total.Number=sum(Total.Number),
            Tow_area_Km2=sum(Tow_area_Km2)) %>%
  data.frame()


years<-unique(iom09$Year)

# Plot station locations --------------------------------------------------
# Station Locations
#Bathymetry plot
Depth.df<-as.data.frame(Depth, xy = TRUE) 
colnames(Depth.df)<-c("lon","lat","IoM_bathymetry")

ocean.pal <- colorRampPalette(
  c("#000000", "#000413", "#000728", "#002650", "#005E8C",
    "#0096C8", "#45BCBB", "#8AE2AE", "#BCF8B9", "#DBFBDC")
)

st<-ggplot() + 
  geom_raster(data=subset(Depth.df,IoM_bathymetry < 0),aes(x=lon, y=lat,fill=IoM_bathymetry))+
  geom_point(iom09,mapping=aes(x=Lon,y=Lat),size=3,shape=21, fill="yellow",colour="black")+
  geom_text(iom09,mapping=aes(x=Lon,y=Lat),label=iom09$Site,vjust = -.5,hjust=0)+
  geom_polygon(data = UKeez,aes(x=long,y=lat,group=group),
               fill='transparent', colour="red",linetype="dashed",size=.9)+
  geom_polygon(data = UK,aes(x=long,y=lat,group=group),
               fill='grey', colour="black")+
  coord_cartesian(xlim = c(min(iom09$Lon),max(iom09$Lon)),
                  ylim=c(min(iom09$Lat),max(iom09$Lat)))+
  labs(fill="Bottom Depth",x="Latitude (°)",y="Longitude (°)")+
  scale_fill_gradientn(colours=ocean.pal(5),na.value="transparent")+
  theme(axis.title.y = element_text(size=13, margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(size=13, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size=12),
        legend.position = "right")


#Region plot
r.p<-ggplot() + 
  geom_point(data=region, aes(x=Lon, y=Lat),size=3,shape=21, fill="red",colour="black")+
  labs(x="Latitude (°)",y="Longitude (°)")+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_text(size=13, margin = margin(t = 20, r = 0, b = 0, l = 0)),
        axis.text = element_text(size=12))

  
ggarrange(st,r.p)


# VAST model of the survey data ---------------------------------------------------------
# Setting up data for VAST ------------------------------------------------
# I want to test different spatial and spatial-temporal formulations
# O1,O2,E1,E2 can have one of the following spatial/spatiotemporal components 0,"IID","AR1"

VAST_fitting<-function(input,
                       FO1,FO2,FE1,FE2, #FieldCongif
                       RB1,RB2,RE1,RE2, # RhoConfig
                       knots){
  dat<-list()
  
  dat$sampling_data<-input[,c("Lat",
                              "Lon",
                              "Year",
                              "Total_ScallopBiomass",
                              "Tow_area_Km2",
                              "Ivessel")]
  
  dat$catchability_data<-input[,c("Lat",
                                  "Lon",
                                  "Year",
                                  "dredge_type")] 
  
  #dat$covariate_data<-region[,c("Lat",
  #                              "Lon",
  #                              "Depth")] #Covariate data from stations or region? 
  
  dat$covariate_data<-input[,c("Lat",
                               "Lon",
                               "Depth")]
  
  dat$covariate_data$Depth<-abs(dat$covariate_data$Depth) #Depth values to absolute
  dat$covariate_data[,'Year'] = NA #Covariate constant over time
  
  # Rescale covariates being used to have an SD >0.1 and <10 (for numerical stability)
  dat$covariate_data[,'Depth'] = dat$covariate_data[,'Depth']/100
  
  
  dat$strata.limits="All_areas"
  
  # Formulas for catchability and density covariates
  Q1_formula = ~ dredge_type
  Q2_formula = ~ dredge_type
  
  X1_formula = ~ bs(log(Depth), degree=2, intercept=FALSE)
  X2_formula = ~ poly(log(Depth), degree=2) 
  
  # Model Settings
  settings = make_settings(n_x = knots, #Increase later 
                           Region = "User", 
                           knot_method='grid',
                           max_cells = Inf,
                           purpose = "index2", 
                           FieldConfig=c("Omega1"=FO1, #Omega specifies  whether spatial variation is present and/or correlated among variables
                                         "Omega2"=FO2, #1 and 2 indicate weather it affects first or second linear predictor
                                         "Epsilon1"=FE1, #Epsilon specifies whether spatio-temporal variation is present and/or correlated among variables
                                         "Epsilon2"=FE2),
                           RhoConfig = c(Beta1 = RB1, 
                                         Beta2 = RB2, 
                                         Epsilon1 = RE1, 
                                         Epsilon2 = RE2),
                           fine_scale = TRUE,
                           bias.correct = FALSE,
                           use_anisotropy= FALSE)
  
  # Fit VAST model using different family predictors: 
  fit<- tryCatch({
    fit_model(settings = settings,
              Lat_i = dat$sampling_data[,'Lat'], 
              Lon_i = dat$sampling_data[,'Lon'], 
              t_i = as.integer(dat$sampling_data[,'Year']), 
              b_i = dat$sampling_data[,'Total_ScallopBiomass'], 
              a_i=dat$sampling_data[,'Tow_area_Km2'],
              v_i = as.integer(dat$sampling_data[,'Ivessel']),
              #c_i = rep(0,nrow(dat$sampling_data)), # No catch category. 
              Q1_formula= Q1_formula,
              Q2_formula= Q2_formula,
              X1_formula = X1_formula,
              X2_formula = X2_formula,
              covariate_data = dat$covariate_data,
              catchability_data= dat$catchability_data,
              input_grid=region,
              ObsModel_ez= c(2,3)) #0: Conventional delta-model using logit-link for encounter probability and log-link for positive catch rates
    #2: Gamma
    #4: Lornormal, using bias-corrected-mean and log-coefficient of variation (CV) as parameters
    #3: Conventional delta-model, but fixing encounter probability=1 for any year where all samples encounter the species
                            
  },
  error=function(e){
    message(paste("Non-convergence"))
    message(e)
    return(e)
  })
  
  return(fit)
}

res.pos<-list()
res.pos[[1]]<-VAST_fitting(iom09,
                           FO1="AR1",FO2="AR1",FE1="AR1",FE2="AR1",
                           RB1=0,RB2=0,RE1=0,RE2=0,knots = 300)
# It is not converging. Why? 

res.pos[[2]]<-VAST_fitting(iom09,FO1="IID",FO2="IID",FE1="IID",FE2="IID",
                           RB1=0,RB2=0,RE1=0,RE2=0,knots = 300) 
# Converging after updating TMB

res.pos[[3]]<-VAST_fitting(iom09,FO1=0,FO2=0,FE1="IID",FE2="IID",
                           RB1=0,RB2=0,RE1=0,RE2=0,knots = 300)
# In my opinion, the model that would make more sense

res.pos[[4]]<-VAST_fitting(iom09,FO1=0,FO2=0,FE1=1,FE2=1,
                           RB1=0,RB2=0,RE1=0,RE2=0,knots = 300)
# Seems to be the same result as model above

res.pos[[5]]<-VAST_fitting(iom09,FO1=0,FO2=0,FE1=0,FE2=0,
                           RB1=0,RB2=0,RE1=0,RE2=0,knots = 300)
# Turned off spatial and spatiotemporal components

res.pos[[6]]<-VAST_fitting(iom09,FO1=0,FO2="IID",FE1="IID",FE2="IID",
                           RB1=0,RB2=0,RE1=4,RE2=4,knots = 300) #4 is AR1
# I find this set up difficult to interpret. Need to understand bettwer the difference in Epsilon
# for FieldConfig and RhoConfig


# Model Selection
res.pos[[1]]$parameter_estimates$AIC
res.pos[[2]]$parameter_estimates$AIC
res.pos[[3]]$parameter_estimates$AIC
res.pos[[4]]$parameter_estimates$AIC
res.pos[[5]]$parameter_estimates$AIC
res.pos[[6]]$parameter_estimates$AIC

# Best model
fit<-res.pos[[3]]
plotGamma0<-plot(fit)

#Save the Workspace
#save.image(file=file.path(dataDir,"/VAST/IoM_VAST_WK.RData"))


# Outputs -----------------------------------------------------------------
# Index of abundance and raw CPUE
# Scale index of LPUE smooth

Index.F<-array(NA,dim=c(length(years),length(res.pos)))

for (r in 1:length(res.pos)) {
  if (is.null(res.pos[[r]]$Report$Index_ctl)) {Index.F[r,]<- NA}
  else {
    Index.F[,r]<-res.pos[[r]]$Report$Index_ctl[, ,1] # Where are the standard errors from index? 
  }
}
Index.F <- data.frame(Index.F)
Index.F$Year<-years
Index.F<-gather(Index.F,model,mu,X1:X6)

Index.F.std<-Index.F %>% 
  group_by(model) %>%
  summarise(Year=Year,
            mu.std=mu/mean(mu)) %>%
  mutate(model=factor(model)) %>%
  data.frame()

#V_index<-plotGamma0$Index$Table
#V_index$mu.std<-V_index$Estimate_metric_tons/mean(V_index$Estimate_metric_tons)
#V_index$std.low<-(V_index$Estimate_metric_tons-V_index$SD_mt)/mean(V_index$Estimate_metric_tons)
#V_index$std.up<-(V_index$Estimate_metric_tons+V_index$SD_mt)/mean(V_index$Estimate_metric_tons)
#V_index$model<-"VAST"

# Raw LPUE
iom09$CPUE<-iom09$Total_ScallopBiomass/iom09$Tow_area_Km2
raw_index<-aggregate(CPUE~Year,iom09,FUN=mean)
raw_index$mu.std<-raw_index$CPUE/mean(raw_index$CPUE)
raw_index$model<-"nominal"


I.p<-ggplot(Index.F.std,aes(x=Year,y=mu.std,group=model,colour=model,fill=model))+
  geom_point()+
  geom_line()

I.p+
  geom_point(data = raw_index,aes(x=Year,y=mu.std),
             size=2,shape=7,colour="red")+
  labs(y="Standarized index",x= "Year", fill="Model", colour="Model",
       shape="Model")+
  scale_y_continuous(limits = c(0,2))+
  scale_x_continuous(breaks = c(seq(2009,2019,1)))+
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=11),
        axis.text.x = element_text(angle=45,hjust = 1),
        legend.text = element_text(size=12),
        legend.title = element_text(size=14),
        axis.title.x = element_blank())


# Visualize covariates
# Must add data-frames to global environment (hope to fix in future)
covariate_data_full = res.pos[[2]]$effects$covariate_data_full
catchability_data_full = res.pos[[2]]$effects$catchability_data_full

# Plot 1st linear predictor, but could use `transformation` to apply link function
pred = Effect.fit_model( res.pos[[2]],
                         focal.predictors = c("Depth"),
                         which_formula = "X2",
                         xlevels = 100,
                         transformation = list(link=identity, inverse=identity) )
x11()
plot(pred)

test<-data.frame(pred)
p<-ggplot(test,aes(x=Depth*100,y=fit,group=1))+
  geom_line()+
  geom_ribbon(aes(ymin=lower,ymax=upper),alpha=.2)

#Plot parameter estimate
rep<-res.pos[[2]]$parameter_estimates$SD
#summary(rep, "random")                      ## Only random effects
#summary(rep, "report")                      ## Only report
df<-as.data.frame(summary(rep, "fixed", p.value = TRUE))       ## Only non-random effects
df$parameter<-rownames(df)
colnames(df)<-c("Estimate","SD","Zvalue","pvalue","parameter")
df$parameter <- factor(df$parameter, levels = df$parameter)
df$Lpredictor<-str_extract(df$parameter, "\\d")
df$Lpredictor[df$parameter %in% "logSigmaM"]<-2

df$starloc <- NA
min<-min(df$Estimate-df$SD,na.rm = T)
max<-max(df$Estimate+df$SD,na.rm = T)
df$starloc <- max + (max - min)/10

#df$Sig <- with(df, ifelse(pvalue<0.05, "*", ""))
df$Sig <- with(df, ifelse((Estimate-SD)*(Estimate+SD)>0, "*", ""))


ggplot(df,aes(x=parameter,y=Estimate))+
  geom_point()+
  geom_errorbar(aes(ymin=Estimate-SD,ymax=Estimate+SD))+
  geom_rect(aes(xmin=0,
                xmax=dim(df[df$Lpredictor==1,])[1] + 0.5,
                ymin=-Inf,ymax=Inf),
                fill = "grey",alpha=0.02,colour="black")+
  #geom_rect(aes(xmin=dim(df[df$Lpredictor==1,])[1] + 0.5,
  #              xmax=dim(df)[1] + 0.5,
  #              ymin=-Inf,ymax=Inf),fill = "lightgrey",colour="black",alpha=0.02)+
  #geom_rect(aes(xmin=which(rownames(df) == "gamma1_cp")  - 0.5,
  #              xmax=which(rownames(df) == "gamma1_cp.1") + 0.5 ,
  #              ymin=-Inf,ymax=Inf),fill = "grey",colour="black",alpha=0.018)+
  #geom_rect(aes(xmin=which(rownames(df) == "gamma2_cp")  - 0.5,
  #              xmax=which(rownames(df) == "gamma2_cp.1") + 0.5 ,
  #              ymin=-Inf,ymax=Inf),fill = "lightgrey",colour="black",alpha=0.018)+
  #geom_text(aes(label = Sig, y = starloc),
  #          position = position_dodge(w = 0.7),
  #          show.legend = F,size=6) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


#Plot predicted densities
mdl <- make_map_info(Region = res.pos[[6]]$settings$Region,
                     spatial_list = res.pos[[6]]$spatial_list,
                     Extrapolation_List = res.pos[[6]]$extrapolation_list)

names(res.pos[[6]]$Report)[grepl('_gc|_gct', x=names(res.pos[[3]]$Report))]
D_gt <- res.pos[[6]]$Report$D_gct[,1,]
dimnames(D_gt) <- list(cell=1:nrow(D_gt), year=years)
library(dplyr)
library(tidyr)
D_gt <- D_gt %>% as.data.frame() %>%
  tibble::rownames_to_column(var = "cell") %>%
  pivot_longer(-cell, names_to = "Year", values_to='D')

D <- merge(D_gt, mdl$PlotDF, by.x='cell', by.y='x2i')

ggplot()+
  geom_polygon(data = UK,aes(x=long,y=lat,group=group),
               fill='grey', colour="black")+
  geom_point(data=subset(D ,log(D) > -3), mapping=aes(Lon, Lat, fill=log(D),colour=log(D),group=NULL),
             size=2, stroke=0,shape=22) + facet_wrap('Year')+
  scale_fill_gradientn(colours=matlab.like(20),na.value="transparent")+
  scale_colour_gradientn(colours=matlab.like(20),na.value="transparent")+
  coord_cartesian(xlim = c(min(D$Lon),max(D$Lon)),
                  ylim=c(min(D$Lat),max(D$Lat)))
