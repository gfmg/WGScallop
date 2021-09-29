# File: IoM_VAST_Grid.R
# Author: Guillermo Martin 
# Template Created: Mon Aug 23 13:06:02 2021
# ---------------------------------------------------------------------------
# Description:
# From the VAST wiki on github
# ---------------------------------------------------------------------------

rm(list = ls())

### An example of how to create user-defined extrapolation
### regions (extents) for VAST.

### Cecilia O'Leary and Cole Monnahan | December 2020

### The extrapolation region defines the extent over which the
### model predictions are integrated. Any density outside of this
### region will not be included in estimates of the index. It is
### not used in model fitting. It comes with many built-in
### regions but often a user needs to define their own. Here, we
### demonstrate two ways of doing this: (1) From a set of points
### representing the outer extent of the region; (2) from an
### existing shape file.

library(sp) 
library(sf) 
library(alphahull)
library(rgeos)
library(rgdal)
library(openxlsx)
library(raster)
library(ggplot2)

#Directories
dataDir<-file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/Stock_Assessment/SCA/",
                   "WGScallop_IrishSeaKing/Data")
gisDir<-file.path(dataDir,"GIS")
spatialdir<-file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/",
                      "Common_R/geoData")

#Common proj
projWGS84 <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


## Take a data.frame of coordinates in longitude/latitude that
## define the outer limits of the region (the extent).
# Load Isle of Man data
iom<-read.xlsx(xlsxFile = file.path(dataDir,
                                    "IOM_Datasheet2019_Abund_ICES_TORC_V1.xlsx"),
               sheet = 1,
               colNames=TRUE,
               detectDates = TRUE)
iom_coords<-read.csv(file=file.path(dataDir,
                                    "IoM_StationCoordinates.csv"),
                     stringsAsFactors = FALSE)

# Load UK borders
UK<-readOGR(dsn = spatialdir,
            layer = "greatbritain",
            encoding = "utf8")
UK <- spTransform(UK, CRSobj=projWGS84)


# Load Bathymetry data
Depth<-  raster(file.path(gisDir,"IoM_bathymetry.asc"))
crs(Depth)<-projWGS84

#Match IoM data with coordinates station
iom$lat<-iom_coords$Latitude..dd[match(iom$Site,iom_coords$Station)]
iom$lon<-iom_coords$Longitude..dd.[match(iom$Site,iom_coords$Station)]

#2009 onwards
iom09<-subset(iom,Year >= 2009)

#Remove stations where coordinates not available
iom09<-subset(iom09,!is.na(lon) & !is.na(lat))

### Method 1: use a set of lat/lon coordinates which define the
### outer edge of the region. For instance you might want to plot
### your data and simply create a region that captures it. The
### locator() function can be useful for this as shown
### below. Here we use a subset of the Eastern Bering Sea.

### Use this to draw points around your data
#plot(iom09$lon, iom09$lat)
#LL <- locator()
## saveRDS(LL, 'extent_LL.rds')

## Take a data.frame of coordinates in longitude/latitude that
## define the outer limits of the region (the extent).
#LL <- readRDS('extent_LL.rds')
#region_extent <- data.frame(long=LL$x, lat=LL$y)
#str(region_extent)

#GM_edit:
#Draw alpha hull around the stations
region_extent <- ahull(iom09[!duplicated(paste(iom09$lon, iom09$lat)),c("lon","lat")], 
                       alpha = .5)
#plot(iom09[,c("lon","lat")], pch = 19, col = "darkseagreen")
#plot(region_extent, col = "magenta", add = TRUE)

#Transform ahull to SpatialLines and then to spatial polygon. Code from lib
## Load functions and packages:
source(file.path("C:/Users/ggonzales/Desktop/gmartin_work_folder/Stock_Assessment/SCA/",
                 "WGScallop_IrishSeaKing/lib/ahull2poly.R"))
sps <- ahull2poly(region_extent)
proj4string(sps)<- CRS("+proj=longlat +datum=WGS84")

#Remove UK from the polygon we just created. 
sps<-gDifference(sps,UK)
sps <- SpatialPolygonsDataFrame(sps, data.frame(Id=factor('all'), F_AREA=1))
sps <- spTransform(sps, CRS("+proj=longlat +lat_0=90 +lon_0=180 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0 "))

### Get UTM zone for conversion to UTM projection
## retrieves spatial bounding box from spatial data [,1] is
## longitude
lon <- sum(bbox(sps)[1,])/2
## convert decimal degrees to utm zone for average longitude, use
## for new CRS
utmzone <- floor((lon + 180)/6)+1
crs_LL <- CRS('+proj=longlat +ellps=WGS84 +no_defs')
sps@proj4string <- crs_LL
### End method 1


### --------------------------------------------------
### Create the VAST extroplation grid for method 1 and 2
## Convert the final in polygon to UTM
crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=m +no_defs "))
region_polygon <- spTransform(sps, crs_UTM)

### Construct the extroplation grid for VAST using sf package
## Size of grid **in meters** (since working in UTM). Controls
## the resolution of the grid.
cell_size <- 1500
## This step is slow at high resolutions
region_grid <- st_make_grid(region_polygon, cellsize = cell_size, what = "centers")
## Convert region_grid to Spatial Points to SpatialPointsDataFrame
region_grid <- as(region_grid, "Spatial")
region_grid_sp <- as(region_grid, "SpatialPointsDataFrame")
## combine shapefile data (region_polygon) with Spatial Points
## (region_grid_spatial) & place in SpatialPointsDataFrame data
## (this provides you with your strata identifier (here called
## Id) in your data frame))
region_grid_sp@data <- over(region_grid, region_polygon)

#Intersect Depth and grid data to get information about bathymetry
Depth_UTM<-projectRaster(Depth,crs = crs_UTM)
region_grid_sp@data$Depth<-raster::extract(Depth_UTM,region_grid_sp)


## Convert back to lon/lat coordinates as that is what VAST uses
region_grid_LL <- as.data.frame(spTransform(region_grid_sp, crs_LL))
region_df <- with(region_grid_LL,
                  data.frame(Lon=coords.x1,
                             Lat=coords.x2, 
                             Id,
                             Depth=Depth,
                             Area_km2=((cell_size/1000)^2),
                             row=1:nrow(region_grid_LL)))
## Filter out the grid that does not overlap (outside extent)
region <- subset(region_df, !is.na(Id))
## This is the final file needed.
str(region)

#Remove any points with NA or positve depths
region<-subset(region,region$Depth < 0)
which(is.na(region$Depth))

#Remove any points below 10meters depth
region<-subset(region,region$Depth < -10)

ggplot(region,aes(x=Lon,y=Lat,fill=Depth,colour=Depth))+geom_point()


### Save it to be read in and passed to VAST later.
saveRDS(region, file =file.path(dataDir,"GIS/IoM_survey_region.rds"))
