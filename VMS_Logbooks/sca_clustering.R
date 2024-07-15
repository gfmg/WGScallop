sca_cluster<-function(data,fishing_speed=c(0.2,4),epsg=4326,
                      median_intv=15,median_fspeed=1.3,MinPts=3) {
  
  ## Purpose:
  ## Defining fishing pings in scallop data using a combination of speed threshold
  ## and a spatial clustering algorithm (db-scan). The function automatically transform
  ## the data for the original epsg to UTM by detecting UTM zone. 
  ## The clustering algorithm requires an expected distance at which to find clusters.
  ## As the VMS data is usually reported in standard intervals, knowing this interval
  ## and the usual speed at which fishing scallops occur, we can know the expected 
  ## distance covered between consecutive pings during fishing operations. 
  ## The function returns a column (dbclus). Values marked as 0 do not belong to any cluster
  ## and are likely not fishing activity. 
  ## ---------------------------------------------------------------------------
  ## Arguments:
  ## Data: tacsat data in TACSAT format. Required columns: 
  ##  SI_LATI: Latitude 
  ##  SI_LONG: Longitude
  ##  SI_SP: speed in knots
  ##  SI_STATE: Optional. if not available in the data, ping activity define based 
  ##  on speed threshold
  ##  FT_REF: trip reference number
  ## fishing_speed: min and max fishing speed to define fishing activity. Not used 
  ## if SI_STATE already defined in the data
  ## epsg: coordinate reference system of the data. Default 4326 for WGS84 
  ## median_intv: median interval rate of the tacsat data. Approximate
  ## median_fspeed: median fishing speed
  ## MinPts: minimum number of points to define a cluster
  ## ---------------------------------------------------------------------------
  ## Author: Guillermo Martin; Fri Jul 12 12:28:43 2024
  
  require(sf)
  require(fpc)
  require(sp)
  
  # Converting to UTM: 
  tmp<-st_as_sf(data,coords=c("SI_LONG","SI_LATI"),crs=epsg)
  ### Get UTM zone for conversion to UTM projection
  lon <- sum(st_bbox(tmp)[c(1,3)])/2
  ## convert decimal degrees to utm zone for average longitude, use
  ## for new CRS
  utmzone <- floor((lon + 180)/6)+1
  
  ## Convert the final in points to UTM
  crs_UTM <- CRS(paste0("+proj=utm +zone=",utmzone," +ellps=WGS84 +datum=WGS84 +units=km +no_defs "))
  tmp <- st_transform(tmp, crs_UTM) 
  
  tmp$XKm<-st_coordinates(tmp)[,1]
  tmp$YKm<-st_coordinates(tmp)[,2]
  
  # Assign speed threshold if not previously define
  if(!"SI_STATE" %in% names(tmp)) {
    tmp$SI_STATE<-NA 
    tmp$SI_STATE<-ifelse(tmp$SI_SP >= fishing_speed[1] & tacsatp$SI_SP<= fishing_speed[2],
                         "f","s")
  }
  
  #Applying dbscan at trip level for pings within speed threshold
  eps<-(median_fspeed*(median_intv*60))/1000

  res<-by(tmp,
          tmp$FT_REF,
          function(x){
            db <- fpc::dbscan(st_drop_geometry(x[x$SI_STATE == "f",c("XKm","YKm")]), 
                              eps = round(eps), MinPts = MinPts) 
            
            clus_res<-rep(NA, nrow(x))
            clus_res[x$SI_STATE == "f"] <- db$cluster
            
            # If all clusters are zero, set to NA
            if (all(db$cluster == 0)) {
              clus_res[x$SI_STATE == "f"] <- NA
            }
            
            #x$dbclus<-clus_res
            
            return(data.frame(index = which(tmp$FT_REF == unique(x$FT_REF)), 
                              cluster = clus_res))
          },simplify = F)
  
  combined_res <- do.call(rbind, res)
  combined_res <- combined_res[order(combined_res$index),]
  
  data$dbclus <- combined_res$cluster

  return(data)
}

#Run and visualize results
test<-read.csv(file=file.path("./test_data.csv"))
test<-sca_cluster(test)

library(ggplot2)
ggplot(test,aes(x=SI_LONG,y=SI_LATI,colour=factor(dbclus))) +
  geom_point() + 
  facet_wrap(.~FT_REF)

