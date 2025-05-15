#install.packages("rgdal", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)
#install.packages("stars", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)
#install.packages("raster", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)
#install.packages("gstat", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)

library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)

Start <- Sys.time()
# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("E:/Bryan/Projects/Ethiopia/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in Merra2 grid; need "foreign" library to read dbf from shapefile
merra <- read.csv("E:/Bryan/Projects/Ethiopia/Merra2_StudyRegion_Pro.csv",as.is=T)

# Directory of satellite data
Dir <-paste("E:/Bryan/Projects/Ethiopia/Merra2/Soil/Raw/",sep="")
# Get file names
file1 <- sort(list.files(Dir))                    
year <- substr(file1,28,31)
month <- substr(file1,32,33)
day <- substr(file1,34,35)

for (fi in 1:length(file1)){
  
  # "stars" library needed to read in .nc files
  #b = read_stars(paste0(Dir,file1[fi]), quiet = TRUE)
  dat <- nc_open(paste0(Dir,file1[fi]))
  lon <- data.frame(lon=dat$dim$lon$vals)
  lat <- data.frame(lat=dat$dim$lat$vals)
  dat2 <- data.frame(Num=seq(1,(nrow(lon)*nrow(lat)),1))
  dat2$lon <-rep(lon$lon,times=length(lat))
  dat2$lat <-rep(lat$lat,each=nrow(lon))
  dat3 <- subset(dat2,lon >= 38.0 & lon <= 40.75 & lat >= 8.5 & lat <= 10.5)
  dat3$Num <- seq(1,(nrow(dat3)),1)
  #write.csv(dat3, file = paste0("E:/Bryan/Projects/Ethiopia/Merra2/Merra2_StudyRegion.csv"), row.names=F)
  times <- as.POSIXct("2018-01-01 00:30:00")+as.difftime(ncvar_get(dat, "time"),units="hours")
  # Names of variables to extract
  vars <- c("BASEFLOW","ECHANGE","EVLAND","EVPINTR","EVPSBLN","EVPSOIL","EVPTRNS","FRSAT", "FRSNO","FRUNST","FRWLT","GHLAND",
            "GRN","GWETPROF","GWETROOT","GWETTOP","LAI","LHLAND","LWLAND","PARDFLAND","PARDRLAND","PRECSNOLAND","PRECTOTLAND",
            "PRMC","QINFIL","RUNOFF","RZMC","SFMC","SHLAND","SMLAND","SNODP","SNOMAS","SPLAND","SPSNOW","SPWATR","SWLAND",     
            "TELAND","TPSNOW","TSAT","TSOIL1","TSOIL2","TSOIL3","TSOIL4","TSOIL5","TSOIL6","TSURF","TUNST","TWLAND","TWLT","WCHANGE")
  #vars <- c("PS","SLP","PHIS","EPV","H","O3","OMEGA","QI","QL","QV","RH","U","V")
  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  for (v in 1:length(vars)){
    
    dat3 <- dat2
    for (t in 1:length(times)){
      #c = b[vars[v]]
      c <- ncvar_get(dat, vars[v])  
      # Subset by day

      d = c[1:576,1:361,t] 

      # Average per pixel
      #e = st_apply(d, 1:2, mean)
      e <- as.vector(d)
      
      # Create dataframe with mean variable
      dat3 <- cbind(dat3,e)
      #write.csv(dat2, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/ERA5_StudyRegion_d2m_20180101_fulltest.csv"), row.names=F)
    }
    dat4 <- subset(dat3,lon >= 38.0 & lon <= 40.75 & lat >= 8.5 & lat <= 10.5)
    dat4$Num <- seq(1,nrow(dat4),1)
    dat4$mean <- rowMeans(dat4[,4:27])
    dat5 <- merge(dat4[,c("Num","lon","lat","mean")],merra[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
    dat6 <- na.omit(dat5)
      #write.csv(dat4, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/ERA5_StudyRegion_d2m_20180101_test2.csv"), row.names=F)
      #dat4$point_x <- cbind(dat4$XPro,dat4$YPro)[,1]
      #dat4$point_y <- cbind(dat4$XPro,dat4$YPro)[,2]
    
    if (nrow(dat6 > 20)){
      
      coordinates(dat6) <- ~ X_Pro + Y_Pro
      
      #Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
      krigged <- idw(mean ~ 1, dat6, ExtendedGrid, nmax=4)
      result <- data.frame(krigged$var1.pred)
      colnames(result) <- paste0(vars[v])
      a3 <- cbind(a3,result)
      print(paste0("Finished variable - ",vars[v]))
    } else {
      print(paste0(vars[v]," Skipped because too many missing pixels"))
    }
   
  }
  
  saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Merra2/Soil/Processed/Ethiopia_Processed_Merra2_StudyRegion_250m_Soil_",year[fi],"_",month[fi],"_",day[fi],".rds"))
  print(paste0("Finished Date - ",year[fi],"_",month[fi],"_",day[fi]))
}
Sys.time()-Start
