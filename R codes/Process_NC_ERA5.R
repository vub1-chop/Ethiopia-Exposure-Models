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

# Read in ERA5 grid; need "foreign" library to read dbf from shapefile
era <- read.csv("E:/Bryan/Projects/Ethiopia/ERA5_StudyRegion_Pro.csv",as.is=T)

# Directory of satellite data
Dir <-paste("E:/Bryan/Projects/Ethiopia/ERA5/",sep="")
# Get file names
file1 <- sort(list.files(Dir))                    

for (fi in 1:length(file1)){
  
  # "stars" library needed to read in .nc files
  b = read_stars(paste0(Dir,file1[fi]), quiet = TRUE)
  dat <- nc_open(paste0(Dir,file1[fi]))
  lon <- data.frame(lon=dat$dim$longitude$vals)
  lat <- data.frame(lat=dat$dim$latitude$vals)
  dat2 <- data.frame(Num=seq(1,(nrow(lon)*nrow(lat)),1))
  dat2$lon <-rep(lon$lon,times=length(lat))
  dat2$lat <-rep(lat$lat,each=nrow(lon))
  #write.csv(dat2, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/ERA5_StudyRegion.csv"), row.names=F)
  t <- as.POSIXct("1900-01-01 00:00")+as.difftime(ncvar_get(dat, "time"),units="hours")
  year <- substr(t,1,4)
  month <- substr(t,6,7)
  day <- substr(t,9,10)
  
  # Names of variables to extract
  vars <- c("d2m","e","fal","lai_hv","lai_lv","sp","ssr","ssrd","stl1","stl2","stl3","stl4","t2m","tp","u10","v10")
  totaldays <- length(t)/24
  day_begin <- c(seq(1,length(t),24))
  day_end <- c(seq(24,length(t),24))
  
  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]

  for (v in 1:length(vars)){
      
  c = b[vars[v]]
      
    for (ti in 1:length(day_begin)){
      # Subset by day
      d = c[,,,day_begin[ti]:day_end[ti]]
        
      # Average per pixel
      e = st_apply(d, 1:2, mean)
      f <- as.vector(e$mean)
        
      # Create dataframe with mean variable
      dat2$mean <- f
      #write.csv(dat2, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/ERA5_StudyRegion_d2m_20180101_fulltest.csv"), row.names=F)
      dat3 <- subset(dat2,lon >= 38.5 & lon <= 40.25 & lat >= 8.7 & lat <= 10.3)
      dat3$Num <- seq(1,nrow(dat3),1)
      dat4 <- merge(dat3,era[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
      dat5 <- na.omit(dat4)
      #write.csv(dat4, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/ERA5_StudyRegion_d2m_20180101_test2.csv"), row.names=F)
      #dat4$point_x <- cbind(dat4$XPro,dat4$YPro)[,1]
      #dat4$point_y <- cbind(dat4$XPro,dat4$YPro)[,2]
      coordinates(dat5) <- ~ X_Pro + Y_Pro
        
      # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
      krigged <- idw(mean ~ 1, dat5, ExtendedGrid, nmax=4)
      result <- data.frame(krigged$var1.pred)
      colnames(result) <- paste0(vars[v],"_",year[day_end[ti]],"_",month[day_end[ti]],"_",day[day_end[ti]])
      a3 <- cbind(a3,result)
      print(paste0("Finished variable - ",vars[v], " - for date ",year[day_end[ti]],"-",month[day_end[ti]],"-",day[day_end[ti]]))
        
      }
     
  }
  
  for (da in 1:length(unique(day))){
    dates <- paste0(unique(year),"_",unique(month),"_",unique(day)[da])
    dat5 <- a3[ , grepl( dates, names( a3 ) ) ]
    a5 <- cbind(a,dat5)
    colnames(a5) <- c("FID","Id","ORIG_FID","X","Y","X_Pro","Y_Pro","d2m","e","fal","lai_hv","lai_lv","sp","ssr","ssrd","stl1","stl2","stl3","stl4","t2m","tp","u10","v10")
    saveRDS(a5, file = paste0("E:/Bryan/Projects/Ethiopia/ERA5/Processed/",unique(year)[da],"/Ethiopia_Processed_ERA5_StudyRegion_250m_",dates,".rds"))
    print(paste0("Finished Date - ",dates))
  }
  
  print(paste0("Finished file - ",file1[fi]))
 
  }
  
Sys.time()-Start
