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
a <- read.csv("D:/HaSET/PROCESS/Grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in ERA5 grid; need "foreign" library to read dbf from shapefile
ghap <- read.csv("D:/HaSET/PROCESS/Grids/GHAPPM25_StudyRegion_Pro.csv",as.is=T)

# test code
# b = read_stars("D:/HaSET/GHAP_PM2.5/2018/GHAP_PM2.5_D1K_20180605_V1.nc", quiet = TRUE)
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/processing/GHAP/raw/2022/GHAP_PM2.5_D1K_20220601_V1.nc")


years <- c(2018:2022)
# Directory of satellite data
# Dir <-paste("D:/HaSET/GHAP_PM2.5/",years[1],"/",sep="")
Dir <-paste("D:/HaSET/GHAP_PM2.5/2018_new/",sep="")
# Get file names
file1 <- sort(list.files(Dir))  
year <- substr(file1,16,19)
month <- substr(file1,20,21)
day <- substr(file1,22,23)

# Dataframe to bind all interpolated columns
a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
for (fi in 1:length(file1)){
  
  # "stars" library needed to read in .nc files
  b = read_stars(paste0(Dir,file1[fi]), quiet = TRUE)
  dat <- nc_open(paste0(Dir,file1[fi]))
  lat <- ncvar_get(dat, "lat")
  lon <- ncvar_get(dat, "lon")
  lat_index <- which(lat >= 8.7 & lat <= 10.3)
  lon_index <- which(lon >= 38.5 & lon <= 40.25)
  select_lat <- lat[lat_index]
  select_lon <- lon[lon_index]
  
  pm25 <- ncvar_get(
    dat, 
    "PM2.5", 
    start = c(lon_index[1], lat_index[1]), 
    count = c(length(lon_index), length(lat_index)))
  
  vars2 <- as.vector(pm25)
  
  #Extract PM2.5, latitude and longitude
  dat2 <- data.frame(PM25=vars2)
  dat2$lon<-rep(select_lon,times=length(lat_index))
  dat2$lat<-rep(select_lat,each=length(lon_index))
  dat2$Num <- seq(1,nrow(dat2),1)
  # write.csv(dat2, file = paste0("/Users/lenovo/Desktop/HaSET/processing/GHAP/GHAP_StudyRegion.csv"), row.names=F)
  
  dat3 <- merge(dat2,ghap[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
  dat4 <- na.omit(dat3)
  coordinates(dat4) <- ~ X_Pro + Y_Pro
      
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(PM25 ~ 1, dat4, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("GHAP_PM25")
  a4 <- cbind(a3,result)
  
  # write.csv(a4, file = paste0("/Users/lenovo/Desktop/HaSET/processing/GHAP/processed/GHAP_StudyRegion_test.csv"), row.names=F)
  # saveRDS(a3, file = paste0("/Users/lenovo/Desktop/HaSET/processing/GHAP/processed/Ethiopia_Processed_GHAPPM25_StudyRegion_250m.rds"))
  
  saveRDS(a4, file = paste0("D:/HaSET/GHAP_PM2.5/processed/",year[fi],"/Ethiopia_Processed_GHAPPM25_StudyRegion_250m_",year[fi],"_",month[fi],"_",day[fi],".rds"))
  print(paste0("Finished file - ",file1[fi]))
  
}
Sys.time()-Start


