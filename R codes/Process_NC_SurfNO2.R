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
s.NO2 <- read.csv("E:/Bryan/Projects/Ethiopia/Surface_NO2/SurfNO2_StudyRegion_Pro.csv",as.is=T)

# Directory of satellite data
Dir <-paste("E:/Bryan/Projects/Ethiopia/Surface_NO2/Raw/",sep="")
# Get file names
file1 <- sort(list.files(Dir))  
year <- substr(file1,22,25)

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
  
  surfNO2 <- ncvar_get(
    dat, 
    "SurfaceNO2", 
    start = c(lon_index[1], lat_index[1]), 
    count = c(length(lon_index), length(lat_index)))
  
  vars2 <- as.vector(surfNO2)
  
  #Extract PM2.5, latitude and longitude
  dat2 <- data.frame(surfNO2=vars2)
  dat2$lon<-rep(select_lon,times=length(lat_index))
  dat2$lat<-rep(select_lat,each=length(lon_index))
  dat2$Num <- seq(1,nrow(dat2),1)
  #write.csv(dat2, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/SurfNO2_StudyRegion.csv"), row.names=F)
  
  dat3 <- merge(dat2,s.NO2[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
  dat4 <- na.omit(dat3)
  coordinates(dat4) <- ~ X_Pro + Y_Pro
  
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(surfNO2 ~ 1, dat4, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("Surf_NO2")
  a3 <- cbind(a3,result)
  #write.csv(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/SurfNO2_StudyRegion_test.csv"), row.names=F)
  
    if (year[fi]==2020){
      for (m in 1:12){
        if (m==2){
          for (d in 1:29){
            if (d <10){
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
            } else {
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
            }
          } 
        } else if (m==1 | m==3 | m==5 | m==7 | m==8){
          for (d in 1:31){
            if (d <10){
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
            } else {
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
            }
          }
        } else if (m==10 | m==12) {
          for (d in 1:31){
          if (d <10){
            saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_0",d,".rds"))
          } else {
            saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_",d,".rds"))
          }
          }
        } else if (m==11){
          for (d in 1:30){
            if (d <10){
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_0",d,".rds"))
            } else {
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_",d,".rds"))
            }
          }
        } else {
          for (d in 1:30){
            if (d <10){
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
            } else {
              saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
            }
          }
        }
          
        }
      } else {
        for (m in 1:12){
          if (m==2){
            for (d in 1:28){
              if (d <10){
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
              } else {
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
              }
            } 
          } else if (m==1 | m==3 | m==5 | m==7 | m==8){
            for (d in 1:31){
              if (d <10){
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
              } else {
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
              }
            }
          } else if (m==10 | m==12) {
            for (d in 1:31){
              if (d <10){
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_0",d,".rds"))
              } else {
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_",d,".rds"))
              }
            }
          } else if (m==11){
            for (d in 1:30){
              if (d <10){
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_0",d,".rds"))
              } else {
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_",m,"_",d,".rds"))
              }
            }
          } else {
            for (d in 1:30){
              if (d <10){
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_0",d,".rds"))
              } else {
                saveRDS(a3, file = paste0("E:/Bryan/Projects/Ethiopia/Surface_NO2/Processed/",year[fi],"/Ethiopia_Processed_SurfaceNO2_StudyRegion_250m_",year[fi],"_0",m,"_",d,".rds"))
              }
            }
          }
          
        }
      }
  print(paste0("Finished file - ",file1[fi]))
  }
Sys.time()-Start
# Took 21.22874 mins to run 2018-2020
