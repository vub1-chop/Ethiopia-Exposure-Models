library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# dat <- nc_opecn("/Users/lenovo/Desktop/HaSET/meteosat/DSLF/PRODUCTS/MSG/MEM/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("D:/HaSET/Codes/Grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatialc
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in Meteosat grid; need "foreign" library to read dbf from shapefile
meteosat <- read.csv("D:/HaSET/Codes/grids/Meteosat_DSSF_StudyRegion.csv",as.is=T)

# read in date
date <- read.csv("D:/HaSET/Codes/date/date.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# load nc file ------------------------------------------------------------

for (i in 519:length(year)){
  b = read_stars(paste0("D:/HaSET/Raw_NC/Meteosat_DSLF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_DIDSLF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"), quiet = TRUE)
  dat <- nc_open(paste0("D:/HaSET/Raw_NC/Meteosat_DSLF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_DIDSLF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"))

  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  # extract DSLF 
  full_DSLF = b["DSLF"]
  df_full_DSLF <- as.data.frame(full_DSLF)
  df_full_DSLF <-  df_full_DSLF %>%
    dplyr::select(-"time") %>% 
    filter(x >= 38.5 & x <= 40.25 & y >= 8.7 & y <= 10.3) 
  
  df_full_DSLF$mean <- as.numeric(gsub(" \\[.*\\]", "", df_full_DSLF$DSLF))
  df_full_DSLF <- subset(df_full_DSLF, select = -DSLF)
  
  colnames(df_full_DSLF) <- c("lon","lat","mean")
  
  df_full_DSLF$Num <- seq(1,nrow(df_full_DSLF),1)
  
  # write.csv(df_full_DSLF, file="/Users/lenovo/Desktop/HaSET/meteosat/DSLF/DSLF_before.csv")
  
  # add projection
  merge_DSLF <- merge(df_full_DSLF, meteosat[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
  merge_DSLF <- na.omit(merge_DSLF)
  
  coordinates(merge_DSLF) <- ~ X_Pro + Y_Pro
  
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(mean ~ 1, merge_DSLF, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("DSLF")
  a3 <- cbind(a3,result)
  
  # write.csv(a3,file = "/Users/lenovo/Desktop/HaSET/meteosat/DSLF/DSLF_after.csv")
  
  
  
  colnames(a3) <- c("FID","X","Y","X_Pro","Y_Pro", "DSLF")
  
  # write.csv(a3, file="/Users/lenovo/Desktop/HaSET/meteosat/DSLF/full_DSLF_after.csv")
  
  saveRDS(a3, file = paste0("D:/HaSET/Processed/Meteosat_DSLF/",year[i],"/Ethiopia_Processed_Meteosat_DSLF_StudyRegion_250m_",year[i],month[i],day[i],".rds"))
  print(paste0("Finished Date - ",year[i],month[i],day[i]))
  
}

Sys.time()-Start



# generate study area -----------------------------------------------------
# 
# coords <- df_full_DSLF %>% mutate(X_Pro=lon,Y_Pro=lat) %>% select(-"mean")
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# Meteosat_DSLF_StudyRegion_Pro <- as.data.frame(projected_coords)
# Meteosat_DSLF_StudyRegion_Pro <- Meteosat_DSLF_StudyRegion_Pro %>%
#   rename(X_Pro=coords.x1, Y_Pro=coords.x2)
# Meteosat_DSLF_StudyRegion_Pro$Num <- seq(1:nrow(Meteosat_DSLF_StudyRegion_Pro))
# write.csv(Meteosat_DSLF_StudyRegion_Pro, file = paste0("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_DSLF_StudyRegion.csv"), row.names=F)



