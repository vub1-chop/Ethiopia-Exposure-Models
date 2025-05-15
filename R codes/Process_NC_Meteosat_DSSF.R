library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/meteosat/DSSF/PRODUCTS/MSG/MEM/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in Meteosat grid; need "foreign" library to read dbf from shapefile
meteosat <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_DSSF_StudyRegion.csv",as.is=T)

# read in date
date <- read.csv("/Users/lenovo/Desktop/HaSET/date/date2018.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# load nc file ------------------------------------------------------------

for (i in 1:length(year)){
  b = read_stars(paste0("/Users/lenovo/Desktop/HaSET/meteosat/DSSF/PRODUCTS/MSG/MDIDSSF/NETCDF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_DIDSSF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"), quiet = TRUE)
  dat <- nc_open(paste0("/Users/lenovo/Desktop/HaSET/meteosat/DSSF/PRODUCTS/MSG/MDIDSSF/NETCDF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_DIDSSF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"))

  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  # extract DSSF 
  full_DSSF = b["DSSF"]
  df_full_DSSF <- as.data.frame(full_DSSF)
  df_full_DSSF <-  df_full_DSSF %>%
    dplyr::select(-"time") %>% 
    filter(x >= 38.5 & x <= 40.25 & y >= 8.7 & y <= 10.3) 
  
  df_full_DSSF$mean <- as.numeric(gsub(" \\[.*\\]", "", df_full_DSSF$DSSF))
  df_full_DSSF <- subset(df_full_DSSF, select = -DSSF)
  
  colnames(df_full_DSSF) <- c("lon","lat","mean")

  df_full_DSSF$Num <- seq(1,nrow(df_full_DSSF),1)
      
  # write.csv(df_full_DSSF, file="/Users/lenovo/Desktop/HaSET/meteosat/DSSF/DSSF_before.csv")
    
  # add projection
  merge_DSSF <- merge(df_full_DSSF, meteosat[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
  merge_DSSF <- na.omit(merge_DSSF)
  
  coordinates(merge_DSSF) <- ~ X_Pro + Y_Pro
  
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(mean ~ 1, merge_DSSF, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("DSSF")
  a3 <- cbind(a3,result)
  
  # write.csv(a3,file = "/Users/lenovo/Desktop/HaSET/meteosat/DSSF/DSSF_after.csv")
  
    

colnames(a3) <- c("FID","X","Y","X_Pro","Y_Pro", "DSSF")

  # write.csv(a3, file="/Users/lenovo/Desktop/HaSET/meteosat/DSSF/full_DSSF_after.csv")
  
  saveRDS(a3, file = paste0("/Users/lenovo/Desktop/HaSET/processing/Meteosat_DSSF/",year[i],"/Ethiopia_Processed_Meteosat_DSSF_StudyRegion_250m_",year[i],month[i],day[i],".rds"))
  print(paste0("Finished Date - ",year[i],month[i],day[i]))
    
  }
  
Sys.time()-Start



# generate study area -----------------------------------------------------
# 
# coords <- df_full_DSSF %>% mutate(X_Pro=lon,Y_Pro=lat) %>% select(-"mean")
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# Meteosat_DSSF_StudyRegion_Pro <- as.data.frame(projected_coords)
# Meteosat_DSSF_StudyRegion_Pro <- Meteosat_DSSF_StudyRegion_Pro %>%
#   rename(X_Pro=coords.x1, Y_Pro=coords.x2)
# Meteosat_DSSF_StudyRegion_Pro$Num <- seq(1:nrow(Meteosat_DSSF_StudyRegion_Pro))
# write.csv(Meteosat_DSSF_StudyRegion_Pro, file = paste0("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_DSSF_StudyRegion.csv"), row.names=F)





