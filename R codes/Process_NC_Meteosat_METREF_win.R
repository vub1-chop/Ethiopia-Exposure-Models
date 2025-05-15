library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/meteosat/METREF/PRODUCTS/MSG/MEM/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("D:/HaSET/Codes/Grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in Meteosat grid; need "foreign" library to read dbf from shapefile
meteosat <- read.csv("D:/HaSET/Codes/grids/Meteosat_METREF_StudyRegion.csv",as.is=T)

# read in date
date <- read.csv("D:/HaSET/Codes/date/date.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# load nc file ------------------------------------------------------------

for (i in 1446:length(year)){
  b = read_stars(paste0("D:/HaSET/Raw_NC/Meteosat_METREF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_METREF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"), quiet = TRUE)
  dat <- nc_open(paste0("D:/HaSET/Raw_NC/Meteosat_METREF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_METREF_MSG-Disk_",year[i],month[i],day[i],"0000.nc"))

  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  # extract METREF 
  full_METREF = b["METREF"]
  df_full_METREF <- as.data.frame(full_METREF)
  df_full_METREF <-  df_full_METREF %>%
    dplyr::select(-"time") %>% 
    filter(x >= 38.5 & x <= 40.25 & y >= 8.7 & y <= 10.3) 
  
  df_full_METREF$mean <- as.numeric(gsub(" \\[.*\\]", "", df_full_METREF$METREF))
  df_full_METREF <- subset(df_full_METREF, select = -METREF)
  
  colnames(df_full_METREF) <- c("lon","lat","mean")

  df_full_METREF$Num <- seq(1,nrow(df_full_METREF),1)
      
  # write.csv(df_full_METREF, file="/Users/lenovo/Desktop/HaSET/meteosat/METREF/METREF_before.csv")
    
  # add projection
  merge_METREF <- merge(df_full_METREF, meteosat[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
  merge_METREF <- na.omit(merge_METREF)
  
  coordinates(merge_METREF) <- ~ X_Pro + Y_Pro
  
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(mean ~ 1, merge_METREF, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("METREF")
  a3 <- cbind(a3,result)
  
  # write.csv(a3,file = "/Users/lenovo/Desktop/HaSET/meteosat/METREF/METREF_after.csv")
  
    

colnames(a3) <- c("FID","X","Y","X_Pro","Y_Pro", "METREF")

  # write.csv(a3, file="/Users/lenovo/Desktop/HaSET/meteosat/METREF/full_METREF_after.csv")
  
  saveRDS(a3, file = paste0("D:/HaSET/Processed/Meteosat_METREF/",year[i],"/Ethiopia_Processed_Meteosat_METREF_StudyRegion_250m_",year[i],month[i],day[i],".rds"))
  print(paste0("Finished Date - ",year[i],month[i],day[i]))
    
  }
  
Sys.time()-Start



# generate study area -----------------------------------------------------
# 
# coords <- df_full_METREF %>% mutate(X_Pro=lon,Y_Pro=lat) %>% select(-"mean")
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# Meteosat_METREF_StudyRegion_Pro <- as.data.frame(projected_coords)
# Meteosat_METREF_StudyRegion_Pro <- Meteosat_METREF_StudyRegion_Pro %>%
#   rename(X_Pro=coords.x1, Y_Pro=coords.x2)
# Meteosat_METREF_StudyRegion_Pro$Num <- seq(1:nrow(Meteosat_METREF_StudyRegion_Pro))
# write.csv(Meteosat_METREF_StudyRegion_Pro, file = paste0("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_METREF_StudyRegion.csv"), row.names=F)





