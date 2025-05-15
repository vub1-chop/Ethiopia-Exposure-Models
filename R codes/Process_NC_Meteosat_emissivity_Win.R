library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/meteosat/LSE/PRODUCTS/MSG/MEM/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("D:/HaSET/Codes/Grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in Meteosat grid; need "foreign" library to read dbf from shapefile
meteosat <- read.csv("D:/HaSET/Codes/Grids/Meteosat_LSE_StudyRegion.csv",as.is=T)

# read in date
date <- read.csv("D:/HaSET/Codes/date/date2023.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# load nc file ------------------------------------------------------------

for (i in 1:length(year)){
  b = read_stars(paste0("D:/HaSET/Raw_NC/Meteosat_LSE/PRODUCTS/MSG/MEM/NETCDF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_",year[i],month[i],day[i],"0000.nc"), quiet = TRUE)
  dat <- nc_open(paste0("D:/HaSET/Raw_NC/Meteosat_LSE/PRODUCTS/MSG/MEM/NETCDF/",year[i],"/",month[i],"/",day[i],"/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_",year[i],month[i],day[i],"0000.nc"))
  

  # Names of variables to extract
  vars <- c("EM_108","EM_120","EM_39","EM_87","EM_BB")

  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  for (v in 1:length(vars)){
    
    full_LSE = b[vars[v]]
    df_full_LSE <- as.data.frame(full_LSE)
    df_full_LSE <-  df_full_LSE %>%
      dplyr::select(-"time") %>% 
      filter(x >= 38.5 & x <= 40.25 & y >= 8.7 & y <= 10.3)
    colnames(df_full_LSE) <- c("lon","lat","mean")

      df_full_LSE$Num <- seq(1,nrow(df_full_LSE),1)
      
      # write.csv(df_full_LSE, file="/Users/lenovo/Desktop/HaSET/meteosat/LSE/PRODUCTS/MSG/MEM/NETCDF/LSE_before.csv")
      
      # add projection
      merge_LSE <- merge(df_full_LSE, meteosat[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
      merge_LSE <- na.omit(merge_LSE)
      
      # write.csv(merge_LSE, file = paste0("/Users/lenovo/Desktop/HaSET/processing/Meteosat_LSE/Meteosat_LSE_StudyRegion_20180101_test.csv"), row.names=F)
      # dat4$point_x <- cbind(dat4$XPro,dat4$YPro)[,1]
      # dat4$point_y <- cbind(dat4$XPro,dat4$YPro)[,2]
      
      coordinates(merge_LSE) <- ~ X_Pro + Y_Pro
      
      # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
      krigged <- idw(mean ~ 1, merge_LSE, ExtendedGrid, nmax=4)
      result <- data.frame(krigged$var1.pred)
      colnames(result) <- paste0(vars[v],"_",year[i],"_",month[i],"_",day[i])
      a3 <- cbind(a3,result)
      
      # write.csv(a3,file = "/Users/lenovo/Desktop/HaSET/meteosat/LSE/PRODUCTS/MSG/MEM/NETCDF/LSE_after.csv")
      
      print(paste0("Finished variable - ",vars[v], " - for date ",year[i],"-",month[i],"-",day[i]))
      
  }
  
  colnames(a3) <- c("FID","X","Y","X_Pro","Y_Pro", "EM_108","EM_120","EM_39","EM_87","EM_BB")
  a3$mean <- rowMeans(a3[, c("EM_108","EM_120","EM_39","EM_87","EM_BB")], na.rm = TRUE)

  # write.csv(a3, file="/Users/lenovo/Desktop/HaSET/meteosat/LSE/PRODUCTS/MSG/MEM/NETCDF/full_LSE_after.csv")
  
  saveRDS(a3, file = paste0("D:/HaSET/Processed/Meteosat_LSE/",year[i],"/Ethiopia_Processed_Meteosat_LSE_StudyRegion_250m_",year[i],month[i],day[i],".rds"))
  print(paste0("Finished Date - ",year[i],month[i],day[i]))
    
  }
  
Sys.time()-Start



# generate study area -----------------------------------------------------

# coords <- df_full_LSE %>% mutate(X_Pro=lon,Y_Pro=lat)
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# Meteosat_LSE_StudyRegion_Pro <- as.data.frame(projected_coords)
# Meteosat_LSE_StudyRegion_Pro <- Meteosat_LSE_StudyRegion_Pro %>%
#   rename(X_Pro=coords.x1, Y_Pro=coords.x2) %>% select(-"mean")
# Meteosat_LSE_StudyRegion_Pro$Num <- seq(1:nrow(Meteosat_LSE_StudyRegion_Pro))
# write.csv(Meteosat_LSE_StudyRegion_Pro, file = paste0("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_LSE_StudyRegion.csv"), row.names=F)
