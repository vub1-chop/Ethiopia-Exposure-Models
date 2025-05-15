library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# b = read_stars("/Users/lenovo/Desktop/HaSET/meteosat/MH/PRODUCTS/MSG/MMH-ASv2/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_MMH-ASv2_MSG-Disk_201801010000.nc", quiet = TRUE)
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/meteosat/MH/PRODUCTS/MSG/MMH-ASv2/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_MMH-ASv2_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro



# read in date
date <- read.csv("/Users/lenovo/Desktop/HaSET/date/date2018.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# load nc file ------------------------------------------------------------

for (i in 1:length(year)){
  
  # Read in Meteosat grid; need "foreign" library to read dbf from shapefile
  meteosat <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_MH_StudyRegion.csv",as.is=T)
  
  # MH for each day
  Dir <-paste0("/Users/lenovo/Desktop/HaSET/meteosat/MH/MHv3/NETCDF/",year[i],"/",month[i],"/",day[i],"/")
  file1 <- sort(list.files(Dir))
  
  # extract time (the last four digits before .nc)
  time <- sub(".*(\\d{4})\\.nc$", "\\1", file1)
  
  # Dataframe to bind all interpolated columns
  a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
  # extract MH in each file
  for (fi in 1:length(file1)){
    
    b = read_stars(paste0(Dir,file1[fi]), quiet = TRUE)
    dat <- nc_open(paste0(Dir,file1[fi]))
    
    full = b["MH"]
    df_full <- as.data.frame(full)
    
    merge <-  df_full %>%
      select(-"time") %>% 
      filter(x >= 38.5 & x <= 40.25 & y >= 8.7 & y <= 10.3)
    
    # convert unit variable into numeric
    MH <- merge %>% 
      mutate(!!time[fi] := as.numeric(sub(" \\[W/m^2\\]", "", merge$MH))) %>% 
      select(time[fi]) %>% 
      mutate(Num=seq(1:nrow(merge)))
    
    
    meteosat <- merge(meteosat,MH,by="Num")
    print(paste0("Finished Date - ",year[i],month[i],day[i],time[fi]))
    
    
  }
  
  # calculate mean daily MH
  MH_mean <- meteosat %>%  
    mutate(mean = rowMeans(select(., 6:ncol(meteosat)), na.rm = TRUE)) %>%
    # mutate(mean = rowMeans(select(., 6:(length(file1)+5)), na.rm = TRUE)) %>%
    select("Num","x","y","X_Pro","Y_Pro","mean")
  MH_mean <- na.omit(MH_mean)
  
  # interpolation
  coordinates(MH_mean) <- ~ X_Pro + Y_Pro
  
  # Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
  krigged <- idw(mean ~ 1, MH_mean, ExtendedGrid, nmax=4)
  result <- data.frame(krigged$var1.pred)
  colnames(result) <- paste0("MH")
  a3 <- cbind(a3,result)
  
  saveRDS(a3, file = paste0("/Users/lenovo/Desktop/HaSET/processing/Meteosat_MH/",year[i],"/Ethiopia_Processed_Meteosat_MH_StudyRegion_250m_",year[i],month[i],day[i],".rds"))
  print(paste0("Finished Date - ",year[i],month[i],day[i]))
  
  
}




Sys.time()-Start

# generate study area -----------------------------------------------------

# coords <- merge %>% mutate(X_Pro=x,Y_Pro=y) %>% select(-"MH")
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# Meteosat_MH_StudyRegion_Pro <- as.data.frame(projected_coords)
# Meteosat_MH_StudyRegion_Pro <- Meteosat_MH_StudyRegion_Pro %>%
#   rename(X_Pro=coords.x1, Y_Pro=coords.x2) %>%
#   mutate(Num=seq(1,nrow(Meteosat_MH_StudyRegion_Pro),1))
# write.csv(Meteosat_MH_StudyRegion_Pro, file = paste0("/Users/lenovo/Desktop/HaSET/processing/grids/Meteosat_MH_StudyRegion.csv"), row.names=F)

