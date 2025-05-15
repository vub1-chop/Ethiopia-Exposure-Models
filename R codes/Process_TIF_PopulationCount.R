library(raster)
library(sp)
library(dplyr)
library(rgdal)
library(gstat)

# Load data ---------------------------------------------------------------

Start <- Sys.time()

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("D:/HaSET/Codes/Grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in population grid; need "foreign" library to read dbf from shapefile
population_count <- read.csv("D:/HaSET/Codes/Grids/Population_Count_StudyRegion_Pro.csv", as.is=T)

# read in date file
# date <- read.csv("/Users/lenovo/Desktop/HaSET/population/date/date2023.csv",colClasses=c("character","character","character"))
# year <- as.vector(date$yaer)
# month <- as.vector(date$month)
# day <- as.vector(date$day)


ls8 <- raster("D:/HaSET/Raw_TIF/popolation/population_count/gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif")
projection(ls8) # "+proj=longlat +datum=WGS84 +no_defs"

# code for study region extraction
# coords <- ls8.3 %>%
#   select("x","y") %>%
#   rename(lat=y, lon=x) %>%
#   mutate(X_Pro=lon, Y_Pro=lat)
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# population_count <- as.data.frame(projected_coords)
# population_count$num <- seq(1,dim(population_count)[1])
# population_count <- population_count %>% 
#   rename(X_Pro=coords.x1,Y_Pro=coords.x2)
# write.csv(population_count, file="D:/HaSET/Codes/Grids/Population_Count_StudyRegion_Pro.csv",row.names = FALSE)

# test code
# b = read_stars("/Users/lenovo/Desktop/HaSET/population/count/gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif", quiet = TRUE)
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/population/count/gpw-v4-population-count-rev11_2020_30_sec_tif/gpw_v4_population_count_rev11_2020_30_sec.tif")

# Extract data ------------------------------------------------------------

stats.tab <- c()
a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]
  
# Crop the original area
# Define the bounds in geographic coordinates (longitude/latitude)
lon_min <- 38.5
lon_max <- 40.25
lat_min <- 8.7
lat_max <- 10.3
  
# Crop the file using define bounds
geo_extent <- extent(lon_min, lon_max, lat_min, lat_max)
ls8_crop <- crop(ls8, geo_extent)

  
# Extract data from the cropped area
ls8.2 <- rasterToPoints(ls8_crop, spatial=T)
proj4string(ls8.2) # "+proj=longlat +datum=WGS84 +no_defs"
ls8.3 <- as.data.frame(ls8.2)
  
ls8.3 <- ls8.3 %>% 
  rename(
    count = gpw_v4_population_count_rev11_2020_30_sec,
    lat = y,
    lon = x
    )
ls8.3$num <- seq(1,dim(population_count)[1])
# write.csv(ls8.3, file = "/Users/lenovo/Desktop/HaSET/population/output/before_interpolation2020.csv", row.names=F)

# merge extracted population data with projection
  
ls8.4 <- merge(ls8.3,population_count[,c("num","X_Pro","Y_Pro")],by=c("num"))
ls8.4 <- na.omit(ls8.4)
coordinates(ls8.4) <- ~ X_Pro + Y_Pro  
  
# Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
krigged <- idw(count ~ 1, ls8.4, ExtendedGrid, nmax=4)
result <- data.frame(krigged$var1.pred)
colnames(result) <- paste0("count")
result_final <- cbind(a3,result)
result_final$count_grid <- (result_final$count)/16
  
  
# save as rds file
# for (fi in 1:length(year)){
# 
#   saveRDS(result_final, file = paste0("/Users/lenovo/Desktop/HaSET/population/output/",year[fi],"/Ethiopia_Processed_Population_Density_StudyRegion_250m_",year[fi],"_",month[fi],"_",day[fi],".rds"))
#   print(paste0("Finished file - ",year[fi],"_",month[fi],"_",day[fi]))
#   
# }
#   
# print(paste0("Finished file - ",year))
#   

saveRDS(result_final,file="D:/HaSET/Processed/Population_Count/Ethiopia_Processed_Population_Density_StudyRegion_250m_2020.rds")



