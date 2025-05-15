library(raster)
library(sp)
library(dplyr)
library(rgdal)

# Load data ---------------------------------------------------------------

Start <- Sys.time()

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# Read in ERA5 grid; need "foreign" library to read dbf from shapefile
population <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/population_StudyRegion_Pro.csv", as.is=T)

# read in date file
date <- read.csv("/Users/lenovo/Desktop/HaSET/population/date/date2023.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)


ls8 <- raster("/Users/lenovo/Desktop/HaSET/population/population_density/gpw_v4_population_density_rev11_2020_30_sec.tif")
projection(ls8)

# code for study region extraction
# coords <- ls8.3 %>%
#   select("x","y") %>%
#   rename(lat=y, lon=x) %>%
#   mutate(X_Pro=lon, Y_Pro=lat)
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# population <- as.data.frame(projected_coords)
# population$Num <- seq(1,dim(population)[1])
# write.csv(population, file="/Users/lenovo/Desktop/HaSET/processing/grids/population_StudyRegion_Pro.csv")

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
proj4string(ls8.2)
ls8.3 <- as.data.frame(ls8.2)
  
ls8.3 <- ls8.3 %>% 
  rename(
    density = gpw_v4_population_density_rev11_2020_30_sec,
    lat = y,
    lon = x
    )
ls8.3$Num <- seq(1,dim(population)[1])
# write.csv(ls8.3, file = "/Users/lenovo/Desktop/HaSET/population/output/before_interpolation2020.csv", row.names=F)

# merge extracted population data with projection
  
ls8.4 <- merge(ls8.3,population[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
ls8.4 <- na.omit(ls8.4)
coordinates(ls8.4) <- ~ X_Pro + Y_Pro  
  
# Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
krigged <- idw(density ~ 1, ls8.4, ExtendedGrid, nmax=4)
result <- data.frame(krigged$var1.pred)
colnames(result) <- paste0("density")
result_final <- cbind(a3,result)
  
  
# save as rds file
for (fi in 1:length(year)){

  saveRDS(result_final, file = paste0("/Users/lenovo/Desktop/HaSET/population/output/",year[fi],"/Ethiopia_Processed_Population_Density_StudyRegion_250m_",year[fi],"_",month[fi],"_",day[fi],".rds"))
  print(paste0("Finished file - ",year[fi],"_",month[fi],"_",day[fi]))
  
}
  
print(paste0("Finished file - ",year))
  





