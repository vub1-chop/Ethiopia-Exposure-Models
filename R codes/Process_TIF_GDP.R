library(raster)
library(sp)
library(dplyr)
library(rgdal)
library(gstat)

# Load data ---------------------------------------------------------------

Start <- Sys.time()

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

a3 <- a[,c("FID","X","Y","X_Pro","Y_Pro")]

# Read in GDP grid; need "foreign" library to read dbf from shapefile
GDP_StudyRegion_Pro <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/GDP_StudyRegion_Pro.csv")

# read in GDP data
ls8 <- raster("/Users/lenovo/Desktop/HaSET/GDP/20/2019GDP.tif")
projection(ls8) # "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


# code for GDP study region extraction
# coords <- ls8.3[,c("lon","lat")]
# coords <- coords %>% mutate(X_Pro=lon,Y_Pro=lat)
# coordinates(coords) <- ~X_Pro+Y_Pro
# proj4string(coords) <- CRS("+init=epsg:4326")
# projected_coords <- spTransform(coords, CRS("+init=epsg:32637"))
# GDP_StudyRegion_Pro <- as.data.frame(projected_coords)
# GDP_StudyRegion_Pro$Num <- seq(1,dim(GDP_StudyRegion_Pro)[1])
# write.csv(GDP_StudyRegion_Pro, file="/Users/lenovo/Desktop/HaSET/processing/grids/GDP_StudyRegion_Pro.csv")


# Unify projection -----------------------------------------------

# Convert Mollweide projection into longitude-latitude CRS
Start <- Sys.time()
proj_ls8 <- projectRaster(ls8, crs = "+proj=longlat +datum=WGS84 +no_defs")
Sys.time()-Start


# Extract data ------------------------------------------------------------

# Define the bounds in geographic coordinates (longitude/latitude)
lon_min <- 38.5
lon_max <- 40.25
lat_min <- 8.7
lat_max <- 10.3

# Create an extent object in geographic coordinates
geo_extent <- extent(lon_min, lon_max, lat_min, lat_max)

# Crop the GDP tif data with defined area
ls8.2 <- crop(proj_ls8, geo_extent)

# Extract data from the cropped area and convert into longlat projection
ls8.2 <- rasterToPoints(ls8.2, spatial=T)
proj4string(ls8.2)  # "+proj=longlat +datum=WGS84 +no_defs"

ls8.3 <- na.omit(as.data.frame(ls8.2))
ls8.3 <- ls8.3 %>% 
  rename(lon=x,lat=y)
summary(ls8.3)

ls8.3$Num <- seq(1,dim(ls8.3)[1])
# write.csv(ls8.4, file = "/Users/lenovo/Desktop/HaSET/processing/GDP/before_interpolation2018.csv", row.names=F)

# merge extracted GDP data with projection

ls8.4 <- merge(ls8.3,GDP_StudyRegion_Pro[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
coordinates(ls8.4) <- ~ X_Pro + Y_Pro  


# Interpolation -----------------------------------------------------------

# Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
krigged <- idw(X2019GDP ~ 1, ls8.4, ExtendedGrid, nmax=4)
result <- data.frame(krigged$var1.pred)
colnames(result) <- paste0("GDP")
result_final <- cbind(a3,result)
# write.csv(result_final, file = "/Users/lenovo/Desktop/HaSET/processing/GDP/after_interpolation2018.csv", row.names=F)


# Save as RDS file --------------------------------------------------------

# read in date file
date <- read.csv("/Users/lenovo/Desktop/HaSET/population/date/date2023.csv",colClasses=c("character","character","character"))
year <- as.vector(date$yaer)
month <- as.vector(date$month)
day <- as.vector(date$day)

# save as rds file
for (fi in 1:length(year)){
  
  saveRDS(result_final, file = paste0("/Users/lenovo/Desktop/HaSET/processing/GDP/",year[fi],"/Ethiopia_Processed_GDP_250m_",year[fi],"_",month[fi],"_",day[fi],".rds"))
  print(paste0("Finished file - ",year[fi],"_",month[fi],"_",day[fi]))
  
}

print(paste0("Finished file - ",year))






