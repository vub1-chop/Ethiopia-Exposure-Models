library(raster)
library(sp)
library(dplyr)
# library(rgdal)
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
# EC_StudyRegion_Pro <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/EC_StudyRegion_Pro.csv")

# read in hmi data
ls8 <- raster("/Users/lenovo/Desktop/HaSET/human_modification_index/lulc_human_modification_terrestrial_systems_geographic.tif")
projection(ls8) # "+proj=longlat +datum=WGS84 +no_defs"

# Read in electricity grid; need "foreign" library to read dbf from shapefile
HMI_StudyRegion_Pro <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/HMI_StudyRegion_Pro.csv")

# Define the bounds in geographic coordinates (longitude/latitude)
lon_min <- 38.5
lon_max <- 40.25
lat_min <- 8.7
lat_max <- 10.3

# Create an extent object in geographic coordinates
geo_extent <- extent(lon_min, lon_max, lat_min, lat_max)

# Crop the GDP tif data with defined area
ls8.2 <- crop(ls8, geo_extent)


# Extraction --------------------------------------------------------------

# extract data
ls8.2 <- rasterToPoints(ls8.2, spatial=T)
proj4string(ls8.2)

# convert to data frame
ls8.3 <- na.omit(as.data.frame(ls8.2))

ls8.3 <- ls8.3 %>% 
  rename(lon=x,lat=y,hmi=lulc_human_modification_terrestrial_systems_geographic)

summary(ls8.3)

ls8.3$Num <- seq(1,dim(ls8.3)[1])

# write.csv(ls8.3, file = "/Users/lenovo/Desktop/HaSET/human_modification_index/before_interpolation_hmi.csv", row.names=F)

# merge extracted GDP data with projection
ls8.4 <- merge(ls8.3,HMI_StudyRegion_Pro[,c("Num","X_Pro","Y_Pro")],by=c("Num"))
coordinates(ls8.4) <- ~ X_Pro + Y_Pro  

# Interpolation -----------------------------------------------------------

# Interpolation -- "gstat" library needed for idw function, searching 4 nearest neighbors
krigged <- idw(hmi ~ 1, ls8.4, ExtendedGrid, nmax=4)
result <- data.frame(krigged$var1.pred)
colnames(result) <- paste0("hmi")
result_final <- cbind(a3,result)
write.csv(result_final, file = "/Users/lenovo/Desktop/HaSET/processing/HMI/hmi_after_interpolation.csv", row.names=F)

