library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(tidyverse)

# test code
# dat <- nc_open("/Users/lenovo/Desktop/HaSET/meteosat/LAI/PRODUCTS/MSG/MEM/NETCDF/2018/01/01/NETCDF4_LSASAF_MSG_EMMAPS_MSG-Disk_201801010000.nc")

Start <- Sys.time()


# read in necessary file --------------------------------------------------

# Read in 1km Grid; need "foreign" library to read dbf from shapefile
a <- read.csv("/Users/lenovo/Desktop/HaSET/processing/grids/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)
