#install.packages("rgdal", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)
#install.packages("raster", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)
#install.packages("gstat", repos="https://stat.ethz.ch/CRAN/", dependencies=TRUE)

library(stars)
library(sp)
library(raster)
library(gstat)
library(ncdf4)
library(lubridate)
library(terra)
library(tidyverse)
library(stringr)

Start <- Sys.time()
# Read in 1km Grid; need "foreign" library to read dbf from shapefile
#a <- read.csv("E:/Bryan/Projects/Ethiopia/ThiessenFishnet_1km_noborder_to250mGrid_Pro.csv",as.is=T)

# Subset only XY columns
#ExtendedGrid <- data.frame(a[,c("X_Pro","Y_Pro")])
# Convert to spatial
#coordinates(ExtendedGrid) = ~X_Pro + Y_Pro

# CRS for the MODIS data
sincrs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m"

# CRS we want (lon & lat)
lonlat <- '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0' 
# Read in Merra2 grid; need "foreign" library to read dbf from shapefile
#merra <- read.csv("E:/Bryan/Projects/Ethiopia/Merra2_StudyRegion_Pro.csv",as.is=T)

# Directory of satellite data
Dir <-paste("D:/HaSET/Raw_HDF/MODIS_Aqua_Land_Surface_Temperature/2023/",sep="")
# Dir <-paste("E:/Bryan/Projects/Ethiopia/MODIS/AOD/",sep="")
# Get file names
file1 <- sort(list.files(Dir))                    
#years <- substr(file1,10,13)
#doy <- substr(file1,14,16)
days <- substr(file1,10,17)
dates <- unique(days)
#dates <- as.Date(as.numeric(doy), origin = paste0(as.numeric(unique(years))-1,"-12-31"))
#months <- month(dates)
#day <- day(dates)
tile <- substr(file1,19,24)

missingness <- c()
for (fi in 1:length(dates)){
  
  # return the indices of the elements in file1 that contain assigned dates
  # e.g. fi=1, return the indices in file1 that contains dates[1]("2018001")
  files <- grep(dates[fi],file1)
  
  alldat <- c()
  for (f in 1:length(files)){
    
    # "stars" library needed to read in .nc files
    #b = read_stars(paste0(Dir,file1[fi]), quiet = TRUE)
    dat <- nc_open(paste0(Dir,file1[files[f]]))
    #lon <- data.frame(lon=dat$dim$'XDim:grid1km'$vals)
    #lat <- data.frame(lat=dat$dim$'YDim:grid1km'$vals)
    
    
    
    # create a grid manually to store extracted data,and divide by 1200
    dat2 <- data.frame(Num=seq(1,(1200*1200),1))
    
    # boundary of each tile
    if (tile[f]=="h21v08"){
      lon_test <- seq(3335851.559,4447802.078667,length.out=1200)
      lat_test <- seq(0,1111950.519667,length.out=1200)
    } else if (tile[f]=="h22v07"){
      lon_test <- seq(4447802.078667,5559752.598333,length.out=1200)
      lat_test <- seq(1111950.519667,2223901.039333,length.out=1200)
    } else if (tile[f]=="h22v08"){
      lon_test <- seq(4447802.078667,5559752.598333,length.out=1200)
      lat_test <- seq(0,1111950.519667,length.out=1200)
    } else { #for tile h21v07
      lon_test <- seq(3335851.559,4447802.078667,length.out=1200)
      lat_test <- seq(1111950.519667,2223901.039333,length.out=1200)
    }
    
    DF <- data.frame(x=lon_test,y=lat_test)
    
    # transform to the CRS we want
    s <- SpatialPoints(DF, proj4string=CRS(sincrs))
    x <- spTransform(s, lonlat)
    summary(as.data.frame(x))
    x2 <- as.data.frame(x)
    colnames(x2) <- c("lon","lat")
    dat2$lon <-rep(x2$lon,times=nrow(x2))
    dat2$lat <-rep(x2$lat,each=nrow(x2))
    
    vars <- c("LST_1KM")
    
    for (v in 1:length(vars)){
      
      vari <- ncvar_get(dat,vars[v])
      dims <- dim(vari)
      
      if (length(dims) ==2){
        
        vari2 <- data.frame(V = as.vector(vari[1:1200, 1:1200]))
        dat2 <- cbind(dat2,vari2)
        
      }else if (length(dims)==3){
        
        avg.orb <- dat2
        for (d in 1:dims[3]){
          vari2 <- data.frame(as.vector(vari[1:1200, 1:1200, d]))
          avg.orb <- cbind(avg.orb,vari2)
        }
        avg.orb$Avg <- rowMeans(avg.orb[,4:length(avg.orb)], na.rm=TRUE)
        dat2 <- cbind(dat2,avg.orb$Avg)
        
      }else{
        
        stop("Unexpected number of dimensions in vari")
        
      }
      
      
      # if (length(dims) == 3 && dims[3] >= 2) {
      #   vari2 <- data.frame(V = as.vector(vari[1:1200, 1:1200, 1]))
      # } else if (length(dims) == 3 && dims[3] == 1) {
      #   vari2 <- data.frame(V = as.vector(vari[1:1200, 1:1200]))
      # } else if (length(dims) == 2) {
      #   vari2 <- data.frame(V = as.vector(vari[1:1200, 1:1200]))
      # } else {
      #   stop("Unexpected number of dimensions in vari")
      # }
      
      # print(paste0("Finished - ",vars[v]))
    }
    
    nc_close(dat)
    
    colnames(dat2) <- c("ID","lon","lat",vars)
    datsub <- subset(dat2,lon >= 38.0 & lon <= 40.75 & lat >= 8.5 & lat <= 10.5)
    alldat <- rbind(alldat,datsub)
    
  }
  missing <- data.frame(Date= dates[fi], 
                        Missing_Count=nrow(alldat[rowSums(is.na(alldat)) > 0,]),
                        Missing_Perct=(nrow(alldat[rowSums(is.na(alldat)) > 0,])/nrow(alldat))*100)
  missingness <- rbind(missingness,missing)
  
  write.csv(alldat, file = paste0("D:/HaSET/Processed/MODIS_Aqua_LST/2023/MODIS_Aqua_LST_RAW_",dates[fi],".csv"), row.names=F)
  print(paste0("Finished date - ",dates[fi]))
}

write.csv(missingness, file = paste0("D:/HaSET/Processed/MODIS_Aqua_LST/missingness_new/MODIS_Aqua_LST_RAW_MissingnessTable_2023.csv"), row.names=F)

print("Finished - 2023")

Sys.time()-Start

# rework

num_file <- c()
for (fi in 1:length(dates)){
  cal <- length(files <- grep(dates[fi],file1))
  num_file <- c(num_file,cal)
}
print(num_file)

index <- which(num_file!=4)
print(index)


master <- read.csv("D:/HaSET/Processed/MODIS_Aqua_LST/2020/MODIS_Aqua_LST_RAW_A2020001.csv")

test <- read.csv("D:/HaSET/Processed/MODIS_Aqua_LST/2023/MODIS_Aqua_LST_RAW_A2023133.csv")


# for dates with >4 files
mean <- test %>%
  group_by(ID) %>% 
  summarise(mean_lst=mean(LST_1KM,na.rm=TRUE))

all_dat <- merge(mean,master,by="ID")

all_dat <- all_dat %>% 
  select(-LST_1KM) %>% 
  rename(LST_1KM=mean_lst)

# for dates with <4 files
master <- master %>% select(-LST_1KM)
test <- test %>% select(-lat,-lon)
all_dat <- left_join(master, test, by="ID")

write.csv(all_dat, file="D:/HaSET/Processed/MODIS_Aqua_LST/2023/MODIS_Aqua_LST_RAW_A2023133.csv",row.names = FALSE)

