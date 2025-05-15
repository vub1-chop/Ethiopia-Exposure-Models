library(tidyverse)
library(lubridate)

# spatial joined file from ArcGIS
# master1 <- read.csv("D:/HaSET/Processed/MODIS_Aqua_LST/MODIS_Match_ID.csv")
master <- read.csv("D:/HaSET/Bryan/Ethiopia/ThiessenFishnet_1km_noborder_to250mGrid_Pro_wMODIS_AODgrid.csv")
master2 <- subset(master,Distance<137)
master2$modis_id <- master2$ID_1
master2$grid_id <- master2$ORIG_FID
master3 <- master2[,c("grid_id","X","Y","X_Pro","Y_Pro","modis_id")]

# file for population count
population_count <- readRDS("D:/HaSET/Processed/Population_Count/Ethiopia_Processed_Population_Density_StudyRegion_250m_2023.rds")
population_count <- population_count %>% 
  rename(grid_id=FID) %>% 
  select(grid_id, count, count_grid)

# file for land use
hmi <- read.csv("D:/HaSET/Processed/human_modification_index.csv")
hmi <- hmi %>% 
  rename(grid_id=FID) %>% 
  select(grid_id,hmi)

# file for elevation
elevation <- read.csv("D:/HaSET/Processed/ASTER_GDEM_Elevation.csv")
elevation <- elevation %>% 
  rename(grid_id=ORIG_FID) %>% 
  select(grid_id,elevation_mean)

# function to calculate relative humidity
calculate_relative_humidity <- function(dew_point_K, temp_K) {
  rh <- 100 * (exp((17.625 * (dew_point_K - 273.15)) / (dew_point_K - 30.11))/ 
                 exp((17.625 * (temp_K - 273.15)) / (temp_K - 30.11)))
  return(rh)
}

# generate a file w/o NA
rm_na <- c()

for (y in 2023:2023){
  
  # load date file
  date <- read.csv(paste0("D:/HaSET/Codes/date/date",y,".csv"),colClasses=c("character","character","character"))
  year <- as.vector(date$year)
  month <- as.vector(date$month)
  day <- as.vector(date$day)
  
  # generate a list of date
  #dates_2023 <- seq(from = as.Date("2023-01-01"), to = as.Date("2023-12-31"), by = "day")
  dates_all <- seq(from = as.Date(paste0(year[1],"-",month[1],"-",day[1])), to = as.Date(paste0(year[length(year)],"-",month[length(year)],"-",day[length(year)])), by = "day")
  
  Start <- Sys.time()
  # directory for processed MODIS Aqua temperature
  # dir_aqua_lst <-paste("D:/HaSET/Processed/MODIS_Aqua_LST/2023/",sep="")
  # file_aqua_lst <- sort(list.files(dir_aqua_lst))
  
  # directory for processed MODIS Terra temperature
  # dir_terra_lst <-paste("D:/HaSET/Processed/MODIS_Terra_LST/2023/",sep="")
  # file_terra_lst <- sort(list.files(dir_terra_lst))
  
  # directory for processed MODIS AOD
  dir_aod <-paste("D:/HaSET/Processed/MODIS_AOD/",y,"/",sep="")
  file_aod <- sort(list.files(dir_aod)) 
  
  # directory for processed ERA5
  dir_era5 <- paste("D:/HaSET/Processed/EAR5/processed/",y,"/", sep="")
  file_era5 <- sort(list.files(dir_era5))
  
  # directory for population density
  dir_population_density <- paste("D:/HaSET/Processed/Population_Density/",y,"/",sep="")
  file_population_density <- sort(list.files(dir_population_density))
  
  for (i in 1:length(dates_all)){
    
    # load processed MODIS Aqua temp data
    # aqua_lst <- read.csv(paste0(dir_aqua_lst,file_aqua_lst[i]))
    # aqua_lst <- aqua_lst %>% rename(modis_id=ID,aqua_lst=LST_1KM)
    
    # load processed MODIS Terra temp data
    # terra_lst <- read.csv(paste0(dir_terra_lst,file_terra_lst[i]))
    # terra_lst <- terra_lst %>% rename(modis_id=ID,terra_lst=LST_1KM)
    
    # load processed MODIS AOD data
    aod <- read.csv(paste0(dir_aod,file_aod[i]))
    aod <- aod %>% rename(modis_id=ID)
    
    # load processed population data
    population_density <- readRDS(paste0(dir_population_density,file_population_density[i]))
    population_density <- population_density %>% 
      rename(grid_id=FID) %>% 
      select(grid_id,density)
    
    # load processed ERA5 data
    era5 <- readRDS(paste0(dir_era5,file_era5[i]))
  era5 <- era5 %>% 
      rename(grid_id=ORIG_FID) %>% 
      select(-c("Id","FID","X","Y","X_Pro","Y_Pro"))
    
    # match Aqua temp data to the 250m grid
    # match_modis_grid <- left_join(x=master,
    #                           y=aqua_lst[,c("modis_id","aqua_lst")],
    #                           by="modis_id")
    
    # match Terra temp data to the 250m grid                          
    # match_modis_grid <- left_join(x=match_modis_grid,
    #                           y=terra_lst[,c("modis_id","terra_lst")],
    #                           by="modis_id")                          
    
    # match AOD data to the 250m grid
    match_modis_grid <- left_join(x=master3,
                                  y=aod[,c("modis_id","Optical_Depth_047","Optical_Depth_055")],
                                  by="modis_id")      
    
    # merge ERA5 data
    match_modis_grid <- right_join(x=match_modis_grid,
                                  y=era5,
                                  by="grid_id")
    
    # merge population density data
    match_modis_grid <- left_join(x=match_modis_grid,
                                  y=population_density,
                                  by="grid_id")
    
    # merge population count data
    match_modis_grid <- left_join(x=match_modis_grid,
                                  y=population_count,
                                  by="grid_id")
    
    # merge elevation data
    match_modis_grid <- left_join(x=match_modis_grid,
                                  y=elevation,
                                  by="grid_id")
    
    # merge land use data
    match_modis_grid <- left_join(x=match_modis_grid,
                                  y=hmi,
                                  by="grid_id")
    
    
    colnames(match_modis_grid) <- tolower(colnames(match_modis_grid))
    
    # calculate cos(yday) and sin(yday)
    match_modis_grid$date <- ymd(dates_all[i])
    match_modis_grid$yday <- yday(match_modis_grid$date)
    match_modis_grid$cos_date <- cos(2*pi*match_modis_grid$yday/365)
    match_modis_grid$sin_date <- sin(2*pi*match_modis_grid$yday/365)
    
    # calculate wind speed from u and v
    match_modis_grid$wind_speed <- sqrt(match_modis_grid$u10^2+match_modis_grid$v10^2)
    
    # calculate wind direction
    match_modis_grid$wind_direction <- atan2(-match_modis_grid$u10,-match_modis_grid$v10)*(180/pi)+180
    
    match_modis_grid <- match_modis_grid %>%
      mutate(wind_direction = ifelse(wind_direction > 360, wind_direction - 360, wind_direction))
    
    
    # calculate relative humidity from dew point
    match_modis_grid$rh <- calculate_relative_humidity(match_modis_grid$d2m,
                                                       match_modis_grid$t2m)
    
    # select predictor for gap filling
    gap_filling <- match_modis_grid %>% 
      select(x,y,x_pro,y_pro,grid_id,optical_depth_047,optical_depth_055,
             t2m,rh,elevation_mean,hmi,wind_speed,wind_direction,
             density, count, count_grid,
             sin_date,cos_date,yday,date)
    
    
    # generate a file w/o NA
    
    grid_rmna <- na.omit(gap_filling)
    rm_na <- rbind(rm_na,grid_rmna)
    
    # daily file for AOD, LST and ERA5
    write.csv(match_modis_grid, file=paste0("D:/HaSET/Processed/spatial_join_result/full_082124/",y,"/Spatial_Join_Full_data_250m_",year[i],"_",month[i],"_",day[i],".csv"))
    
    #saveRDS(gap_filling, file=paste0("D:/HaSET/Processed/spatial_join_result/full_data_new/",year[i],"/Spatial_Join_Full_data_250m_",year[i],"_",month[i],"_",day[i],".rds"))
    
    
    print(paste0("Finished file - ",year[i],"_",month[i],"_",day[i]))
}

}

# one big file contains no NA

 write.csv(rm_na,file=paste0("D:/HaSET/Processed/spatial_join_result/no_na/MODIS_rm_na_2023.csv"))

 saveRDS(rm_na,file=paste0("D:/HaSET/Processed/spatial_join_result/no_na_new/New/MODIS_rm_na_2023_New.rds"))       
          
#print(paste0("Finished - ",year[i]))

Sys.time()-Start

Dir <- paste("D:/HaSET/Processed/spatial_join_result/no_na_new/New/",sep="")
files <- sort(list.files(Dir))

all_observations <- c()

for (n in 1:length(files)){
  dat <- readRDS(paste0(Dir,files[n]))
  
  dim(all_observations) <- rbind(all_observations,dat)
  
  print(paste0("finished-",n))
}
