library(tidyverse)

# spatial joined file from ArcGIS
# master <- read.csv("D:/HaSET/Processed/MODIS_Aqua_LST/MODIS_Match_ID.csv")

# load date file
date <- read.csv("D:/HaSET/Codes/date/date2019.csv",colClasses=c("character","character","character"))
year <- as.vector(date$year)
month <- as.vector(date$month)
day <- as.vector(date$day)

Start <- Sys.time()

dir <- paste("D:/HaSET/Processed/spatial_join_result/full_data/2019/",sep="")
file <- sort(list.files(dir))

# # directory for processed MODIS Aqua temperature
# dir_aqua_lst <-paste("D:/HaSET/Processed/MODIS_Aqua_LST/2019/",sep="")
# file_aqua_lst <- sort(list.files(dir_aqua_lst))
# 
# # directory for processed MODIS Terra temperature
# dir_terra_lst <-paste("D:/HaSET/Processed/MODIS_Terra_LST/2019/",sep="")
# file_terra_lst <- sort(list.files(dir_terra_lst))
# 
# # directory for processed MODIS AOD
# dir_aod <-paste("D:/HaSET/Processed/MODIS_AOD/2019/",sep="")
# file_aod <- sort(list.files(dir_aod)) 
# 
# # directory for processed ERA5
# dir_era5 <- paste("D:/HaSET/Processed/EAR5/processed/2019/", sep="")
# file_era5 <- sort(list.files(dir_era5))

# directory for population density
dir_population <- paste("D:/HaSET/Processed/Population_Density/2019/",sep="")
file_population <- sort(list.files(dir_population))

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

# generate a file w/o NA
# rm_na <- c()

for (i in 1:length(file)){
  
  test <- readRDS(paste0(dir,file[i]))
  
  # # load processed MODIS Aqua temp data
  # aqua_lst <- read.csv(paste0(dir_aqua_lst,file_aqua_lst[i]))
  # aqua_lst <- aqua_lst %>% rename(modis_id=ID,aqua_lst=LST_1KM)
  # 
  # # load processed MODIS Terra temp data
  # terra_lst <- read.csv(paste0(dir_terra_lst,file_terra_lst[i]))
  # terra_lst <- terra_lst %>% rename(modis_id=ID,terra_lst=LST_1KM)
  # 
  # # load processed MODIS AOD data
  # aod <- read.csv(paste0(dir_aod,file_aod[i]))
  # aod <- aod %>% rename(modis_id=ID)
  
  # load processed population data
  population <- readRDS(paste0(dir_population,file_population[i]))
  population <- population %>% 
    rename(grid_id=FID) %>% 
    select(grid_id,density)
  
  # # load processed ERA5 data
  # era5 <- readRDS(paste0(dir_era5,file_era5[i]))
  # era5 <- era5 %>% 
  #   rename(grid_id=ORIG_FID) %>% 
  #   select(-c("Id","FID","X","Y","X_Pro","Y_Pro"))
  
  # # match Aqua temp data to the 250m grid
  # match_modis_grid <- left_join(x=master,
  #                           y=aqua_lst[,c("modis_id","aqua_lst")],
  #                           by="modis_id")
  # 
  # # match Terra temp data to the 250m grid                          
  # match_modis_grid <- left_join(x=match_modis_grid,
  #                           y=terra_lst[,c("modis_id","terra_lst")],
  #                           by="modis_id")                          
  # 
  # # match AOD data to the 250m grid
  # match_modis_grid <- left_join(x=match_modis_grid,
  #                           y=aod[,c("modis_id","Optical_Depth_047","Optical_Depth_055")],
  #                           by="modis_id")      
  # 
  # # merge ERA5 data
  # match_modis_grid <- left_join(x=match_modis_grid,
  #                           y=era5,
  #                           by="grid_id")
  # 
  # merge population data
  match_modis_grid <- left_join(x=population,
                            y=test,
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
  
  
  # # generate a file w/o NA
  # 
  # grid_rmna <- na.omit(match_modis_grid)
  # rm_na <- rbind(rm_na,grid_rmna)

  # daily file for AOD, LST and ERA5
  # write.csv(match_modis_grid, file=paste0("D:/HaSET/Processed/spatial_join_result/full_data/",year[i],"/Spatial_Join_Full_data_250m_",year[i],"_",month[i],"_",day[i],".csv"))
  
  saveRDS(match_modis_grid, file=paste0("D:/HaSET/Processed/spatial_join_result/full_data/",year[i],"/Spatial_Join_Full_data_250m_",year[i],"_",month[i],"_",day[i],".rds"))
  
  
  print(paste0("Finished file - ",year[i],"_",month[i],"_",day[i]))
}

# one big file contains no NA

# write.csv(rm_na,file=past0("D:/HaSET/Processed/spatial_join_result/no_na/MODIS_rm_na_",year[i],".csv")

# saveRDS(rm_na,file=paste0("D:/HaSET/Processed/spatial_join_result/no_na/MODIS_rm_na_",year[i],".rds"))        
#           
# print(paste0("Finished - ",year[i]))

Sys.time()-Start