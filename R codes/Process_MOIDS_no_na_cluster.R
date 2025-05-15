#install.packages("tidyverse",repos="https://cloud.r-project.org",lib="/n/home09/yqian/R/library")
.libPaths("/n/home09/yqian/R/library")
library(tidyverse)

file_list <- list.files(path="/n/holyscratch01/gjchan_lab/MODIS_Spatial_Join/daily_file/",pattern="*.rds",full.names = TRUE)

merged_data <- do.call(bind_rows,lapply(file_list,function(file){
  
  df <- readRDS(file)
  df <- na.omit(df)
  return(df)
  
}))


saveRDS(rm_na,file="/n/holyscratch01/gjchan_lab/MODIS_Spatial_Join/no_na/all_obervation.rds")