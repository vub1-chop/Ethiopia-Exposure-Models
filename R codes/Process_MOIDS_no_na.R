library(tidyverse)

# dir <-paste("D:/HaSET/Processed/spatial_join_result/full_data_new/2018/",sep="")
# file <- sort(list.files(dir))

# rm_na <- c()

Start <- Sys.time()

file_list <- list.files(path="D:/HaSET/Processed/spatial_join_result/full_data_new/2018",pattern="*.rds",full.names = TRUE)

merged_data <- do.call(bind_rows,lapply(file_list,function(file){
  
  df <- readRDS(file)
  df <- na.omit(df)
  return(df)
  
}))

saveRDS(merged_data,file="D:/HaSET/Processed/spatial_join_result/no_na_new/MODIS_rm_na_2018.rds")

Sys.time()-Start