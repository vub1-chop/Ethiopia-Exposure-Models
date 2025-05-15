#install.packages("peakRAM")
#install.packages("LiblineaR")
library(e1071)
library(caret)
library(doParallel)
library(data.table)  # For fast data loading
library(dplyr)
library(pryr)
library(peakRAM)
library(LiblineaR)

n_cores <- parallel::detectCores()

cl <- makeCluster(n_cores)
registerDoParallel(cl)

cat("Registered", getDoParWorkers(), "cores\n")

dataset <- readRDS("/mnt/isilon/vub1/HaSET/Processed/spatial_join_result/updated_1002/all_obervation_updated_1002.rds")
#dataset <- readRDS("A:/HaSET/Processed/spatial_join_result/updated_1002/all_obervation_updated_1002.rds")

set.seed(82690)
# Define the response and predictor variables
response_var1 <- "optical_depth_047" # Replace with your actual response variable
response_var2 <- "optical_depth_055" # Replace with your actual response variable
predictor_vars <- c("Closed.Shrubland.Open.Shrubland","Annual.Cropland","Dense.Forest.Moderate.Forest.Sparse.Forest", 
                    "Water.Body","Closed.Grassland.Open.Grassland","Bare.Soil","Lava.Flow","Wetland","Settlement","Rock.Outcrop",                               
                    "Woodland","Perennial.Cropland","t2m","rh","elevation_mean","hmi","wind_speed","wind_direction","density",
                    "count","count_grid","sin_date","cos_date","yday","tp_lag0","tp_lag1","dry","rainy","sin_x","sin_y","cos_x","cos_y","nightlight","x","y","date")
dataset2 <- dataset[,c(response_var2,predictor_vars)]
#dataset2.1 <- sample_n(dataset2, 10000) 

set.seed(82690)

# Define the size of the dataset
n <- nrow(dataset2)  # Total number of rows in the dataset

# Define the proportion for splitting
train_proportion <- 0.5  # 50% for the training set

# Create a random sample of row indices for the training set
train_indices <- sample(n, size = round(train_proportion * n))

# Create training and testing datasets using the indices
set1 <- dataset2[train_indices, ]
set2 <- dataset2[-train_indices, ]

# prints out the number of rows to confirm split
cat("Set1 rows:", nrow(set1), "\n")
cat("Set2 rows:", nrow(set2), "\n")
print(object.size(set1), units = "GB")
cat("Input data size: ", format(object.size(set2), units = "GB"), "\n")

train_ctrl <- trainControl(method = "cv", number = 10, verboseIter = TRUE, allowParallel = TRUE)

# Normalize (center/scale) predictors
pre_proc <- c("center", "scale")

svm_tune_grid <- expand.grid(C = c(0.01, 0.1, 1, 10, 100))

start <- Sys.time()
mem_used()
peakRAM(
  svm_model <- train(
  optical_depth_055 ~ .,
  data = set2,
  method = "svmLinear",
  preProcess = pre_proc,
  tuneGrid = svm_tune_grid,
  trControl = train_ctrl,
  metric = "RMSE"
)
)


  # svm_model <- train(
  #   optical_depth_055 ~ .,
  #   data = set2,
  #   method = "svmLinear",
  #   preProcess = pre_proc,
  #   tuneGrid = svm_tune_grid,
  #   trControl = train_ctrl,
  #   metric = "RMSE"
  # )

mem_used()
end <- Sys.time()-start
end

saveRDS(svm_model, file = "/mnt/isilon/vub1/HaSET/Cluster/Models/SVM/Training/SVM_AODGapFill_055_Set2.rds")

# Best model
print(svm_model)

# Plot tuning results
plot(svm_model)

stopCluster(cl)
registerDoSEQ()

cat("Training complete. Model saved.\n")
