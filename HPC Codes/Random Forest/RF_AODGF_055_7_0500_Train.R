library(ranger)
library(doParallel)

dataset <- readRDS("/mnt/isilon/vub1/HaSET/Processed/spatial_join_result/updated_1002/all_obervation_updated_1002.rds")

# Define the response and predictor variables
response_var1 <- "optical_depth_047" # Replace with your actual response variable
response_var2 <- "optical_depth_055" # Replace with your actual response variable
predictor_vars <- c("Closed.Shrubland.Open.Shrubland","Annual.Cropland","Dense.Forest.Moderate.Forest.Sparse.Forest", 
                    "Water.Body","Closed.Grassland.Open.Grassland","Bare.Soil","Lava.Flow","Wetland","Settlement","Rock.Outcrop",                               
                    "Woodland","Perennial.Cropland","t2m","rh","elevation_mean","hmi","wind_speed","wind_direction","density",
                    "count","count_grid","sin_date","cos_date","yday","tp_lag0","tp_lag1","dry","rainy","sin_x","sin_y","cos_x","cos_y","nightlight","x","y","date")
dataset2 <- dataset[,c(response_var2,predictor_vars)]

set.seed(82690)

# Define the size of the dataset
n <- nrow(dataset2)  # Total number of rows in the dataset

# Define the proportion for splitting
train_proportion <- 0.5  # 50% for the training set

# Create a random sample of row indices for the training set
train_indices <- sample(n, size = round(train_proportion * n))

# Create training and testing datasets using the indices
train_set <- dataset2[train_indices, ]
test_set <- dataset2[-train_indices, ]

# prints out the number of rows to confirm split
cat("Training set rows:", nrow(train_set), "\n")
cat("Testing set rows:", nrow(test_set), "\n")

model <- ranger(
  formula = as.formula(paste(response_var2, "~ .")),
  data = train_set,
  mtry = 7,
  num.trees = 500,
  num.threads = 5,  # Utilize 5 cores
  importance = 'impurity',
  verbose = TRUE
)

# Print the model summary
print(model)

saveRDS(model, "/mnt/isilon/vub1/HaSET/Cluster/Models/RF_AODGapFill_055_7_0500_Train.rds") 


     