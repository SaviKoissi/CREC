# Training Day 2
# Koissi Savi (Ph.D.)
# MAXENT


# Download and install MaxEnt

utils::download.file(url = "https://raw.githubusercontent.com/mrmaxent/Maxent/master/ArchivedReleases/3.3.3k/maxent.jar", destfile = paste0(system.file("java", package = "dismo"), 
                                                                                                                                            "/maxent.jar"), mode = "wb")

library(tidyverse)
library(readxl)
library(tmap)
library(sf)
library(raster)
library(rasterVis)
library(httr)
library(jsonlite)
library(rpart)
library(rpart.plot)
library(ENMeval)
library(dismo) 
library(ecospat)
library(jsonlite)
install.packages("ROCR")
library(ROCR)

#Load dataset

occur <- read.delim("/Users/koissi/Desktop/Seminar_docs/Training/CREC/Day2/Data/occurrence.txt")


# Select relevant columns
selected_columns <- c("decimalLatitude", "decimalLongitude", 
                      "eventDate", "kingdom", "phylum", "class", 
                      "order", "family", "genus", "species") 

# Subset data
occur_subset <- occur %>% 
  dplyr::select(all_of(selected_columns)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  distinct(.keep_all = TRUE) %>% 
  drop_na() 

# Create a variable presence 
occur_subset$presence <- ifelse(!is.na(occur_subset$eventDate), 1, 0)

# Generate abscence 

generate_absence_points_same_structure <- function(original_data, num_points) {
  # Generate random absence points with the same structure as original_data
  absence_points <- tibble(
    decimalLatitude = runif(num_points, min = min(original_data$decimalLatitude), 
                            max = max(original_data$decimalLatitude)),
    decimalLongitude = runif(num_points, 
                             min = min(original_data$decimalLongitude),
                             max = max(original_data$decimalLongitude)),
    eventDate = as.Date(Sys.Date()),
    kingdom = sample(unique(original_data$kingdom), num_points, replace = TRUE),
    phylum = sample(unique(original_data$phylum), num_points, replace = TRUE),
    class = sample(unique(original_data$class), num_points, replace = TRUE),
    order = sample(unique(original_data$order), num_points, replace = TRUE),
    family = sample(unique(original_data$family), num_points, replace = TRUE),
    genus = sample(unique(original_data$genus), num_points, replace = TRUE),
    species = sample(unique(original_data$species), num_points, replace = TRUE),
    presence = 0
  )
  return(absence_points)
}
# Set a seed for reproducibility
set.seed(1123)

# Generate 300 absence points with the same structure as occur_subset
num_absence_points_same_structure <- 1000

absence_points_same_structure <- generate_absence_points_same_structure(
  occur_subset, num_absence_points_same_structure)

# Combine original data and absence points
occur_subset <- bind_rows(occur_subset, absence_points_same_structure)


## MAXENT


# Visualization of thematic maps

# stack all the raster files 
worldclim_path <- "/Users/koissi/Downloads"
wc_elev <- raster(file.path(worldclim_path, "wc2.1_10m_elev.tif"))

# Get Temperature data 
wc_tavg_files <- list.files(file.path(worldclim_path, "/wc2.1_10m_tavg"),
                            pattern = "\\.tif$", full.names = TRUE)

# Load the temperature raster
wc_tavg <- raster(wc_tavg_files[1])


files <- list.files(path = file.path(worldclim_path, "/wc2.1_10m_bio"), 
                    pattern = "tif$", full.names = TRUE)

# Read the raster stack
# The significance of the bioclim data is available 
# (https://chelsa-climate.org/bioclim/#:~:text=Bioclimatic%20variables%20are%20derived%20variables,modeling%20and%20related%20ecological%20applications.)
desired_bios <- c("bio_1", "bio_2", "bio_3", "bio_4")
selected_files <- grep(paste(desired_bios, collapse = "|"), files, value = TRUE)
selected_files <- selected_files[c(1,12:14)]

bio1 <- raster(selected_files[1]) # mean annual air temperature
bio2 <- raster(selected_files[2]) # mean diurnal air temperature range
bio3 <- raster(selected_files[3]) # isothermality
bio4 <- raster(selected_files[4]) # temperature seasonality

env_data <- stack(selected_files)

occur_sf <- st_as_sf(occur_subset, 
                     coords = c("decimalLongitude", "decimalLatitude"), 
                     crs = 4326)
# Generate color
occur_sf$color <- ifelse(occur_sf$presence == 1, "red", "blue")

# Thematic map 
# Set the color based on the presence variable
plot(bio1, main= "Mean annual air temperature")
points(occur_sf, col= occur_sf$color)

# Reproduce the same process with the remaining variables and interpret them

# Create an RasterStack
env_stack <- stack(bio1, bio2, bio3, bio4, wc_elev, wc_tavg)

# Extract environmental variables at occurrence points
occurrence_data <- raster::extract(env_stack, occur_sf)

# Create a dataframe with occurrence data and environmental variables
maxent_data <- cbind(occur_subset, occurrence_data) %>% 
  drop_na()

# Split the data into training and testing sets

set.seed(1123)  # Set seed for reproducibility

train_indices <- sample(1:nrow(maxent_data), 0.8 * nrow(maxent_data))
train_data <- maxent_data[train_indices, ]
test_data <- maxent_data[-train_indices, ]

# Maxent Model Training
p <- train_data$presence 
dataT <- train_data[,c(12:16)]

mod <- maxnet::maxnet(p, dataT)
plot(mod, type="cloglog")


# Predict on the test set
predictions <- predict(mod, newdata = test_data[, c(12:16)], type = 'cloglog')

# Create a binary vector of predictions
binary_predictions <- ifelse(predictions > 0.5, 1, 0)

# Create a prediction object for ROCR
prediction_obj <- prediction(predictions, test_data$presence)

# Create a performance object for ROCR
performance_obj <- performance(prediction_obj, "tpr", "fpr")
dev.off()
# Plot ROC curve
plot(performance_obj, main = "ROC Curve", col = "blue", lwd = 2)

# Calculate AUC
auc_value <- performance(prediction_obj, "auc")@y.values[[1]]
cat("AUC:", auc_value, "\n") #indicates better-than-random performance.

