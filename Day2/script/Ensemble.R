# Training Day 2
# Koissi Savi (Ph.D.)
### ENSEMBLE MODELS 

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
# library(dismo) # required a specific way to install it
library(ecospat)
library(jsonlite)

#Load dataset

occur <- read.delim("/Users/koissi/Desktop/Seminar_docs/Training/CREC/Day2/Data/occurrence.txt")


# Select relevant columns
selected_columns <- c("decimalLatitude", "decimalLongitude", "eventDate", "kingdom", "phylum", "class", "order", "family", "genus", "species") 

# Subset data
occur_subset <- occur %>% 
  dplyr::select(all_of(selected_columns)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  distinct(.keep_all = TRUE) %>% 
  drop_na() 

# Create a variable presence 
occur_subset$presence <- ifelse(!is.na(occur_subset$eventDate), 1, 0)

# Let generate some absence

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


occur_sf <- st_as_sf(occur_subset, 
                     coords = c("decimalLongitude", "decimalLatitude"), 
                     crs = 4326)


# Obtaining additional data

# Replace 'path/to/worldclim' with the actual path to your WorldClim data

worldclim_path <- "/Users/koissi/Downloads"

# Load and extract elevation data 

wc_elev <- raster(file.path(worldclim_path, "wc2.1_10m_elev.tif"))

# Assuming 'occur_subset' contains latitude and longitude columns
occur_subset <- occur_subset %>%
  mutate(elevation = raster::extract(wc_elev, cbind(decimalLongitude, decimalLatitude)))

# Get Temperature data 
wc_tavg_files <- list.files(file.path(worldclim_path, "/wc2.1_10m_tavg"),
                            pattern = "\\.tif$", full.names = TRUE)

# Load the temperature raster
wc_tavg <- raster(wc_tavg_files[1])

# Extract temperature values for each occurrence point and day
occur_subset <- occur_subset %>%
  rowwise() %>%
  dplyr::mutate(temperature = 
                  raster::extract(wc_tavg, 
                                  cbind(decimalLongitude, 
                                        decimalLatitude))) %>% 
  drop_na()

summary(occur_subset[, c("temperature", "elevation")])


# Set a seed for reproducibility
set.seed(1123)

# Generate 300 absence points with the same structure as occur_subset
num_absence_points_same_structure <- 10

absence_points_same_structure <- generate_absence_points_same_structure(
  occur_subset, num_absence_points_same_structure)

# Combine original data and absence points
occur_subset <- bind_rows(occur_subset, absence_points_same_structure)



# Assuming 'occur_subset' is your data
set.seed(123)  # for reproducibility

# Split the data into training and testing sets
# Here, I'm using 80% for training and 20% for testing
index <- sample(1:nrow(occur_subset), 0.8 * nrow(occur_subset))
train_data <- occur_subset[index, ]
test_data <- occur_subset[-index, ]

# Train individual models (you can use any model you prefer)
model1 <- lm(presence ~ elevation + temperature, data = train_data)
model2 <- glm(presence ~elevation + temperature, data = train_data, family = "binomial")

# Make predictions on the test set
predictions1 <- predict(model1, newdata = test_data)
predictions2 <- predict(model2, newdata = test_data, type = "response")

# Define weights for the models
weight1 <- 0.7
weight2 <- 0.3

# Combine predictions using a weighted average
weighted_average <- (weight1 * predictions1 + weight2 * predictions2) / (weight1 + weight2)


# Convert probabilities to binary predictions (assuming a threshold of 0.5)
binary_predictions <- ifelse(weighted_average  >= 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(actual = test_data$presence, predicted = binary_predictions)
print(confusion_matrix)

# Assuming 'confusion_matrix' is your confusion matrix
conf_matrix <- as.matrix(confusion_matrix)

# Create a 2 by 2 matrix with diagonal elements from the original confusion matrix
transformed_matrix <- matrix(c(conf_matrix[1, 1], 0, 0, conf_matrix[2, 1]), nrow = 2, byrow = TRUE)

# Print the transformed matrix
print(transformed_matrix)

# Function for evaluation metrics
# Function to calculate performance metrics
calculate_metrics <- function(conf_matrix) {
  true_negatives <- conf_matrix[1, 1]
  true_positives <- conf_matrix[2, 2]
  false_negatives <- conf_matrix[1, 2]
  false_positives<- conf_matrix[2, 1]
  
  accuracy <- (true_positives + true_negatives) / sum(conf_matrix)
  sensitivity <- true_positives / (true_positives + false_negatives)
  specificity <- true_negatives / (true_negatives + false_positives)
  precision <- true_positives / (true_positives + true_negatives)
  
  metrics <- c(accuracy = accuracy, sensitivity = sensitivity, specificity = specificity, precision = precision)
  return(metrics)
}


# Calculate metrics using the function calculate_metrics
metrics <- calculate_metrics(transformed_matrix)
print(metrics)

# Interpret the weighted average, the outcomes of models 1 & 2

