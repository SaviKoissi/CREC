# Training Day 2
# Koissi Savi (Ph.D.)
# 

library(tidyverse)
library(readxl)
library(tmap)
library(sf)
library(raster)
library(httr)
library(jsonlite)
library(rpart)
library(rpart.plot)

#Load dataset

occur <- read.delim("/Users/koissi/Desktop/Seminar_docs/Training/CREC/Day2/Data/occurrence.txt")

# occur <- read.table("/Users/koissi/Desktop/Seminar_docs/Training/CREC/Day2/Data/occurrence.txt", header = TRUE, sep = "\t", fill = TRUE)

# Select relevant columns
selected_columns <- c("decimalLatitude", "decimalLongitude", "eventDate", "kingdom", "phylum", "class", "order", "family", "genus", "species") #"landCover", "temperature", ,  "precipitation","elevation",  "habitat", 

# Subset data
occur_subset <- occur %>% 
  dplyr::select(all_of(selected_columns)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  distinct(.keep_all = TRUE) %>% 
  drop_na() 

# Create a variable presence 
occur_subset$presence <- ifelse(!is.na(occur_subset$eventDate), 1, 0)

# Load the in-built map
data("World")


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


# Set a seed for reproducibility
set.seed(1123)

# Generate 300 absence points with the same structure as occur_subset
num_absence_points_same_structure <- 10

absence_points_same_structure <- generate_absence_points_same_structure(
  occur_subset, num_absence_points_same_structure)

# Combine original data and absence points
occur_subset <- bind_rows(occur_subset, absence_points_same_structure)


# Explore and clean data
summary(occur_subset)
# Perform additional cleaning steps as needed

# Visualize the data
ggplot(occur_subset, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point() +
  theme_minimal()

# Assuming "decimalLatitude" and "decimalLongitude" are the latitude and longitude columns in your dataset
# Create a simple sf object
occur_sf <- st_as_sf(occur_subset, 
                     coords = c("decimalLongitude", "decimalLatitude"), 
                     crs = 4326)

# Set the color based on the presence variable
occur_sf$color <- ifelse(occur_sf$presence == 1, "red", "blue")


# Set up a thematic map using tm_shape
tm <- tmap_mode("view")+
  tm_shape(World) +
  tm_borders() +
  tm_shape(occur_sf) +
  tm_dots(size = 0.01, col = "color") +
  tm_layout(legend.show = FALSE)

# Display the map
tm 

# Obtaining additional data

# Replace 'path/to/worldclim' with the actual path to your WorldClim data

worldclim_path <- "/Users/koissi/Downloads"

# Load and extract elevation data 

wc_elev <- raster(file.path(worldclim_path, "wc2.1_10m_elev.tif"))

# Assuming 'occur_subset' contains latitude and longitude columns
occur_subset <- occur_subset %>%
  mutate(elevation = extract(wc_elev, cbind(decimalLongitude, decimalLatitude)))

# Get Temperature data 
wc_tavg_files <- list.files(file.path(worldclim_path, "/wc2.1_10m_tavg"),
                            pattern = "\\.tif$", full.names = TRUE)

# Load the temperature raster
wc_tavg <- raster(wc_tavg_files[1])
  
# Extract temperature values for each occurrence point and day
occur_subset <- occur_subset %>%
    rowwise() %>%
    dplyr::mutate(temperature = 
                    extract(wc_tavg, cbind(decimalLongitude, decimalLatitude))) %>% 
  drop_na()
  
summary(occur_subset[, c("temperature", "elevation")])

# Define the response variable
response_variable <- "species"  # Replace with your actual species column name

# Select relevant predictor variables (e.g., latitude, longitude, elevation, temperature)
predictor_vars <- c("decimalLatitude", "decimalLongitude", "elevation", "temperature")

# Split dataset into train and test 
# make it reproducible 
set.seed(1)

#Use 80% of dataset as training set and remaining 20% as testing set
sample <- sample(c(TRUE, FALSE), nrow(occur_subset), replace=TRUE, prob=c(0.8,0.2))
train  <- occur_subset[sample, ]
test   <- occur_subset[!sample, ]


###CLASSIFICATION TREE MODEL


# Build the Classification Tree model
cta_model <- rpart(presence ~ ., data = dplyr::select(train, predictor_vars, presence), method = "class")

print(cta_model)
prp(cta_model, extra = 1)

# Interpretation
# There are 765 observations in the dataset; there are 5 misclassification
# The predicted presence has a probability of approximately 0.99.
# There are 7 observations, 3 misclassification with a probability of 0.57
# for the predicted class 0

# Interpret the third node 

# Model evaluation
# Assuming you have a test dataset 'test'
predictions <- predict(cta_model, newdata = test, type = "class")
confusion_matrix <- table(predictions, test$species)

# Interpretation
# The model predicted 0, and it was actually 0. In this case, there are 3 
# instances where Anopheles stephensi is actually 0, and the model correctly 
# predicted it as 0. The model predicted 1, and it was actually 1. 
# In this case, there are 182 instances where Anopheles stephensi is actually 1,
# and the model correctly predicted it as 1.

# A more manual way to write the confusion the confusion matrix
confusion_matrix <- matrix(c(3, 0, 0, 182), nrow = 2, byrow = TRUE, dimnames = list(actual = c(0, 1), predicted = c(0, 1)))

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

# Calculate metrics
metrics <- calculate_metrics(confusion_matrix)

# Print the results
print(metrics)

## ENSEMBLE MODELS