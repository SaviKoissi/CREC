# Training Day 2
# Koissi Savi (Ph.D.)
# 

# List of required packages
required_packages <- c(
  "tidyverse", "readxl", "tmap", "sf", "raster", 
  "rasterVis", "httr", "jsonlite", "rpart", 
  "rpart.plot", "ENMeval", "ecospat", "jsonlite"
)

# Install and load missing packages
for (pkg in required_packages) {
  if (!(pkg %in% installed.packages()[,"Package"])) {
    install.packages(pkg, dependencies = TRUE)
  }
  library(pkg, character.only = TRUE)
}

# Load all installed packages
installed_packages <- as.data.frame(installed.packages())
loadable_packages <- installed_packages$Package[installed_packages$LibPath == .libPaths()[1]]
invisible(sapply(loadable_packages, function(pkg) {
  try(library(pkg, character.only = TRUE), silent = TRUE)
}))


# library(tidyverse)
# library(readxl)
# library(tmap)
# library(sf)
# library(raster)
# library(rasterVis)
# library(httr)
# library(jsonlite)
# library(rpart)
# library(rpart.plot)
# library(ENMeval)
# # library(dismo) # required a specific way to install it
# library(ecospat)
# library(jsonlite)

#Load dataset

occur <- read.delim("/Users/koissi/Desktop/Seminar_docs/Training/CREC/Day2/Data/occurrence.txt")



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

data("World")
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
binary_predictions <- ifelse(weighted_predictions >= 0.5, 1, 0)

# Create a confusion matrix
confusion_matrix <- table(actual = test_data$presence, predicted = binary_predictions)
print(confusion_matrix)

# Assuming 'confusion_matrix' is your confusion matrix
conf_matrix <- as.matrix(confusion_matrix)

# Create a 2 by 2 matrix with diagonal elements from the original confusion matrix
transformed_matrix <- matrix(c(conf_matrix[1, 1], 0, 0, conf_matrix[2, 1]), nrow = 2, byrow = TRUE)

# Print the transformed matrix
print(transformed_matrix)

# Calculate metrics using the function calculate_metrics
metrics <- calculate_metrics(transformed_matrix)
print(metrics)

# Interpret the weighted average, the outcomes of models 1 & 2

## ENVELOPE MODEL

# Load occurrence data (occur_subset) and environmental data (Bioclim variables)
# Assuming 'occur_subset' has columns 'decimalLatitude', 'decimalLongitude'
# and 'species' representing occurrence data

# Only presence data 
occur_subset <- occur %>% 
  dplyr::select(all_of(selected_columns)) %>% 
  mutate(eventDate = as.Date(eventDate)) %>% 
  distinct(.keep_all = TRUE) %>% 
  drop_na() 

occurrences <- occur_subset[, c("decimalLatitude", "decimalLongitude")]

# Convert to SpatialPointsDataFrame
occurrences_sp <- st_as_sf(occurrences, 
         coords = c("decimalLongitude", "decimalLatitude"), 
         crs = 4326)

worldclim_path <- "/Users/koissi/Downloads"

files <- list.files(path = file.path(worldclim_path, "/wc2.1_10m_bio"), 
                    pattern = "tif$", full.names = TRUE)

# Read the raster stack
desired_bios <- c("bio_1", "bio_2", "bio_3", "bio_4")
selected_files <- grep(paste(desired_bios, collapse = "|"), files, value = TRUE)
selected_files <- selected_files[c(1,12:14)]
env_data <- stack(selected_files)

# Extract values from raster stack to points
occurrences_data <- raster::extract(env_data, occurrences_sp)

# Combine occurrences data with environmental data
data <- cbind(occurrences_sp, occurrences_data)

# Generate background points
bg <- as.data.frame(dismo::randomPoints(env_data, n = nrow(data)))
names(bg) <- names(occurrences)

# bg <- bg %>% 
#   dplyr::rename( decimalLongitude = 'x',
#                  decimalLatitude = 'y')

# Extract values from raster stack to background points
bg_data <- raster::extract(env_data, 
                           st_as_sf(bg, 
                                    coords = c("decimalLongitude",
                                               "decimalLatitude"), crs = 4326))

# Combine background points data with environmental data
bg_z <- cbind(st_as_sf(bg, coords = c("decimalLongitude", "decimalLatitude"), 
                       crs = 4326), bg_data)


eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
bg_z <- sf::st_transform(bg_z, crs = eckertIV)

# Partition methods
# Partitioning is a technique used to divide the dataset into training and
# testing subsets. This process helps to evaluate model performance and 
# avoid overfitting. When using an envelope model, such as in the 
# ENMeval package in R, several partitioning algorithms are available.


# Block method partition data 
#Divides the study area into non-overlapping blocks and assigns occurrences 
# randomly to these blocks. The model is trained on one block and tested on 
# the remaining blocks iteratively.

block <- get.block(occurrences, bg, orientation = "lat_lon")
table(block$occs.grp)

# Plotting likelihood 
evalplot.grps(pts = occurrences, pts.grp = block$occs.grp, envs = env_data) + 
  ggplot2::ggtitle("Spatial block partitions: occurrences")

# PLotting the background shows that the background extent is partitioned in a way 
# that maximizes evenness of points across the four bins, not to maximize evenness of area.
evalplot.grps(pts = bg, pts.grp = block$bg.grp, envs = env_data) + 
  ggplot2::ggtitle("Spatial block partitions: background")


# If we are curious how different the environment associated with each partition is from 
# that of all the others, we can use this function to plot histograms or rasters of MESS 
# predictions with each partition as the reference.
# First we need to extract the predictor variable values at our occurrence and 
# background localities.

occs.z <- cbind(occurrences, raster::extract(env_data, occurrences))
bg.z <- cbind(bg, raster::extract(env_data, bg))

# Plots environmental similarity of reference partitions 
# (occurrences or background) to remaining data 
# (occurrence and background for all other partitions)

evalplot.envSim.hist(sim.type = "mess", ref.data = "occurrences", 
                     occs.z = occs.z, bg.z = bg.z, 
                     occs.grp = block$occs.grp, 
                     bg.grp = block$bg.grp, categoricals = "biome")


# Here we plot environmental similarity values for the entire extent with respect 
# to each validation group.
# We use the bb.buf (bounding box buffer) argument to zoom in to our study extent.

evalplot.envSim.map(sim.type = "most_diff", ref.data = "occurrences", envs = env_data, 
                    occs.z = occs.z, 
                    bg.z = bg.z, occs.grp = block$occs.grp, bg.grp = block$bg.grp, 
                    categoricals = "biome", bb.buf = 7)

# Second method of partition Checkerboard1  (Radosavljevic & Anderson 2014).
# These generate checkerboard grids across the study extent and partition the 
# localities into groups based on where they fall on the checkerboard. In 
# contrast to the block method, both checkerboard methods subdivide geographic 
# space equally but do not ensure a balanced number of occurrence localities 
# in each bin.

cb1 <- get.checkerboard1(occurrences, env_data, bg, aggregation.factor=5)

#Plot occurrence partition groups over an environmental predictor raster
evalplot.grps(pts = occurrences, pts.grp = cb1$bg.grp, envs = env_data)

# Jacknife partition methods

# The next two methods differ from the first three in that (a) they do not 
# partition the background points into different groups (meaning that the full 
# background is used to evaluate each partition), and (b) they do not account 
# for spatial autocorrelation between validation and training localities. 
# Primarily when working with relatively small data sets (e.g. < ca. 25 presence
# localities), users may choose a special case of k-fold cross-validation where 
# the number of bins (k) is equal to the number of occurrence localities (n) 
# in the data set (Pearson et al. 2007; Shcheglovitova & Anderson 2013). 
# This is referred to as jackknife, or leave-one-out, 
# partitioning (Hastie et al. 2009). As n models are processed with this
# partitioning method, the computation time could be long for large 
# occurrence datasets.

jack <- get.jackknife(occurrences, bg)

# If the number of input points is larger than 10, the legend for the groups 
# is suppressed.
evalplot.grps(pts = occurrences, pts.grp = jack$occs.grp, envs = env_data)

# Random k-fold

# The ‘random k-fold’ method partitions occurrence localities randomly into a 
# user-specified number of (k) bins (Hastie et al. 2009). This method is 
# equivalent to the ‘cross-validate’ partitioning scheme available in the 
# current version of the Maxent software GUI. Especially with larger occurrence 
# datasets, this partitioning method could randomly result in some spatial 
# clustering of groups, which is why spatial partitioning methods are preferable 
# for addressing spatial autocorrelation (Roberts et al. 2017). 
# Below, we partition the data into five random groups.

rand <- get.randomkfold(occurrences, bg, k = 5)
evalplot.grps(pts = occurrences, pts.grp = rand$occs.grp, envs = env_data)

# Buiild ecological niche models  ENMeval model with adjusted parameters
e.mx.l <- ENMevaluate(occs = occurrences, envs = env_data, bg = bg, 
                      algorithm = 'maxnet', partitions = 'block', 
                      tune.args = list(fc = "L", rm = 1:2))

e.max <- ENMevaluate(occs = occurrences, envs = env_data, bg = bg, 
                     algorithm = 'maxnet', partitions = 'block', 
                     tune.args = list(fc = c("L","LQ","LQH","H"), rm = 1:2))
# "Maximum Entropy Modeling on the Net," aims to model the probability 
# distribution of species occurrences based on environmental variables.

# L, LQ, H, LQH, LQHP, LQHPT; where L = linear,Q = quadratic,
# H = hinge, P = product and T = threshold (Muscarella et al. 2014)

overlap <- calc.niche.overlap(e.max@predictions, overlapStat = "D")
# Compute pairwise niche overlap (similarity of estimated suitability scores) 
# in geographic space for Maxent predictions. The value ranges from 
# 0 (no overlap) to 1 (identical predictions). Based on the ‘nicheOverlap’ 
# function of the dismo R package (Hijmans et al. 2011)

# Interpretation:
# Each row and column correspond to a specific combination of 
# feature class (fc) and regularization multiplier (rm). Higher values indicate 
# greater niche overlap, suggesting that the corresponding models are more 
# similar in their predictions.
# Looking at the first non-NA value (0.7153720) in the matrix, 
# it suggests that the niche overlap between models with fc.LQ_rm.1 and 
# fc.L_rm.1 is 0.7153720.

eem_result <- ENMevaluate(occurrences, envs = env_data, bg = bg, 
                          tune.args = list(fc = c("L", "LQ", "LQH", "H"), rm = 1:5), 
                          partitions = "block", 
                          other.settings = list(abs.auc.diff = FALSE, 
                                                pred.type = "cloglog", 
                                                validation.bg = "partition"),
                          partition.settings = list(orientation = "lat_lon"),
                          algorithm = "maxnet", 
                          overlap = TRUE)


m1.mx <- eval.models(eem_result)[["fc.LQH_rm.1"]]
# Interpretation
# generalized linear models with elastic net regularization %Dev: 
# Represents the percentage of deviance explained by the model. 
# Deviance is a measure of goodness-of-fit in statistical models. 
# A value of 0% deviance means that the model does not explain any 
# variability in the response.
# Represents the regularization parameter (lambda) at 
# which the model was evaluated. Lambda controls the 
# strength of the regularization. Larger values of 
# lambda result in stronger regularization, and smaller
# values allow for more flexibility in the model.

enframe(m1.mx$betas)
eval.predictions(e.max)

# Visualization
# We can plot more than one statistic at once with ggplot facetting.
evalplot.stats(e = e.max, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")

# Model selection
# Overall results
res <- eval.results(e.max)


opt.aicc <- res %>% filter(delta.AICc == 0)

opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq

# We can select a single model from the ENMevaluation object using the tune.args of our
# optimal model.
mod.seq <- eval.models(e.max)[[opt.seq$tune.args]]
mod.seq$betas
plot(mod.seq, type = "cloglog")
dev.off()

pred.seq <- eval.predictions(e.max)[[opt.seq$tune.args]]
plot(pred.seq)
points(eval.bg(e.max), pch = 3, col = eval.bg.grp(e.max), cex = 0.5)
points(eval.occs(e.max), pch = 21, bg = eval.occs.grp(e.max))

# Finally, let's cut the plotting area into two rows to visualize the predictions 
# side-by-side.
par(mfrow=c(2,1), mar=c(2,1,2,0))
# The simplest model: linear features only and high regularization.
plot(eval.predictions(e.max)[['fc.L_rm.2']], ylim = c(-50,40), 
     xlim = c(-20,53), 
     legend = FALSE, main = 'L_5 prediction')
# The most complex model: linear, quadratic, and hinge features with low regularization
plot(eval.predictions(e.max)[['fc.LQH_rm.1']], ylim = c(-50,40), 
     xlim = c(-20,53), 
     legend = FALSE, main = 'LQH_1 prediction')

