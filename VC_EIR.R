# Training Day 1
# Koissi Savi (Ph.D) Harvard School of Medicine

# Computation of VC and EIR using Dummy data

#Step 1: Install and Load Required Packages
if (requireNamespace("tidyverse", quietly = TRUE)) {
  library(tidyverse)
} else {
  install.packages("tidyverse", repos = "https://cran.rstudio.com")
  library(tidyverse)
}

#Step 2: Generate Dummy Data

# Set seed for reproducibility
set.seed(123)

# Generate dummy data for Mosquito Population 1
mosquito_data_1 <- data.frame(
  mosquito_biting_rate = sample(10:30, 10, replace = TRUE),
  sporozoite_rate = runif(10, 0.1, 0.5),
  transmission_probability = runif(10, 0.1, 0.5),
  mosquito_survival_rate = runif(10, 0.5, 0.9),
  sporogonic_cycle_duration = sample(10:20, 10, replace = TRUE)
)

# Generate dummy data for Mosquito Population 2
mosquito_data_2 <- data.frame(
  mosquito_biting_rate = sample(15:35, 10, replace = TRUE),
  sporozoite_rate = runif(10, 0.2, 0.6),
  transmission_probability = runif(10, 0.2, 0.6),
  mosquito_survival_rate = runif(10, 0.6, 0.9),
  sporogonic_cycle_duration = sample(15:25, 10, replace = TRUE)
)

#Step 3: Calculate Vectorial Capacity (VC)
# Function to calculate Vectorial Capacity
calculate_VC <- function(data) {
  with(data, (mosquito_biting_rate * sporozoite_rate * transmission_probability * mosquito_survival_rate) / sporogonic_cycle_duration)
}

# Calculate VC for both mosquito populations
vc_mosquito_1 <- calculate_VC(mosquito_data_1)
vc_mosquito_2 <- calculate_VC(mosquito_data_2)

#Step 4: Calculate Entomological Inoculation Rate (EIR)
# Function to calculate Entomological Inoculation Rate (EIR)
calculate_EIR <- function(data) {
  with(data, mosquito_biting_rate * sporozoite_rate)
}

# Calculate EIR for both mosquito populations
eir_mosquito_1 <- calculate_EIR(mosquito_data_1)
eir_mosquito_2 <- calculate_EIR(mosquito_data_2)

#Step 5: Interpret the Results
# Display the results
result_df <- data.frame(
  Population = c(rep("Mosquito 1", 10), rep("Mosquito 2", 10)),
  VC = c(vc_mosquito_1, vc_mosquito_2),
  EIR = c(eir_mosquito_1, eir_mosquito_2)
)

print(result_df)
