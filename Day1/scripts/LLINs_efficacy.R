# Training Day 1 
# Koissi Savi (Ph.D.) 
# Jan. 2024

# Application with field data
if (requireNamespace(c("readxl", "survival"), quietly = TRUE)) {
  library(readxl) 
  library(survival)
} else {
  install.packages(c("readxl", "survival"), repos = "https://cran.rstudio.com")
  library(readxl) 
  library(survival)
}

library(tidyverse)
library(cowplot)

# Load raw data 

data <- read_xlsx("AMMnet.xlsx", sheet = "Raw_data")
data %>% 
  glimpse()

# Check for missing values
summary(is.na(data))

# Summary statistics
summary(data)

# Verification 
data %>%
  select(hut, sleeper) %>%
  summarise(sum_hut_greater_than_sleeper = sum(hut >= sleeper, na.rm = TRUE))

data_2 <- data %>%
  filter(hut< sleeper) 

# Given that hut are used to investigate mosquito entry, exit, and
# feeding patterns whereas Sleepers are individuals who rest or 
# sleep inside the experimental huts, attracting mosquitoes 
# that may enter the hut seeking a blood meal, shouldn't we always have 
# hut > sleepers

# Descriptive statistics for key variables
summary(data$tot_live)
summary(data$tot_dead)
summary(data$"%bl_fed")

#Additional data wraggling 

# data_long <- data %>%
#   pivot_longer(cols = ends_with("h_dead"), 
#                names_to = "time_postT", 
#                values_to = "number_dead") %>%
#   mutate(time_postT= parse_number(time_postT)) 
# 
# When the data is pivoted we realized that some data are cumulative one 

# Visualize distributions
hit_live <-data %>% 
  select(tot_live, treat_name) %>% 
  ggplot(aes(x=treat_name, y=tot_live))+
  geom_bar(aes(fill = treat_name, color = treat_name), 
           stat = "identity", position = position_dodge(0.8), width = 0.7 )+
  theme_bw()+
  labs(x = "Treatment", y = "Total Live") +  # Change axis labels
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

hist_dead <- data %>% 
  select(tot_dead, treat_name) %>% 
  ggplot(aes(x=treat_name, y=tot_dead))+
  geom_bar(aes(fill = treat_name, color = treat_name), 
           stat = "identity", position = position_dodge(0.8), width = 0.7 )+
  theme_bw()+
  labs(x = "Treatment", y = "Total Dead") +  # Change axis labels
  theme(legend.position = "none",  
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  

plot_grid(hit_live, hist_dead, labels = "AUTO")
# Put side by side we can observe that Royal guard recorded the highest number 
# of alive and dead mosquitoes. This might implies that we migh have some typing
# error. In a nutshell this plot can be used to insure the quality of our dataset 

# Progression of number of dead over time
  data_long <- data %>%
    select(tot_dead, treat_name, tot_live, ends_with("h_dead")) %>% 
    pivot_longer(cols = ends_with("h_dead"),
                 names_to = "time_postT",
                 values_to = "number_dead") %>%
    mutate(time_postT= parse_number(time_postT))
  
  
data_long %>% 
    ggplot( aes(x = treat_name , y = number_dead, fill = as.factor(time_postT))) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Progression of Number of Dead Over Time",
         x = "Treatment",
         y = "Number of Dead") +
    theme_minimal()+
    theme(
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
    )
  
# Convert 'day' to date format
data$day <- as.Date(data$day, origin = "2023-01-01")

# Time series plot
plot(data$day, data$tot_dead, type = "l", col = "blue", xlab = "Date", ylab = "Total Dead")

data$cumulative_dead <- ave(data$tot_dead, data$treat_name, FUN = cumsum)
data %>% 
  ggplot( aes(x = day, y = cumulative_dead, color = treat_name)) +
  geom_line() +
  labs(title = "Time Series of Total Dead Over Time",
       x = "Day",
       y = "Cumulative Dead") +
  theme_minimal()


#######Section 2 

# Hypothesis testing
# The treated nets significantly reduce the number of dead mosquitoes compared to untreated nets.
# Assuming your data frame is named 'data'

# follows a Poisson distribution
poisson_model <- glm(tot_dead ~ treat_name, data = data, family = poisson)

# Print the summary of the Poisson GLM
summary(poisson_model)

# Assuming your data frame is named 'data'
# Install and load the MASS package for the negative binomial GLM
# install.packages("MASS")
library(MASS)

# Negative Binomial regression is used when there is overdispersion in the data (variance exceeds the mean).

neg_binom_model <- glm.nb(tot_dead ~ treat_name, data = data)

# Print the summary of the negative binomial GLM
summary(neg_binom_model)

# Zero-Inflated Poisson (ZIP) Model

# Zero-Inflated Poisson model is appropriate when there are excess zeros in the count data.

# Install and load the pscl package for zero-inflated models
# install.packages("pscl")
library(pscl)

# Fit Zero-Inflated Poisson model
zip_model <- zeroinfl(tot_dead ~ treat_name | 1, data = data, dist = "poisson")

# Print the summary
summary(zip_model)


# Zero-Inflated Negative Binomial (ZINB) Model

# Zero-Inflated Negative Binomial model is a combination of 
# zero-inflated and negative binomial models.

# Fit Zero-Inflated Negative Binomial model
zinb_model <- zeroinfl(tot_dead ~ treat_name | 1, data = data, dist = "negbin")

# Print the summary
summary(zinb_model)


aic_values <- c(AIC(poisson_model), AIC(neg_binom_model), 
                AIC(zip_model), AIC(zinb_model))
# Choose the model with the lowest AIC
best_model <- which.min(aic_values)
# Print the best model
print(paste("The best model is Model", best_model))
summary(zinb_model)

# Interpretation
# This parameter indicates the degree of overdispersion. 
# A lower value suggests less variability than a Poisson distribution, 
# while a higher value suggests more.
# Negative coefficients suggest a decrease in the expected count of the outcome variable.
# Based on the count model coefficients, "treat_name Untreated Net" has the 
# most negative coefficient (-2.8362), suggesting the largest decrease in the 
# expected count compared to the baseline. The most effective is then "Royal guard 20W"


# 

# Regression Analysis
# Fit a linear regression model
model <- glm(tot_dead ~   data$'%bl_fed' + tot_exop, data = data, family = "poisson")

# Summary of the model
summary(model)

# Check assumptions
plot(model)




