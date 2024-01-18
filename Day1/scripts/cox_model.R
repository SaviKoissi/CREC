# Training Day 1 Long term modelling 
# Koissi Savi (Ph.D.)
# Jan. 2024

# Long-Term Effectiveness Model
# Consider survival analysis (e.g., Cox Proportional Hazards model)

#EXAMPLE
#In this example, we'll assume we have data on the usage of LLINs 
#(Long-Lasting Insecticidal Nets) over time and want to assess their 
#effectiveness in preventing a specific event, such as the occurrence of malaria.

# Load necessary libraries
library(tidyverse)
library(survival)
library(survminer)

# Set a seed for reproducibility
set.seed(123)

# Generate dummy data
n <- 500  # number of observations
days_follow_up <- 365  # days of follow-up

# Create a dummy dataset
data <- tibble(
  participant_id = seq(1, n),
  treatment = sample(c("LLIN_A", "LLIN_B", "Control"), n, replace = TRUE),
  follow_up_days = sample(1:days_follow_up, n, replace = TRUE),
  event = rbinom(n, 1, plogis(-0.5 + 0.3 * as.numeric(factor(treatment)) - 0.01 * follow_up_days)),
  age = rnorm(n, mean = 30, sd = 5),
  sex = sample(c("Male", "Female"), n, replace = TRUE)
)

# Define survival object
surv_object <- with(data, Surv(follow_up_days, event))

# Fit Cox Proportional Hazards Model
cox_model <- coxph(surv_object ~ treatment + age + sex, data = data)

# Display summary
summary(cox_model)

### Interpretation
# The hazard ratio is 1.35432. This implies that, compared to the Control group, 
# participants in the LLIN_A group have a 35.43% higher hazard of the event.
# The hazard ratio is 1.45854. This implies that, compared to the Control group, 
# participants in the LLIN_B group have a 45.85% higher hazard of the event.
# The hazard ratio is 1.01880. This implies that, for each one-unit increase 
# in age, the hazard of the event increases by 1.88%. 
# The hazard ratio is 1.16630. This implies that males have a 16.63% 
# higher hazard of the event compared to females.
# The concordance is a measure of how well the model predicts the order of 
# event times. In this case, it is 0.56, indicating moderate predictive accuracy.
# These tests assess the overall significance of the model. In this case, 
# none of them are statistically significant, suggesting that the model may not 
# significantly improve predictions compared to a null model.


# Fit the survival model
fit <- survfit(Surv(follow_up_days, event) ~ treatment, data = data)

dev.off()


# Create survival plot
ggsurvplot(
  fit,
  data = data,
  pval = TRUE,
  risk.table = TRUE,
  conf.int = TRUE,
  xlim = c(0, 365),
  break.time.by = 100,
  ggtheme = theme_minimal(),
  risk.table.y.text.col = TRUE
)
