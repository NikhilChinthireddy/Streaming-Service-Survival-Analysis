# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(reshape2)
library(survival)
library(survminer)

# Load the dataset
data <- read.csv("StreamingServiceSurvivalDataset.csv")

# View the first few rows of the dataset
head(data)
str(data)
sum(is.na(data))

# Impute missing values with the median
data$noofshowstarted[is.na(data$noofshowstarted)] <- median(data$noofshowstarted, na.rm = TRUE)
data$noofshowsfinished[is.na(data$noofshowsfinished)] <- median(data$noofshowsfinished, na.rm = TRUE)
sum(is.na(data))

# Convert binary and categorical variables to factors
data$sex1ismale <- as.factor(data$sex1ismale)
data$customerfromtvtrialoffer <- as.factor(data$customerfromtvtrialoffer)
data$specialoffertocontinuewasmadetouser <- as.factor(data$specialoffertocontinuewasmadetouser)
data$couponcodeusedforinitialsignup <- as.factor(data$couponcodeusedforinitialsignup)
data$timeslotwatched <- factor(data$timeslotwatched,
                               levels = c(1, 2, 3, 4),
                               labels = c("Morning", "Evening", "Night", "Afternoon"))

data$pandemicregistration <- as.factor(data$pandemicregistration)
data$renewed1nonrenewed2 <- as.factor(data$renewed1nonrenewed2)
str(data)

# Summary statistics for numerical variables
summary(data[c("noofshowstarted", "noofshowsfinished", "hoursstreamed", "daysperweekwatching")])

# Summary for categorical variables
summary(data[c("sex1ismale", "customerfromtvtrialoffer", "specialoffertocontinuewasmadetouser",
               "couponcodeusedforinitialsignup", "timeslotwatched", "pandemicregistration", "renewed1nonrenewed2")])

# Histogram of hours streamed
ggplot(data, aes(x = hoursstreamed)) +
  geom_histogram(binwidth = 100, color = "black", fill = "#69b3a2") +
  labs(title = "Distribution of Hours Streamed",
       x = "Hours Streamed",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(), 
        legend.position = "none") + 
  scale_x_continuous(breaks = seq(0, max(data$hoursstreamed), by = 500)) + 
  scale_y_continuous(labels = scales::comma) 


# Relationship between hours streamed and renewal status
ggplot(data, aes(x=renewed1nonrenewed2, y=hoursstreamed, fill=renewed1nonrenewed2)) + geom_boxplot() +
  theme_minimal() + ggtitle("Hours Streamed by Renewal Status")

# Correlation between numerical variables
numerical_data <- data %>% select(noofshowstarted, noofshowsfinished, hoursstreamed, daysperweekwatching)

cor_matrix <- cor(numerical_data)
# Melt the correlation matrix into a long format
melted_cor_matrix <- melt(cor_matrix)

# Create the heatmap
ggplot(melted_cor_matrix, aes(x=Var1, y=Var2, fill=value)) +
  geom_tile(color = "white") + # Use white lines to separate the tiles
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + # Use a minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.text.y = element_text(size = 12)) +
  coord_fixed() + # Ensure the tiles are square
  labs(x = '', y = '', title = 'Correlation Matrix Heatmap') +
  geom_text(aes(label = sprintf("%.2f", value)), size = 3, color = "black") # Add correlation coefficients


# 1 indicates renewal (event did not occur) and 2 indicates non-renewal (event occurred)
data$event_occurred <- ifelse(data$renewed1nonrenewed2 == 2, 1, 0)  # Recoding for clarity

# Prepare the survival object
surv_obj <- Surv(time = data$hoursstreamed, event = data$event_occurred)

# Fit Kaplan-Meier survival estimate
km_fit <- survfit(surv_obj ~ 1, data = data)  # '1' indicates we're not stratifying by any factor here

# Plot Kaplan-Meier survival estimate
ggsurvplot(km_fit, data = data, risk.table = TRUE, conf.int = TRUE,
           title = "Kaplan-Meier Estimate of Streaming Service Renewal",
           xlab = "Follow-Up Time", ylab = "Survival Probability")

# Stratifying by gender
km_fit_strat <- survfit(surv_obj ~ sex1ismale, data = data)

# Plotting stratified Kaplan-Meier estimates
ggsurvplot(km_fit_strat, data = data, risk.table = TRUE, pval = TRUE, conf.int = TRUE,
           title = "Kaplan-Meier Estimate by Gender",
           xlab = "Follow-Up Time", ylab = "Survival Probability")


# Create a binary variable for streaming more than 1000 hours
data$streamed_over_1000 <- ifelse(data$hoursstreamed > 1000, "Over 1000 Hours", "Under 1000 Hours")
data$streamed_over_1000 <- as.factor(data$streamed_over_1000)

# Fit Kaplan-Meier survival estimate stratified by streaming over 1000 hours
km_fit_strat <- survfit(Surv(hoursstreamed, event_occurred) ~ streamed_over_1000, data = data)

# Plot Kaplan-Meier survival estimate
ggsurvplot(km_fit_strat, data = data, palette = c("#00BA38", "#F8766D"),
           title = "Kaplan-Meier Estimate by Streaming Hours",
           xlab = "Follow-Up Time (hours streamed)",
           ylab = "Survival Probability",
           risk.table = TRUE)
# Perform Log-Rank Test
surv_diff <- survdiff(Surv(hoursstreamed, event_occurred) ~ streamed_over_1000, data = data)
print(surv_diff)


# Fit Cox proportional hazards model
cox_model <- coxph(surv_obj ~ sex1ismale + customerfromtvtrialoffer + specialoffertocontinuewasmadetouser +
                     couponcodeusedforinitialsignup + noofshowstarted + noofshowsfinished +
                     timeslotwatched + pandemicregistration + daysperweekwatching, data = data)
# Summary of the Cox model
summary(cox_model)
# Visualizing the impact of covariates
ggforest(cox_model, data = data)
testCPH<-cox.zph(cox_model)
print(testCPH)

