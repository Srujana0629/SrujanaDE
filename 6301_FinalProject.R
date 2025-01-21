library(dplyr)
library(VGAM)
library(ggplot2)
library(patchwork)
library(tidyr)

# Reading data and creating a dataframe
data <- read.csv("C:/Users/satya/Downloads/archive (4)/depression_anxiety_data.csv")
data <- as.data.frame(data)
columns <- c('school_year', 'age', 'gender', 'who_bmi', 'phq_score', 'suicidal', 'anxiousness', 'sleepiness', 'depression_severity')
df <- data[columns]
df <- df %>% 
  rename(BMI_category = who_bmi)
head(df)

# Shape of the dataset
dim(df)

# Checking missing values
missing_values <- is.na(df)
missing_count <- colSums(missing_values)
print(missing_count)

# Custom function to calculate mode to handle missing values
custom_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}
# Replacing missing values with mode for each column
df <- df %>%
  mutate(across(everything(), ~ifelse(is.na(.), custom_mode(.), .)))
# Check for missing values
missing_values <- colSums(is.na(df))
print(missing_values)

# checking unique values in each categorical column
categorical_columns <- c('BMI_category', 'suicidal', 'depression_severity', 'anxiousness', 'sleepiness') 
# Using lapply function to apply unique() to each column
unique_values <- lapply(df[categorical_columns], function(x) {
  if(is.factor(x)) {
    levels(x)
  } else {
    unique(x)
  }
})
# Print unique values for each column
print(unique_values)

# Handling inconsistent values in BMI_category
df$BMI_category <- ifelse(df$BMI_category == "Not Availble", "Normal", df$BMI_category)

# Define the level mapping as a named vector
level_mapping <- c('Mild' = 'Mild',
                   'Moderately severe' = 'Severe',
                   'None-minimal' = 'Mild',
                   'Moderate' = 'Moderate',
                   'Severe' = 'Severe',
                   'none' = 'Mild')
# Replace values in 'depression_severity' column using the level mapping
df$depression_severity <- level_mapping[df$depression_severity]
unique_values <- unique(df$depression_severity)
print(unique_values)

# Group by 'school_year', 'anxiousness', and 'depression_severity', and count the occurrences
counts <- df %>%
  group_by(school_year, anxiousness, depression_severity) %>%
  summarise(counts = n()) %>%
  ungroup()
# Print the resulting counts
print(counts)

# Explanatory Data Analysis
# Define teal blue color palette
teal_palette <- c("#2B7A78", "#4EACB8", "#BFF8FF")
blue_palette <- c("#1f77b4", "#aec7e8", "#7fbfff", "#5d9fd8", "#4c78a8")

# Plot-1
# Function to calculate counts and create stacked bar plot
create_stacked_bar_plot <- function(df, x_var, title) {
  counts <- df %>%
    count({{x_var}}, depression_severity) %>%
    arrange({{x_var}}, desc(depression_severity))
  
  plot <- ggplot(counts, aes(x = {{x_var}}, y = n, fill = depression_severity)) +
    geom_bar(stat = "identity",  width = 0.8) +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = "black") +
    scale_fill_manual(values = teal_palette) +
    labs(title = title,
         x = as_label(rlang::enquo(x_var)),
         y = "Count",
         fill = "Depression Severity") +
    theme_minimal()
  
  return(plot)
}
# Create stacked bar plots
plot_anxiousness <- create_stacked_bar_plot(df, anxiousness, "Anxiousness by Depression Severity")
plot_gender <- create_stacked_bar_plot(df, gender, "Gender by Depression Severity")
# Arrange plots side by side
side_by_side_plots <- plot_anxiousness + plot_gender
print(side_by_side_plots)

# Plot-2
data_long <- df %>%
  pivot_longer(cols = c(age, phq_score), names_to = "Variable", values_to = "Value")
# Create side-by-side box plots for Age and PHQ Score
boxplot_age_phq <- ggplot(data_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot() +
  scale_fill_manual(values = blue_palette) +
  labs(title = "Distribution of Age and PHQ Score",
       x = NULL,  # Remove x-axis label
       y = "Value",  # Change y-axis label as needed
       fill = NULL) +  # Remove legend title
  theme_minimal()

# Display the plot
print(boxplot_age_phq)

# Plot-3
# Create a violin plot for depression severity
violin_plot <- ggplot(df, aes(x = depression_severity, y = phq_score, fill = depression_severity)) +
  geom_violin(trim = FALSE) +  # Set trim to FALSE to show the entire density
  scale_fill_manual(values = teal_palette) +  # Set fill color
  labs(title = "Distribution of Depression Severity",
       x = "Depression Severity",
       y = "PHQ Score") +
  theme_minimal()

# Display the plot
print(violin_plot)

# Plot-4
counts <- df %>%
  group_by(school_year, depression_severity) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  arrange(school_year, desc(depression_severity))

stacked_school_year <- ggplot(counts, aes(x = school_year, y = count, fill = depression_severity)) +
  geom_bar(stat = "identity", width = 0.5) +  # Adjust width of bars here
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), color = "black") +
  scale_fill_manual(values = blue_palette) +
  labs(title = "Stacked Bar Plot of School year by Depression Severity",
       x = "School Year",
       y = "Count",
       fill = "Depression Severity") +
  coord_flip() +
  theme_minimal()

# Display the plot
print(stacked_school_year)

# Contingency table
data <- data.frame(School_Year = c(rep("1", 2), rep("2", 2),rep("3", 2), rep("4", 2)),  
                   anx = c("False", "True", "False", "True", "False", "True", "False", "True"),  
                   Mild = c(170,26,112,28,105,10,127,11),   
                   Moderate = c(20,35,21,16,10,11,20,7),   
                   Severe = c(5,19,2,13,0,4,1,10) ) 
print(data)

# Cummulative models
# Cumulative Model with Interaction term
datafit1 <- vglm(cbind(Mild,Moderate,Severe)~ School_Year*anx, family = cumulative(parallel = TRUE), data = data)
summary(datafit1)
# Goodness-of-fit for model1
pearson_chi_sq <- sum(residuals(datafit1, type = "pearson")^2)
df <- df.residual(datafit1)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")

# Cumulative Model without Interaction term
datafit2 <- vglm(cbind(Mild,Moderate,Severe)~ School_Year+anx, family = cumulative(parallel = TRUE), data = data)
summary(datafit2)
# Goodness-of-fit for model2
pearson_chi_sq <- sum(residuals(datafit2, type = "pearson")^2)
df <- df.residual(datafit2)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")
# Likelihood-ratio-test for model1 and model2
dev <- deviance(datafit2) - deviance(datafit1)
df <- df.residual(datafit2)-df.residual(datafit1)
p_value <- 1-pchisq(dev,df)
cat("P-value:", p_value, "\n")

# Cumulative Model with only anxiousness
datafit3 <- vglm(cbind(Mild,Moderate,Severe)~ anx, family = cumulative(parallel = TRUE), data = data)
summary(datafit3)
# Goodness-of-fit for model3
pearson_chi_sq <- sum(residuals(datafit3, type = "pearson")^2)
df <- df.residual(datafit3)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")
# Likelihood-ratio-test for model2 and model3
dev <- deviance(datafit3) - deviance(datafit2)
df <- df.residual(datafit3)-df.residual(datafit2)
p_value <- 1-pchisq(dev,df)
cat("P-value:", p_value, "\n")

# Cumulative Model with only scool_year
datafit4 <- vglm(cbind(Mild,Moderate,Severe)~ School_Year, family = cumulative(parallel = TRUE), data = data)
summary(datafit4)
# Goodness-of-fit for model4
pearson_chi_sq <- sum(residuals(datafit4, type = "pearson")^2)
df <- df.residual(datafit4)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")

# AIC and BIC values for each cumulative model
AIC_complex <- AIC(datafit1)
AIC_Main <- AIC(datafit2)
AIC_simple <- AIC(datafit3)

BIC_complex <- BIC(datafit1)
BIC_Main <- BIC(datafit2)
BIC_simple <- BIC(datafit3)

print(AIC_complex)
print(AIC_Main)
print(AIC_simple)

print(BIC_complex)
print(BIC_Main)
print(BIC_simple)

# Baseline models
# Baseline Model without Interaction term
basefit1 <- vglm(cbind(Mild,Moderate,Severe)~ School_Year+anx, family = multinomial, data = data)
summary(basefit1)
# Goodness-of-fit for model1
pearson_chi_sq <- sum(residuals(basefit1, type = "pearson")^2)
df <- df.residual(basefit1)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")

# Baseline Model with Interaction term
basefit2 <- vglm(cbind(Mild,Moderate,Severe)~ School_Year*anx, family = multinomial, data = data)
summary(basefit2)
# Goodness-of-fit for model2
pearson_chi_sq <- sum(residuals(basefit2, type = "pearson")^2)
df <- df.residual(basefit2)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")
# Likelihood-ratio-test for model1 and model2
dev <- deviance(basefit1) - deviance(basefit2)
df <- df.residual(basefit1)-df.residual(basefit2)
p_value <- 1-pchisq(dev,df)
cat("P-value:", p_value, "\n")

# Baseline Model with only anxiousness
basefit3 <- vglm(cbind(Mild,Moderate,Severe)~ anx, family = multinomial, data = data)
summary(basefit3)
# Goodness-of-fit for model3
pearson_chi_sq <- sum(residuals(basefit3, type = "pearson")^2)
df <- df.residual(basefit3)
p_value <- pchisq(pearson_chi_sq, df, lower.tail = FALSE)
cat("P-value:", p_value, "\n")
# Likelihood-ratio-test for model2 and model3
dev <- deviance(basefit3) - deviance(basefit2)
df <- df.residual(basefit3)-df.residual(basefit2)
p_value <- 1-pchisq(dev,df)
cat("P-value:", p_value, "\n")

# AIC and BIC values for each cumulative model
AIC_complex <- AIC(basefit2)
AIC_Main <- AIC(basefit1)
AIC_simple <- AIC(basefit3)

BIC_complex <- BIC(basefit2)
BIC_Main <- BIC(basefit1)
BIC_simple <- BIC(basefit3)

print(AIC_complex)
print(AIC_Main)
print(AIC_simple)

print(BIC_complex)
print(BIC_Main)
print(BIC_simple)
