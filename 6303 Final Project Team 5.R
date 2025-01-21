##Loading Data 
data <- read.csv("C:/Users/satya/Downloads/income.csv/income.csv")

##Dimension of dataset
dim(data)

##checking missing values
missing_values <- is.na(data)
missing_count <- colSums(missing_values)
print(missing_count)

##Removing duplicates 
duplicates_count <- nrow(data) - nrow(unique(data))
data_unique <- unique(data)

##Removing education column
data_unique <- data_unique[, -which(names(data_unique) == "education")]

##Replacing "?" with NA
data_unique <- replace(data_unique, data_unique == '?', NA)
missing_counts <- colSums(is.na(data_unique))
barplot(missing_counts, 
        main="Missing Value Counts",
        xlab="Columns",
        ylab="Missing Value Count",
        col="blue",
        names.arg=names(missing_counts))

##Replacing null values with mode
cols <- c('workclass', 'occupation', 'native.country')
for (col in cols) {
  most_frequent <- names(sort(-table(data_unique[[col]])))[1]
  data_unique[[col]][is.na(data_unique[[col]])] <- most_frequent
  print(paste('All the missing values in column', col, 'are replaced with', most_frequent))
}
df1<-data_unique

filtered_rows <- data_unique$capital.gain < 50000 & data_unique$capital.loss < 3000
data_unique$capital <- filtered_rows

data_unique <- data_unique[, -which(names(data_unique) == "capital.gain")]
data_unique <- data_unique[, -which(names(data_unique) == "capital.loss")]

data_unique$marital.status <- ifelse(data_unique$marital.status %in% c("Married-civ-spouse", "Married-AF-spouse","Married-spouse-absent"), "Married", "Not in relationship")

df<-data_unique





#-----------------------------------------------------------------------------------------------------------------------------------------------
#EDA

#Univariant Variable

#1.
# Load necessary library
library(e1071)

# Select numerical variables
num_vars <- c("age", "education.num", "capital.gain", "capital.loss", "hours.per.week")

# Calculate skewness for each numerical variable
skewness <- sapply(data[num_vars], skewness)

# Print skewness values
print(skewness)

# Select numerical variables for which you want to create histograms
num_vars <- c("age","education.num","hours.per.week")

# Set up the layout for the histograms
par(mfrow = c(2, 3))  # Arrange histograms in a 2x3 grid

# Create histograms for each numerical variable
for (var in num_vars) {
  hist(data[[var]], main = paste("Histogram of", var), xlab = var)
}

##Barplot for Hours per week vs Gender
# Load the ggplot2 library
library(ggplot2)

# Convert sex column to factor
data$sex <- factor(data$sex)

# Create the bar plot
ggplot(data = data, aes(x = sex, y = hours.per.week)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Sex", y = "Hours per Week", title = "Hours per Week by Sex")

#7.
library(ggplot2)

# Create a bar plot for work class frequency
workclass_freq <- table(df$workclass)
workclass_df <- as.data.frame(workclass_freq)
colnames(workclass_df) <- c("Workclass", "Frequency")

# Specify fill colors using shades of blue and yellow
fill_colors <- c("#4682B4", "#87CEEB", "#ADD8E6","#FFA500","#FFD700","#FF6347", "#20B2AA", "#9370DB")

# Plot the bar graph with specified colors
ggplot(data = workclass_df, aes(x = Workclass, y = Frequency, fill = Workclass)) +
  geom_bar(stat = "identity") +  # Use fill aesthetic
  scale_fill_manual(values = fill_colors[1:length(unique(df$workclass))]) +  # Specify fill colors
  labs(title = "Frequency of Work Class", x = "Work Class", y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1));  # Rotate x-axis labels if needed


#income vs workclass
bivariate_categorical_plot <- function(df, var_1, var_2) {
  # Create the cross-tabulation
  cross_tab <- table(df[[var_1]], df[[var_2]])
  
  # Create the bar plot 
  barplot(cross_tab, beside = TRUE, col = c("skyblue", "goldenrod"), 
          main = paste(var_1, "vs", var_2), xlab = var_1, ylab = "Count", 
          legend.text = TRUE)
}

# Call the function with the names of the variables
bivariate_categorical_plot(df, "income", "workclass");


#10.BOX PLOTS
# Columns to exclude from the box plot
columns_to_exclude <- c('sex', 'workclass', 'education.num', 'marital.status', 'occupation', 'race', 'relationship', 'income', 'native.country')

# Get all columns except the ones to exclude
columns <- setdiff(names(df1), columns_to_exclude)

# Calculate the number of rows and columns for subplots
num_columns <- length(columns)
num_rows <- ceiling(num_columns / 2)

# Create a new plot window
par(mfrow = c(num_rows, 2))

# Loop through each column to create box plots
for (i in 1:num_columns) {
  column <- columns[i]
  boxplot(df1[[column]], main = paste("Box Plot of", column), col = "skyblue")
}

# Reset the plotting parameters
par(mfrow = c(1, 1))

# Convert relevant columns to numeric
numeric_columns <- c("hours.per.week","age", "capital.gain","capital.loss")
df1[numeric_columns] <- lapply(df1[numeric_columns], as.numeric)

# Create box plots
par(mfrow=c(2,2))  # Set the layout to 2x2 grid
for (column in numeric_columns) {
  boxplot(df1[[column]], main = paste("Box Plot of", column), col = "skyblue")
}


#---------------------------------------------------------------------------------------------------------------------------------------
#MODELING
##Categorical to Numeric by label encoding
columns_to_encode <- c('workclass', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'native.country','income')
for (column in columns_to_encode) {
  data_unique[[paste0(column, "_encoded")]] <- as.numeric(factor(data_unique[[column]]))
}
data_unique$income_encoded
# Show the mapping between original and encoded values
for (column in columns_to_encode) {
  cat("Unique values for", column, ":\n")
  unique_values <- unique(data_unique[[column]])
  for (value in unique_values) {
    encoded_value <- unique(data_unique[data_unique[[column]] == value, paste0(column, "_encoded")])
    cat("Original value:", value," , ", "Encoded value:", encoded_value, "\n")
  }
  cat("\n")
}


columns_to_remove <- c('workclass', 'marital.status', 'occupation', 'relationship', 'race', 'sex', 'native.country','income')
data_unique <- data_unique[, !names(data_unique) %in% columns_to_remove]

names(data_unique)


# Install and load required packages

library(corrplot)
library(gplots)

# Compute correlation matrix
df_corr <- cor(data_unique)

# Create the heatmap
heatmap.2(df_corr, 
          dendrogram = 'none',  # No dendrogram
          trace = 'none',       # No trace
          col = colorRampPalette(c("blue", "white", "red"))(100),  # Color palette
          cellnote = round(df_corr, 2),  # Display correlation values
          notecol = "black",    # Color of correlation values
          density.info = "none", # No density plot
          key = TRUE,           # Display color key
          keysize = 1.5,        # Size of the color key
          cexRow = 1.0,         # Size of row labels
          cexCol = 1.0,         # Size of column labels
          margins = c(10, 10),  # Margins around heatmap
          main = "Correlation Heatmap"  # Title
)


df <- df[, -which(names(df) == "fnlwgt")]
names(df)
##Modeling
set.seed(5664)  # Set seed for reproducibility
X <- df[, !(names(df) %in% c('income'))]
y <- df$income

##Balancing the data
# Install and load the ROSE package
install.packages("ROSE")
library(ROSE)

# Perform SMOTE oversampling
oversampled_data <- ovun.sample(y ~ ., data = cbind(X, y), method = "both", N = nrow(X), seed = 42)

# Extract the balanced feature matrix and target variable
X_balanced <- oversampled_data$data[, names(oversampled_data$data) != "y"]
y_balanced <- oversampled_data$data$y

library(caret)
set.seed(5664)
train_indices <- createDataPartition(y_balanced, p = 0.8, list = FALSE)
X_train <- X_balanced[train_indices, ]
X_test <- X_balanced[-train_indices, ]
y_train <- y_balanced[train_indices]
y_test <- y_balanced[-train_indices]


# Load the rpart package
library(rpart)

model_gini <- rpart(income ~ ., data = data.frame(X_train, income = y_train), method = "class")

y_pred_gini <- predict(model_gini, X_test,type="class")

# Calculate accuracy
accuracy_gini <- mean(y_pred_gini == y_test)

# Display accuracy
print(paste("Accuracy of the model:", accuracy_gini))

# Obtain confusion matrix
conf_matrix_gini <- table(y_test, y_pred_gini)
print("Confusion Matrix:")
print(conf_matrix_gini)

library(ggplot2)
library(tidyr)
# Create data frame from the confusion matrix
conf_matrix_gini_df <- as.data.frame.matrix(conf_matrix_gini)

conf_matrix_gini_df <- cbind("Actual" = rownames(conf_matrix_gini_df), conf_matrix_gini_df)
rownames(conf_matrix_gini_df) <- NULL

# Reshape data using tidyr's gather function
conf_matrix_gini_df <- gather(conf_matrix_gini_df, "Predicted", "Count", -Actual)

# Plot heatmap with text labels
ggplot(data = conf_matrix_gini_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Count), color = "black") +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "green") +
  labs(title = "Confusion Matrix Heatmap of Gini Index", x = "Predicted", y = "Actual") +
  theme_minimal()

##Roc
library(pROC)
y_test_binary <- ifelse(y_test == ">50K", 1, 0)
y_pred_binary_gini<- ifelse(y_pred_gini == ">50K", 1, 0)

# Create ROC curve
roc_obj_1 <- roc(y_test_binary, y_pred_binary_gini)

# Plot ROC curve
plot(roc_obj_1, main = "ROC Curve", col = "blue",legacy.axes = TRUE, print.auc = TRUE, smooth = TRUE)
plot(roc_obj_1, col = "red", add = TRUE, type = "p")
# Calculate AUC
auc_score <- auc(roc_obj_1)

##Decision Tree
library(rpart.plot)

rpart.plot(model_gini)



##Model 2 with Gain ratio
model_gain_ratio <- rpart(income ~ ., data = data.frame(X_train, income = y_train), method = "class", parms = list(split = "information"))

y_pred_gain_ratio <- predict(model_gain_ratio, X_test,type="class")

# Calculate accuracy
accuracy_gain_ratio <- mean(y_pred_gain_ratio == y_test)

# Display accuracy
print(paste("Accuracy of the model:", accuracy_gain_ratio))

# Obtain confusion matrix
conf_matrix_gain_ratio <- table(y_test, y_pred_gain_ratio)
print("Confusion Matrix:")
print(conf_matrix_gain_ratio)

##Confusion Matrix with heatmap
# Create data frame from the confusion matrix
conf_matrix_gain_df <- as.data.frame.matrix(conf_matrix_gain_ratio)

conf_matrix_gain_df <- cbind("Actual" = rownames(conf_matrix_gain_df), conf_matrix_gain_df)
rownames(conf_matrix_gain_df) <- NULL

# Reshape data using tidyr's gather function
conf_matrix_gain_df <- gather(conf_matrix_gain_df, "Predicted", "Count", -Actual)

# Plot heatmap with text labels
ggplot(data = conf_matrix_gain_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Count), color = "black") +
  geom_text(aes(label = Count), vjust = 1) +
  scale_fill_gradient(low = "yellow", high = "green") +
  labs(title = "Confusion Matrix Heatmap of Gain Ratio", x = "Predicted", y = "Actual") +
  theme_minimal()

#ROC PLOT

y_pred_binary_gain<- ifelse(y_pred_gain_ratio == ">50K", 1, 0)

# Create ROC curve
roc_obj_2 <- roc(y_test_binary, y_pred_binary_gain)

# Plot ROC curve
plot(roc_obj_2, main = "ROC Curve", col = "blue",legacy.axes = TRUE, print.auc = TRUE, smooth = TRUE)
plot(roc_obj_2, col = "red", add = TRUE, type = "p")
# Calculate AUC
auc_score <- auc(roc_obj_2)

##Decision Tree
rpart.plot(model_gain_ratio)


rmarkdown::render("C:/Users/satya/Desktop/6303 Final Project Team 5.R",output_format = 'word_document')
