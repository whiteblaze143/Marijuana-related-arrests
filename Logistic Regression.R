# Load necessary libraries
library(tidyverse)
library(ggplot2)
library(dplyr)

data <- read.csv("marijuana_arrests.csv", header = TRUE)

# clean data by removing or imputing missing values
data <- na.omit(data)

# Check for missing values
sum(is.na(data))

# Basic summary statistics
summary(data)

# Structure of the Data
str(data)


# Exploratory Data Analysis

# Summary Statistics
summary(data)

# Structure of the Data
str(data)

# Breakdown of categorical variables
table(data$colour)

table(data$sex)

table(data$employed)

table(data$citizen)

# Age Distribution
ggplot(data, aes(x = age)) +
  geom_histogram(bins = 30, fill = 'blue', color = 'black') +
  labs(title = "Age Distribution of Arrestees", x = "Age", y = "Frequency")

# Checks Distribution
ggplot(data, aes(x = checks)) +
  geom_histogram(bins = 30, fill = 'green', color = 'black') +
  labs(title = "Distribution of Police Checks", x = "Number of Checks", y = "Frequency")

# Race and Release Outcomes
ggplot(data, aes(x = colour, fill = released)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Release Outcomes by Race", x = "Race", y = "Percentage")

# Pairwise relationships with GGally
if("GGally" %in% rownames(installed.packages()) == FALSE) {install.packages("GGally")}
library(GGally)
ggpairs(data, columns = 2:ncol(data), aes(colour = colour))

# Correlation matrix for numerical variables
# Install the corrplot package
if(!"corrplot" %in% installed.packages()){
  install.packages("corrplot", dependencies = TRUE)
}

# Load the corrplot package
library(corrplot)

# correlation matrix for numerical variables
numeric_vars <- sapply(data, is.numeric)
cor_data <- cor(data[, numeric_vars])
corrplot(cor_data, method = "circle")

# Boxplots for Age by Race and Release status
ggplot(data, aes(x = colour, y = age, fill = released)) +
  geom_boxplot() +
  labs(title = "Age Distribution by Race and Release Status", x = "Race", y = "Age")

# Ensure that the outcome variable 'released' is a factor if it's binary
data$colour <- as.factor(data$colour)
data$sex <- as.factor(data$sex)
data$employed <- as.factor(data$employed)
data$citizen <- as.factor(data$citizen)
data$released <- as.factor(data$released)

# Create a logistic regression model to predict the likelihood of being released
# with 'released' as the response variable and others as predictors
logistic_model <- glm(released ~ colour + age + sex + employed + citizen + checks, 
                      data = data, 
                      family = binomial)

# Summarize the model to get the coefficients and significance
summary(logistic_model)
#check the levels of the actual outcome variable
levels(data$released)

# If the levels are not "0" and "1", adjust them
data$released <- factor(data$released, levels = c("No", "Yes"))

# Predict probabilities
predicted_probs <- predict(logistic_model, type = "response")

# Convert probabilities to binary outcome based on a 0.5 threshold
predicted_values <- ifelse(predicted_probs > 0.5, "Yes", "No")

# Convert to factor with the same levels as the actual outcome variable
predicted_factors <- factor(predicted_values, levels = levels(data$released))
# Model Evaluation using Confusion Matrix
library(caret)
predicted_classes <- ifelse(predict(logistic_model, type = "response") > 0.5, "Yes", "No")
confusion_matrix <- confusionMatrix(factor(predicted_classes, levels = c("No", "Yes")), data$released)

# Output the confusion matrix
print(confusion_matrix)

# Variable Importance Assessment using Wald statistics
# Extract model coefficients
model_coefficients <- summary(logistic_model)$coefficients

# Calculate Wald statistics (coefficient divided by standard error)
wald_stats <- abs(model_coefficients[, "Estimate"] / model_coefficients[, "Std. Error"])

# Create a data frame for variable importance
variable_importance <- data.frame(
  Variable = rownames(model_coefficients),
  Wald_Statistic = wald_stats,
  Coefficient = model_coefficients[, "Estimate"]
)

# Sort by Wald statistic to find most important variables
variable_importance <- variable_importance[order(-variable_importance$Wald_Statistic), ]

# Output the sorted variable importance
print(variable_importance)

#plot the importance, use ggplot2
ggplot(variable_importance, aes(x = reorder(Variable, Wald_Statistic), y = Wald_Statistic)) +
  geom_col() +
  coord_flip() +
  labs(title = "Variable Importance based on Wald Statistics", x = "Variables", y = "Wald Statistic")


