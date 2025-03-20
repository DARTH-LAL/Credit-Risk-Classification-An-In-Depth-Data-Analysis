# ============================
# ***Data Import***
# ============================

setwd("/Users/ajneya/Desktop/RStudio/Assignment")

#Load Dataset
credit_risk <- read.csv("5.credit_risk_classification(2).csv", header = T, stringsAsFactors = T)

#View data
View(credit_risk)

#Check dimensions of the data
dim(credit_risk)

#Check first rows of the data
head(credit_risk)

#Check the data
summary(credit_risk)

#Backup data

backup_data <- credit_risk
View(backup_data)

#Double Check Backup Data 
dim(backup_data)

summary(backup_data)

# ============================
# ***Data Cleaning***
# ============================

#Check for NA values 
colSums(is.na(credit_risk))

sum(is.na(credit_risk))

#Checking for Missing Values
sum(is.na(credit_risk$checking_status))

#Check for Duplicated rows
sum(duplicated(credit_risk))

#Check Data Type for each column
str(credit_risk)

lapply(credit_risk, class)

#Check For outliers

  #Statistically check for outliers

detect_outliers_iqr <- function(column) {
  Q1 <- quantile(column, 0.25, na.rm = TRUE)
  Q3 <- quantile(column, 0.75, na.rm = TRUE)
  IQR_value <- IQR(column, na.rm = TRUE)
  
  # Outliers are values below Q1 - 1.5 * IQR or above Q3 + 1.5 * IQR
  outliers <- column < (Q1 - 1.5 * IQR_value) | column > (Q3 + 1.5 * IQR_value)
  return(outliers)
}

# Apply the function to detect outliers in each numeric column
outliers_info <- lapply(credit_risk[sapply(credit_risk, is.numeric)], detect_outliers_iqr)

# Create a new data frame to display only the outliers
outliers_display <- credit_risk  # Copy the original data

# Replace non-outlier values with NA in the new data frame
for (col in names(outliers_info)) {
  outliers_display[[col]][!outliers_info[[col]]] <- NA
}

# Display only the outliers
View(outliers_display)

#Visually check for outliers

#Define Numeric Columns
numeric_columns <- credit_risk[sapply(credit_risk, is.numeric)]

# Detect outliers for each numeric column
outliers_info <- lapply(numeric_columns, detect_outliers_iqr)

# Set up plotting area
par(mfrow = c(ceiling(length(numeric_columns) / 2), 2))

# Plot each column with outliers highlighted
for (col in names(numeric_columns)) {
  # Create an index for outliers
  outlier_indices <- which(outliers_info[[col]])
  
  # Scatter plot
  plot(numeric_columns[[col]], main = paste("Outliers in", col),
       xlab = "Index", ylab = col, col = "black", pch = 16)
  
  # Highlight outliers in red
  points(outlier_indices, numeric_columns[[col]][outlier_indices], col = "red", pch = 16)
}

# ============================
# ***Data Cleaning and Processing by columns***
# ============================


# ***Column 1 : Checking Status***

#Check Data Type
str(credit_risk$checking_status)

#Check unique values
unique(credit_risk$checking_status)

#Frequency of unique values
table(credit_risk$checking_status)

#Standardize labels in a new column
credit_risk$new_checking_status <- ifelse(credit_risk$checking_status == "no checking", "no checking",
                                          ifelse(credit_risk$checking_status == "<0", "negative checking",
                                                 ifelse(credit_risk$checking_status == ">=200", "good ghecking", 
                                                        ifelse(credit_risk$checking_status == "0<=X<200", "average checking", NA))))

#Confirm change of labels
table(credit_risk$new_checking_status)

#Covert Convert new checking status column to factor 
credit_risk$new_checking_status <- as.factor(credit_risk$new_checking_status)

#Confirm change
str(credit_risk$new_checking_status)


# ***Column 2 : Duration***


#Check Data Type
str(credit_risk$duration)

#Frequency of unique values
table(credit_risk$duration)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$duration)

#Apply outlier fucntion to the column
outliers_duration <- detect_outliers_iqr(credit_risk$duration)

# View the summary of outliers
summary(outliers_duration)

# Extract the rows with outliers
outliers_data <- credit_risk[outliers_duration, ]

# Display the outliers
View(outliers_data)

# Visualize the  outliers
boxplot(credit_risk$duration, main = "Boxplot of Duration", 
        ylab = "Duration", col = "pink", horizontal = TRUE)

# Highlight outliers 
outlier_values <- credit_risk$duration[outliers_duration]
points(outlier_values, rep(1, length(outlier_values)), col = "red", pch = 16)

#Convert decimal values in duration column to non-decimal in a new column
credit_risk$new_duration = ceiling(credit_risk$duration)

#Check Changes
table(credit_risk$new_duration)
summary(credit_risk$new_duration)

# ***Column 3 : Credit History***

#Check Data Type
str(credit_risk$credit_history)

#Check unique values
unique(credit_risk$credit_history)

#Frequency of unique values
table(credit_risk$credit_history)

#Convert credit history data type to character in new column
credit_risk$new_credit_history = as.character(credit_risk$credit_history)

#Confirm datatype change
str(credit_risk$new_credit_history)

#Replace redundant values
credit_risk$new_credit_history[which(credit_risk$new_credit_history == "all paid")] = "no credits/all paid"

#Confirm Change
table(credit_risk$new_credit_history)

#Convert credit history data type to factor
credit_risk$new_credit_history <- as.factor(credit_risk$new_credit_history)

#Confirm datatype change
str(credit_risk$new_credit_history)

# ***Column 4 : Purpose***

#Check Data Type
str(credit_risk$purpose)

#Check unique values
unique(credit_risk$purpose)

#Frequency of unique values
table(credit_risk$purpose)

#New column to standardize labels ? (merge cars and change radio/tv to electronics)

# ***Column 5 : Credit Amount***

#Check Data Type
str(credit_risk$credit_amount)

#Frequency of unique values
table(credit_risk$credit_amount)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$credit_amount)

# Apply the outlier function to the  column
outliers_credit_amount <- detect_outliers_iqr(credit_risk$credit_amount)

# View the summary of outliers
summary(outliers_credit_amount)

# Extract the rows with outliers
outliers_credit_data <- credit_risk[outliers_credit_amount, ]

# Display the outliers
View(outliers_credit_data)

# Visualize the outliers
boxplot(credit_risk$credit_amount, main = "Boxplot of Credit Amount", 
        ylab = "Duration", col = "blue", horizontal = TRUE)

# Highlight outliers 
outlier_values_credit <- credit_risk$credit_amount[outliers_credit_amount]
points(outlier_values_credit, rep(1, length(outlier_values_credit)), col = "red", pch = 16)

# ***Column 6 : Savings Status***

#Check Data Type
str(credit_risk$savings_status)

#Check unique values
unique(credit_risk$savings_status)

#Frequency of unique values
table(credit_risk$savings_status)

#Standardize labels in a new column
credit_risk$new_savings_status <- ifelse(credit_risk$savings_status == "<100", "poor saving",
                                         ifelse(credit_risk$savings_status == ">=1000", "excellent saving",
                                                ifelse(credit_risk$savings_status == "100<=X<500", "average saving", 
                                                       ifelse(credit_risk$savings_status == "500<=X<10000", "good saving", 
                                                              ifelse(credit_risk$savings_status == "no known savings", "no savings", "no savings")))))

#Confirm change of labels
table(credit_risk$new_savings_status)

#Covert Convert new checking status column to factor 
credit_risk$new_savings_status <- as.factor(credit_risk$new_savings_status)

#Confirm change
str(credit_risk$new_savings_status)

# ***Column 6 : Employment***

#Check Data Type
str(credit_risk$employment)

#Check unique values
unique(credit_risk$employment)

#Frequency of unique values
table(credit_risk$employment)

#Change data type to character in a new column
credit_risk$new_employment = as.character(credit_risk$employment)

#Confirm datatype change
str(credit_risk$new_employment)

#Standardize labels in a new column
credit_risk$new_employment <- ifelse(credit_risk$employment == "<1", "1",
                                     ifelse(credit_risk$employment == ">=7", "7",
                                            ifelse(credit_risk$employment == "1<=X<4", "2.5", 
                                                   ifelse(credit_risk$employment == "4<=X<7", "5.5", 
                                                          ifelse(credit_risk$employment == "unemployed", "0", "0")))))

#Confirm change of labels
table(credit_risk$new_employment)

##Change data type to numeric
credit_risk$new_employment <- as.numeric(credit_risk$new_employment)

#Confirm datatype change
str(credit_risk$new_employment)

# ***Column 7 : Installment Commitment***

#Check Data Type
str(credit_risk$installment_commitment)

#Frequency of unique values
table(credit_risk$installment_commitment)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$installment_commitment)

# Apply the outlier function to the column
outliers_installment_commitment <- detect_outliers_iqr(credit_risk$installment_commitment)

# View the summary of outliers
summary(outliers_installment_commitment)

# Extract the rows with outliers
outliers_installment_data <- credit_risk[outliers_installment_commitment, ]

# Display the outliers
View(outliers_installment_data)

# Visualize the outliers
boxplot(credit_risk$installment_commitment, main = "Boxplot of Installment Commitment", ylab = "Installment Commitment", col = "lightgreen", 
        horizontal = TRUE)

# Highlight outliers
outlier_values_installment <- credit_risk$installment_commitment[outliers_installment_commitment]
points(outlier_values_installment, rep(1, length(outlier_values_installment)), col = "red", pch = 16)

# ***Column 8 : Personal Status***

#Check Data Type
str(credit_risk$personal_status)

#Check unique values
unique(credit_risk$personal_status)

#Frequency of unique values
table(credit_risk$personal_status)

#Standardize labels in a new column
credit_risk$new_personal_status <- ifelse(credit_risk$personal_status == "male div/sep", "male divorced/separated",
                                          ifelse(credit_risk$personal_status == "female div/dep/mar", "female divorced/separated/married",
                                                 ifelse(credit_risk$personal_status == "male single", "male single",
                                                        ifelse(credit_risk$personal_status == "male mar/wid", "male married/widowed", 
                                                               "other"))))

#Confirm change of labels
table(credit_risk$new_personal_status)

#Covert Convert new personal status column to factor 
credit_risk$new_personal_status <- as.factor(credit_risk$new_personal_status)

#Confirm change
str(credit_risk$new_personal_status)

# ***Column 9 : Other Parties***

#Check Data Type
str(credit_risk$other_parties)

#Check unique values
unique(credit_risk$other_parties)

#Frequency of unique values
table(credit_risk$other_parties)

# ***Column 9 : Residence Since***

#Check Data Type
str(credit_risk$residence_since)

#Frequency of unique values
table(credit_risk$residence_since)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$residence_since)

# Apply the outlier function to the column
outliers_residence_since <- detect_outliers_iqr(credit_risk$residence_since)

# View the summary of outliers
summary(outliers_residence_since)

# Extract the rows with outliers
outliers_residence_data <- credit_risk[outliers_residence_since, ]

# Display the outliers
View(outliers_residence_data)

# Visualize the outliers
boxplot(credit_risk$residence_since, main = "Boxplot of Residence Since", ylab = "Residence Since (Years)", col = "purple", horizontal = TRUE)

# Highlight outliers
outlier_values_residence <- credit_risk$residence_since[outliers_residence_since]
points(outlier_values_residence, rep(1, length(outlier_values_residence)), col = "red", pch = 16)

#Convert decimal values in residence since column to non-decimal in a new column
credit_risk$new_residence_since = round(credit_risk$residence_since)

#Check Changes
table(credit_risk$new_residence_since)
summary(credit_risk$new_residence_since)

# ***Column 10 : Property Magnitude***
 
#Check Data Type
str(credit_risk$property_magnitude)

#Check unique values
unique(credit_risk$property_magnitude)

#Frequency of unique values
table(credit_risk$property_magnitude)

#Standardize labels in a new column
credit_risk$new_property_magnitude <- ifelse(credit_risk$property_magnitude == "real estate", "real estate",
                                         ifelse(credit_risk$property_magnitude == "life insurance", "life insurance",
                                                ifelse(credit_risk$property_magnitude == "car", "car",
                                                       ifelse(credit_risk$property_magnitude == "no known property", "no property", 
                                                              credit_risk$property_magnitude))))


#Confirm change of labels
table(credit_risk$new_property_magnitude)

#Covert new personal status column to factor 
credit_risk$new_property_magnitude <- as.factor(credit_risk$new_property_magnitude)

#Confirm change
str(credit_risk$new_property_magnitude)

# ***Column 11 : Age***

#Check Data Type
str(credit_risk$age)

#Frequency of unique values
table(credit_risk$age)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$age)

# Apply the outlier function to the  column
outliers_age <- detect_outliers_iqr(credit_risk$age)

# View the summary of outliers
summary(outliers_age)

# Extract the rows with outliers
outliers_age_data <- credit_risk[outliers_age, ]

# Display the outliers
View(outliers_age_data)

# Visualize the outliers
boxplot(credit_risk$age, main = "Boxplot of Age", ylab = "Age (Years)", col = "orange", horizontal = TRUE)

# Highlight outliers
outlier_values_age <- credit_risk$age[outliers_age]
points(outlier_values_age, rep(1, length(outlier_values_age)), col = "red", pch = 16)

#Convert decimal values in age column to non-decimal in a new column
credit_risk$new_age = floor(credit_risk$age)

#Check Changes
table(credit_risk$new_age)
summary(credit_risk$new_age)

# ***Column 11 : Other Payment Plans***

#Check Data Type
str(credit_risk$other_payment_plans)

#Check unique values
unique(credit_risk$other_payment_plans)

#Frequency of unique values
table(credit_risk$other_payment_plans)

#Frequency of blank values
length(which(credit_risk$other_payment_plans == ""))

#Convert new payment plan data type to character in new column
credit_risk$new_other_payment_plan = as.character(credit_risk$other_payment_plans)

#Standardize blank value with labels
credit_risk$new_other_payment_plan[which(credit_risk$other_payment_plans == "")] = "no payment plan"

#Confirm change of labels
table(credit_risk$new_other_payment_plan)

#Covert new other payment plan column to factor 
credit_risk$new_other_payment_plan = as.factor(credit_risk$new_other_payment_plan)

#Confirm change
str(credit_risk$new_other_payment_plan)

# ***Column 12 : Housing***

#Check Data Type
str(credit_risk$housing)

#Check unique values
unique(credit_risk$housing)

#Frequency of unique values
table(credit_risk$housing)

#Standardize labels in a new column
credit_risk$new_housing <- ifelse(credit_risk$housing == "own", "own",
                              ifelse(credit_risk$housing == "for free", "free",
                                     ifelse(credit_risk$housing == "rent", "rent", 
                                            credit_risk$housing)))
#Confirm change of labels
table(credit_risk$new_housing)

#Covert new personal status column to factor 
credit_risk$new_housing <- as.factor(credit_risk$new_housing)

#Confirm change
str(credit_risk$new_housing)

# ***Column 13 : Existing Credits***

#Check Data Type
str(credit_risk$existing_credits)

#Frequency of unique values
table(credit_risk$existing_credits)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$existing_credits)

# Apply the outlier function to the column
outliers_existing_credits <- detect_outliers_iqr(credit_risk$existing_credits)

# View the summary of outliers
summary(outliers_existing_credits)

# Extract the rows with outliers
outliers_existing_credits_data <- credit_risk[outliers_existing_credits, ]

# Display the outliers
View(outliers_existing_credits_data)

# Visualize the outliers 
boxplot(credit_risk$existing_credits, 
        main = "Boxplot of Existing Credits", 
        ylab = "Number of Existing Credits", 
        col = "green", 
        horizontal = TRUE)

# Highlight outliers
outlier_values_existing_credits <- credit_risk$existing_credits[outliers_existing_credits]
points(outlier_values_existing_credits, rep(1, length(outlier_values_existing_credits)), 
       col = "red", 
       pch = 16)

#Convert decimal values in age column to non-decimal in a new column
credit_risk$new_existing_credits = ceiling(credit_risk$existing_credits)

#Check Changes
table(credit_risk$new_existing_credits)
summary(credit_risk$new_existing_credits)

# ***Column 14 : Job***

#Check Data Type
str(credit_risk$job)

#Check unique values
unique(credit_risk$job)

#Frequency of unique values
table(credit_risk$job)

#Standardize labels in a new column
credit_risk$new_job <- ifelse(credit_risk$job == "skilled", "skilled",
                          ifelse(credit_risk$job == "unskilled resident", "unskilled resident",
                                 ifelse(credit_risk$job == "unemp/unskilled non res", "unemployed/unskilled non resident",
                                        ifelse(credit_risk$job == "high qualif/self emp/mgmt", "high skilled/self employed/management", 
                                               credit_risk$job))))
#Confirm change of labels
table(credit_risk$new_job)

#Covert new job column to factor 
credit_risk$new_job <- as.factor(credit_risk$new_job)

#Confirm change
str(credit_risk$new_job)

# ***Column 15 : Number of Dependents***

#Check Data Type
str(credit_risk$num_dependants)

#Frequency of unique values
table(credit_risk$num_dependants)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$num_dependants)

#Convert decimal values in age column to non-decimal in a new column
credit_risk$new_num_dependants = ceiling(credit_risk$num_dependants)

#Check Changes
table(credit_risk$new_num_dependants)
summary(credit_risk$new_num_dependants)

# Apply the outlier function to the column
outliers_num_dependants <- detect_outliers_iqr(credit_risk$new_num_dependants)

# View the summary of outliers
summary(outliers_num_dependants)

# Extract the rows with outliers
outliers_num_dependants_data <- credit_risk[outliers_num_dependants, ]

# Display the outliers
View(outliers_num_dependants_data)

# Visualize the outliers
boxplot(credit_risk$new_num_dependants, 
        main = "Boxplot of Number of Dependents", 
        ylab = "Number of Dependents", 
        col = "lightgreen", 
        horizontal = TRUE)

# Highlight outliers 
outlier_values_num_dependants <- credit_risk$new_num_dependants[outliers_num_dependants]
points(outlier_values_num_dependants, rep(1, length(outlier_values_num_dependants)), 
       col = "red", 
       pch = 16)

# ***Column 16 : Own Telephone**

#Check Data Type
str(credit_risk$own_telephone)

#Check unique values
unique(credit_risk$own_telephone)

#Frequency of unique values
table(credit_risk$own_telephone)

#Standardize labels in a new column
credit_risk$new_own_telephone <- ifelse(credit_risk$own_telephone == "none", "no",
                                    ifelse(credit_risk$own_telephone == "yes", "yes",
                                           credit_risk$own_telephone))

#Confirm change of labels
table(credit_risk$new_own_telephone)

#Covert new own telephone column to factor 
credit_risk$new_own_telephone <- as.factor(credit_risk$new_own_telephone)

#Confirm change
str(credit_risk$new_own_telephone)

# ***Column 17 : Foreign Worker**

#Check Data Type
str(credit_risk$foreign_worker)

#Check unique values
unique(credit_risk$foreign_worker)

#Frequency of unique values
table(credit_risk$foreign_worker)

# ***Column 18 : Class**

#Check Data Type
str(credit_risk$class)

#Check unique values
unique(credit_risk$class)

#Frequency of unique values
table(credit_risk$class)

# Create a binary column for class
credit_risk$class_binary <- ifelse(credit_risk$class == "good", 1, 0)

#Check column
table(credit_risk$class_binary)
summary(credit_risk$class_binary)


# ***Column 18 : Income**

#Create column called income
credit_risk$income <- (credit_risk$credit_amount*100) / (credit_risk$new_duration*credit_risk$installment_commitment)

#Check Data Type
str(credit_risk$income)

#Frequency of unique values
table(credit_risk$income)

#Summary of Duration Column (Numerical Column)
summary(credit_risk$income)

# Apply the outlier function to the column
outliers_income <- detect_outliers_iqr(credit_risk$income)

# View the summary of outliers
summary(outliers_income)

# Extract the rows with outliers
outliers_income_data <- credit_risk[outliers_income, ]

# Display the outliers
View(outliers_income_data)

# Visualize the outliers using a boxplot
boxplot(credit_risk$income, main = "Boxplot of Income", ylab = "Income", col = "gold", horizontal = TRUE)

# Highlight outliers in the boxplot
outlier_values_income <- credit_risk$income[outliers_income]
points(outlier_values_income, rep(1, length(outlier_values_income)), 
       col = "red", 
       pch = 16)

# ============================
# ***Group EDA/Explorations***
# ============================

# ***Correlation between Numerical Variables***

# Load necessary libraries
library(ggplot2)
library(reshape2)
library(corrplot)

# Select only numeric columns from the cleaned dataset
numeric_columns <- credit_risk[, sapply(credit_risk, is.numeric)]

# **Correlation Matrix**

# Compute the correlation matrix
numeric_columns <- credit_risk[, sapply(credit_risk, is.numeric)]
correlation_matrix <- cor(numeric_columns, use = "complete.obs")

# Visualize the correlation matrix
png("correlation_Map.jpeg", width = 1500, height = 1300, res = 150)
corrplot(correlation_matrix, method = "number", type = "upper",
         tl.col = "black", tl.srt = 45, # Rotate and color labels
         number.cex = 0.8,              # Adjust size of numbers
         col = colorRampPalette(c("blue", "white", "red"))(200), 
         main = "Correlation Heatmap")
dev.off()

# **Heat Map**

# Reshape the correlation matrix for ggplot2
correlation_data <- melt(correlation_matrix)

# Create a heatmap using ggplot2
heatmap_plot <- ggplot(data = correlation_data, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +  # Add gridlines
  geom_text(aes(label = sprintf("%.2f", value)), size = 3) +  # Add numbers
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 8),  # Adjust x-axis labels
        axis.text.y = element_text(size = 8),  # Adjust y-axis labels
        plot.title = element_text(size = 14)) +  # Title size
  labs(title = "Heatmap of Correlations for Numerical Variables", 
       x = "Variables", y = "Variables")

# Save the plot as an image
ggsave("heatmap.jpeg", plot = heatmap_plot, width = 10, height = 8, dpi = 300)

























# ============================
# ***Analysis : Ajneya***
# ============================


# ***Variable 1 : Age***

#Analysis 1


#Summary for Age column
summary(credit_risk$new_age)

#Standard deviation for Age column
sd(credit_risk$new_age)

#Inter quartile range for age columns
IQR(credit_risk$new_age)

#Percentiles for Age column
quantile(credit_risk$age, probs = seq(0, 1, 0.1), na.rm = TRUE) 

library(moments)
# Skewness for Age column
skewness(credit_risk$age, na.rm = TRUE)

#Kurtosis for Age column
kurtosis(credit_risk$age, na.rm = TRUE)  

#Distribution of Age column

#Histogram for Age column
hist(credit_risk$age, 
     breaks = 20, 
     col = "lightblue", 
     main = "Age Distribution", 
     xlab = "Age", 
     border = "black")

#Density Plot for Age column
plot(density(credit_risk$age, na.rm = TRUE), 
     col = "red", 
     lwd = 2, 
     main = "Density Plot of Age", 
     xlab = "Age")

#Histogram And Density plot combined for age column
hist(credit_risk$age, 
     breaks = 20, 
     col = "lightblue", 
     main = "Age Distribution with Density Curve", 
     xlab = "Age", 
     freq = FALSE)
lines(density(credit_risk$age, na.rm = TRUE), 
      col = "red", 
      lwd = 2)

#Histogram with Quantiles
hist(credit_risk$age, 
     breaks = 20, 
     col = "lightgreen", 
     main = "Age Distribution with Quantiles", 
     xlab = "Age")
abline(v = quantile(credit_risk$age, probs = c(0.25, 0.5, 0.75), na.rm = TRUE), 
       col = c("red", "blue", "purple"), 
       lty = 2, lwd = 2)
legend("topright", legend = c("25th Percentile", "Median", "75th Percentile"), 
       col = c("red", "blue", "purple"), lty = 2, cex = 0.8)

#Outliers for Age column

#Method 1 : IQR Method (Statistical)
Q1 <- quantile(credit_risk$new_age, 0.25, na.rm = TRUE)
Q3 <- quantile(credit_risk$new_age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

outliers <- credit_risk$new_age[credit_risk$new_age < lower_bound | credit_risk$new_age > upper_bound]

print(outliers)

summary(outliers)

#Method 2 : Isolation Forest (Statistical)
library(solitude)

# Prepare the data
iso_forest <- isolationForest$new()

# Fit the model
iso_forest$fit(credit_risk[, "new_age", drop = FALSE])

# Predict anomaly scores
anomaly_scores <- iso_forest$predict(credit_risk[, "new_age", drop = FALSE])

# View anomaly scores
head(anomaly_scores)
summary(anomaly_scores$anomaly_score)

# Identify potential outliers (e.g., scores > 0.7 are anomalies)
outliers <- credit_risk$new_age[anomaly_scores$anomaly_score > 0.7]

cat("Outliers Detected:\n")
print(outliers)
summary(outliers)

#Method 3 : Boxplot (Visual)
boxplot(credit_risk$new_age, 
        main = "Boxplot of Age", 
        ylab = "Age", 
        col = "red",
        border = "black")

outlier_values <- boxplot.stats(credit_risk$new_age)$out
points(rep(1, length(outlier_values)), outlier_values, col = "red", pch = 16)



#Analysis 2

# Summary statistics for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, summary)

# Standard deviation for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, function(x) sd(x, na.rm = TRUE))

# Interquartile range for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, function(x) IQR(x, na.rm = TRUE))

# Percentiles for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, function(x) quantile(x, probs = seq(0, 1, 0.1), na.rm = TRUE))

# Load necessary library for skewness and kurtosis
library(moments)

# Skewness for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, function(x) skewness(x, na.rm = TRUE))

# Kurtosis for Age grouped by Class
aggregate(new_age ~ class, data = credit_risk, function(x) kurtosis(x, na.rm = TRUE))

#Distribution of Age column with Class

# Stacked Histogram of Age by Class
ggplot(credit_risk, aes(x = new_age, fill = class)) +
  geom_bar(position = "stack", color = "black") +  # Ensure bars are stacked
  labs(title = "Stacked Histogram of Age by Class", 
       x = "Age", 
       y = "Count") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +  # Customize colors
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Class"))

# Density Plot of Age by Class
ggplot(credit_risk, aes(x = new_age, color = class)) +
  geom_density(size = 1) +
  labs(title = "Density Plot of Age Distribution by Class", 
       x = "Age", 
       y = "Density") +
  scale_color_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(color = guide_legend(title = "Class"))

# Beeswarm Plot
library(ggbeeswarm)
ggplot(credit_risk, aes(x = class, y = new_age, color = class)) +
  geom_beeswarm(size = 1.5) +
  labs(title = "Beeswarm Plot of Age Distribution by Class", 
       x = "Class", 
       y = "Age") +
  scale_color_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +
  theme_minimal() +
  theme(legend.position = "none")


#Outliers for Age column with Class

#Method 1 : IQR Method (Statistical)
Q1 <- quantile(credit_risk$new_age, 0.25, na.rm = TRUE)
Q3 <- quantile(credit_risk$new_age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify outliers
credit_risk$outlier_flag <- ifelse(credit_risk$new_age < lower_bound | credit_risk$new_age > upper_bound, "Outlier", "Normal")

# Group by Class and Summarize Outliers
outlier_summary <- table(credit_risk$class, credit_risk$outlier_flag)

# View Outlier Summary
print(outlier_summary)


#Method 2 : Isolation Forest (Statistical)
library(solitude)

# Prepare Isolation Forest
iso_forest <- isolationForest$new()

# Fit the Isolation Forest Model to the Entire Data
iso_forest$fit(credit_risk[, "new_age", drop = FALSE])

# Predict Anomaly Scores
anomaly_scores <- iso_forest$predict(credit_risk[, "new_age", drop = FALSE])

# Add Anomaly Scores and Flags to the Dataset
credit_risk$anomaly_score <- anomaly_scores$anomaly_score
credit_risk$outlier_flag <- ifelse(credit_risk$anomaly_score > 0.7, "Outlier", "Normal")

# Summarize Outliers by Class
outlier_summary <- table(credit_risk$class, credit_risk$outlier_flag)

# View Outlier Summary
print(outlier_summary)

#Method 3 : Boxplot (Visual)
ggplot(credit_risk, aes(x = class, y = new_age, fill = class)) +
  geom_boxplot(outlier.color = "red", outlier.shape = 16) +
  labs(title = "Boxplot of Age by Class",
       x = "Class",
       y = "Age",
       fill = "Class") +
  scale_fill_manual(values = c("bad" = "red", "good" = "blue")) +
  theme_minimal()

#Analysis 3 : Hypothesis Testing 

#Create New Column
credit_risk$age_group_55 <- ifelse(credit_risk$new_age > 55, "Over 55", 
                                   ifelse(credit_risk$new_age < 30, "Under 30", "Other"))
credit_risk$age_group_55 <- factor(credit_risk$age_group_55, levels = c("Under 30", "Over 55", "Other"))
table(credit_risk$age_group_55)
filtered_data <- subset(credit_risk, age_group_55 %in% c("Under 30", "Over 55"))

#Welch's t-test

# Split the binary class data by age group
under_30 <- filtered_data$class_binary[filtered_data$age_group_55 == "Under 30"]
over_55 <- filtered_data$class_binary[filtered_data$age_group_55 == "Over 55"]

# Perform Welch's t-test
t_test_result <- t.test(under_30, over_55, var.equal = FALSE)

# Display the results
print(t_test_result)

#Chi Square Test

#Filter the dataset
filtered_data <- subset(credit_risk, new_age > 55 | new_age < 30)

#Create new column
filtered_data$age_group <- ifelse(filtered_data$new_age > 55, "Over 55", "Under 30")

#Contingency table
age_class_table <- table(filtered_data$age_group, filtered_data$class)
age_class_table


chi_square_test <- chisq.test(age_class_table)
print(chi_square_test)

print(chi_square_test$expected)

print(chi_square_test$residuals)

#Cramer's V Test
library(vcd)
cramers_v <- assocstats(age_class_table)$cramer

print(cramers_v)

#Z Proportion Test

# Counts of "Good" classifications for each age group
successes <- c(sum(filtered_data$class_binary[filtered_data$age_group_55 == "Under 30"]), sum(filtered_data$class_binary[filtered_data$age_group_55 == "Over 55"]))

# Total counts for each age group
totals <- c(sum(filtered_data$age_group_55 == "Under 30"), sum(filtered_data$age_group_55 == "Over 55"))

# Perform proportion Z-Test
prop_test_result <- prop.test(successes, totals)

# Display results
print(prop_test_result)

filtered_data$class_binary <- ifelse(filtered_data$class == "good", 1, 0)

#logistic regression
logistic_model <- glm(class_binary ~ age_group, data = filtered_data, family = "binomial")
summary(logistic_model)

odds_ratios <- exp(coef(logistic_model))  
conf_intervals <- exp(confint(logistic_model))  

cat("Odds Ratios:\n")
print(odds_ratios)

cat("\nConfidence Intervals:\n")
print(conf_intervals)

#Confusion Matrix
predicted_probabilities <- predict(logistic_model, filtered_data, type = "response")

#Set threshold
threshold <- 0.5

# Convert probabilities
predicted_classes <- ifelse(predicted_probabilities > threshold, 1, 0)

actual_classes <- filtered_data$class_binary
confusion_matrix <- table(Predicted = predicted_classes, Actual = actual_classes)

cat("Confusion Matrix:\n")
print(confusion_matrix)

true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[2, 1]
false_negatives <- confusion_matrix[1, 2]

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Display metrics
cat("\nPerformance Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

#Decision Trees
library(rpart)
library(rpart.plot)

filtered_data$class_binary <- ifelse(filtered_data$class == "good", 1, 0)
filtered_data$age_group <- factor(filtered_data$age_group, levels = c("Under 30", "Over 55"))


decision_tree <- rpart(class_binary ~ age_group, 
                       data = filtered_data, 
                       method = "class", 
                       parms = list(split = "gini"), 
                       control = rpart.control(minsplit = 20, cp = 0.01))

#Plot decision tree
rpart.plot(
  decision_tree,
  type = 3,              
  extra = 104,           
  under = TRUE,          
  faclen = 0,            
  cex = 0.8,             
  main = "Decision Tree for Age Group and Credit Risk",
  box.palette = "RdBu",  
  shadow.col = "gray",   
  split.cex = 1.0        
)

#Confusion Matrix for Decision tree
predicted_classes <- predict(decision_tree, filtered_data, type = "class")

confusion_matrix <- table(Predicted = predicted_classes, Actual = filtered_data$class_binary)

true_positives <- confusion_matrix[2, 2]
true_negatives <- confusion_matrix[1, 1]
false_positives <- confusion_matrix[2, 1]
false_negatives <- confusion_matrix[1, 2]

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- true_positives / (true_positives + false_positives)
recall <- true_positives / (true_positives + false_negatives)
f1_score <- 2 * (precision * recall) / (precision + recall)

cat("\nPerformance Metrics:\n")
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")


# ***Variable 2 : Saving Status***

#Analysis 4

#More Processing and Cleaning

# Modify the labels
credit_risk$new_savings_status <- ifelse(credit_risk$savings_status == ">=1000", "Good Savings",
                                         ifelse(credit_risk$savings_status == "<100", "Poor Savings",
                                                ifelse(credit_risk$savings_status == "100<=X<500", "Average Savings", 
                                                       ifelse(credit_risk$savings_status == "500<=X<10000", "Good Savings", 
                                                              ifelse(credit_risk$savings_status == "no known savings", "No Savings", "No Savings")))))

# Confirm changes
table(credit_risk$new_savings_status)

#EDA

# Proportion of each category in savings status
prop.table(table(credit_risk$new_savings_status))

#Pie Chart For Savings Status
savings_status_counts <- table(credit_risk$new_savings_status)

# Custom colors for the pie chart
pie_colors <- c("#4CAF50", "#FF9800", "#2196F3", "#E91E63")

pie(savings_status_counts, 
    labels = names(savings_status_counts), 
    col = pie_colors, 
    main = "Pie Chart of Savings Status")


#Analysis 5

#Savings Status grouped by Class
aggregate(new_savings_status ~ class, data = credit_risk, FUN = table)

#Bar plot of savings status with class
ggplot(credit_risk, aes(x = new_savings_status, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Savings Status by Class", 
       x = "Savings Status", 
       y = "Count", 
       fill = "Credit Class") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) + 
  theme_minimal()

#Save proportions to variable 
savings_status_by_class <- prop.table(table(credit_risk$new_savings_status, credit_risk$class_binary), 1)

#Convert proportions to a data frame
savings_status_class_df <- as.data.frame(savings_status_by_class)

#Rename the columns
colnames(savings_status_class_df) <- c("Savings_Status", "Class", "Proportion")

#Bar chart of proportions
library(ggplot2)
ggplot(savings_status_class_df, aes(x = Savings_Status, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position bars side by side
  labs(title = "Proportion of Savings Status by Credit Class", x = "Savings Status", y = "Proportion") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +  # Customize colors for class
  theme_minimal()

#Analysis 6 : Hypothesis Testing

# Create a new column for savings balance category
credit_risk$savings_category <- ifelse(credit_risk$new_savings_status %in% c("Poor Savings", "No Savings"), 
                                       "Below $1000", 
                                       "Above $1000")

# Convert the new column to factor
credit_risk$savings_category <- factor(credit_risk$savings_category, levels = c("Below $1000", "Above $1000"))

#Confirm Changes
table(credit_risk$savings_category)

#Distribution for Below and Above $1000

# Calculate counts for savings categories
library(plotrix)
savings_counts <- table(credit_risk$savings_category)

#Colors for the pie chart
pie_colors <- c("springgreen", "orange")

#Pie chart
pie3D(
  savings_counts,
  labels = paste0(names(savings_counts), "\n(", round(100 * prop.table(savings_counts), 1), "%)"),
  col = pie_colors,
  main = "3D Distribution of Saving's Categories",
  explode = 0.1, 
  labelcex = 1.5,
)

#Bar Chart for Below and Above $1000 with class
ggplot(credit_risk, aes(x = savings_category, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Distribution of Savings Categories by Class", 
       x = "Savings Category", 
       y = "Count", 
       fill = "Credit Class") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +  # Customize colors
  theme_minimal()

#Proportions for labels
proportions_data <- credit_risk %>%
  group_by(savings_category, class) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(savings_category) %>%
  mutate(proportion = count / sum(count) * 100)

#Proportional stacked bar chart
ggplot(proportions_data, aes(x = savings_category, y = proportion, fill = class)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(proportion, 2), "%")), 
            position = position_stack(vjust = 0.5), size = 5) + 
  labs(title = "Proportional Distribution of Savings Categories by Class", 
       x = "Savings Category", 
       y = "Percentage", 
       fill = "Credit Class") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) + 
  theme_minimal()

#Chi-Square Test
contingency_table <- table(credit_risk$savings_category, credit_risk$class)
chisq_test_result <- chisq.test(contingency_table)
print(chisq_test_result)


# ***Variable 3 : Own Telephone***


#Analysis 7 

# Summary of the column
summary(credit_risk$new_own_telephone)

# Proportion of the column
prop.table(table(credit_risk$new_own_telephone)) * 100

telephone_counts <- table(credit_risk$new_own_telephone)
pie_colors <- c("yellow", "dodgerblue")

#3D pie chart
pie3D(
  telephone_counts,
  labels = paste0(names(telephone_counts), "\n(", round(100 * prop.table(telephone_counts), 1), "%)"), 
  explode = 0.1,            
  col = pie_colors,         
  main = "Own Telephone Distribution", 
  labelcex = 1.2,           
  radius = 1,            
  theta = 0.8               
)


#Analysis 8

#Contingency table 
telephone_summary <- table(credit_risk$class, credit_risk$new_own_telephone)
print(telephone_summary)

#Proportion Table
prop_table <- prop.table(telephone_summary, margin = 1) 
print(round(prop_table * 100, 2))


#Bar Plot
telephone_summary <- as.data.frame(table(credit_risk$new_own_telephone, credit_risk$class))
colnames(telephone_summary) <- c("Telephone", "Class", "Count")

ggplot(telephone_summary, aes(x = Telephone, y = Count, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +
  scale_x_discrete(labels = c("no" = "No", "yes" = "Yes")) +  # Corrected axis labels
  labs(title = "Bar Graph of Own Telephone by Credit Class",
       x = "Own Telephone",
       y = "Count",
       fill = "Credit Class") +
  theme_minimal() +
  theme(legend.position = "top")


# Convert to a data frame for ggplot
stacked_data <- as.data.frame(prop_table)
colnames(stacked_data) <- c("Telephone", "Class", "Proportion")

# Add percentages for labels
stacked_data$Percentage <- round(stacked_data$Proportion * 100, 1)

#Stacked Bar Chart with percentages
library(ggplot2)
ggplot(stacked_data, aes(x = Telephone, y = Proportion, fill = Class)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_fill(vjust = 0.5), size = 5) +
  scale_fill_manual(values = c("red", "blue"), labels = c("Bad", "Good")) +
  labs(title = "Stacked Bar Chart for Own Telephone by Class",
       x = "Own Telephone (Yes/No)",
       y = "Proportion") +
  theme_minimal() +
  theme(legend.position = "top") +
  guides(fill = guide_legend(title = "Credit Class"))

#Analysis 9

#Chi-Square Test
telephone_summary <- table(credit_risk$class, credit_risk$new_own_telephone)

#Perform Chi-Square Test
chi_square_test <- chisq.test(telephone_summary)
print(chi_square_test)

#Cramer's V Test
library(rcompanion)
cramers_v <- cramerV(telephone_summary, bias.correct = TRUE)
print(cramers_v)
summary(cramers_v)

# Logistic Regression
logistic_model <- glm(class_binary ~ new_own_telephone, data = credit_risk, family = "binomial")
summary(logistic_model)

#Predicted Probability
#Create new data
prediction_data <- data.frame(new_own_telephone = c("no", "yes"))

#Predict probabilities
prediction_data$Probability <- predict(logistic_model, newdata = prediction_data, type = "response")

#Bar Plot
library(ggplot2)
ggplot(prediction_data, aes(x = new_own_telephone, y = Probability, fill = new_own_telephone)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("red", "blue")) +
  labs(title = "Predictive Probability of Good Credit Risk by Telephone Ownership",
       x = "Own Telephone",
       y = "Predicted Probability") +
  theme_minimal() +
  theme(legend.position = "none")

#Confusion Matrix
#Predict probabilities
predicted_probs <- predict(logistic_model, type = "response")

#Convert probabilities to predicted classes
predicted_classes <- ifelse(predicted_probs > 0.5, 1, 0)

library(caret)
conf_matrix <- confusionMatrix(factor(predicted_classes, levels = c(0, 1)),
                               factor(credit_risk$class_binary, levels = c(0, 1)),
                               positive = "1")

print(conf_matrix)


#Random Forests
library(randomForest)
library(caret)

#Confirm data type is factor
credit_risk$class_binary <- as.factor(credit_risk$class_binary)

#Split the data into training and testing sets 
set.seed(123)
train_index <- createDataPartition(credit_risk$class_binary, p = 0.7, list = FALSE)
train_data <- credit_risk[train_index, ]
test_data <- credit_risk[-train_index, ]

#Fit the Random Forest model
set.seed(123) 
rf_model <- randomForest(class_binary ~ new_own_telephone, data = train_data, ntree = 500, mtry = 1, importance = TRUE)

print(rf_model)

#Evaluate the test set
predictions <- predict(rf_model, test_data)

#Accuracy of Test Set
accuracy <- sum(predictions == test_data$class_binary) / nrow(test_data)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")


library(rattle)
library(rpart.plot)

tree <- rpart(class_binary ~ new_own_telephone, data = train_data, method = "class")

rpart.plot(tree, 
           type = 4,              
           extra = 106,           
           under = TRUE,          
           tweak = 1.3,          
           box.palette = "RdYlGn",
           main = "Visualization of Random Forest for Telephone Ownership",
           shadow.col = "gray")   

# ***Complex Hypothesis***

#Analysis 10 


#Summary Statistics  of Age and Own_Telephone
filtered_age_group <- subset(credit_risk, age_group_55 %in% c("Over 55", "Under 30"))
summary_stats <- table(filtered_age_group$age_group_55, filtered_age_group$new_own_telephone)
print(summary_stats)

prop_table <- prop.table(summary_stats, margin = 1)
print(round(prop_table * 100, 2))



#Distribution of Age and Own_Telephone

#Bar plot
ggplot(filtered_age_group, aes(x = age_group_55, fill = new_own_telephone)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("yes" = "darkorchid", "no" = "goldenrod1")) +
  labs(
    title = "Distribution of Telephone Ownership by Age Group",
    x = "Age Group",
    y = "Count",
    fill = "Telephone Ownership"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


#Proportions for labels
pie_data <- filtered_age_group %>%
  group_by(age_group_55, new_own_telephone) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(age_group_55) %>%
  mutate(proportion = count / sum(count),
         percentage = paste0(round(proportion * 100, 2), "%"))

#Pie Chart
ggplot(pie_data, aes(x = "", y = proportion, fill = new_own_telephone)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = percentage),
    position = position_stack(vjust = 0.5),
    size = 5,
    color = "black"
  ) +
  scale_fill_manual(values = c("yes" = "darkorchid", "no" = "goldenrod1")) +
  labs(
    title = "Pie Chart of Telephone Ownership by Age Group",
    x = NULL,
    y = NULL,
    fill = "Telephone Ownership"
  ) +
  facet_wrap(~age_group_55) + 
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 16)
  )

#Analysis 11

filtered_data$age_group_55 <- factor(filtered_data$age_group_55, levels = c("Over 55", "Under 30"))

#Contingency table for age group, telephone ownership, and credit classification
summary_stats <- table(filtered_data$age_group_55, filtered_data$new_own_telephone, filtered_data$class)
print(summary_stats)

prop_table <- prop.table(summary_stats, margin = 1)
print(round(prop_table * 100, 2))


#Proportions table to data frame
prop_table_data <- as.data.frame(prop_table)

#Rename columns
colnames(prop_table_data) <- c("Age Group", "Telephone Ownership", "Class", "Proportion")

prop_table_data$Percentage <- paste0(round(prop_table_data$Proportion * 100, 2), "%")


#Heatmap
ggplot(prop_table_data, aes(x = `Telephone Ownership`, y = `Age Group`, fill = Proportion)) +
  geom_tile() +
  geom_text(aes(label = Percentage), color = "gray0", size = 5.5) +
  scale_fill_gradient(low = "indianred1", high = "red4", name = "Proportion (%)") +
  facet_wrap(~ Class) +
  labs(
    title = "Heatmap of Proportions by Age Group, Telephone Ownership, and Class",
    x = "Telephone Ownership",
    y = "Age Group"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "gray70"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )


#Analysis 12 
#Drop unused levels
filtered_data$age_group_55 <- droplevels(filtered_data$age_group_55)

#Contingency table
contingency_table <- table(
  filtered_data$age_group_55,
  filtered_data$new_own_telephone,
  filtered_data$class_binary
)
print(ftable(contingency_table))

#Chi-Square Test
chi_square_test <- chisq.test(ftable(contingency_table))
print(chi_square_test)

#Logistic Regression

filtered_data <- subset(credit_risk, age_group_55 %in% c("Over 55", "Under 30"))

#Combined variable for reference group
filtered_data$combined_group <- interaction(
  filtered_data$age_group_55, 
  filtered_data$new_own_telephone, 
  sep = "."
)

#Convert Data Type
filtered_data$combined_group <- as.factor(filtered_data$combined_group)

print(table(filtered_data$combined_group))


#Set reference group
filtered_data$combined_group <- relevel(filtered_data$combined_group, ref = "Over 55.yes")

logistic_model <- glm(class_binary ~ combined_group,data = filtered_data,family = "binomial")

summary(logistic_model)

#Odds ratios 
exp(coef(logistic_model))

#Confidence intervals
exp(confint(logistic_model))

#Filter the data
filtered_data <- droplevels(filtered_data)

#New prediction data frame
prediction_data <- data.frame(
  combined_group = levels(filtered_data$combined_group)
)

#Predict probabilities
prediction_data$predicted_prob <- predict(logistic_model, newdata = prediction_data, type = "response")

print(prediction_data)

#Bar Chart
library(ggplot2)
ggplot(prediction_data, aes(x = combined_group, y = predicted_prob, fill = combined_group)) +
  geom_bar(stat = "identity", width = 0.6) +
  scale_fill_manual(values = c("blue", "red", "green", "orange")) +
  labs(
    title = "Predicted Probability of Being a Good Credit Risk",
    x = "Group (Age Group and Telephone Ownership)",
    y = "Predicted Probability",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5)
  )

#Confusion Matrix

# Convert probabilities to binary predictions
filtered_data$predicted_class <- ifelse(filtered_data$predicted_prob > 0.5, 1, 0)

confusion_matrix <- table(Actual = filtered_data$class_binary,Predicted = filtered_data$predicted_class)

print(confusion_matrix)

#Accuracy
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Accuracy:", round(accuracy * 100, 2), "%\n")





