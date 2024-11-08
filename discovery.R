# Load required libraries
library(readxl)
library(ggplot2)
library(GGally)

# Load the dataset from the Excel file
data <- read_excel("dei_dataset.xlsx")

# Check column names to identify any unnamed or unexpected columns
print("Column names in the dataset:")
print(colnames(data))

# Remove columns with blank names or those that start with "..."
data <- data[, !(colnames(data) == "" | grepl("^\\.+", colnames(data)))]

# Display the cleaned column names
print("Column names after removing unnamed or unexpected columns:")
print(colnames(data))

# Convert all columns to numeric, handling non-numeric values as NA
data_numeric <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

# Check the proportion of NA values in each column
na_proportion <- colMeans(is.na(data_numeric))
print("Proportion of NA values in each column:")
print(na_proportion)

# Remove columns with more than 50% NA values
data_numeric <- data_numeric[, na_proportion < 0.5]

# Remove rows with any remaining NA values
data_numeric <- na.omit(data_numeric)

# Verify the number of rows remaining
print(paste("Number of rows after NA removal:", nrow(data_numeric)))

# If there are enough rows left, calculate the correlation matrix and plot pairwise correlations
if (nrow(data_numeric) > 1) {
  # Calculate the correlation matrix
  cor_matrix <- cor(data_numeric, use = "complete.obs")
  
  # Display the correlation matrix
  print("Correlation matrix of cleaned data:")
  print(cor_matrix)
  
  # Find pairs of variables with high absolute correlation (e.g., > 0.7 or < -0.7)
  high_corr_pairs <- which(abs(cor_matrix) > 0.7 & abs(cor_matrix) < 1, arr.ind = TRUE)
  
  # Create a data frame to summarize pairs with high correlations
  high_corr_summary <- data.frame(
    Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
    Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
    Correlation = cor_matrix[high_corr_pairs]
  )
  
  # Remove duplicate pairs (i.e., A-B and B-A)
  high_corr_summary <- high_corr_summary[!duplicated(t(apply(high_corr_summary, 1, sort))), ]
  
  # Display pairs with high correlation for potential regression
  print("Pairs of variables with high correlation (potential candidates for regression):")
  print(high_corr_summary)
  
  # Pairwise correlation plot for visual inspection
  ggpairs(data_numeric) +
    ggtitle("Pairwise Correlation Plot for Numeric Variables")
} else {
  print("Not enough data left after NA removal for correlation analysis.")
}
