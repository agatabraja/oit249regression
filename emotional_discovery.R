# Load required libraries
library(readxl)
library(GGally)
library(ggplot2)

# Load the dataset
data <- read_excel("emotional_support.xlsx")

# Convert all columns to numeric, treating non-numeric values as NA
data_numeric <- data.frame(lapply(data, function(x) as.numeric(as.character(x))))

# Check for missing values in each column
missing_proportion <- colMeans(is.na(data_numeric))
print("Proportion of missing values in each column:")
print(missing_proportion)

# Remove columns with more than 50% missing values
data_numeric <- data_numeric[, missing_proportion < 0.5]

# Check if there are still any columns left
if (ncol(data_numeric) > 1) {
  # Remove rows with any remaining NA values
  data_numeric <- na.omit(data_numeric)
  
  # Verify if there are enough rows left after NA removal
  if (nrow(data_numeric) > 1) {
    # Generate the pairwise correlation plot
    ggpairs(data_numeric) +
      ggtitle("Pairwise Correlation Plot for Numeric Variables in Emotional Support Dataset")
  } else {
    print("Not enough data left after row-wise NA removal.")
  }
} else {
  print("No numeric columns left after column-wise NA filtering.")
}
