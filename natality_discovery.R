# Load required libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(GGally)

# Read the data directly using read.delim
data <- read.delim("birth_data_1-25_2021_MM-Yes.txt", sep="\t", header=TRUE, 
                   na.strings=c("", "NA", "---"))

# Select numeric columns of interest
numeric_cols <- c("Births", "Average.Age.of.Mother..years.", 
                  "Average.Birth.Weight..grams.", "Average.Number.of.Prenatal.Visits")

# Clean the data
cleaned_data <- data %>%
  select(all_of(numeric_cols)) %>%
  # Remove any non-numeric characters and convert to numeric
  mutate(across(everything(), ~as.numeric(gsub("[^0-9\\.-]", "", .)))) %>%
  # Remove rows with NA values
  na.omit()

# Print summary of cleaned data
print("Summary of cleaned data:")
summary(cleaned_data)

# Calculate correlation matrix
cor_matrix <- cor(cleaned_data, use = "complete.obs")

# Print correlation matrix
print("\nCorrelation matrix:")
print(round(cor_matrix, 3))

# Find pairs with high correlation (absolute value > 0.3)
high_corr_pairs <- which(abs(cor_matrix) > 0.3 & abs(cor_matrix) < 1, arr.ind = TRUE)

# Create summary of high correlations
high_corr_summary <- data.frame(
  Variable1 = rownames(cor_matrix)[high_corr_pairs[, 1]],
  Variable2 = colnames(cor_matrix)[high_corr_pairs[, 2]],
  Correlation = cor_matrix[high_corr_pairs]
)

# Remove duplicate pairs
high_corr_summary <- high_corr_summary[!duplicated(t(apply(high_corr_summary, 1, sort))), ]

# Print high correlation pairs
print("\nPairs with correlation > 0.3:")
print(high_corr_summary)

# Create correlation heatmap
ggplot(data = reshape2::melt(cor_matrix), 
       aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Heatmap",
       x = "", y = "", fill = "Correlation") +
  coord_fixed()

# Create scatterplot matrix
ggpairs(cleaned_data, 
        title = "Pairwise Correlation Plot for Birth Data Variables") +
  theme_minimal()