# ----------------------------
# Load necessary libraries
# ----------------------------
library(ggplot2)
library(dplyr)
library(corrplot)  # for correlation plot

# ----------------------------
# Load dataset
# ----------------------------
# Use double backslashes \\ or forward slashes /
df <- read.csv("C:/Users/veneshaa vanan/Downloads/heart.csv")

# ----------------------------
# 1. Count missing values per column
# ----------------------------
missing_values <- sapply(df, function(x) sum(is.na(x)))
cat("Missing values per column:\n")
print(missing_values)

# ----------------------------
# 2. Compute correlations and show top 3
# ----------------------------
# Select only numeric columns
numeric_cols <- df %>% select(where(is.numeric))
cor_matrix <- cor(numeric_cols, use = "complete.obs")

# Print correlation matrix
print(cor_matrix)

# Find top 3 absolute correlations (excluding 1)
cor_values <- abs(cor_matrix[upper.tri(cor_matrix)])
top3_indices <- order(cor_values, decreasing = TRUE)[1:3]

# Get names of correlated pairs
cor_pairs <- which(upper.tri(cor_matrix), arr.ind = TRUE)
top3_pairs <- cor_pairs[top3_indices, ]
cat("\nTop 3 correlations:\n")
for(i in 1:3){
  r <- top3_pairs[i,1]
  c <- top3_pairs[i,2]
  cat(rownames(cor_matrix)[r], "-", colnames(cor_matrix)[c], 
      ":", cor_matrix[r,c], "\n")
}

# Optional: visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)

# ----------------------------
# 3. Histogram of 'age'
# ----------------------------
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  labs(title = "Age Distribution of Patients", x = "Age", y = "Frequency") +
  theme_minimal()

# ----------------------------
# 4. Basic statistics for 'age'
# ----------------------------
mean_age <- mean(df$age, na.rm = TRUE)
median_age <- median(df$age, na.rm = TRUE)
var_age <- var(df$age, na.rm = TRUE)

cat("\nMean Age:", mean_age, "\n")
cat("Median Age:", median_age, "\n")
cat("Variance of Age:", var_age, "\n")

# ----------------------------
# 5. Bar chart for 'sex'
# ----------------------------
ggplot(df, aes(x = factor(sex))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Count of Patients by Sex",
       x = "Sex (0 = Female, 1 = Male)",
       y = "Count") +
  theme_minimal()
