# Load necessary libraries
library(tidyverse)
library(writexl)

# Load the data
df <- read.csv("./data/df_dist_before_after.csv")

# Convert dist_from_trib to a categorical variable (Before/After 0)
df$Location <- ifelse(df$dist_from_trib < 0, "Before 0", "After 0")

# Define the variables to test
variables <- c("D50", "D65", "D84", "PS", "SSI")

# Initialize an empty dataframe to store results
final_results <- data.frame(Variable = rep(variables, each = 2), Statistic = rep(c("Test Type", "P-Value"), length(variables)))

# Perform statistical tests for each tributary
trib <- "T1"
var <- "D50"
for (trib in unique(df$Tributary)) {
  test_types <- c()
  p_values <- c()
  
  for (var in variables) {
    df_subset <- dplyr::filter(df, Tributary == trib) %>% select(Location, all_of(var))
    
    # Check for normality using Shapiro-Wilk test
    shapiro_before <- shapiro.test(df_subset[[var]][df_subset$Location == "Before 0"])
    shapiro_after <- shapiro.test(df_subset[[var]][df_subset$Location == "After 0"])
    
    normal_before <- shapiro_before$p.value > 0.05
    normal_after <- shapiro_after$p.value > 0.05
    
    df_subset$Location <- factor(df_subset$Location, levels = c("Before 0", "After 0"))
    
    # Perform appropriate test
    if (normal_before & normal_after) {
      test_result <- t.test(df_subset[[var]] ~ df_subset$Location)
      test_type <- "T-test"
    } else {
      test_result <- wilcox.test(df_subset[[var]] ~ df_subset$Location)
      test_type <- "Mann-Whitney U test"
    }
    
    # Store results
    test_types <- c(test_types, test_type)
    p_values <- c(p_values, round(test_result$p.value, 5))
  }
  final_results[[trib]] <- c(test_types, p_values)
}

# Export results to CSV and Excel
write.csv(final_results, "./data/statistical_results.csv", row.names = FALSE)
write_xlsx(final_results, "./data/statistical_results.xlsx")

# Display results
print(final_results)
