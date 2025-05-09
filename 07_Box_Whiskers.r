# Load necessary libraries
library(ggplot2)
library(tidyr)

# Load the data
df <- read.csv("./data/df_dist_before_after.csv")

# Convert dist_from_trib to a categorical variable (Before/After 0)
df$Location <- ifelse(df$dist_from_trib < 0, "Before", "After")

# Define the variables to plot
variables <- c("D50", "D65", "D84", "PS", "SSI")

# Transform data into long format for plotting
df_long <- pivot_longer(df,
  cols = all_of(variables), names_to = "variable",
  values_to = "value"
)

# Ensure Tributary is a factor
df_long$Tributary <- as.factor(df_long$Tributary)

df_long$Location <- factor(df_long$Location, levels = c("Before", "After"))
# Generate a single plot with 7 columns (one for each Tributary) and
# rows (one for each variable)
Elias_Scatter <- ggplot(df_long, aes(x = Location, y = value, fill = Location)) + # nolint
  geom_boxplot() +
  geom_jitter(width = 0.2, alpha = 0.5) +
  # Adds jittered points for better visibility
  facet_grid(variable ~ Tributary, scales = "free_y") +
  theme_bw() +
  theme(
    text = element_text(size = 14, color = "black"),
    axis.text = element_text(size = 12, color = "black"), # Adjusted text size
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom" # Keeping the legend for clarity
  ) +
  labs(
    x = "Location Relative to Tributary",
    y = "Value",
  )
ggsave(Elias_Scatter,
  filename = "./figures/Plot_8.png", width = 12,
  height = 10, dpi = 1000
)
