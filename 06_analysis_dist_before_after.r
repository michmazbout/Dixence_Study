library("reshape2")
library("ggplot2")
library("readxl")
# Load image data
df_img <- data.frame(read.csv("./data/df_img.csv", sep = "\t"))

##################################
# Add Distance variables
##################################
df_all <- df_img
# Load distance from tributaries variable
df_dist <- data.frame(read_excel("./data/Distance_Before_After_Trib.xlsx"))
rownames(df_dist) <- paste0(df_dist$img, "_", df_dist$Tributary)

# Match img values and add the columns from df_all to df_dist
df_dist$D50 <- df_all$D50[match(df_dist$img, df_all$img)]
df_dist$D65 <- df_all$D65[match(df_dist$img, df_all$img)]
df_dist$D84 <- df_all$D84[match(df_dist$img, df_all$img)]
df_dist$PS  <- df_all$PS[match(df_dist$img, df_all$img)]
df_dist$SSI <- df_dist$D84 / df_dist$D50

# Save results to csv for downstream analysis
write.csv(df_dist, file = "./data/df_dist_before_after.csv", row.names = TRUE, quote = FALSE)





library(ggplot2)
library(tidyr)

# Reshape the data into long format for multiple y variables
df_long <- df_dist %>%
  pivot_longer(cols = c(D50, D84, SSI, PS), names_to = "Metric", values_to = "Value")

# Create the scatter plot with fitted lines
ggplot(df_long, aes(x = dist_from_trib, y = Value, fill = Metric)) +
  geom_point(shape = 21, size = 4, color = "black", stroke = 0.5) +
  geom_smooth(method = "lm", color = "black", se = FALSE) + # Linear fit lines
  scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#c9a6ff", "#9ac1fc")) +
  facet_grid(Metric ~ Tributary, scale = "free_y") +                        # Facet grid: rows = Metric, cols = Tributary
  labs(
    title = "Scatter Plot of Metrics vs Distance from Tributary",
    x = "Distance from Tributary (m)",
    y = "Value"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )



library(ggplot2)
library(dplyr)
library(patchwork)  # For combining multiple plots

# Create an empty list to store plots
plots <- list()

# Generate plots for each tributary (T1 to T7) and store them in the list
for (trib in paste0("T", 1:7)) {
  p <- df_long %>%
    filter(Tributary == trib) %>%
    ggplot(aes(x = dist_from_trib, y = Value, fill = Metric)) +
    geom_point(shape = 21, size = 4, color = "black", stroke = 0.7, alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Metric), size = 1.2) +  # Quadratic fit
    scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#c9a6ff", "#9ac1fc")) +
    scale_color_manual(values = c("#d73027", "#4575b4", "#74add1", "#fee090")) +  
    facet_wrap(~ Metric, scales = "free_y") +  
    labs(
      title = paste("Tributary:", trib),
      x = "Distance from Tributary (m)",
      y = "Value",
      fill = "Metric",
      color = "Metric"
    ) +
    theme_classic() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  # Store the plot in the list
  plots[[trib]] <- p
}

# Combine all plots into one large figure
combined_plot <- (plots[["T1"]] | plots[["T2"]] | plots[["T3"]]) /
                 (plots[["T4"]] | plots[["T5"]] | plots[["T6"]]) /
                 plots[["T7"]]

# Display the combined plot
print(combined_plot)

# Save the combined figure
ggsave("combined_plot.png", plot = combined_plot, width = 16, height = 12, dpi = 300)







library(ggplot2)

# Create the single combined plot with separate scales for each graph
p <- ggplot(df_long, aes(x = dist_from_trib, y = Value, fill = Metric)) +
  geom_point(shape = 21, size = 4, color = "black", stroke = 0.7, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Metric), size = 1.2) +  # Quadratic fit
  scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#c9a6ff", "#9ac1fc")) +
  scale_color_manual(values = c("#d73027", "#4575b4", "#74add1", "#fee090")) +  
  facet_grid(Metric ~ Tributary, scales = "free") +  # 7 cols (T1-T7), 4 rows (Metrics)
  labs(
    title = "Comparison of Metrics Across Tributaries",
    x = "Distance from Tributary (m)",
    y = "Value",
    fill = "Metric",
    color = "Metric"
  ) +
  theme_classic() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Display the plot
print(p)

# Save the combined figure
ggsave("comparison_plot.png", plot = p, width = 16, height = 10, dpi = 300)





library(ggplot2)
library(dplyr)
library(patchwork)  # For arranging multiple plots

# Create an empty list to store plots
plots <- list()

# Generate plots for each tributary (T1 to T7) and store them in the list
for (trib in paste0("T", 1:7)) {
  p <- df_long %>%
    filter(Tributary == trib) %>%
    ggplot(aes(x = dist_from_trib, y = Value, fill = Metric)) +
    geom_point(shape = 21, size = 4, color = "black", stroke = 0.7, alpha = 0.7) +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, aes(color = Metric), size = 1.2) +  # Quadratic fit
    scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#c9a6ff", "#9ac1fc")) +
    scale_color_manual(values = c("#d73027", "#4575b4", "#74add1", "#fee090")) +  
    facet_wrap(~ Metric, scales = "free_y") +  
    labs(
      title = paste("Tributary:", trib),
      x = "Distance from Tributary (m)",
      y = "Value",
      fill = "Metric",
      color = "Metric"
    ) +
    theme_classic() +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "right"
    )

  # Store the plot in the list
  plots[[trib]] <- p
}

# Create a 3-3-2 layout using patchwork
combined_plot <- (plots[["T1"]] | plots[["T2"]] | plots[["T3"]]) /
                 (plots[["T4"]] | plots[["T5"]] | plots[["T6"]]) /
                 (plots[["T7"]] | patchwork::plot_spacer())

# Display the combined plot
print(combined_plot)

# Save the combined figure
ggsave("combined_plot_3_3_2.png", plot = combined_plot, width = 16, height = 12, dpi = 300)
