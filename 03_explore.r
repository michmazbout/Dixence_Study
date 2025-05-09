library("ggplot2")
library("gridExtra")
library("GGally")
library("dplyr")
library("flextable")
library("officer")

# Load image data
df_img <- data.frame(read.csv("./data/df_img.csv", sep = "\t"))
head(df_img)
df_img$PS==0
##################################
# Rocksizes Variable distribution
##################################
# Separate the data for D50, D65, D84 and PS
df_melt <- reshape2::melt(df_img,
                          measure.vars = c("D50", "D65", "D84"),
                          variable.name = "Metric",
                          value.name = "Value")
df_melt$ID <- rep(rownames(df_img), 3)

df_melt_ps <- reshape2::melt(df_img, measure.vars = c("PS"),
                             variable.name = "Metric",
                             value.name = "Value")

# Create a plot for D50, D65, and D84
p_metrics <- ggplot(df_melt, aes(x = Metric, y = Value, color = Metric, fill = Metric)) +
        geom_violin(alpha = 0.2) +
		geom_boxplot(width = 0.4) + 
		geom_point(alpha = 0.5, size = 2.5) + 
		labs(title = "D50, D65, D84 Distribution", x = "Metric", y = "Value") + 
		theme_classic() + 
		scale_color_manual(values = c("black", "black", "black")) +
        scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#9ac1fc")) +
		theme(legend.position = "none",
				text = element_text(size = 14, color = "black"),
				axis.text = element_text(size = 14, color = "black"))

# Create a separate plot for PS
p_ps <- ggplot(df_melt_ps, aes(x = Metric, y = Value, color = Metric, fill = Metric)) + 
	geom_violin(alpha = 0.2) +
	geom_boxplot(width = 0.3) + 
	geom_jitter(alpha = 0.5, size = 2.5) + 
	labs(title = "PS Distribution", x = "Metric", y = "Value") + 
	theme_classic() + 
	scale_color_manual(values = c("black")) +
    scale_fill_manual(values = c("#c9a6ff")) +
	theme(legend.position = "none",
				text = element_text(size = 14, color = "black"),
				axis.text = element_text(size = 14, color = "black"))

# Arrange the plots
plot <- grid.arrange(p_metrics, p_ps, ncol = 2, nrow = 1)
ggsave(plot, filename = "./figures/plot_1.png", width = 10, height = 6)

##################################
# GGally plot
##################################
# Custom function for densityDiag with specific colors
custom_density_diag <- function(data, mapping, ...) {
  variable <- as_label(mapping$x)  # Correctly extract the variable name
  colors <- c("#fc9a9a", "#aefc9a", "#9ac1fc", "#c9a6ff")  # Define your colors
  variables <- c("D50", "D65", "D84", "PS")  # List of variables in order
  fill_color <- colors[which(variables == variable)]  # Match color to variable order
  ggplot(data = data, mapping = mapping) +
    geom_density(alpha = 0.5, fill = fill_color, ...)  # Apply the correct fill color
}

# Custom function for correlation with bold text
custom_cor <- function(data, mapping, ...) {
  ggally_cor(data, mapping, ...) + 
    theme_void() +
    theme(
      text = element_text(face = "bold")  # Make the text bold
    )
}

# ggpairs plot with custom diagonal density
p_ggpairs <- ggpairs(
  df_img[, c("D50", "D65", "D84", "PS")],
  lower = list(continuous = wrap("points", alpha = 0.5)),
  diag = list(continuous = custom_density_diag),
  upper = list(continuous = custom_cor)          # Use custom correlation function
) +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank()  # Remove minor grid lines
  )

ggsave(p_ggpairs, filename = "./figures/plot_2.png", width = 10, height = 10)

##################################
# Variable Summary Table
##################################
df_summary <- df_img[, c("D50", "D65", "D84", "PS")] %>%
  summarise(
    Mean = sapply(., function(x) round(mean(x, na.rm = TRUE), 2)),
    Min = sapply(., function(x) round(min(x, na.rm = TRUE), 2)),
    Max = sapply(., function(x) round(max(x, na.rm = TRUE), 2)),
    Range = sapply(., function(x) round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 2)),
    Median = sapply(., function(x) round(median(x, na.rm = TRUE), 2)),
    SD = sapply(., function(x) round(sd(x, na.rm = TRUE), 2))
  ) %>%
  tibble::rownames_to_column(var = "Variable")

df_summary$Variable <- c("D50", "D65", "D84", "PS")

# Create a flextable
ft <- flextable(df_summary) %>%
  theme_vanilla() %>%  # Apply a clean theme
  set_caption(caption = "Summary Statistics Table") %>%
  autofit() # Automatically adjust column widths

# Save the flextable as an image
save_as_image(ft, path = "./figures/table_1.png")
