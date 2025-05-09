library("reshape2")
library("ggplot2")
# Load image data
df_img <- data.frame(read.csv("./data/df_img.csv", sep = "\t"))

##################################
# Add Distance variables
##################################
df_all <- df_img
# Load distance variable
df_dist <- data.frame(read.csv("./data/waypoints_distances.csv", sep = ","))
rownames(df_dist) <- df_dist$Waypoint

# Check if all images are here
df_all$img %in% df_dist$Waypoint
df_dist$Waypoint %in% df_all$img

# Add distance column
df_all$dist <- df_dist[as.character(df_all$img), "Distance_km"]

# Save the dataframe as RDS to reuse in downstream analysis
saveRDS(df_all, "./data/r_objects/df_all.RDS")

# Melt data for easier plotting
df_melt <- melt(df_all, id.vars = "dist", measure.vars = c("D50", "D65", "D84", "PS"))

# Create scatter plot with linear regression line for each variable
g_scatter <- ggplot(df_melt, aes(x = log(dist), y = value, fill = variable)) +
  geom_point(shape = 21, size = 4, color = "black", stroke = 0.5) + # Shape 21, black edge
  scale_fill_manual(values = c("#fc9a9a", "#aefc9a", "#9ac1fc", "#c9a6ff")) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  facet_wrap(~ variable, nrow = 1, scales = "free_y") +
  labs(
    x = "The distance from Grande Dixence Dam (Km)",
    y = "Value"
  ) +
  theme_bw() +
  theme(legend.position = "none",
        strip.text = element_text(size = 16, face = "bold"), # Increase facet label text size
        strip.background = element_rect(fill = "lightgrey", color = "black"), # Adjust background
		text = element_text(size = 16, color = "black"),
	    axis.text = element_text(size = 16, color = "black"))

ggsave(g_scatter, filename = "./figures/plot_3.png", width = 20, height = 6)

######################################################
# Model relationship between distance & rock features
######################################################
# Load necessary libraries
library("dplyr")
library("lm.beta")

# Function to standardize data
standardize <- function(x) {
  return((x - mean(x))/sd(x))
}

# Add new predictors
df_all$dist2 <- df_all$dist^2
df_all$dist3 <- df_all$dist^3
df_all$log_dist <- log(df_all$dist)

df_all_std <- data.frame(apply(df_all, 2, standardize))

# Define a function to extract results
extract_model_results <- function(model, pred) {
  summary_model <- summary(model)
  coefficients <- summary_model$coefficients
  
  coeff <- coefficients[pred, "Estimate"]
  std_error <- coefficients[pred, "Std. Error"]
  t_value <- coefficients[pred, "t value"]
  p_value <- coefficients[pred, "Pr(>|t|)"]
  
  data.frame(
    Predictor = pred,
    `Standardized Coefficient` = coeff,
    `Std. Error` = std_error,
    `t-value` = t_value,
    `p-value` = p_value
  )
}

# Fit models and extract results
models <- mapply(pred = c("dist", "dist2", "dist3", "log_dist"), function(pred) {
  res.list <- mapply(out = c("D50", "D65", "D84", "PS"), function(out) {
    model.formula <- formula(paste0(out, " ~ ", pred))
    model <- lm(model.formula, data = df_all_std)
    res <- extract_model_results(model, pred)
    res$Outcome = out
    res$Predictor = pred
    return(res)
  }, SIMPLIFY = FALSE)
  return(do.call(rbind, res.list))
}, SIMPLIFY = FALSE)
models <- do.call(rbind, models)

# Save results to csv for report
write.csv(models, file = "./reports/models_report.csv", row.names = TRUE, quote = FALSE)

# Create plot to compare coefficients
library("ggplot2")
library("dplyr")

# ALL MODELS PLOT
models_melt <- models %>%
  mutate(RowID = row.names(models))

# Create the plot 
p_models <- ggplot(models_melt, aes(x = Standardized.Coefficient, y = RowID)) +
  geom_point(size = 2.5, color = "black") + # Add points for standardized coefficients
  geom_errorbarh(aes(xmin = Standardized.Coefficient - Std..Error, 
                     xmax = Standardized.Coefficient + Std..Error), 
                 height = 0.2, color = "black", alpha = 0.6) + # Add horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # Add vertical dashed line at x = 0
  labs(
    x = "Standardized Coefficient",
    y = "Models",
    title = "Standardized Coefficients with Error Bars"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(p_models, filename = "./figures/plot_4_all_models.png", width = 6, height = 5)

# Subset of models plot
models_melt <- models[1:4,] %>%
  mutate(RowID = row.names(models[1:4,]))

# Create the plot 
p_models <- ggplot(models_melt, aes(x = Standardized.Coefficient, y = RowID)) +
  geom_point(size = 2.5, color = "black") + # Add points for standardized coefficients
  geom_errorbarh(aes(xmin = Standardized.Coefficient - Std..Error, 
                     xmax = Standardized.Coefficient + Std..Error), 
                 height = 0.2, color = "black", alpha = 0.6) + # Add horizontal error bars
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + # Add vertical dashed line at x = 0
  labs(
    x = "Standardized Coefficient",
    y = "Models",
    title = "Standardized Coefficients with Error Bars"
  ) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 14, face = "bold")
  )

ggsave(p_models, filename = "./figures/plot_4.png", width = 5, height = 2.5)
