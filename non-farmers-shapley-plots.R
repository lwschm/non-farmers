library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(vtable)
library(ggpubr)
library(readxl)
library(tidyverse)
library(ggrepel)


shap_df <- read.csv("directory\\shap_values_merged_20230725.csv")


##### shap plot for distance ausfahrt #####
data_shap_long_dist <- filter(shap_df, variable_name == "DISTANCE_ausfahrt")

quintiles_dist <- quantile(data_shap_long_dist$variable_value, probs = c(0.2, 0.4, 0.6, 0.8))

plot_shap_dist <- ggplot(data_shap_long_dist, aes(x = variable_value, y = shapley_value)) +
 geom_point(size=0.2, alpha=0.1, fill = "grey") +
  labs(
    title = "(B) Distance to highway exit",
    x = "km",
    y = "SHAP value") +
  geom_smooth(se=TRUE, color="red") +
  geom_hline(yintercept = 0, color = "black") +
  theme_classic() + xlim(0,80) + ylim(-2000, 3000) + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

print(plot_shap_dist)

### add vertical lines
#for(i in quintiles_dist) {
#  plot_shap_dist <- plot_shap_dist + 
#    geom_vline(xintercept = i, linetype="dashed", color="blue")
#}


## some des stats for the quintiles ##
data_shap_long_dist %>%
  mutate(quintile = cut(variable_value,
                        breaks = c(-Inf, quintiles_dist, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = max(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))



##### shap plot for area #####

data_shap_long_area <- filter(shap_df, variable_name == "FLAC")


quintiles_FLAC <- quantile(data_shap_long_area$variable_value, probs = c(0.2, 0.4, 0.6, 0.8))

plot_shap_area <- ggplot(data_shap_long_area, aes(x = variable_value, y = shapley_value)) +
  geom_point(size=0.2, alpha=0.1, fill = "grey") +
  labs(
    title = "(A) Parcel size",
    x = "ha",
    y = "SHAP value") +
  geom_smooth(se = TRUE, color="red") +
  geom_hline(yintercept = 0, color = "black") +
  theme_classic() + xlim(0,30) + ylim(-4000, 6000) + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

print(plot_shap_area)

### add vertical lines
#for(i in quintiles_FLAC) {
#  plot_shap_area <- plot_shap_area + 
#    geom_vline(xintercept = i, linetype="dashed", color="blue")
#}


## some des stats for the quintiles ##
data_shap_long_area %>%
  mutate(quintile = cut(value,
                        breaks = c(-Inf, quintiles_FLAC, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = min(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))

## arrange first line of plots ##
plot_line_1 <- ggarrange(plot_shap_area, plot_shap_dist, nrow = 1, ncol = 2)




print(plot_line_1)

##### shap plot for soil quality ######

data_shap_long_sq <- filter(shap_df, variable_name == "soilquality")

quintiles_sq <- quantile(data_shap_long_sq$variable_value, probs = c(0.2, 0.4, 0.6, 0.8))

plot_shap_sq <- ggplot(data_shap_long_sq, aes(x = variable_value, y = shapley_value)) +
  geom_point(size=0.2, alpha=0.1, fill = "grey") +
  labs(
    title = "(C) Soil quality",
    x = "Soil quality",
    y = "SHAP value") +
  geom_smooth(se = TRUE, color="red") +
  geom_hline(yintercept = 0, color = "black") +
  theme_classic() + xlim(0,103) + ylim(-2000, 2000) + 
  theme(
    plot.title = element_text(size = 10),
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9)
  )

print(plot_shap_sq)

### add vertical lines
#for(i in quintiles_sq) {
#  plot_shap_sq <- plot_shap_sq + 
#    geom_vline(xintercept = i, linetype="dashed", color="blue")
#}


## some des stats for the quintiles ##
data_shap_long_sq %>%
  mutate(quintile = cut(value,
                        breaks = c(-Inf, quintiles_FLAC, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = min(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))


## combining idstance size and soil quality plot 

dist_size_sq_plot <- ggarrange(plot_shap_area, plot_shap_dist, plot_shap_sq, nrow = 3, ncol = 1)

print(dist_size_sq_plot)

ggsave("dist_size_sq_plot_hoch.tiff", dpi = 300, height = 297, width = 150, units = "mm")



#### descriptive stats ####

# calculate the statistics for each dataset
stats_sq <- data_shap_long_sq %>%
  mutate(quintile = cut(variable_value,
                        breaks = c(-Inf, quintiles_sq, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = max(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))

stats_area <- data_shap_long_area %>%
  mutate(quintile = cut(variable_value,
                        breaks = c(-Inf, quintiles_FLAC, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = max(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))

stats_dist <- data_shap_long_dist %>%
  mutate(quintile = cut(variable_value,
                        breaks = c(-Inf, quintiles_dist, Inf),
                        labels = 1:5,
                        include.lowest = TRUE)) %>%
  group_by(quintile) %>%
  summarise(mean_shapley_value = mean(shapley_value, na.rm = TRUE),
            min_shapley_value = min(shapley_value, na.rm = TRUE),
            max_shapley_value = max(shapley_value, na.rm = TRUE),
            median_shapley_value = median(shapley_value, na.rm = TRUE))

# combine the statistics data frames
combined_stats <- bind_rows(sq = stats_sq, area = stats_area, dist = stats_dist, .id = 'dataset')

# export the combined data frame as a csv file
write.csv(combined_stats, "combined_stats_quintiles_shap.csv")





