library(ggplot2)
library(reshape2)
library(tidyr)
library(dplyr)
library(vtable)
library(ggpubr)
library(readxl)
library(tidyverse)

### creating descriptive statistics ###

df <- read.csv("directory\\des_all.csv")

df_NA <- read.csv("directory\\des_NA.csv")

df_public <- read.csv("directory\\des_public.csv")

df_prop <- read.csv("directory\\des_all_prop.csv")


## summary function ##

generate_summary <- function(df) {
  
  # Ensure the W column is a factor
  df$W <- as.factor(df$W)
  
  # Reshape the data to long format
  long_df <- df %>%
    tidyr::pivot_longer(-W, names_to = "variable", values_to = "value")
  
  # Calculate mean and SD for each variable and group
  summary_df <- long_df %>%
    dplyr::group_by(W, variable) %>%
    dplyr::summarise(Mean = mean(value, na.rm = TRUE),
                     SD = sd(value, na.rm = TRUE)) %>%
    tidyr::pivot_longer(cols = c(Mean, SD), names_to = "statistic", values_to = "value")
  
  # Pivot wider to have separate columns for each group and statistic
  summary_df <- summary_df %>%
    tidyr::pivot_wider(names_from = c(W, statistic), values_from = value)
  
  # Function to compare groups
  compare_groups <- function(var_name) {
    # Check if variable is binary
    if (length(unique(df[[var_name]])) == 2) {
      # Apply chi-squared test
      tab <- table(df[["W"]], df[[var_name]])
      test <- chisq.test(tab)
      p_value <- test$p.value
    } else {
      # Apply two-sample t-test (Welch)
      group0 <- df[[var_name]][df[["W"]] == 0]
      group1 <- df[[var_name]][df[["W"]] == 1]
      test <- t.test(group0, group1, var.equal = FALSE)
      p_value <- test$p.value
    }
    
    return(p_value)
  }
  
  # Apply function to each variable
  variables <- setdiff(names(df), "W")  # get variable names, excluding the group variable "W"
  p_values <- sapply(variables, compare_groups)
  
  # Adjust for multiple comparisons
  p_values_adjusted <- p.adjust(p_values, method = "BH")
  
  # Convert p_values_adjusted to a dataframe
  p_values_adjusted_df <- tibble::tibble(variable = names(p_values_adjusted), 
                                         p_value = unlist(p_values_adjusted))
  
  # Merge p_values_adjusted_df to summary_df
  summary_df <- summary_df %>%
    dplyr::left_join(p_values_adjusted_df, by = "variable")
  
  return(summary_df)
}



summary_df <- generate_summary(df)

summary_df_NA <- generate_summary(df_NA)

summary_df_public <- generate_summary(df_public)

summary_prop <- generate_summary(df_prop)


write.csv(summary_df, "P:\\Wisola\\Betriebslehre\\Lorenz\\TreatmentEffects\\Paper\\Tables\\descriptive_new.csv")
write.csv(summary_df_NA, "P:\\Wisola\\Betriebslehre\\Lorenz\\TreatmentEffects\\Paper\\Tables\\descriptive_NA.csv")
write.csv(summary_df_public, "P:\\Wisola\\Betriebslehre\\Lorenz\\TreatmentEffects\\Paper\\Tables\\descriptive_public.csv")
write.csv(summary_prop, "P:\\Wisola\\Betriebslehre\\Lorenz\\TreatmentEffects\\Paper\\Tables\\descriptive_prop.csv")


sum(df_prop$W) / length(df_prop$W)

min(df$soilquality) 
max(df$soilquality)

sum(df$W)



ggplot(df, aes(FLAC, fill = as.factor(W))) +
  geom_histogram(alpha = 0.5, bins = 100) + xlim(0,30)
