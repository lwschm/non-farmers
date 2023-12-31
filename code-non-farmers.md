---
title: "Do non-farmers pay more for farmland than farmers?"
author: "Lorenz Schmidt, Martin Odening and Matthias Ritter"
date: "2023-8-8"
output: github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, warning=FALSE)
```

```{=tex}
\newcommand{\htau}{\widehat{\tau}}
\newcommand{\hmu}{\widehat{\mu}}
\newcommand{\hGamma}{\widehat{\Gamma}}
\newcommand{\he}{\widehat{e}}
\DeclareMathOperator{\E}{E}
\DeclareMathOperator{\PP}{P}
\newcommand{\p}[1]{\left( #1 \right)}
```

```{r, message=FALSE, echo = FALSE}
        library(dplyr)       # Data manipulation (0.8.0.1)
        library(fBasics)     # Summary statistics (3042.89)
        library(corrplot)    # Correlations (0.84)
        library(psych)       # Correlation p-values (1.8.12)
        library(grf)         # Generalized random forests (0.10.2)
        library(rpart)       # Classification and regression trees, or CART (4.1-13)
        library(rpart.plot)  # Plotting trees (3.0.6)
        library(treeClust)   # Predicting leaf position for causal trees (1.1-7)
        library(remotes)     # Install packages from github (2.0.1)
        library(readr)       # Reading csv files (1.3.1)
        library(tidyr)       # Database o
        library(tibble)      # Modern alternative to data frames (2.1.1)
        library(knitr)       # RMarkdown (1.21)
        library(ggplot2)     # general plotting tool (3.1.0)
        library(haven)       # read stata files (2.0.0)
        library(aod)         # hypothesis testing (1.3.1)
        library(evtree)      # 
        library(estimatr)    # 
        library(lmtest)      #
        library(sandwich)    #
        library(splines)     # 
        library(reshape2)    #
        library(ggthemes)    # updated ggplot themes
        library(stargazer)   # for latex output
        library(fastDummies) # for creating dummy variables
        library(viridis)
        library(NbClust) #
        library(MASS)
        library(ggpubr) 
        library(scales)
        
```

```{r}
renv::snapshot()
```

# 1. Data filtering for ATE and CATE estimation

```{r}
### clean working environment ###
rm(list=ls())
```

**Loading the data:**

```{r}
servername = 'pandia_20230712'

### set working directory ###
setwd('/directory/')
```

```{r}
### set seed for reproducability ###
set.seed(100)

```

```{r}
### load data ###
data <- read.csv('\\DIRECTORY/data_filtered_20221123.csv')

### load data on linux computer ###

data$X <- NULL

```

#### Minimum size:

```{r}
##### FLITERING FOR MINIMUM SIZE ######

data <- filter(data, FLAC >= 100)

```

#### Uncommon Circumstances

```{r}
### creating field for cicumstances ###
data$UNGEWO <- paste(data$UNGEAU, data$UNGEUS, sep = "")

data <- dummy_cols(data, select_columns = "UNGEWO")
```

```{r}
### creating unique variables ##

data$circumstances <- ifelse(data$UNGEWO == 10, 10, data$UNGEUS)
```

```{r}
### dummy variable for circumstances ### 
data <- dummy_cols(data, select_columns = "circumstances")
```

```{r}
# remove data with unknown circumstances
data <- filter(data, circumstances_9 == 0 & circumstances_10 == 0)
```

#### Farm land types

```{r}
### filtering for farm land type ###

data <- filter(data, GRUA < 330)
```

#### Dummy variable for treatment effect:

```{r}
### creating variable for buyer category 3, 4 and 5 --> non agricultural ###

data$NA_all <- ifelse(data$ERWL > 2 & data$ERWL< 6, 1,0) 

```

```{r}
## filter out private companies ###
data <- filter(data, ERWL != "2")
```

```{r}
## Creating Dummy Variables for treatment effect ####

# Create dummy variable for buyer type
data <- dummy_cols(data, select_columns = "ERWL")

```

#### Dummy variable for seller type:

```{r}
### creating dummy for Verauesserer 

data <- dummy_cols(data, select_columns = "VERL")
```

#### Dummy variable for farmland type:

```{r}
### creating dummy variable for GRUA ###


data <-data %>% 
  mutate(grassland = ifelse(as.numeric(GRUA) >= 320 & as.numeric(GRUA) < 330, 1,0)) 
data <- data %>%
  mutate(arableland = ifelse(as.numeric(GRUA) >= 310 & as.numeric(GRUA) < 320, 1,0)) 

```

```{r}
### Soil quality indicator based on ACZA and GRZA ###
data <- data %>% 
  mutate(soilquality = ifelse(as.numeric(GRUA) >= 320 & as.numeric(GRUA) < 330, as.numeric(GRZA), as.numeric(ACZA)))

data <- data %>% 
  mutate(soilquality = coalesce(soilquality, GRZA))

data <- filter(data, soilquality < 105)

data <- filter(data, soilquality > 1)

```

#### Calculations:

```{r}
## Calculating price per ha ###

data$priceha <- data$preissqm * 10000


### Calculation of relative indicators ###
data$installed_biomass_power[is.na(data$installed_biomass_power)] <- 0
data$planned_biomass_power[is.na(data$planned_biomass_power)] <- 0
data$installed_wind_power[is.na(data$installed_wind_power)] <- 0
data$planned_wind_power[is.na(data$planned_wind_power)] <- 0

data$GebGesDiff_share   <- as.numeric(data$GebGesDiff) / as.numeric(data$Bevoelkerung)
data$Wandergewinn_share <- as.numeric(data$Wandergewinn) / as.numeric(data$Bevoelkerung)


data$hog_farms_share    <- data$Betrieb_schweine_int / data$Landwirtschaftsflaeche * 100
data$cattle_farms_share <- data$Betrieb_rinder_int / data$Landwirtschaftsflaeche * 100
data$hog_share          <- data$Anzahl_schweine_int / data$Landwirtschaftsflaeche * 100
data$cattle_share       <- data$Anzahl_rinder_int / data$Landwirtschaftsflaeche * 100


data$Freizeitflaeche_share  <- data$Freizeitflaeche  / data$Bodenflaeche
data$Industrieflaeche_share <- data$Industrieflaeche / data$Bodenflaeche
data$Strassenflaeche_share  <- data$Strassenflaeche  / data$Bodenflaeche
data$Wohnbauflaeche_share   <- data$Wohnbauflaeche   / data$Bodenflaeche

data$Wandergewinn <- as.numeric(data$Wandergewinn)

# from sqm to ha
data$FLAC <- data$FLAC / 10000
# from m to km
data$DISTANCE_ausfahrt <- data$DISTANCE_ausfahrt / 1000
# from kw to mw
data$installed_wind_power <- data$installed_wind_power / 1000
data$installed_biomass_power <- data$installed_biomass_power / 1000       

data$Jahr <- data$Jahr - 2004
```

```{r}
write.csv(data, '/directory/data_ATE_20230302.csv')
```

```{r}
## filtering for prices > 100 ###

data <- filter(data, priceha > 100)
```

```{r}
### Assigning covariates names ###

covariates_names <- c("FLAC", # Area
                      "soilquality",
                      "Jahr", # Year
                          
                          
                     ### Agricultural indicators ###
                        
                          "hog_share",          
                          "cattle_share", 
                          "DISTANCE_ausfahrt",
                          
                      ### General indicators
                          "Einwohnerqkm",
                          "Alter",
                          "GebGesDiff_share", 
                          "Wandergewinn_share",
                          "agrar_share",
                          
                          "Freizeitflaeche_share",
                          "Industrieflaeche_share",
                          "Strassenflaeche_share",
                          "Wohnbauflaeche_share",
                          
                    #### renewable energy indicator###
                          "planned_biomass_power",
                          "planned_wind_power",
                     #### Seller types ####
                          "VERL_1",
                          "VERL_2",
                          "VERL_3",
                          "VERL_4",  
                          ### Grundstuecksarten
                          "grassland",
                     #### Circumstances ###
                          "circumstances_0", 
                          "circumstances_1", 
                          "circumstances_2",
                          "circumstances_3",
                          "circumstances_4",
                          "circumstances_5",
                          "circumstances_6",
                          "circumstances_8")

```

```{r}
### Assigning covariates names ###

covariates_names_des <- c("FLAC", # Area
                      "soilquality",
                      "Jahr", # Year
                          
                          
                     ### Agricultural indicators ###
                        
                          "hog_share",          
                          "cattle_share", 
                          "DISTANCE_ausfahrt",
                          
                      ### General indicators
                          
                          "Einwohnerqkm",
                          "Alter",
                          "GebGesDiff_share", 
                          "Wandergewinn_share",
                          "agrar_share",
                          
                          "Freizeitflaeche_share",
                          "Industrieflaeche_share",
                          "Strassenflaeche_share",
                          "Wohnbauflaeche_share",
                          
                    #### renewable energy indicator###
                          "planned_biomass_power",
                          "planned_wind_power",
                     #### Seller types ####
                          "VERL_1",
                          "VERL_2",
                          "VERL_3",
                          "VERL_4",  
                          ### Grundstuecksarten
                          "grassland",
                     #### Circumstances ###
                          "circumstances_0", 
                          "circumstances_1", 
                          "circumstances_2",
                          "circumstances_3",
                          "circumstances_4",
                          "circumstances_5",
                          "circumstances_6",
                          "circumstances_8"
                          )
```

```{r}
# These are the covariates we'll use
covariates <- subset(data, select = covariates_names)
covariates_des <- subset(data, select = covariates_names_des)

# Extracting outcome and treatment variables
outcome <- data$priceha
treatment <- data$NA_all
ERWL <- data$ERWL

# Setting up the data, renaming columns
df <- data.frame(covariates, #binary_covariates, 
                  W = treatment, Y = outcome, ERWL
                )

df_des <- data.frame(covariates_des, #binary_covariates, 
                  W = treatment, Y = outcome, ERWL
                )

df <- df %>% drop_na()

df_des <- df_des %>% drop_na()
```

```{r}
# making data frame with numeric data 

df <- as.data.frame(lapply(df, as.numeric))
covariates <- as.data.frame(lapply(covariates, as.numeric))

```

## Descriptive statistics

```{r}
# make a dataframe containing summary statistics of interest

summ_stats_complete <- fBasics::basicStats(df_des)
summ_stats_complete <- as.data.frame(t(summ_stats_complete))

# rename some of the columns for convenience 

summ_stats_complete <- summ_stats_complete[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")

write.csv(summ_stats_complete, '/directory/descriptive_statistics_complete.csv')

### descriptive statistics for treated NA_all 

df_des_W <- filter(df_des, W == 1)

summ_stats_NA_all  <- fBasics::basicStats(df_des_W)
summ_stats_NA_all  <- as.data.frame(t(summ_stats_NA_all))

# rename some of the columns for convenience 

summ_stats_NA_all  <- summ_stats_NA_all[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")

write.csv(summ_stats_NA_all, '/directory/descriptive_statistics_completeNA_all.csv')

### descriptive statistics for control 

df_des_0 <- filter(df_des, W == 0)

summ_stats_0  <- fBasics::basicStats(df_des_0)
summ_stats_0  <- as.data.frame(t(summ_stats_0))

# rename some of the columns for convenience 

summ_stats_0  <- summ_stats_0[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")

write.csv(summ_stats_0, '/directory/descriptive_statistics_complete_control.csv')

### descriptive statistics for public buyers

df_des_public <- filter(df_des, ERWL == 3)

summ_stats_public  <- fBasics::basicStats(df_des_public)
summ_stats_public  <- as.data.frame(t(summ_stats_public))

# rename some of the columns for convenience 

summ_stats_public  <- summ_stats_public[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")

write.csv(summ_stats_public, '/directory/descriptive_statistics_public.csv')

### descriptive statistics for NA buyers

df_des_NA <- filter(df_des, ERWL > 3 & ERWL < 6)

summ_stats_NA  <- fBasics::basicStats(df_des_NA)
summ_stats_NA  <- as.data.frame(t(summ_stats_NA))

# rename some of the columns for convenience 

summ_stats_NA  <- summ_stats_NA[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")

write.csv(summ_stats_NA, '/directory/descriptive_statistics_NichtLandwirt.csv')


```

```{r}
### Treatment as factor ###

df_des$W <- as.factor(df_des$W)
```

# 2. Average Treatment Effect (ATE) Estimation

```{r}
# Creating data frame for public and private non-farmers
df_NA <- filter(df, ERWL > 3 & ERWL < 6 | W == 0)
df_public <- filter(df, ERWL == 3| W == 0)

#Create a matrix for covariates other than treatment(W) and outcome(Y)
X = subset(df, select = covariates_names)
#Create a vectors for the outcome variable (Y) and the treatment (W)
W = df$W # treatment
Y = df$Y # outcome

df_descriptive <- data.frame(W = W, Y = Y, X)

write_csv(df_descriptive, '/directory/dataframe_complete.csv')

#Create a matrix for covariates other than treatment(W) and outcome(Y)
X_public = subset(df_public, select = covariates_names)
#Create a vectors for the outcome variable (Y) and the treatment (W)
W_public = df_public$W # treatment
Y_public = df_public$Y # outcome

df_public_descriptive <- data.frame(W = W_public, Y = Y_public, X_public)

write_csv(df_public_descriptive, '/directory/dataframe_public.csv')

#Create a matrix for covariates other than treatment(W) and outcome(Y)
X_NA = subset(df_NA, select = covariates_names)
#Create a vectors for the outcome variable (Y) and the treatment (W)
W_NA = df_NA$W # treatment
Y_NA = df_NA$Y # outcome

df_private_descriptive <- data.frame(W = W_NA, Y = Y_NA, X_NA)

write_csv(df_private_descriptive, '/directory/dataframe_public.csv')

```

```{r}
#### setting up significance level alpha 0.05 and calculate diff in means 

signif.level <- qnorm(0.025, lower.tail = FALSE)

difference_in_means <- function(dataset) {
  treated_idx <- which(dataset$W == 1)
  control_idx <- which(dataset$W == 0)
  
  # Filter treatment / control observations, pulls outcome variable as a vector
  y1 <- dataset[treated_idx, "Y"] # Outcome in treatment grp
  y0 <- dataset[control_idx, "Y"] # Outcome in control group
  
  n1 <- sum(dataset[,"W"])     # Number of obs in treatment
  n0 <- sum(1 - dataset[,"W"]) # Number of obs in control
  
  # Difference in means is ATE
  tauhat <- mean(y1) - mean(y0)
  
  # 95% Confidence intervals
  se_hat <- sqrt( var(y0)/(n0-1) + var(y1)/(n1-1) )
  lower_ci <- tauhat - signif.level * se_hat
  upper_ci <- tauhat + signif.level * se_hat
  
  return(c(ATE = tauhat, lower_ci = lower_ci, upper_ci = upper_ci))
}
```

```{r}
difference_in_means_rel <- function(dataset, signif.level = signif.level) {
  treated_idx <- which(dataset$W == 1)
  control_idx <- which(dataset$W == 0)
  
  # Filter treatment / control observations, pulls outcome variable as a vector
  y1 <- dataset[treated_idx, "Y"] # Outcome in treatment grp
  y0 <- dataset[control_idx, "Y"] # Outcome in control group
  
  n1 <- sum(dataset[,"W"])     # Number of obs in treatment
  n0 <- sum(1 - dataset[,"W"]) # Number of obs in control
  
  # Difference in means is ATE
  tauhat <- mean(y1) - mean(y0)
  
  # 95% Confidence intervals
  se_hat <- sqrt( var(y0)/(n0-1) + var(y1)/(n1-1) )
  lower_ci <- tauhat - signif.level * se_hat
  upper_ci <- tauhat + signif.level * se_hat
  
  # Calculate the average sales price
  avg_sales_price <- mean(dataset$Y)
  
  # Calculate the relative ATE and its confidence intervals
  relative_tauhat <- tauhat / avg_sales_price
  relative_lower_ci <- lower_ci / avg_sales_price
  relative_upper_ci <- upper_ci / avg_sales_price
  
  return(c(ATE = relative_tauhat, lower_ci = relative_lower_ci, upper_ci = relative_upper_ci))
}
```

```{r}
tauhat_rct_all <- difference_in_means_rel(df, signif.level)
tauhat_rct_public <- difference_in_means_rel(df_public, signif.level)
tauhat_rct_NA <- difference_in_means_rel(df_NA, signif.level)
```

## Causal forest

For each treatment group a different causal forest will be trained but with the same settings.

```{r}
cf <- causal_forest(X = as.matrix(X),
                    Y = Y,
                    W = W,
                    tune.parameters = "all")
```

```{r}
cf_public <- causal_forest(X = as.matrix(X_public),
                    Y = Y_public,
                    W = W_public,
                    tune.parameters = "all")
```

```{r}
cf_NA <- causal_forest(X = as.matrix(X_NA),
                    Y = Y_NA,
                    W = W_NA,
                    tune.parameters = "all")
```

```{r}
cf_tuning_param <- cf[["tuning.output"]][["params"]]
write.csv(cf_tuning_param,file = '/directory/cf_param.csv', row.names = TRUE)

cf_public_tuning_param <- cf_public[["tuning.output"]][["params"]]
write.csv(cf_public_tuning_param, '/directory/cf_public_param.csv', row.names = TRUE)

cf_NA_tuning_param <- cf_NA[["tuning.output"]][["params"]]
write.csv(cf_NA_tuning_param, '/directory/cf_NA_param.csv', row.names = TRUE)
```

#### Testing calibration of forests

```{r}
cf_calib <- test_calibration(cf)

cf_calib_df <- as.data.frame(cf_calib[,])

cf_calib_df <- cbind(Variable = rownames(cf_calib_df), cf_calib_df)

write_csv(cf_calib_df, '/directory/cf_calib_df.csv' )
```

```{r}
cf_public_calib <- test_calibration(cf_public)
cf_public_calib_df <- as.data.frame(cf_public_calib[,])

cf_public_calib_df <- cbind(Variable = rownames(cf_public_calib_df), cf_public_calib_df)

write_csv(cf_public_calib_df, '/directory/cf_public_calib_df.csv' )
```

```{r}
cf_NA_calib <- test_calibration(cf_NA)

cf_NA_calib_df <- as.data.frame(cf_NA_calib[,])

cf_NA_calib_df <- cbind(Variable = rownames(cf_NA_calib_df), cf_NA_calib_df)

write_csv(cf_NA_calib_df, '/directory/cf_NA_calib_df.csv' )

```

```{r}
### estimating

p_rf        <- as.data.frame(cf$W.hat)%>% rename(p_cf='cf$W.hat')
p_rf_NA     <- as.data.frame(cf_NA$W.hat) %>% rename(p_cf='cf_NA$W.hat')
p_rf_public <- as.data.frame(cf_public$W.hat) %>% rename(p_cf='cf_public$W.hat')

df$e.hat <- p_rf
df_NA$e.hat <- p_rf_NA
df_public$e.hat <- p_rf_public

df$m.hat <- cf$Y.hat
df_NA$m.hat <- cf_NA$Y.hat
df_public$m.hat <- cf_public$Y.hat

tau.hat_all <- predict(cf)$predictions
df$cate <- tau.hat_all # tau(X) estimates

tau.hat_public <- predict(cf_public)$predictions
df_public$cate <- tau.hat_public # tau(X) estimates

tau.hat_NA <- predict(cf_NA)$predictions
df_NA$cate <- tau.hat_NA # tau(X) estimates

df$mu.hat.0 <- df$m.hat - df$e.hat * df$cate
df$mu.hat.1 <- df$m.hat + (1- df$e.hat) * df$cate

df_NA$mu.hat.1 <- df_NA$m.hat + (1- df_NA$e.hat) * df_NA$cate
df_NA$mu.hat.0 <- df_NA$m.hat - df_NA$e.hat * df_NA$cate

df_NA$mu.hat.1 <- df_NA$m.hat + (1- df_NA$e.hat) * df_NA$cate
df_NA$mu.hat.0 <- df_NA$m.hat - df_NA$e.hat * df_NA$cate

df_public$mu.hat.1 <- df_public$m.hat + (1- df_public$e.hat) * df_public$cate
df_public$mu.hat.0 <- df_public$m.hat - df_public$e.hat * df_public$cate


df$aipw.scores <- df$cate + df$W /df$e.hat * (df$Y - df$mu.hat.1) - (1- df$W) / (1 - df$e.hat) * (df$Y - df$mu.hat.0)

df_NA$aipw.scores <- df_NA$cate + df_NA$W /df_NA$e.hat * (df_NA$Y - df_NA$mu.hat.1) - (1- df_NA$W) / (1 - df_NA$e.hat) * (df_NA$Y - df_NA$mu.hat.0)

df_public$aipw.scores <- df_public$cate + df_public$W /df_public$e.hat * (df_public$Y - df_public$mu.hat.1) - (1- df_public$W) / (1 - df_public$e.hat) * (df_public$Y - df_public$mu.hat.0)

```

```{r}
### filter for low and high propensity scores and assign new covariate frames for ATE and GATE

df_ATE <- filter(df, e.hat >= 0.1 & e.hat <= 0.9)
df_ATE_NA <- filter(df_NA, e.hat >= 0.1 & e.hat <= 0.9)
df_ATE_public <- filter(df_public, e.hat >= 0.1 & e.hat <= 0.9)

X_ATE = subset(df_ATE, select = covariates_names)
X_ATE_NA = subset(df_ATE_NA, select = covariates_names)
X_ATE_public = subset(df_ATE_public, select = covariates_names)

```

### Plotting Overlap for identification

```{r}
## plotting geom density

ggplot(df, aes(x = e.hat$p_cf, fill = as.factor(W)))+
  geom_density(alpha = 0.7)

```

```{r}
### propensity scores ###

prop_all <- ggplot(df, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_histogram(bins=100, alpha = 0.5, position = "stack") +
  #geom_density(aes(x = p_cf), position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1),color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Frequency") +
  ggtitle("(1) Non-farmers") +
  theme(text = element_text(size = 8)) + scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

prop_NA <- ggplot(df_NA, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_histogram(bins=100, alpha = 0.5, position = "stack") +
  #geom_density(aes(x = p_cf), position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1),color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Frequency") +
  ggtitle("(2) Private non-farmers") +
  theme(text = element_text(size = 8)) + scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

prop_public <- ggplot(df_public, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_histogram(bins=100, alpha = 0.5, position = "stack") +
  #geom_density(aes(x = p_cf), position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1),color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Frequency") +
  ggtitle("(3) Public non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

ggarrange(prop_all, prop_NA, prop_public, ncol = 2, nrow = 2, common.legend = TRUE)

ggsave("prop_alltreatments.jpg", width = 8, height = 6, bg = "white")


```

```{r}
prop_all <- ggplot(df, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_density(alpha = 0.5, position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1), color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9), color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Density") +
  ggtitle("(1) Non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

ggsave("prop_alltreatments_density.jpg", prop_all, width = 8, height = 6, bg = "white")

prop_NA <- ggplot(df_NA, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_density(alpha = 0.5, position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1), color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9), color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Density") +
  ggtitle("(2) Private non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

prop_public <- ggplot(df_public, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_density(alpha = 0.5, position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1), color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9), color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Density") +
  ggtitle("(3) Public non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

combined_plots <- ggarrange(prop_all, prop_NA, prop_public, ncol = 2, nrow = 2, common.legend = TRUE)

ggsave("prop_alltreatments_density.jpg", combined_plots, width = 8, height = 6, bg = "white")
```

```{r}
prop_all <- ggplot(df, aes(x = e.hat$p_cf, fill = as.factor(W))) +
  geom_histogram(bins=100, alpha = 0.5, position = "stack") +
  #geom_density(aes(x = p_cf), position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1),color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Frequency") +
  theme(text = element_text(size = 12)) + scale_fill_discrete(name = "", labels = c("Control group", "Treatment group")) + theme(legend.position = "top")

ggsave("prop_all.png", prop_all, width = 8, height = 4)
ggsave("prop_all.tiff", prop_all, dpi = 300, width = 8, height = 4)
```

```{r}
#Compare the predicted probabilities of getting treated, versus actual probabilities of getting treated.
#A perfectly calibrated model will fall exactly on the 45 degree line.

regfor_calibration <- smooth.spline(p_rf$p_cf, W, df = 4)
regfor_calibration <- as.data.frame(cbind(regfor_calibration$x, regfor_calibration$y))

#Visual Check
rgf_spline <- ggplot(regfor_calibration,aes(x=V1, y= V2)) +
  geom_point(color="orange")+
  labs(title = "Regression Forest Calibration Check", 
       x= "True Treatment Assignment",
       y="Fitted Treatment Assignment") +
  geom_abline(intercept = 0) + xlim( c(0,1)) + ylim( c(0,1))+
  ggthemes::theme_few()

rgf_spline
```

```{r}
# compare the log likelihoods (bigger is better)

loglik = mean(W * log(p_rf$p_cf) + (1 - W) * log(1 - p_rf$p_cf))
      
loglik
```

### Augmented inverse propensity weighting with causal forest

```{r}
# Define a custom function to compute the relative average treatment effect using the AIPW method
relative_average_treatment_effect <- function(forest, subset = NULL, signif.level = NULL) {
  # Compute the average treatment effect using the grf function with the AIPW method
  ate <- grf::average_treatment_effect(forest, method = "AIPW", target.sample = "all", subset = subset)
  
  # Calculate the average sales price for the specified subset (use the appropriate variable name for your dataset)
  avg_sales_price <- mean(forest$Y.orig[subset])
  
  # Calculate the relative average treatment effect
  relative_ate <- ate[["estimate"]] / avg_sales_price
  
  # Calculate the confidence intervals
  lower_ci <- relative_ate - signif.level * (ate[["std.err"]] / avg_sales_price)
  upper_ci <- relative_ate + signif.level * (ate[["std.err"]] / avg_sales_price)
  
  # Format the output
  return(c(ATE = relative_ate,
              lower_ci = lower_ci,
              upper_ci = upper_ci,
              mean_calc = avg_sales_price))
}
```

```{r}
# This approach is justified via the orthogonal moments argument (as seen in class)
ate_cf_aipw_all <- average_treatment_effect(cf, target.sample = "all", subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9))
tauhat_rf_aipw_all <- relative_average_treatment_effect(cf, subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9), signif.level)

ate_cf_aipw_public <- average_treatment_effect(cf_public, target.sample = "all", subset = !(cf_public$W.hat <= 0.1 | cf_public$W.hat >= 0.9))
tauhat_rf_aipw_public <- relative_average_treatment_effect(cf_public, subset = !(cf_public$W.hat <= 0.1 | cf_public$W.hat >= 0.9), signif.level)

ate_cf_aipw_NA <- average_treatment_effect(cf_NA, target.sample = "all", subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9))
tauhat_rf_aipw_NA <- relative_average_treatment_effect(cf_NA, subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9), signif.level)

```

```{r}
aipw_average_treatment_effect <- function(forest, subset = NULL, signif.level = NULL) {
  # Compute the average treatment effect using the grf function with the AIPW method
  ate <- grf::average_treatment_effect(forest, method = "AIPW", target.sample = "all", subset = subset)
  
  # Get the estimate and standard error separately
  ate_estimate <- ate[["estimate"]]
  ate_std_err <- ate[["std.err"]]
  
  # Calculate the confidence intervals
  lower_ci <- ate_estimate - signif.level * ate_std_err
  upper_ci <- ate_estimate + signif.level * ate_std_err
  
  # Format the output
  return(c(ATE = ate_estimate,
           lower_ci = lower_ci,
           upper_ci = upper_ci))
}
```

```{r}
absolute_ate_cf <- aipw_average_treatment_effect(cf, subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9), signif.level = signif.level)

absolute_ate_cf_public <- aipw_average_treatment_effect(cf_public, subset = !(cf_public$W.hat <= 0.1 | cf_public$W.hat >= 0.9), signif.level = signif.level)

absolute_ate_cf_private <- aipw_average_treatment_effect(cf_NA, subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9), signif.level = signif.level)

all_estimators_abs <- rbind(
  
  "Non-farmer+public buyer" = absolute_ate_cf,
  "Non-farmer buyer" = absolute_ate_cf_public,
  "Public buyer" = absolute_ate_cf_private
  )

all_estimators_abs <- data.frame(all_estimators_abs)
all_estimators_abs$method <- rownames(all_estimators_abs)
rownames(all_estimators_abs) = NULL
all_estimators_abs$ci_length <- all_estimators_abs$upper_ci - all_estimators_abs$lower_ci

write_csv(all_estimators_abs, '/directory/all_estimators_abs.csv')

```

```{r}
print(average_treatment_effect(cf_NA, method = "TMLE", target.sample = "all", subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9)))
```

```{r}
print(average_treatment_effect(cf_NA, target.sample = "all", subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9)))
```

```{r}
relative_average_treatment_effect(cf_NA, subset = !(cf_NA$W.hat <= 0.1 | cf_NA$W.hat >= 0.9), signif.level)
```

## Summary

```{r}
all_estimators <- rbind(
  "Non-farmer+public buyer" = tauhat_rf_aipw_all,
  "Non-farmer buyer" = tauhat_rf_aipw_NA,
  "Public buyer" = tauhat_rf_aipw_public
  )

all_estimators <- data.frame(all_estimators)
all_estimators$method <- rownames(all_estimators)
rownames(all_estimators) = NULL
all_estimators$ci_length <- all_estimators$upper_ci - all_estimators$lower_ci

```

```{r}
write_csv(all_estimators, '/directory/all_estimators_1.csv')
```

# 3. Conditional average treatment effect

```{r}
# Get predictions from forest fitted above.
tau.hat_all <- predict(cf)$predictions
df$cate <- tau.hat_all # tau(X) estimates

tau.hat_public <- predict(cf_public)$predictions
df_public$cate <- tau.hat_public # tau(X) estimates

tau.hat_NA <- predict(cf_NA)$predictions
df_NA$cate <- tau.hat_NA # tau(X) estimates
```

```{r}
## calculating relative cates

df_ATE$cate_rel <- df_ATE$cate / df_ATE$Y
df_ATE_NA$cate_rel <- df_ATE_NA$cate / df_ATE_NA$Y
df_ATE_public$cate_rel <- df_ATE_public$cate / df_ATE_public$Y
```

```{r}
cat("min:", min(df_ATE$cate), "max:",max(df_ATE$cate))
```

```{r}
prop_score <- cf$W.hat
prop_score_df <- as.data.frame(prop_score)
write_csv(prop_score_df, '/directory/prop_score.csv' )
```

### Treatment effect heterogeneity

```{r}
# Histogram of CATEs for each treatment group

hist_all <- ggplot(df_ATE, aes(cate, fill = as.character(W))) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-10000,20000) +
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  ggtitle("Non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group")) + theme(legend.position = "top")

hist_public <- ggplot(df_ATE_public, aes(cate, fill = as.character(W))) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-10000,20000) +
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  ggtitle("(b) Public non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))

hist_NA <- ggplot(df_ATE_NA, aes(cate, fill = as.character(W))) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-10000,20000) +
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  ggtitle("(a) Private non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))
  
ggarrange(hist_all, ggarrange(hist_NA, hist_public, ncol = 2, nrow = 1, legend = FALSE), ncol = 1, nrow = 2, common.legend = TRUE)

ggsave("hist_cate_alltreatments.png", width = 8, height = 6)
```

```{r}
mean(df_ATE$cate)
```

```{r}
hist_all <- ggplot(df_ATE, aes(cate)) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-10000,20000) +
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  theme(text = element_text(size = 12)) 

hist_all

ggsave("hist_cate_all.png", width = 10, height = 4)
ggsave("hist_cate_all.tiff", dpi = 300, width = 10, height = 4)
```

```{r}

# Histogram of CATEs for each treatment group
hist_all <- ggplot(df_ATE, aes(cate_rel)) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-1, 1) +
  theme_classic() +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed", size = 0.3, alpha = 1) +
  xlab("Estimated CATE in percentage per ha") +
  ylab("Frequency") +
  ggtitle("(1) Non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(labels =  percent_format(), limits = c(-1,1))

hist_public <- ggplot(df_ATE_public, aes(cate_rel)) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-1, 1) +
  theme_classic() +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed", size = 0.3, alpha = 1) +
  xlab("Estimated CATE in percentage per ha") +
  ylab("Frequency") +
  ggtitle("(3) Public non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(labels =  percent_format(), limits = c(-1,1))

hist_NA <- ggplot(df_ATE_NA, aes(cate_rel)) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-1, 1) +
  theme_classic() +
  geom_vline(aes(xintercept = 0), color = "black", linetype = "dashed", size = 0.3, alpha = 1) +
  xlab("Estimated CATE in percentage per ha") +
  ylab("Frequency") +
  ggtitle("(2) Private non-farmers") +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(labels =  percent_format(), limits = c(-1,1))

combined_plot <- ggarrange(hist_all, hist_NA, hist_public, ncol = 2, nrow = 2)

ggsave("hist_cate_rel_alltreatments.png", combined_plot, width = 8, height = 6)
```

```{r}
## Share of observation for cate_rel -100% and 100% ##

# all non-farmers
sum((df_ATE$cate >= -10000) & (df_ATE$cate <= 20000)) / length(df_ATE$cate)
```

```{r}
quantile(df_ATE$cate, c(0.025, 0.975))
```

```{r}
# private non-farmers
sum((df_ATE_NA$cate_rel >= -1) & (df_ATE_NA$cate_rel <= 1)) / length(df_ATE_NA$cate_rel)
```

```{r}
# public non-farmers
sum((df_ATE_public$cate_rel >= -1) & (df_ATE_public$cate_rel <= 1)) / length(df_ATE_public$cate_rel)
```

```{r}
# variable importance for all treatments

var_imp <- c(variable_importance(cf))
names(var_imp) <- covariates_names
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp[1:5]  # showing oNAy first few
```

```{r}
# variable importance for public buyers

var_imp <- c(variable_importance(cf_public))
names(var_imp) <- covariates_names
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp[1:5]  # showing only first few
```

```{r}
# Variable importance for other non-ag buyers
var_imp <- c(variable_importance(cf_NA))
names(var_imp) <- covariates_names
sorted_var_imp <- sort(var_imp, decreasing = TRUE)
sorted_var_imp[1:5]  # showing only first few


```

### Best linear projection

```{r}
# Best linear projection of the conditional average treatment effect on covariates
blp_cf <- best_linear_projection(cf, X[, -which(names(X) %in% c("VERL_1", "circumstances_0"))])
stargazer(blp_cf, type = "latex", out = '/directory/blp_cf.txt')
blp_cf

blp_cf_df <- as.data.frame(blp_cf[,])

blp_cf_df <- cbind(Variable = rownames(blp_cf_df), blp_cf_df)

write_csv(blp_cf_df, '/directory/blp_cf.csv' )
```

```{r}
blp_subset <- best_linear_projection(cf, A = X[, -which(names(X) %in% c("VERL_1", "circumstances_0"))], subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9))

blp_subset_df <- as.data.frame(blp_subset[,])

blp_subset_df  <- cbind(Variable = rownames(blp_subset_df ), blp_subset_df)

write_csv(blp_subset_df, '/directory/blp_subset.csv' )
```

```{r}
# Best linear projection of the conditional average treatment effect on covariates


blp_cf_public <- best_linear_projection(cf_public, X_public[, -which(names(X_public) %in% c("VERL_1", "circumstances_0"))])
stargazer(blp_cf_public, type = "latex", out = '/directory/blp_cf_public.txt')

blp_cf_public_df <- as.data.frame(blp_cf_public[,])

blp_cf_public_df <- cbind(Variable = rownames(blp_cf_public_df), blp_cf_public_df)

write_csv(blp_cf_public_df, '/directory/blp_cf_public.csv' )
```

```{r}
# Best linear projection of the conditional average treatment effect on covariates


blp_cf_NA <- best_linear_projection(cf_NA, X_NA[, -which(names(X_NA) %in% c("VERL_1", "circumstances_0"))])
stargazer(blp_cf_NA, type = "latex", out = '/directory/blp_cf_NA.txt')

blp_cf_NA_df <- as.data.frame(blp_cf_NA[,])

blp_cf_NA_df <- cbind(Variable = rownames(blp_cf_NA_df), blp_cf_NA_df)

write_csv(blp_cf_NA_df, '/directory/blp_cf_NA.csv' )

```

## Heterogeneity across subgroups

```{r}
### Heterogeneity across subgroups ###

# Manually creating subgroups of CATE 
num_tiles <- 4  # ntiles = CATE is above / below the median
df$ntile <- factor(ntile(df$cate, n=num_tiles))
df_public$ntile <- factor(ntile(df_public$cate, n=num_tiles))
df_NA$ntile <- factor(ntile(df_NA$cate, n=num_tiles))
```

```{r}
## histogram soilquality ###

ggplot() +
  geom_histogram(data= df, aes(x = soilquality), bins = 100, alpha = 0.8) +
  xlim(0,100) +
  facet_wrap(~as.factor(ERWL), scales = "free")
ggsave("histogram_soilquality.png")
```

```{r}
### Histogram of Size ###
ggplot() +
  geom_histogram(data= df, aes(x = FLAC), bins = 100) +
  xlim(0,30) 
ggsave("histogram_size.png")
```

```{r}
### histogram distance autobahnausfahrt ###

ggplot() +
  geom_histogram(data= df, aes(x = DISTANCE_ausfahrt), bins = 100) +
  xlim(0,50000) +
  facet_wrap(~as.factor(ERWL), scales = "free")
ggsave("histogram_dist.png")
```

```{r}
## histogram hog share

ggplot() +
  geom_histogram(data= df, aes(x = DISTANCE_ausfahrt), bins = 100) +
  geom_vline(aes(xintercept=2),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=4),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=8),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=16),color="red", linetype="dashed", size=0.8, alpha = 1) +
  xlim(0,40)
```

```{r}
# Compute the quintiles in df_ATE
breaks_size <- quantile(df_ATE$FLAC, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_sq <- quantile(df_ATE$soilquality, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_dist <- quantile(df_ATE$DISTANCE_ausfahrt, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Extend the breaks
breaks_size[c(1, length(breaks_size))] <- c(-Inf, Inf)
breaks_sq[c(1, length(breaks_sq))] <- c(-Inf, Inf)
breaks_dist[c(1, length(breaks_dist))] <- c(-Inf, Inf)

# Compute the quintiles in df_ATE_public
breaks_size_public <- quantile(df_ATE_public$FLAC, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_sq_public <- quantile(df_ATE_public$soilquality, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_dist_public <- quantile(df_ATE_public$DISTANCE_ausfahrt, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Extend the breaks
breaks_size_public[c(1, length(breaks_size_public))] <- c(-Inf, Inf)
breaks_sq_public[c(1, length(breaks_sq_public))] <- c(-Inf, Inf)
breaks_dist_public[c(1, length(breaks_dist_public))] <- c(-Inf, Inf)

# Compute the quintiles in df_ATE_NA
breaks_size_NA <- quantile(df_ATE_NA$FLAC, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_sq_NA <- quantile(df_ATE_NA$soilquality, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_dist_NA <- quantile(df_ATE_NA$DISTANCE_ausfahrt, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Extend the breaks
breaks_size_NA[c(1, length(breaks_size_NA))] <- c(-Inf, Inf)
breaks_sq_NA[c(1, length(breaks_sq_NA))] <- c(-Inf, Inf)
breaks_dist_NA[c(1, length(breaks_dist_NA))] <- c(-Inf, Inf)


### df for all treatments ##
df$size_interval <- cut(df$FLAC, breaks = breaks_size, labels = 1:5, include.lowest = TRUE)
df$sq_interval <- cut(df$soilquality, breaks = breaks_sq, labels = 1:5, include.lowest = TRUE)
df$dist_interval <- cut(df$DISTANCE_ausfahrt, breaks = breaks_dist, labels = 1:5, include.lowest = TRUE)

### df for public ##
df_public$size_interval <- cut(df_public$FLAC, breaks = breaks_size_public, labels = 1:5, include.lowest = TRUE)
df_public$sq_interval <- cut(df_public$soilquality, breaks = breaks_sq_public, labels = 1:5, include.lowest = TRUE)
df_public$dist_interval <- cut(df_public$DISTANCE_ausfahrt, breaks = breaks_dist_public, labels = 1:5, include.lowest = TRUE)

df_public$size_interval <- as.factor(df_public$size_interval) 
df_public$sq_interval <- as.factor(df_public$sq_interval)
df_public$dist_interval <- as.factor(df_public$dist_interval)

### df for NL ##
df_NA$size_interval <- cut(df_NA$FLAC, breaks = breaks_size_NA, labels = 1:5, include.lowest = TRUE)
df_NA$sq_interval <- cut(df_NA$soilquality, breaks = breaks_sq_NA, labels = 1:5, include.lowest = TRUE)
df_NA$dist_interval <- cut(df_NA$DISTANCE_ausfahrt, breaks = breaks_dist_NA, labels = 1:5, include.lowest = TRUE)



```

```{r}
# Create list of data frames
breaks_list <- list(
  data.frame(Name = "breaks_size", Percentile = names(breaks_size), Value = breaks_size),
  data.frame(Name = "breaks_sq", Percentile = names(breaks_sq), Value = breaks_sq),
  data.frame(Name = "breaks_dist", Percentile = names(breaks_dist), Value = breaks_dist),
  data.frame(Name = "breaks_size_public", Percentile = names(breaks_size_public), Value = breaks_size_public),
  data.frame(Name = "breaks_sq_public", Percentile = names(breaks_sq_public), Value = breaks_sq_public),
  data.frame(Name = "breaks_dist_public", Percentile = names(breaks_dist_public), Value = breaks_dist_public),
  data.frame(Name = "breaks_size_NA", Percentile = names(breaks_size_NA), Value = breaks_size_NA),
  data.frame(Name = "breaks_sq_NA", Percentile = names(breaks_sq_NA), Value = breaks_sq_NA),
  data.frame(Name = "breaks_dist_NA", Percentile = names(breaks_dist_NA), Value = breaks_dist_NA)
)

# Combine data frames into one
breaks_combined_df <- do.call(rbind, breaks_list)

# Reset row names
row.names(breaks_combined_df) <- NULL

write_csv(breaks_combined_df, "GATE_breaks.csv")

```

## GATE

```{r}
### creating function for Group average treatment effect ###
group_ate <- function(forest, group){
  estimated_aipw_ate <- lapply(
  seq(n_distinct(group)), function(w) {
  ate <- average_treatment_effect(forest, subset = group == w)
})
estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))
interval <- seq(n_distinct(group))
estimated_aipw_ate <- cbind(interval, estimated_aipw_ate)  
}

## adjusting the group ate function for propensity score filtering ## 

group_ate_prop <- function(forest, group, propensityscore){
  estimated_aipw_ate <- lapply(
  seq(n_distinct(group)), function(w) {
  subset = group == w & propensityscore >= 0.1 & propensityscore <= 0.9
  ate <- average_treatment_effect(forest, target.sample = "all", subset = subset)
})
estimated_aipw_ate <- data.frame(do.call(rbind, estimated_aipw_ate))
interval <- seq(n_distinct(group))
estimated_aipw_ate <- cbind(interval, estimated_aipw_ate)  
}


```

```{r}
### GATE relative to the mean price per group ###

group_ate_prop_relative <- function(forest, group, propensityscore, Y){
  estimated_aipw_ate_relative <- lapply(
    seq(n_distinct(group)), function(w) {
      subset = group == w & propensityscore >= 0.1 & propensityscore <= 0.9
      ate <- average_treatment_effect(forest, target.sample = "all", subset = subset)
      mean_y <- mean(Y[subset])
      relative_ate <- ate / mean_y
      return(relative_ate)
    })
  estimated_aipw_ate_relative <- data.frame(do.call(rbind, estimated_aipw_ate_relative))
  interval <- seq(n_distinct(group))
  estimated_aipw_ate_relative <- cbind(interval, estimated_aipw_ate_relative)
}

```

```{r}
## GATE relative to total mean after adjusting for propensity scores

group_ate_prop_relative_total <- function(forest, group, propensityscore, Y){
  estimated_aipw_ate_relative <- lapply(
    seq(n_distinct(group)), function(w) {
      subset = group == w & propensityscore >= 0.1 & propensityscore <= 0.9
      subset_1 = w & propensityscore >= 0.1 & propensityscore <= 0.9
      ate <- average_treatment_effect(forest, target.sample = "all", subset = subset)
      mean_y <- mean(Y[subset_1])
      relative_ate <- ate / mean_y
      return(relative_ate)
    })
  estimated_aipw_ate_relative <- data.frame(do.call(rbind, estimated_aipw_ate_relative))
  interval <- seq(n_distinct(group))
  estimated_aipw_ate_relative <- cbind(interval, estimated_aipw_ate_relative)
}

```

```{r}

gate_size_all <-    group_ate_prop_relative(cf, df$size_interval, cf$W.hat, df$Y)
gate_size_public <- group_ate_prop_relative(cf_public, df_public$size_interval, cf_public$W.hat, df_public$Y)
gate_size_NA <-     group_ate_prop_relative(cf_NA, df_NA$size_interval, cf_NA$W.hat, df_NA$Y)
gate_sq_all <-      group_ate_prop_relative(cf, df$sq_interval, cf$W.hat, df$Y)
gate_sq_public <-   group_ate_prop_relative(cf_public, df_public$sq_interval, cf_public$W.hat, df_public$Y)
gate_sq_NA <-       group_ate_prop_relative(cf_NA, df_NA$sq_interval, cf_NA$W.hat, df_NA$Y)
gate_dist_all <-    group_ate_prop_relative(cf, df$dist_interval, cf$W.hat, df$Y)
gate_dist_public <- group_ate_prop_relative(cf_public, df_public$dist_interval, cf_public$W.hat, df_public$Y)
gate_dist_NA <-     group_ate_prop_relative(cf_NA, df_NA$dist_interval, cf_NA$W.hat, df_NA$Y)

```

```{r}
gate_size_all_total <-    group_ate_prop_relative_total(cf, df$size_interval, cf$W.hat, df$Y)
gate_size_public_total <- group_ate_prop_relative_total(cf_public, df_public$size_interval, cf_public$W.hat, df_public$Y)
gate_size_NA_total <-     group_ate_prop_relative_total(cf_NA, df_NA$size_interval, cf_NA$W.hat, df_NA$Y)
gate_sq_all_total <-      group_ate_prop_relative_total(cf, df$sq_interval, cf$W.hat, df$Y)
gate_sq_public_total <-   group_ate_prop_relative_total(cf_public, df_public$sq_interval, cf_public$W.hat, df_public$Y)
gate_sq_NA_total <-       group_ate_prop_relative_total(cf_NA, df_NA$sq_interval, cf_NA$W.hat, df_NA$Y)
gate_dist_all_total <-    group_ate_prop_relative_total(cf, df$dist_interval, cf$W.hat, df$Y)
gate_dist_public_total <- group_ate_prop_relative_total(cf_public, df_public$dist_interval, cf_public$W.hat, df_public$Y)
gate_dist_NA_total <-     group_ate_prop_relative_total(cf_NA, df_NA$dist_interval, cf_NA$W.hat, df_NA$Y)
```

```{r}
df_NA$grassland_factor <- df_NA$grassland + 1
df_NA$grassland_factor <- as.factor(df_NA$grassland_factor)
```

```{r}
df$JahrFactor <- as.factor(df$Jahr)
```

```{r}
gate_year_all <-    group_ate_prop_relative_total(cf, df$JahrFactor, cf$W.hat, df$Y)
gate_year_all
```

```{r}
gate_year_all$lower_ci= gate_year_all$estimate - signif.level * gate_year_all$std.err
gate_year_all$upper_ci= gate_year_all$estimate + signif.level * gate_year_all$std.err
```

```{r}
p <- ggplot(gate_year_all, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Year") +
  ggtitle("Estimated GATE per treatment for different years") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  scale_y_continuous(labels = percent_format())  # Add this line to format the y-axis as percentages 
  

ggsave("gates_year.png", width = 14, height =8)
```

### Quintile GATEs

```{r}
# Assume df is your data frame with columns 'treatment', 'outcome', and features
# X is a list containing the names of the features in df
X_covariate <- c("FLAC", # Area
                      "soilquality",
                          "hog_share",          
                          "cattle_share", 
                      
                          #"DISTANCE_autobahn",
                          "DISTANCE_ausfahrt",
                          
                          ### General indicators
                          #"Flaeche",
                          "Einwohnerqkm",
                          "Alter",
                          
                          "GebGesDiff_share", 
                          "Wandergewinn_share",
                          
                          "agrar_share",
                          
                          "Freizeitflaeche_share",
                          "Industrieflaeche_share",
                          "Strassenflaeche_share",
                          "Wohnbauflaeche_share",
                          
                          ### renewable energy indicator###
                          "planned_biomass_power",
                          "planned_wind_power"#,
                          
                                             ) 

# Function to compute the propensity scores
propensity_score_function <- function(forest) {
  W <- forest$W.orig
  X <- forest$X.orig
  return(predict(forest, X_covariate, estimate.variance = FALSE)[, 1] / W)
}

# Calculate propensity scores
pscores_cfNA <- cf_NA$W.hat

# Iterate through each feature in X and estimate group ATE for each quintile
results_quint <- lapply(X_covariate, function(feature) {
  # Calculate quintiles for the current feature
  df_NA$quintile_group <- ntile(df_NA[[feature]], 5)
  
  # Estimate group ATE for each quintile using the group_ate_prop function
  group_ate_quintile <- group_ate_prop_relative_total(cf_NA, df_NA$quintile_group, pscores_cfNA, df_NA$Y)
  group_ate_quintile$feature <- feature
  
  return(group_ate_quintile)
})

# Combine results into a single data frame
results_quint_df <- do.call(rbind, results_quint)
```

```{r}
results_quint_df$lower_ci= results_quint_df$estimate - signif.level * results_quint_df$std.err
results_quint_df$upper_ci= results_quint_df$estimate + signif.level * results_quint_df$std.err
```

```{r}
write_csv(results_quint_df,'/directory/results_quint_df_NA.csv')
```

```{r}
## plot for quintile GATEs ##

p <- ggplot(results_quint_df, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Groups") +
  ggtitle("Estimated GATE per treatment for different quintile groups") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ feature) +
  scale_y_continuous(labels = percent_format())  # Add this line to format the y-axis as percentages 
  

ggsave("gates_quint_NA.png", width = 14, height =8)
```

```{r}
## binary GATEs ##

X_bin <- c(     #### Seller types ####
                          "VERL_1",
                          "VERL_2",
                          "VERL_3",
                          "VERL_4",  ## commenting out due to dummy variable trap
                          ### Grundstuecksarten
                          "grassland",
                          "circumstances_1", 
                          "circumstances_2",
                          "circumstances_3",
                          "circumstances_4",
                          "circumstances_5",
                          "circumstances_6",
                          "circumstances_0",
                          "circumstances_8"                                             ) 

# Function to compute the propensity scores
propensity_score_function <- function(forest) {
  W <- forest$W.orig
  X <- forest$X.orig
  return(predict(forest, X_bin, estimate.variance = FALSE)[, 1] / W)
}

# Calculate propensity scores
pscores_cf <- cf$W.hat

# Iterate through each feature in X and estimate group ATE for each binary group
results_bin <- lapply(X_bin, function(feature) {
  # Use the binary variable as the group
  binary_group <- df[[feature]] + 1 # Adding 1 to change group index from 0 and 1 to 1 and 2
  df$binary_group <- binary_group
  
  # Estimate group ATE for each binary group using the group_ate_prop function
  group_ate_binary <- group_ate_prop_relative_total(cf, df$binary_group, pscores_cf, df$Y)
  group_ate_binary$feature <- feature
  
  return(group_ate_binary)
})

# Combine results into a single data frame
results_bin_df <- do.call(rbind, results_bin)


```

```{r}
results_bin_df$lower_ci= results_bin_df$estimate - signif.level * results_bin_df$std.err
results_bin_df$upper_ci= results_bin_df$estimate + signif.level * results_bin_df$std.err
```

```{r}
write_csv(results_bin_df,'/directory/results_bin_df.csv')
```

```{r}
## binary gate plot ##
p <- ggplot(results_bin_df, aes(y = estimate, x = as.factor(interval)))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Groups") +
  ggtitle("Estimated GATE per treatment for different quintile groups") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ feature, scales = "free_y") +
  scale_y_continuous(labels = percent_format())  # Add this line to format the y-axis as percentages 
  

ggsave("gates_bin.png", width = 14, height =8)
```

```{r}
### creating data frame 
all_gates <- rbind(
gate_size_all    = gate_size_all    ,
gate_size_public = gate_size_public ,
gate_size_NA     = gate_size_NA     ,
gate_sq_all      = gate_sq_all      ,
gate_sq_public   = gate_sq_public   ,
gate_sq_NA       = gate_sq_NA       ,
gate_dist_all    = gate_dist_all    ,
gate_dist_public = gate_dist_public ,
gate_dist_NA     = gate_dist_NA     
  )

all_gates <- data.frame(all_gates)
all_gates$method <- rownames(all_gates)
rownames(all_gates) <- NULL

```

```{r}
all_gates$lower_ci= all_gates$estimate - signif.level * all_gates$std.err
all_gates$upper_ci= all_gates$estimate + signif.level * all_gates$std.err
```

```{r}
### creating data frame 
all_gates_total <- rbind(
gate_size_all    = gate_size_all_total    ,
gate_size_public = gate_size_public_total ,
gate_size_NA     = gate_size_NA_total     ,
gate_sq_all      = gate_sq_all_total      ,
gate_sq_public   = gate_sq_public_total   ,
gate_sq_NA       = gate_sq_NA_total       ,
gate_dist_all    = gate_dist_all_total    ,
gate_dist_public = gate_dist_public_total ,
gate_dist_NA     = gate_dist_NA_total     
  )

all_gates_total <- data.frame(all_gates_total)
all_gates_total$method <- rownames(all_gates_total)
rownames(all_gates_total) <- NULL
all_gates_total
```

```{r}
all_gates_total$lower_ci= all_gates_total$estimate - signif.level * all_gates_total$std.err
all_gates_total$upper_ci= all_gates_total$estimate + signif.level * all_gates_total$std.err
```

### Plots

```{r}
### arranging data set

all_gates_total$method <- substr(all_gates_total$method,1,nchar(all_gates_total$method)-2)
all_gates_total$set <- sapply(strsplit(all_gates_total$method, "_"),"[[",3)
all_gates_total$group <- sapply(strsplit(all_gates_total$method, "_"),"[[",2)
all_gates_total$interval <- as.factor(all_gates_total$interval)

all_gates_total$set <- replace(all_gates_total$set, all_gates_total$set == "all", "(1) Non-farmers")
all_gates_total$set <- replace(all_gates_total$set, all_gates_total$set == "public", "(3) Public non-farmers")
all_gates_total$set <- replace(all_gates_total$set, all_gates_total$set == "NA", "(2) Private non-farmers")

all_gates_total$set <- factor(all_gates_total$set,
                        levels = c("(1) Non-farmers", "(2) Private non-farmers", "(3) Public non-farmers"))
```

```{r}
### arranging data set

all_gates$method <- substr(all_gates$method,1,nchar(all_gates$method)-2)
all_gates$set <- sapply(strsplit(all_gates$method, "_"),"[[",3)
all_gates$group <- sapply(strsplit(all_gates$method, "_"),"[[",2)
all_gates$interval <- as.factor(all_gates$interval)

all_gates$set <- replace(all_gates$set, all_gates$set == "all", "(1) Non-farmers")
all_gates$set <- replace(all_gates$set, all_gates$set == "public", "(3) Public non-farmers")
all_gates$set <- replace(all_gates$set, all_gates$set == "NA", "(2) Private non-farmers")

all_gates$set <- factor(all_gates$set,
                        levels = c("(1) Non-farmers", "(2) Private non-farmers", "(3) Public non-farmers"))

```

```{r}
write_csv(all_gates, '/directory/all_gates.csv')

write_csv(all_gates_total, '/directory/all_gates_total.csv')
```

```{r}
### plots for each size gates

size_gates <- filter(all_gates, group == "size")

p <- ggplot(size_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Groups") +
  ggtitle("Estimated GATE per treatment for different size groups") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ set) +
  scale_y_continuous(labels = percent_format())  # Add this line to format the y-axis as percentages 
ggsave("size_gates.png", width = 7, height =4)

```

```{r}
### plots for each distance gates

allnonfarmers_gates <- filter(all_gates, set == "(1) Non-farmers")

allnonfarmers_gates$interval <- as.numeric(allnonfarmers_gates$interval)

# Create named vector for replacements
replacement_values <- c("size1" = "(1) <0.37 ha", "size2" = "(2) 0.37-0.85 ha", "size3" = "(3) 0.85-1.6 ha", "size4" = "(4) 1.6-3.1 ha", "size5" = "(5) >3.1 ha", "sq1" = "(1) <30", "sq2" = "(2) 30-35", "sq3" = "(3) 35-45", "sq4" = "(4) 45-62", "sq5" = "(5) >62", "dist1" = "(1) <4.7 km", "dist2" = "(2) 4.7-9.2 km", "dist3" = "(3) 9.2-14.8 km", "dist4" = "(4) 14.8-21.9 km", "dist5" = "(5) >21.9 km")


# Replace the interval values
allnonfarmers_gates <- allnonfarmers_gates %>%
  mutate(interval = as.character(interval),  # ensure interval is character
         group_interval = paste0(group, interval),  # create helper column
         group = group,
         interval = ifelse(group_interval %in% names(replacement_values), replacement_values[group_interval], interval),  # replace values
         group_interval = NULL)  # remove helper column

# Replace values in the group column
allnonfarmers_gates <- allnonfarmers_gates %>%
  mutate(group = case_when(
    group == "sq" ~ "(C) Soil quality",
    group == "dist" ~ "(B) Distance to highway exit",
    group == "size" ~ "(A) Parcel size",
    TRUE ~ group
  ))

```

```{r}
allnonfarmers_gates_total <- filter(all_gates_total, set == "(1) Non-farmers")

allnonfarmers_gates_total$interval <- as.numeric(allnonfarmers_gates_total$interval)

# Create named vector for replacements
replacement_values <- c("size1" = "(1) <0.37 ha", "size2" = "(2) 0.37-0.85 ha", "size3" = "(3) 0.85-1.6 ha", "size4" = "(4) 1.6-3.1 ha", "size5" = "(5) >3.1 ha", "sq1" = "(1) <30", "sq2" = "(2) 30-35", "sq3" = "(3) 35-45", "sq4" = "(4) 45-62", "sq5" = "(5) >62", "dist1" = "(1) <4.7 km", "dist2" = "(2) 4.7-9.2 km", "dist3" = "(3) 9.2-14.8 km", "dist4" = "(4) 14.8-21.9 km", "dist5" = "(5) >21.9 km")


# Replace the interval values
allnonfarmers_gates_total <- allnonfarmers_gates_total %>%
  mutate(interval = as.character(interval),  # ensure interval is character
         group_interval = paste0(group, interval),  # create helper column
         group = group,
         interval = ifelse(group_interval %in% names(replacement_values), replacement_values[group_interval], interval),  # replace values
         group_interval = NULL)  # remove helper column

# Replace values in the group column
allnonfarmers_gates_total <- allnonfarmers_gates_total %>%
  mutate(group = case_when(
    group == "sq" ~ "(C) Soil quality",
    group == "dist" ~ "(B) Distance to highway exit",
    group == "size" ~ "(A) Parcel size",
    TRUE ~ group
  ))
```

```{r}
privatenonfarmers_gates_total <- filter(all_gates_total, set == "(2) Private non-farmers")

privatenonfarmers_gates_total$interval <- as.numeric(privatenonfarmers_gates_total$interval)

# Create named vector for replacements
replacement_values_private <- c("size1" = "(1) <0.38 ha", "size2" = "(2) 0.37-0.84 ha", "size3" = "(3) 0.84-1.5 ha", "size4" = "(4) 1.5-3.0 ha", "size5" = "(5) >3.0 ha", "sq1" = "(1) <30", "sq2" = "(2) 30-35", "sq3" = "(3) 35-44", "sq4" = "(4) 44-61", "sq5" = "(5) >61", "dist1" = "(1) <4.7 km", "dist2" = "(2) 4.7-9.2 km", "dist3" = "(3) 9.2-14.8 km", "dist4" = "(4) 14.8-21.9 km", "dist5" = "(5) >21.9 km")


# Replace the interval values
privatenonfarmers_gates_total <- privatenonfarmers_gates_total %>%
  mutate(interval = as.character(interval),  # ensure interval is character
         group_interval = paste0(group, interval),  # create helper column
         group = group,
         interval = ifelse(group_interval %in% names(replacement_values_private), replacement_values_private[group_interval], interval),  # replace values
         group_interval = NULL)  # remove helper column

# Replace values in the group column
privatenonfarmers_gates_total <- privatenonfarmers_gates_total %>%
  mutate(group = case_when(
    group == "sq" ~ "(C) Soil quality",
    group == "dist" ~ "(B) Distance to highway exit",
    group == "size" ~ "(A) Parcel size",
    TRUE ~ group
  ))
```

```{r}
publicnonfarmers_gates_total <- filter(all_gates_total, set == "(3) Public non-farmers")

publicnonfarmers_gates_total$interval <- as.numeric(publicnonfarmers_gates_total$interval)

# Create named vector for replacements
replacement_values_public <- c("size1" = "(1) <0.15 ha", "size2" = "(2) 0.15-0.57 ha", "size3" = "(3) 0.57-1.5 ha", "size4" = "(4) 1.5-3.1 ha", "size5" = "(5) >3.1 ha", "sq1" = "(1) <31", "sq2" = "(2) 31-38", "sq3" = "(3) 38-52", "sq4" = "(4) 52-68", "sq5" = "(5) >68", "dist1" = "(1) <4.7 km", "dist2" = "(2) 4.7-9.2 km", "dist3" = "(3) 9.2-14.9 km", "dist4" = "(4) 14.9-22.1 km", "dist5" = "(5) >22.1 km")


# Replace the interval values
publicnonfarmers_gates_total <- publicnonfarmers_gates_total %>%
  mutate(interval = as.character(interval),  # ensure interval is character
         group_interval = paste0(group, interval),  # create helper column
         group = group,
         interval = ifelse(group_interval %in% names(replacement_values_public), replacement_values_public[group_interval], interval),  # replace values
         group_interval = NULL)  # remove helper column

# Replace values in the group column
publicnonfarmers_gates_total <- publicnonfarmers_gates_total %>%
  mutate(group = case_when(
    group == "sq" ~ "(C) Soil quality",
    group == "dist" ~ "(B) Distance to highway exit",
    group == "size" ~ "(A) Parcel size",
    TRUE ~ group
  ))
```

```{r}

p <- ggplot(allnonfarmers_gates , aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  #geom_vline(aes(xintercept=rct_lower),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  #geom_vline(aes(xintercept=rct_upper),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_all["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ group, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("allnonfarmers_gates.png", width = 7, height =4)
ggsave('allnonfarmers_gate.tiff', dpi = 300, width = 6, height =4)
```

```{r}
p <- ggplot(allnonfarmers_gates_total, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  #geom_vline(aes(xintercept=rct_lower),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  #geom_vline(aes(xintercept=rct_upper),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_all["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ group, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("allnonfarmers_gates_total.png", width = 7, height =4)
ggsave('allnonfarmers_gates_total.tiff', dpi = 300, width = 6, height =4)
```

```{r}
library(gridExtra)
```

```{r}
publicnonfarmers_graph <- ggplot(publicnonfarmers_gates_total, aes(y = estimate, x = interval))
publicnonfarmers_graph <- publicnonfarmers_graph + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_public["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  labs(title = "Public non-farmers") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ group, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

privatenonfarmers_graph <- ggplot(privatenonfarmers_gates_total, aes(y = estimate, x = interval))
privatenonfarmers_graph <- privatenonfarmers_graph + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_NA["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  labs(title = "Private non-farmers") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ group, scales = "free_x") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

subgroups_gate <- ggarrange(privatenonfarmers_graph, publicnonfarmers_graph, ncol = 1)
ggsave("subgroups_gate.png", width = 6, height = 8)
ggsave("subgroups_gate.tiff", dpi = 300, width = 6, height = 8)

print(subgroups_gate)
```

```{r}
### plots for each distance gates

dist_gates <- filter(all_gates, group == "dist")

p <- ggplot(dist_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  #geom_vline(aes(xintercept=rct_lower),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  #geom_vline(aes(xintercept=rct_upper),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Groups") +
  scale_y_continuous(labels = percent_format())  +
  ggtitle("Estimated GATE per treatment for different distance to highway exit groups") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ set) 
ggsave("dist_gates.png", width = 7, height =4)
ggsave('dist_gates.tiff',dpi = 300, width = 7, height =4)
```

```{r}
### plots for each sq gates

sq_gates <- filter(all_gates, group == "sq")

p <- ggplot(sq_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  #geom_vline(aes(xintercept=rct_lower),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  #geom_vline(aes(xintercept=rct_upper),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  geom_hline(aes(yintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "Estimated GATE as % of mean sales price") +
  labs(x = "Groups") +
  scale_y_continuous(labels = percent_format())   +
  ggtitle("Estimated GATE per treatment for different soil quality groups") +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  facet_wrap( ~ set) 
ggsave("sq_gates.png", width = 7, height =4)
ggsave('sq_gates.tiff', dpi = 300, width = 7, height =4)
```

```{r}
### Wald test function ###

wald <- function(gate_test, group){
  .L <- cbind(-1, diag(n_distinct(group) - 1))
  waldtest_pvalue_aipw_ate <- wald.test(Sigma = diag(gate_test$std.err^2),
                                      b = gate_test$estimate,
                                      L = .L)$result$chi2[3]
  
  result <- ifelse(waldtest_pvalue_aipw_ate < 0.05, "H0 can be rejected", "H0 can NOT be rejected")
  P <- waldtest_pvalue_aipw_ate
  data.frame(result, P)
  
}
```

```{r}
### wald test

wald_gate_dist_NA       <- wald(gate_dist_NA    , df_NA$dist_interval)
wald_gate_size_all      <- wald(gate_size_all   , df$size_interval)
wald_gate_size_public   <- wald(gate_size_public, df_public$size_interval)
wald_gate_size_NA       <- wald(gate_size_NA    , df_NA$size_interval)
wald_gate_sq_all        <- wald(gate_sq_all     , df$sq_interval) 
wald_gate_sq_public     <- wald(gate_sq_public  , df_public$sq_interval)
wald_gate_sq_NA         <- wald(gate_sq_NA      , df_NA$sq_interval)
wald_gate_dist_all      <- wald(gate_dist_all   , df$dist_interval)
wald_gate_dist_public   <- wald(gate_dist_public, df_public$dist_interval)
wald_gate_dist_NA       <- wald(gate_dist_NA    , df_NA$dist_interval)

```

```{r}
all_walds <- rbind(
wald_gate_dist_NA    = wald_gate_dist_NA    ,
wald_gate_size_all   = wald_gate_size_all   ,
wald_gate_size_public= wald_gate_size_public,
wald_gate_size_NA    = wald_gate_size_NA    ,
wald_gate_sq_all     = wald_gate_sq_all     ,
wald_gate_sq_public  = wald_gate_sq_public  ,
wald_gate_sq_NA      = wald_gate_sq_NA      ,
wald_gate_dist_all   = wald_gate_dist_all   ,
wald_gate_dist_public= wald_gate_dist_public,
wald_gate_dist_NA    = wald_gate_dist_NA       
  )
```

```{r}
all_walds <- data.frame(all_walds)


all_walds$method <- rownames(all_walds)
rownames(all_walds) <- NULL


```

```{r}
write_csv(all_walds, '/directory/wald_results.csv')
```

## Group statistics

```{r}
# clean data frames for propensity scores < 0.1 and > 0.9
df_prop <- filter(df, e.hat >= 0.1 & e.hat <= 0.9)
df_prop_public <- filter(df_public, e.hat >= 0.1 & e.hat <= 0.9)
df_prop_NA <- filter(df_NA, e.hat >= 0.1 & e.hat <= 0.9)
```

```{r}
# grouped summarise for years for non-farmers

jahr_des <- df_prop %>% group_by(JahrFactor) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)

write_csv(jahr_des,'/directory/jahr_descriptive.csv')
```

```{r}
# grouped summarise for soil quality intervals for non-farmers

stats_sq <- df_prop %>% group_by(sq_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)

write_csv(stats_sq,'/directory/sq_descriptive.csv')
```

```{r}
# grouped summarise for distance intervals for non-farmers

stats_dist <- df_prop %>% group_by(dist_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
write_csv(stats_dist,'/directory/dist_descriptive.csv')
```

```{r}
# grouped summarise for size intervals for private non-farmers

stats_size <-  df_prop %>% group_by(size_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W),
  mean_volume = mean(FLAC*Y),
  share_VERL1 = mean(VERL_1),
  share_VERL2 = mean(VERL_2),
  share_VERL3 = mean(VERL_3)
)
write_csv(stats_size,'/directory/size_descriptive.csv')

stats_size 
```

```{r}
# grouped summarise for soil quality intervals for private non-farmers

df_prop_NA %>% group_by(sq_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)

```

```{r}
df_prop_NA %>% group_by(grassland) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

```{r}
# grouped summarise for distance intervals for private non-farmers

df_prop_NA %>% group_by(dist_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

```{r}
# grouped summarise for size intervals for private non-farmers

df_prop_NA %>% group_by(size_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

```{r}
# grouped summarise for soil quality intervals for public non-farmers

df_prop_public %>% group_by(sq_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

```{r}
# grouped summarise for distance intervals for public non-farmers

df_prop_public %>% group_by(dist_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

```{r}
# grouped summarise for size intervals for public non-farmers

df_prop_public %>% group_by(size_interval) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
)
```

### Correlation plot

```{r}
Xcor = cor(X_ATE)

corrplot(Xcor, method = "color", diag = FALSE)
```

```{r}
### data backup ###
df_stat <- subset(df, select=(c(colnames(X_ATE), "W", "Y")))
df_stat_ATE <- subset(df_ATE, select=(c(colnames(X_ATE), "W", "Y")))
des_NA <- subset(df_NA, select=(c(colnames(X_ATE), "W", "Y")))
des_public <- subset(df_public, select=(c(colnames(X_ATE), "W", "Y")))
des_NA_prop <- subset(df_prop_NA, select=(c(colnames(X_ATE), "W", "Y")))
des_public_prop <- subset(df_prop_public, select=(c(colnames(X_ATE), "W", "Y")))
write_csv(des_NA,'/directory/des_NA.csv')
write_csv(des_public,'/directory/des_public.csv')
write_csv(des_NA_prop,'/directory/des_NA_prop.csv')
write_csv(des_public_prop,'/directory/des_public_prop.csv')
write_csv(df_stat_ATE,'/directory/des_all_prop.csv')
write_csv(df_stat,'/directory/des_all.csv')
```

# 4. Shapley Values

```{r}
library("fastshap")
library(iml)
library(parallel)
library(pbapply)
```

```{r}
features_ATE <- subset(df_ATE, select=(colnames(X_ATE)))

write_csv(features_ATE, '/directory/features_ATE.csv')
```

```{r}
features_ATE <- subset(df_ATE, select=(colnames(X_ATE)))

predict.fun = function(obj, newdata) { 
  results <- predict(obj, newdata, estimate.variance = T)
  return(results[[1]])
}

predictor.cf.tuned <- Predictor$new(
  model = cf,
  data = features_ATE,
  predict.fun = predict.fun,
  class = "regression")

cl <- makeCluster(24)

clusterEvalQ(cl, {
  library(iml)
  library(grf)
  library(pbapply)
  extract.shap <- function(n) {
    foo <- Shapley$new(predictor.cf.tuned, x.interest = features_ATE[n,])
    return(foo$results$phi)
  }
})
clusterExport(cl, c("features_ATE", "predictor.cf.tuned"))


SHAP_ATE <- pblapply(1:nrow(df_ATE), function(i) extract.shap(i), cl = cl)
```

```{r}
# Convert the list of Shapley values to a data frame
SHAP_df <- do.call(rbind, SHAP_ATE)

# Assign column names to the SHAP_df data frame (excluding the intercept)
colnames(SHAP_df) <- colnames(features_ATE)

# Remove the intercept column from the SHAP_df data frame
SHAP_df_no_intercept <- SHAP_df

# Write the Shapley values to a CSV file
write.csv(SHAP_df_no_intercept, "/directory/shap_values_all_20230725.csv", row.names = FALSE)


```

```{r}
# Melt the SHAP dataframe to long format
library(reshape2)
SHAP_df_long <- melt(SHAP_df, id.vars = NULL)

# Select first 10000 rows from original dataset 
# (corresponding to the rows used for estimating Shapley values)
df_ATE_subset <- features_ATE

# Melt the subset of original dataframe to long format
df_ATE_subset_long <- melt(df_ATE_subset, id.vars = NULL)

# Merge the two long dataframes
merged_df <- cbind(SHAP_df_long, df_ATE_subset_long$value)
colnames(merged_df) <- c("id","variable_name", "shapley_value", "variable_value")
```

```{r}
write.csv(merged_df, "/directory/shap_values_merged_20230725.csv", row.names = FALSE)
```

# 4.1 Partial dependence plots

```{r}
variables_of_interest <- c("FLAC", "DISTANCE_ausfahrt", "soilquality", 'hog_share')
```

```{r}

# Create a partial dependence plot for each variable of interest
plots <- lapply(variables_of_interest, function(var) {
  # Compute the feature effects
  feature_effect <- FeatureEffect$new(predictor.cf.tuned, feature = var)
  
  # Plot the feature effects
  plot <- feature_effect$plot() +
    labs(x = var, y = "CATE") +
    theme_minimal()
  
  return(plot)
})

# Arrange the plots next to each other
do.call(gridExtra::grid.arrange, c(plots, ncol = length(plots)))
```

# 5. Robustness Checks

### Pseudo outcomce approach

```{r}
# first filter for control group #
pseudo_treatment_df <- filter(df, W == 0)

pseudo_treatment_df$W_pseudo <- ifelse(pseudo_treatment_df$ERWL == 1, 0, 1)

X_pseudo_treatment <- subset(pseudo_treatment_df, select = covariates_names) # covariates
Y_pseudo_treatment<- pseudo_treatment_df$Y # outcome

```

```{r}
cf_pseudo_treatment <- causal_forest(X = as.matrix(X_pseudo_treatment),
                    Y = Y_pseudo_treatment,
                    W = pseudo_treatment_df$W_pseudo,
                    tune.parameters = "all")
```

```{r}
average_treatment_effect(cf_pseudo_treatment, target.sample = "all", subset = !(cf_pseudo_treatment$W.hat <= 0.1 | cf_pseudo_treatment$W.hat >= 0.9))
```

```{r}
relative_average_treatment_effect(cf_pseudo_treatment, subset = !(cf_pseudo_treatment$W.hat <= 0.1 | cf_pseudo_treatment$W.hat >= 0.9), signif.level)
```

```{r}
pseudo_treatment_df$CATE_pred <- predict(cf_pseudo_treatment)$predictions

```

```{r}
relative_average_treatment_effect(cf_pseudo_treatment, subset = !(cf_pseudo_treatment$W.hat <= 0.1 | cf_pseudo_treatment$W.hat >= 0.9), signif.level)
```

### Random W variable

```{r}
X_randomW_all <- subset(df, select = covariates_names) # covariates
W_randomW_all <- sample(c(0, 1), size = length(df$W), replace = TRUE) # treatment 
Y_randomW_all <- df$Y # outcome
```

```{r}
cf_randomw_all <- causal_forest(X = as.matrix(X_randomW_all),
                    Y = Y_randomW_all ,
                    W = W_randomW_all,
                    tune.parameters = "all")
```

```{r}
tau.hat_all_randomw <- predict(cf_randomw_all )$predictions
```

```{r}
# Create a random treatment variable
W_random <- sample(c(0, 1), size = length(df$W), replace = TRUE)

# Fit the causal forest
cf_randomW <- causal_forest(X = as.matrix(df[covariates_names]),
                            Y = df$Y,
                            W = W_random,
                            tune.parameters = "all")

# Predict
tau.hat_randomW <- predict(cf_randomW)$predictions

```

```{r}

# Fit a linear regression model (without intercept) and calculate R2
lin_model_randomW <- lm(tau.hat_randomW  ~ 0 + tau.hat_all , data = data.frame(tau.hat_all, tau.hat_randomW))
rho_randomW <- cor(tau.hat_all, tau.hat_randomW)

# Plot the result
robust_randomW <- ggplot(data.frame(tau.hat_all, tau.hat_randomW), aes(y = tau.hat_randomW, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(slope = coef(lin_model_randomW), intercept = 0, color = "red", linetype = "dashed") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (random W)", x = "CATE", title = "(B) Random W") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_randomW), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_randomW , 3)), parse = TRUE)

print(robust_randomW)

```

### Random Y

```{r}
# Create a random outcome variable
Y_random <- rnorm(n = length(df$Y), mean = mean(df$Y), sd = sd(df$Y))

# Fit the causal forest
cf_randomY <- causal_forest(X = as.matrix(df[covariates_names]),
                            Y = Y_random,
                            W = df$W,
                            tune.parameters = "all")

# Predict
tau.hat_randomY <- predict(cf_randomY)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_randomY <- lm(tau.hat_randomY ~ 0 + tau.hat_all , data = data.frame(tau.hat_all, tau.hat_randomY))
rho_randomY <- cor(tau.hat_all, tau.hat_randomY)
```

```{r}
# Plot the result
robust_randomY <- ggplot(data.frame(tau.hat_all, tau.hat_randomY), aes(y = tau.hat_randomY, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_randomY), intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (random Y)", x = "CATE", title = "(A) Random Y") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_randomY), 3)), parse = TRUE, colour = "red")

print(robust_randomY)
```

### Random confounder U

```{r}
# Generate random confounders for each dataset
random_confounder_df <- rnorm(n = nrow(df))

# Add the random confounders to each dataset
df_with_confounder <- cbind(df, random_confounder = random_confounder_df)


# Update the covariates_names to include the random confounder
covariates_names_with_confounder <- c(covariates_names, "random_confounder")

# Create a new subset of each dataset with the added confounder
X_with_confounder <- subset(df_with_confounder, select = covariates_names_with_confounder)

```

```{r}
# Train causal forests for each dataset with random confounders
cf_with_confounder <- causal_forest(X = as.matrix(X_with_confounder),
                                    Y = df$Y,
                                    W = df$W,
                                    tune.parameters = "all")

# Get predictions for all three causal forests
tau.hat_with_confounder <- predict(cf_with_confounder)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_with_confounder <- lm(tau.hat_with_confounder  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_with_confounder))
rho_with_confounder <- cor(tau.hat_all, tau.hat_with_confounder)
```

```{r}
# Plot the result
robust_with_confounder <- ggplot(data.frame(tau.hat_all, tau.hat_with_confounder), aes(y = tau.hat_with_confounder, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_with_confounder), intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (random confounder)", x = "CATE", title = "(C) Random confounder") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_with_confounder), 3)), parse = TRUE, colour = "red") 

print(robust_with_confounder)

```

### Leave out most important feature

```{r}
# Calculate variable importance for each causal forest
importance_cf <- variable_importance(cf)

# Identify the most important feature for each causal forest
most_important_feature_cf <- covariates_names[which.max(importance_cf)]

# Create new subsets without the most important feature
X_no_most_important_cf <- subset(df, select = covariates_names[!covariates_names %in% most_important_feature_cf])

```

```{r}
print(most_important_feature_cf)
```

```{r}
# Create a data frame for visualization
importance_cf_df <- data.frame(
  Feature = covariates_names,
  Importance = importance_cf 
)

# Order the data frame by importance
importance_cf_df <- importance_cf_df[order(importance_cf_df$Importance, decreasing = TRUE), ]

# Named vector for renaming
rename_vec <- c("FLAC" = "Parcel size (ha)",
                "soilquality" = "Soil quality (index)",
                "grassland" = "Grassland (D)",
                "DISTANCE_ausfahrt" = "Distance to highway exit (km)",
                "Jahr" = "Year",
                "VERL_1" = "Seller - farmer (D)",
                "VERL_4" = "Seller - private non-farmer (D)",
                "VERL_3" = "Seller - public non-farmer (D)",
                "VERL_2" = "Seller - private entity (D)",
                "circumstances_0" = "None (D)",
                "circumstances_5" = "Additional purchase (D)",
                "circumstances_2" = "Family relation (D)",
                "circumstances_8" = "Tenant buyer (D)",
                "circumstances_4" = "Forced sale (D)",
                "circumstances_1" = "Collector's price (D)",
                "circumstances_3" = "Employment relation (D)",
                "circumstances_6" = "Alternative land (D)",
                "hog_share" = "Hog density (per 100 ha AA)",
                "cattle_share" = "Cattle density (per 100 ha AA)",
                "Einwohnerqkm" = "Population density (1/sqkm)",
                "Alter" = "Population's average age",
                "GebGesDiff_share" = "Net reproduction rate",
                "Wandergewinn_share" = "Migration rate",
                "agrar_share" = "Agricultural area (share)",
                "Freizeitflaeche_share" = "Leisure area (share)",
                "Industrieflaeche_share" = "Industrial area (share)",
                "Strassenflaeche_share" = "Street area (share)",
                "Wohnbauflaeche_share" = "Residential area (share)",
                "planned_biomass_power" = "Planned biomass (kW)",
                "planned_wind_power" = "Planned wind energy (kW)")

# Rename the features
importance_cf_df$Feature <- rename_vec[importance_cf_df$Feature]

# Visualize with ggplot
ggplot(importance_cf_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(Importance * 100, 2), "%")), hjust = -0.1, size = 3) +
  coord_flip() +
  labs(x = "", y = "Importance", title = "Variable importance") +
  theme_classic() + 
  scale_y_continuous(labels = percent, lim = c(0, 0.3)) +
  theme(axis.text.y = element_text(size = 8))  # reduce font size if necessary due to long feature names
ggsave("importance_plot.tiff", dpi = 300, width = 8, height = 6)

```

```{r}

# Train causal forests without the most important feature
cf_no_most_important <- causal_forest(X = as.matrix(X_no_most_important_cf),
                                      Y = df$Y,
                                      W = df$W,
                                      tune.parameters = "all")

```

```{r}
# Get predictions for all three causal forests without the most important feature
tau.hat_no_most_important <- predict(cf_no_most_important)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_no_most_important <- lm(tau.hat_no_most_important  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_no_most_important))
rho_no_most_important<- cor(tau.hat_all, tau.hat_no_most_important)
```

```{r}
# Create the plot for the df dataset without the most important feature
# Plot the result
robust_no_most_important <- ggplot(data.frame(tau.hat_all, tau.hat_no_most_important), aes(y = tau.hat_no_most_important, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_no_most_important), intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (without most important feature)", x = "CATE", title = "(D) Without most important feature") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_no_most_important), 3)), parse = TRUE, colour = "red") 

print(robust_no_most_important)
```

### Leave out group of confounders: Land use variables

```{r}
# Specify the features to leave out
features_to_remove <- c("agrar_share", "Freizeitflaeche_share", "Industrieflaeche_share", "Strassenflaeche_share", "Wohnbauflaeche_share")

# Create new subsets without the specified features
X_no_features <- subset(df, select = covariates_names[!covariates_names %in% features_to_remove])
```

```{r}
# Train causal forests without the specified features
cf_no_features <- causal_forest(X = as.matrix(X_no_features),
                                Y = df$Y,
                                W = df$W,
                                tune.parameters = "all")
```

```{r}
tau.hat_no_features <- predict(cf_no_features)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_no_features <- lm(tau.hat_no_features  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_no_features))
rho_no_features<- cor(tau.hat_all, tau.hat_no_features)

```

```{r}
# Plot the result
robust_no_features <- ggplot(data.frame(tau.hat_all, tau.hat_no_features), aes(y = tau.hat_no_features, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_no_features), intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (without group of features)", x = "CATE", title = "(E) Without group of features") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_no_features), 3)), parse = TRUE, colour = "red") 

print(robust_no_features)

```

### Target common cause variable Z

```{r}
# Create target common cause variable 
Z_common <- 5*df$W + df$Y / 2 + rnorm(n = length(df$Y), mean = 0, sd = 1)

df_Z <- df

df_Z$Z_common <- Z_common

```

```{r}
# Update the covariates_names to include the common cause
covariates_names_Z_common <- c(covariates_names, "Z_common")

# Create a new subset of each dataset with the added confounder
X_with_Z_common <- subset(df_Z, select = covariates_names_Z_common)
```

```{r}
# Train causal forests with common cause
cf_Z_common <- causal_forest(X = as.matrix(X_with_Z_common),
                                Y = df$Y,
                                W = df$W,
                                tune.parameters = "all")
```

```{r}
tau.hat_Z_common <- predict(cf_Z_common)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_Z_common <- lm(tau.hat_Z_common  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_Z_common))
rho_Z_common <- cor(tau.hat_all, tau.hat_Z_common)
```

```{r}
# Plot the result
robust_Z_common <- ggplot(data.frame(tau.hat_all, tau.hat_Z_common), aes(y = tau.hat_Z_common, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_Z_common), intercept = 0, color = "red", linetype = "dashed") +
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (common cause)", x = "CATE", title = "(F) Target common cause") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_Z_common), 3)), parse = TRUE, colour = "red") 

print(robust_Z_common)
```

```{r}
robustness_plots <- ggarrange(robust_randomY, robust_randomW, robust_with_confounder, robust_no_most_important, robust_no_features, robust_Z_common, ncol = 2, nrow = 3)
ggsave("robustness_plots.tiff", dpi = 300, height = 297, width = 210, units = "mm")
```

```{r}
print(robustness_plots)
```
