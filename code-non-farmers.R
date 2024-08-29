## loading packages

# check if packages are installed and install them if not
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, fBasics, corrplot, psych, grf, rpart, rpart.plot, treeClust, remotes, readr, tidyr, tibble, knitr, ggplot2, haven, aod, evtree, estimatr, lmtest, sandwich, splines, reshape2, ggthemes, parallel, stargazer, fastDummies, viridis, NbClust, MASS, ggpubr, scales)




library(dplyr) # dplyr macht irgendwie Probleme
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
library(evtree)      # evolutionary learning of globally optimal trees (1.0-7)
library(estimatr)    # simple interface for OLS w/ robust std errors ()
library(lmtest)      # package for testing linear models
library(sandwich)    # package for robust standard errors
library(splines)     # package for splines
library(reshape2)    # package for reshaping data
library(ggthemes)    # updated ggplot themes
library(parallel)    # for parallel processing
library(stargazer)   # for latex output
library(fastDummies) # for creating dummy variables
library(viridis)     # for colorblind friendly plots
library(NbClust)     # for determining number of clusters
library(MASS)        # for stepAIC
library(ggpubr)      # for saving ggplots
library(scales)   # for scales

# remove all variables
rm(list = ls())

### set seed for reproducability ###
set.seed(100)

### load data ###

data <- read.csv('00_data/data_filtered_20231121.csv')
# sink for filter process information
sink("02_results/03_processinformation/filter_process.txt")

data$X <- NULL
cat("Number of observations in raw dataset:", nrow(data), "\n")



# filter for specific GRUA
cat("1. Before filter (GRUA 330):", nrow(data), "\n")
data <- filter(data, GRUA < 330)
cat("1. After filter (GRUA 330):", nrow(data), "\n")


### creating dummy variable for GRUA ###
data <-data %>%  mutate(grassland = ifelse(as.numeric(GRUA) >= 320 & as.numeric(GRUA) < 330, 1,0)) 
data <- data %>%  mutate(arableland = ifelse(as.numeric(GRUA) >= 310 & as.numeric(GRUA) < 320, 1,0)) 


### Soil quality indicator based on ACZA and GRZA ###
data <- data %>% 
  mutate(soilquality = ifelse(as.numeric(GRUA) >= 320 & as.numeric(GRUA) < 330, as.numeric(GRZA), as.numeric(ACZA)))

data <- data %>% 
  mutate(soilquality = coalesce(soilquality, GRZA))


# drop na soil quality 
cat("2. Before filter (soilqualty na):", nrow(data), "\n")
data <- data %>% drop_na(soilquality)
cat("2. After filter (soilquality na):", nrow(data), "\n")
sink()



################# FILTERING FOR TREATMENT EFFECT #####################

## creating dummy variable for treatment effect

data_ERWL_na <- data
data_ERWL_na$NA_ERWL <- ifelse(is.na(data_ERWL_na$ERWL) | is.na(data_ERWL_na$VERL), 1, 0)

data_ERWL_na$preissqm <- as.numeric(data_ERWL_na$preissqm)

data_ERWL_na <- data_ERWL_na %>% 
  filter(!is.infinite(preissqm))

# descriptives staistics with and withj ERWL information
desstat_vgl_ERWL_na <- data_ERWL_na %>% 
  group_by(NA_ERWL) %>%
  summarise(
    Anzahl = n(),
    Mean_preis = mean(preissqm, na.rm = TRUE) * 10000,
    Mean_flaeche = mean(FLAC, na.rm = TRUE) / 10000,
    Mean_jahr = mean(Jahr),
    Mean_sq = mean(soilquality)
  ) %>% write_csv('02_results/02_tables/01_destables/desstat_vgl_ERWL_na.csv')


# group by ERWE and provide information and export as csv
data %>% group_by(ERWE) %>% summarise(
  Anzahl = n()
) %>% write_csv('02_results/02_tables/01_destables/ERWE_information.csv')

sink("02_results/03_processinformation/filter_process.txt", append = TRUE)

# drop not available ERWL
cat("3. Before filter (ERWL NA):", nrow(data), "\n")
data <- data %>% filter(!is.na(ERWL))
cat("3. After filter (ERWL NA):", nrow(data), "\n")

# drop VERL na
cat("3. Before filter (VERL NA):", nrow(data), "\n")
data <- data %>% filter(!is.na(VERL))
cat("3. After filter (VERL NA):", nrow(data), "\n")

# filter out private entities
cat("4. Before filter (ERWL != 2):", nrow(data), "\n")
data <- data %>% filter(ERWL != 2)
cat("4. After filter (ERWL != 2):", nrow(data), "\n")

# filter out public buyers
cat("5. Before filter (ERWL != 3):", nrow(data), "\n")
data <- data %>% filter(ERWL != 3)
cat("5. After filter (ERWL != 3):", nrow(data), "\n")

sink()

### creating variable for buyer category 3, 4 and 5 --> non agricultural ###
data$NA_all <- ifelse(data$ERWL > 2 & data$ERWL < 6, 1, 0)

# providing information how many of each ERWE are NA_all
data %>% group_by(NA_all, ERWE) %>% summarise(
  Anzahl = n()
) %>% write_csv('02_results/02_tables/01_destables/NICHTLANDWIRTE_ERWE.csv')

sink("02_results/03_processinformation/filter_process.txt", append = TRUE)

# share of non farmer
cat("Share of non-farmers: ", sum(data$NA_all) / nrow(data), "\n")
cat(" Share of farmers: ", 1 - sum(data$NA_all) / nrow(data), "\n")

sink()

## Creating Dummy Variables for treatment effect ####

# Create dummy variable for buyer type
data <- dummy_cols(data, select_columns = "ERWL")

group_by(data, ERWL, .add = FALSE)  %>%
    summarise(Beobachtungen = n())  %>% 
        write_csv('02_results/02_tables/01_destables/ERWL_information.csv')


#### DUMMY VARIABLE FOR SELLER TYPE ####
### creating dummy for Verauesserer 
data <- dummy_cols(data, select_columns = "VERL")

# group by VERL and provide information and export as csv
group_by(data, VERL, .add = FALSE)  %>%
    summarise(Beobachtungen = n())  %>%
        write_csv('02_results/02_tables/01_destables/VERL_information.csv')


######################### END OF FILTERING FOR TREATMENT EFFECT #####################

sink("02_results/03_processinformation/filter_process.txt", append = TRUE)
# filter for low and high soil quality
cat("10. before filter (soilquality between 1 and 120):", nrow(data), "\n")
data <- filter(data, soilquality <= 120)
data <- filter(data, soilquality >= 1)
cat("10. after filter (soilquality between 1 and 120):", nrow(data), "\n")
sink()


sink("02_results/03_processinformation/filter_process.txt", append = TRUE)
##### FLITERING FOR MINIMUM SIZE ######
cat("6. Before filter (FLAC >= 100):", nrow(data), "\n")
data <- filter(data, FLAC >= 100)
cat("6. After filter (FLAC >= 100):", nrow(data), "\n")


## Calculating price per ha ###
data$priceha <- data$preissqm * 10000

# filter for minimum price
cat("7. Before filter (priceha >= 100):", nrow(data), "\n")
data <- filter(data, priceha >= 100)
cat("7. After filter (priceha >= 100):", nrow(data), "\n")
sink()

#### UNCOMMON CIRCUMSTANCES ####
### creating field for cicumstances ###
data$UNGEWO <- paste(data$UNGEAU, data$UNGEUS, sep = "")
data <- dummy_cols(data, select_columns = "UNGEWO")

### creating unique variables ##
data$circumstances <- ifelse(data$UNGEWO == 10, 10, data$UNGEUS)

### **Codierung "circumstances":**
### 0 = keine ungewöhnlichen oder persönlichen Verhältnisse\
### 1 = Liebhaberpreis\
### 2 = Verwandtschaftsverhältnis, Erbauseinandersetzung\
### 3 = Dienstverhältnis\
### 4 = Notverkauf, Insolvenz-Verfahren, Konkurs\
### 5 = Zukauf\
### 6 = Ersatzlandkauf\
### 7 = kommunales Bauland\
### 8 = Käufer = Mieter bzw. Pächter\
### 9 = für Auswertung ungeeignet\
### 10 = Ursache nicht bekannt

### dummy variable for circumstances ### 
data <- dummy_cols(data, select_columns = "circumstances")

sink("02_results/03_processinformation/filter_process.txt", append = TRUE)
# filter for circumstances 9
cat("8. Before filter (circumstances_9 == 0):", nrow(data), "\n")
data <- filter(data, circumstances_9 == 0)
cat("8. After filter (circumstances_9 == 0):", nrow(data), "\n")

# filter for circumstances 10
cat("9. Before filter (circumstances_10 == 0):", nrow(data), "\n")
data <- filter(data, circumstances_10 == 0)
cat("9. After filter (circumstances_10 == 0):", nrow(data), "\n")

sink()


## 1.1 Calculations:

### Calculation of relative indicators ###
data$Betrieb_rinder_int[is.na(data$Betrieb_rinder_int)] <- 0
data$Betrieb_schweine_int[is.na(data$Betrieb_schweine_int)] <- 0
data$Anzahl_rinder_int[is.na(data$Anzahl_rinder_int)] <- 0
data$Anzahl_schweine_int[is.na(data$Anzahl_schweine_int)] <- 0
data$installed_biomass_power[is.na(data$installed_biomass_power)] <- 0
data$planned_biomass_power[is.na(data$planned_biomass_power)] <- 0
data$installed_wind_power[is.na(data$installed_wind_power)] <- 0
data$planned_wind_power[is.na(data$planned_wind_power)] <- 0
data$Wandergewinn <- as.numeric(data$Wandergewinn)
data$Wandergewinn[is.na(data$Wandergewinn)] <- 0

# demographic change
data$GebGesDiff_share   <- as.numeric(data$GebGesDiff) / as.numeric(data$Bevoelkerung)
data$Wandergewinn_share <- as.numeric(data$Wandergewinn) / as.numeric(data$Bevoelkerung)

# calculating shares of different animal farms
data$hog_farms_share    <- data$Betrieb_schweine_int / data$Landwirtschaftsflaeche * 100
data$cattle_farms_share <- data$Betrieb_rinder_int / data$Landwirtschaftsflaeche * 100
data$hog_share          <- data$Anzahl_schweine_int / data$Landwirtschaftsflaeche * 100
data$cattle_share       <- data$Anzahl_rinder_int / data$Landwirtschaftsflaeche * 100

# calculating shares of different land use types
data$Freizeitflaeche_share  <- data$Freizeitflaeche  / data$Bodenflaeche
data$Industrieflaeche_share <- data$Industrieflaeche / data$Bodenflaeche
data$Strassenflaeche_share  <- data$Strassenflaeche  / data$Bodenflaeche
data$Wohnbauflaeche_share   <- data$Wohnbauflaeche   / data$Bodenflaeche

# from sqm to ha
data$FLAC <- data$FLAC / 10000
# from m to km
data$DISTANCE_ausfahrt <- data$DISTANCE_ausfahrt / 1000

# setting installed renewable enrgy power realtive to area 
data$planned_wind_power <- data$planned_wind_power / data$Bodenflaeche
data$planned_biomass_power <- data$planned_biomass_power / data$Bodenflaeche

# scaling years from 1 to 15
data$Jahr <- data$Jahr - 2004

# saving data
write.csv(data, '00_data/data_filtered.csv')


########### CALCULATIONS ############

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


covariates_names_des <- c("FLAC", # Area
                        "soilquality",
                        "Jahr", # Year
                        # Agricultural indicators ###
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
                        #### Seller types ###
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

# These are the covariates we'll use
covariates <- subset(data, select = covariates_names)
covariates_des <- subset(data, select = covariates_names_des)

# Extracting outcome and treatment variables
outcome <- data$priceha
treatment <- data$NA_all
ERWL <- data$ERWL

# Setting up the data, renaming columns
df <- data.frame(covariates,
                    W = treatment, Y = outcome, ERWL)

df_des <- data.frame(covariates_des,
                    W = treatment, Y = outcome, ERWL)


# print number of na values per column in final data frame

df %>% 
  summarise_all(~sum(is.na(.))) %>% pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "na_count"
  ) %>% arrange(desc(na_count)) %>% 
  write_csv('02_results/02_tables/na_count.csv')

sink("02_results/03_processinformation/filter_process.txt", append = TRUE)
# drop na values 
cat("11. Before filter (na):", nrow(df), "\n")
df <- df %>% drop_na()
cat("11. After filter (na):", nrow(df), "\n")
sink()

# also for descriptives
df_des <- df_des %>% drop_na()


### 1.2 Descriptive Statistics

## some regional stats ##
landkreis <- subset(data, select = 'KRS_Bezeichnung')

df_landkreis <- data.frame(covariates, landkreis, 
                  W = treatment, Y = outcome, ERWL)

df_landkreis <- df_landkreis %>% drop_na()

df_landkreis_grouped <- df_landkreis %>% group_by(KRS_Bezeichnung, W) %>%
  summarise(
    number = n(),
    price = mean(Y),
    flaeche = sum(FLAC)/10000)

df_landkreis_grouped$KRS_Bezeichnung <- stringi::stri_encode(df_landkreis_grouped$KRS_Bezeichnung, "UTF-8")


ggplot(df_landkreis_grouped , aes(y = number, x = KRS_Bezeichnung, 
    fill = as.factor(W))) +
  geom_bar(stat = 'identity') + xlab('Landkreis') + 
  ggtitle('Number of treated and untreated observations per Landkreis') +
  labs(fill = "Treatment (0 = untreated, 1 = treated)")  + theme(legend.position="bottom", axis.text.x = element_text(size = 8, angle = 45, hjust = 1))  # nolint
ggsave("02_results/01_graphs/observations_by_landkreis_treatment.png")


write.csv(df_landkreis, "02_results/02_tables/01_destables/df_landkreis.csv")


# making data frame with numeric data 
df <- as.data.frame(lapply(df, as.numeric))
covariates <- as.data.frame(lapply(covariates, as.numeric))

# make a dataframe containing summary statistics of interest

summ_stats_complete <- fBasics::basicStats(df_des)
summ_stats_complete <- as.data.frame(t(summ_stats_complete))

# rename some of the columns for convenience 

summ_stats_complete <- summ_stats_complete[c("nobs", "Mean", "Stdev", 
    "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
    rename("Lower quartile" = "1. Quartile", 
        "Upper quartile" = "3. Quartile") %>% 
            write.csv('02_results/02_tables/01_destables/descriptive_statistics_complete.csv')

### descriptive statistics for treated NA_all 

df_des_W <- filter(df_des, W == 1)

summ_stats_NA_all  <- fBasics::basicStats(df_des_W)
summ_stats_NA_all  <- as.data.frame(t(summ_stats_NA_all))

# rename some of the columns for convenience 

summ_stats_NA_all[c("nobs", "Mean", "Stdev", 
    "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
        rename("Lower quartile" = "1. Quartile", 
            "Upper quartile" = "3. Quartile") %>% 
                write.csv('02_results/02_tables/01_destables/descriptive_statistics_completeNA_all.csv')

### descriptive statistics for control 

df_des_0 <- filter(df_des, W == 0)

summ_stats_0  <- fBasics::basicStats(df_des_0)
summ_stats_0  <- as.data.frame(t(summ_stats_0))

# rename some of the columns for convenience 

summ_stats_0  <- summ_stats_0[c("nobs", "Mean", "Stdev", "Minimum", "1. Quartile", "Median", "3. Quartile", "Maximum")] %>%
  rename("Lower quartile" = "1. Quartile", "Upper quartile" = "3. Quartile")
write.csv(summ_stats_0, '02_results/02_tables/01_destables/descriptive_statistics_complete_control.csv')



### Treatment as factor ###

df_des$W <- as.factor(df_des$W)

### Observations per year ###

df_years <-  df_des %>%
   group_by(Jahr, W) %>%
   summarise(n = n(),
             price = mean(Y),
             flaeche = sum(FLAC)/10000)

### Observations per year ###
ggplot(df_years,                                      
       aes(y = n, x = Jahr, fill = W)) +
  geom_bar(stat = "identity",
           position = "stack") + xlab('Year') + 
  ggtitle('Number of treated and untreated observations per year') +
  labs(fill = "Treatment (0 = untreated, 1 = treated)")  + theme(legend.position="bottom")
ggsave("02_results/01_graphs/observations_by_year_treatment.png")

### Price per year ###
ggplot(df_years,                                      
       aes(y = price, x = Jahr)) +
  geom_line(stat = "identity",
           position = "dodge") + xlab('Year') + 
  ggtitle('Mean of price per ha grouped treated and untreated per year') +
  labs(color = "Treatment (0 = untreated, 1 = treated)")  + theme(legend.position="bottom")
ggsave("02_results/01_graphs/price_by_year_treatment.png")

### Area per year ###
ggplot(df_years,                                      
       aes(y = flaeche, x = Jahr, fill = W)) +
  geom_bar(stat = "identity",
           position = "stack") + xlab('Year') + 
  ggtitle('Sum of ha per ha grouped treated and untreated per year') +
  labs(color = "Treatment (0 = untreated, 1 = treated)")  + theme(legend.position="bottom")
ggsave("02_results/01_graphs/flaeche_by_year_treatment.png")



########## AVERAGE TREATMENT EFFECT ##########
#Create a matrix for covariates other than treatment(W) and outcome(Y)
X = subset(df, select = covariates_names)
#Create a vectors for the outcome variable (Y) and the treatment (W)
W = df$W # treatment
Y = df$Y # outcome

df_descriptive <- data.frame(W = W, Y = Y, X)
write_csv(df_descriptive, '02_results/02_tables/03_backuptables/dataframe_complete.csv')

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

tauhat_rct_all <- difference_in_means_rel(df, signif.level)

sink("02_results/03_processinformation/results_process.txt")
cat("Tauhat rct all:", tauhat_rct_all, "\n")
sink()

#We can also do this using a simple linear regression
tauhat_linear <- lm(Y~W, data=df)
tauhat_lm_coeff <- as.double(tauhat_linear$coefficient[2])
tauhat_lm_sdev <- as.double(sqrt(vcovHC(tauhat_linear)[2,2]))
tauhat_lm <- c(ATE = tauhat_lm_coeff, 
               lower_ci = tauhat_lm_coeff - signif.level*tauhat_lm_sdev, 
               upper_ci = tauhat_lm_coeff + signif.level*tauhat_lm_sdev)

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Tauhat linear:", tauhat_lm, "\n")
sink()


### Model as hedonic model with treatment as dummy variable ###
hedonic_covariate_names <- c(covariates_names, "W")

hedonic_ols <- function(dataset, covariates) {
  # Pulling relevant columns
  Y <- dataset$Y
  hedonic.frmla <- as.formula(paste("Y ~ ", paste(covariates, collapse= "+"))) 
  # OLS
  hedonic_model <- lm(hedonic.frmla, data = dataset)
  tau.hat = as.numeric(coef(hedonic_model)["W"])
  se.hat = as.numeric(sqrt(vcovHC(hedonic_model, type = "HC0")["W", "W"]))
  
  print(tau.hat)
  print(se.hat)
  print(signif.level)
  print(as.numeric(sqrt(vcovHC(hedonic_model, type = "HC0")["W", "W"])))
  summary(hedonic_model)
  c(ATE=tau.hat, 
  lower_ci = tau.hat - signif.level * se.hat, 
  upper_ci = tau.hat + signif.level * se.hat)
}

tauhat_hedonic_ols <- hedonic_ols(df, hedonic_covariate_names)

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Tauhat hedonic ols:", tauhat_hedonic_ols, "\n")
sink()


## 2.2 Estimation with inverse propensity weighting using logistic regression

# estimating propensity scores with logistic regression
p_logisic.frmla <- as.formula(paste("W ~ ", paste(covariates_names, collapse = "+")))
p_logistic.fit <- glm(p_logisic.frmla, data = df, family = binomial(link = "logit"))

df$p_logistic <- predict(p_logistic.fit, type = "response")

prop_all <- ggplot(df, aes(x = p_logistic, fill = as.factor(W))) +
  geom_histogram(bins=100, alpha = 0.5, position = "stack") +
  #geom_density(aes(x = p_cf), position = "identity") +
  xlim(0,1) +
  theme_classic() +
  geom_vline(aes(xintercept=0.1),color="black", linetype="dashed", size=0.3, alpha = 1) +
  geom_vline(aes(xintercept=0.9),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated propensity score") +
  ylab("Frequency") +
  ggtitle("Non-farmers - propensity scores logistic regression") +
  theme(text = element_text(size = 8)) + scale_fill_discrete(name = "", labels = c("Control group", "Treatment group"))
ggsave("02_results/01_graphs/prop_alltreatments_logistic.jpg", width = 8, height = 6, bg = "white")


# filter for propensity scores between 0.1 and 0.9
df_ate_logistic <- filter(df, p_logistic >= 0.1 & p_logistic <= 0.9)


## model logistic regression with IPW

ipw <- function(dataset, p){
  W <- dataset$W
  Y <- dataset$Y
  G <- ((Y*W)/p) - ((Y*(1-W))/(1-p))
  tau.hat <- mean(G)
  se.hat <- sqrt(var(G)/(length(G)-1))
  c(ATE = tau.hat, lower_ci = tau.hat - signif.level * se.hat, upper_ci = tau.hat + signif.level * se.hat)
  
}

tauhat_logistic_ipw <- ipw(df_ate_logistic, df_ate_logistic$p_logistic)
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Tauhat logistic ipw:", tauhat_logistic_ipw, "\n")
sink()

## 2.2.1 Estimation with augmented inverse propensity weighting (AIPW) using OLS and Logistic regression

aipw_ols <- function(dataset, p, covariates){
  # Pulling relevant columns
  Y <- dataset$Y
  hedonic.frmla <- as.formula(paste("Y ~ ", paste(covariates, collapse= "+"))) 
  # OLS
  cov_hed = subset(dataset, select = covariates)
  hedonic_model <- lm(hedonic.frmla, data = cov_hed)
  
  # predict on dataset as everyone was treated
  cov_hed_treatall <- cov_hed
  cov_hed_treatall$W <- 1
  treated_pred <- predict(hedonic_model, cov_hed_treatall)
  
  # predict on dataset as no one was treated
  cov_hed_treatnone <- cov_hed
  cov_hed_treatnone$W <- 0
  control_pred <- predict(hedonic_model, cov_hed_treatnone)
  
  # predict on actual status
  actual_pred <- predict(hedonic_model, cov_hed)
  G <- treated_pred - control_pred + 
    ((dataset$W - p) * (dataset$Y - actual_pred)) / (p * (1-p))
  
  tau.hat <- mean(G)
  se.hat <- sqrt(var(G) / (length(G)-1))

  c(ATE = tau.hat, 
    lower_ci = tau.hat - signif.level * se.hat, 
    upper_ci = tau.hat + signif.level * se.hat)
}

tauhat_ols_aipw <- aipw_ols(df_ate_logistic,
                            df_ate_logistic$p_logistic,
                            hedonic_covariate_names)

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Tauhat ols aipw:", tauhat_ols_aipw, "\n")
sink()

## calculate R2 for propensity scores (pseudo R2) and for outcome (R2)
# for hedonic model and logistic regression

# Pseudo R2 for Propensity Scores
observed_treatment <- df$W  # replace 'df$treatment' with your actual treatment variable
logit_model <- glm(observed_treatment ~ df$p_logistic, family = "binomial")
# Define the null model
null_model <- glm(observed_treatment ~ 1, family = "binomial")

# Calculate the log-likelihood of both models
logLik_full <- logLik(logit_model)
logLik_null <- logLik(null_model)

# Calculate pseudo R2
pseudo_r2 <- 1 - (logLik_full/logLik_null)  # null_model is the logistic model with only the intercept

# print pseudo R2 in sink
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Pseudo R2 for Propensity Scores (logistic regregression):", pseudo_r2, "\n")
sink()

## R2 for Outcome Model
# we need to fit the model first
hedonic_frmla <- as.formula(paste("Y ~ ", 
                                  paste(hedonic_covariate_names, collapse= "+"))) 
hedonic_model <- lm(hedonic_frmla, data = df)

observed_outcomes <- df$Y  # replace 'df$outcome' with your actual outcome variable
predicted_outcomes <- predict(hedonic_model)
sse <- sum((observed_outcomes - predicted_outcomes)^2)
sst <- sum((observed_outcomes - mean(observed_outcomes))^2)
r2_outcome_hedonic <- 1 - (sse/sst)

# print R2 in sink
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("R2 for Outcome Model (hedonic model):", r2_outcome_hedonic, "\n")
sink()



## 2.3 Causal forest

cf <- causal_forest(X = as.matrix(X),
                    Y = Y,
                    W = W,
                    tune.parameters = "all")

cf_tuning_param <- cf[["tuning.output"]][["params"]]
write.csv(cf_tuning_param,file = '02_results/02_tables/cf_param.csv', row.names = TRUE)

#### 2.3.1 Testing calibration of forests

cf_calib <- test_calibration(cf)
cf_calib_df <- as.data.frame(cf_calib[,])
cf_calib_df <- cbind(Variable = rownames(cf_calib_df), cf_calib_df)
write_csv(cf_calib_df, '02_results/02_tables/cf_calib_df.csv' )

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Causal forest calibration:", cf_calib, "\n")
sink()

### estimating

p_rf <- as.data.frame(cf$W.hat) %>% rename(p_cf='cf$W.hat')
df$e.hat <- p_rf
df$m.hat <- cf$Y.hat
tau.hat_all <- predict(cf)$predictions
df$cate <- tau.hat_all # tau(X) estimates

# estimate R2 for causal forest for propensity scores (pseudo R2) and for outcome (R2)

# Pseudo R2 for Propensity Scores
observed_treatment <- df$W  # replace 'df$treatment' with your actual treatment variable
logit_model <- glm(observed_treatment ~ p_rf$p_cf, family = "binomial")
# Define the null model
null_model <- glm(observed_treatment ~ 1, family = "binomial")

# Calculate the log-likelihood of both models
logLik_full <- logLik(logit_model)
logLik_null <- logLik(null_model)

# Calculate pseudo R2
pseudo_r2_causal <- 1 - (logLik_full/logLik_null)  # null_model is the logistic model with only the intercept

# print pseudo R2 in sink
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Pseudo R2 for Propensity Scores (causal forest):", pseudo_r2_causal, "\n")
sink()

# R2 for Outcome Model
observed_outcomes <- df$Y  # replace 'df$outcome' with your actual outcome variable
predicted_outcomes <- df$m.hat
sse <- sum((observed_outcomes - predicted_outcomes)^2)
sst <- sum((observed_outcomes - mean(observed_outcomes))^2)
r2_outcome_causal <- 1 - (sse/sst)

# print R2 in sink
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("R2 for Outcome Model (causal forest):", r2_outcome_causal, "\n")
sink()


df$mu.hat.0 <- df$m.hat - df$e.hat * df$cate
df$mu.hat.1 <- df$m.hat + (1- df$e.hat) * df$cate

df$aipw.scores <- df$cate + df$W /df$e.hat * (df$Y - df$mu.hat.1) - (1- df$W) / (1 - df$e.hat) * (df$Y - df$mu.hat.0)


### filter for low and high propensity scores and assign new covariate frames for ATE and GATE

df_ATE <- filter(df, e.hat >= 0.1 & e.hat <= 0.9)

X_ATE = subset(df_ATE, select = covariates_names)

df_ATE %>% group_by(as.factor(W)) %>% summarise(n = n(), mean_price = mean(Y))

### 2.3.2 Plotting Overlap for identification

## plotting geom density

ggplot(df, aes(x = e.hat$p_cf, fill = as.factor(W)))+
  geom_density(alpha = 0.7)
ggsave("02_results/01_graphs/prop_alltreatments_rf_basicdensity.jpg", 
      width = 8, height = 6, bg = "white")


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

ggsave("02_results/01_graphs/prop_alltreatments.jpg", 
        width = 8, height = 6, bg = "white")

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

ggsave("02_results/01_graphs/prop_alltreatments_density.jpg", 
        prop_all, width = 8, height = 6, bg = "white")


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

ggsave("02_results/01_graphs/prop_all.png", prop_all, width = 8, height = 4)
ggsave("02_results/01_graphs/prop_all.tiff", prop_all, dpi = 300, width = 8, height = 4)


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
  geom_abline(intercept = 0) + xlim( c(0,1)) + ylim( c(0,1)) +  ggthemes::theme_few()
ggsave("02_results/01_graphs/regfor_calibration.jpg", 
        rgf_spline, width = 8, height = 6, bg = "white")

# compare the log likelihoods (bigger is better)

loglik = mean(W * log(p_rf$p_cf) + (1 - W) * log(1 - p_rf$p_cf))

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Log likelihood:", loglik, "\n")
sink()


### 2.3.3 Augmented inverse propensity weighting with causal forest

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

# This approach is justified via the orthogonal moments argument
ate_cf_aipw_all <- average_treatment_effect(cf, target.sample = "all", subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9))
tauhat_rf_aipw_all <- relative_average_treatment_effect(cf, subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9), signif.level)

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Tauhat rf aipw all:", tauhat_rf_aipw_all, "\n")
sink()


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


absolute_ate_cf <- aipw_average_treatment_effect(cf, subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9), signif.level = signif.level)

all_estimators_abs <- rbind(
  #diff_means_all = tauhat_rct_all,
  #diff_means_public = tauhat_rct_public,
  #diff_means_NA = tauhat_rct_NA,
  "Hedonic regression - OLS" = tauhat_hedonic_ols,
  "IPW - logistic regression" = tauhat_logistic_ipw,
  "AIPW - logistic + OLS" = tauhat_ols_aipw,
  #Hedonic_ols_regression_public = tauhat_simple_ols_public,
  #Hedonic_ols_regression_NA = tauhat_simple_ols_NA,
  "AIPW - causal forest" = absolute_ate_cf
  )

all_estimators_abs <- data.frame(all_estimators_abs)
all_estimators_abs$method <- rownames(all_estimators_abs)
rownames(all_estimators_abs) = NULL
all_estimators_abs$ci_length <- all_estimators_abs$upper_ci - all_estimators_abs$lower_ci

write_csv(all_estimators_abs, '02_results/02_tables/all_estimators_abs.csv')


sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("ATE TMLE:", average_treatment_effect(cf, method = "TMLE", target.sample = "all", subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9)), "\n")
cat("ATE AIPW:", average_treatment_effect(cf, method = "AIPW", target.sample = "all", subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9)), "\n")
sink()

## 2.4 Summary

all_estimators <- rbind(
  diff_means_all = tauhat_rct_all,
  Hedonic_ols_regression_all = tauhat_hedonic_ols,
  #Hedonic_ols_regression_public = tauhat_simple_ols_public,
  #Hedonic_ols_regression_NA = tauhat_simple_ols_NA,
  "Non-farmer+public buyer" = tauhat_rf_aipw_all
  #"Non-farmer buyer" = tauhat_rf_aipw_NA,
  #"Public buyer" = tauhat_rf_aipw_public
  )

all_estimators <- data.frame(all_estimators)
all_estimators$method <- rownames(all_estimators)
rownames(all_estimators) = NULL
all_estimators$ci_length <- all_estimators$upper_ci - all_estimators$lower_ci

write_csv(all_estimators, '02_results/02_tables/all_estimators_1.csv')


################### CONDITIONAL AVERAGE TREATMENT EFFECT ####################

# Get predictions from forest fitted above.
tau.hat_all <- predict(cf)$predictions
df$cate <- tau.hat_all # tau(X) estimates

## calculating relative cates
df_ATE$cate_rel <- df_ATE$cate / df_ATE$Y

sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("CATE Summary:", summary(df_ATE$cate_rel), "\n")
sink()

prop_score <- cf$W.hat
prop_score_df <- as.data.frame(prop_score)
write_csv(prop_score_df, '02_results/02_tables/prop_score.csv' )

### 3.1 Treatment effect heterogeneity

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

ggsave("02_results/01_graphs/hist_cate_alltreatments.png", width = 8, height = 6)

hist_all 

hist_all <- ggplot(df_ATE, aes(cate)) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-10000,20000) +
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  theme(text = element_text(size = 12)) 

hist_all

ggsave("02_results/01_graphs/hist_cate_all.png", width = 10, height = 4)
ggsave("02_results/01_graphs/hist_cate_all.tiff", dpi = 300, width = 10, height = 4)

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

ggsave("02_results/01_graphs/hist_cate_rel_alltreatments.png", width = 8, height = 6)
hist_all


## Share of observation for cate_rel -100% and 100% ##
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Share of observations with cate above 10000 and below 20000:", 
    sum((df_ATE$cate >= -10000) & (df_ATE$cate <= 20000)) / length(df_ATE$cate), "\n")
# quantile of cate
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("Quantile of cate:", quantile(df_ATE$cate_rel, c(0.025, 0.975)), "\n")
sink()

# variable importance for all treatments

#var_imp <- c(variable_importance(cf))
#names(var_imp) <- covariates_names
#sorted_var_imp <- sort(var_imp, decreasing = TRUE)
#write_csv(sorted_var_imp, '02_results/02_tables/variable_importance.csv')

# plot variable importance
#var_imp_plot <- ggplot(data = data.frame(sorted_var_imp), aes(x = reorder(names(sorted_var_imp), sorted_var_imp), y = sorted_var_imp)) +
# geom_bar(stat = "identity", fill = "steelblue") +
# coord_flip() +
# theme_classic() +
# xlab("Variable") +
# ylab("Variable importance") +
# ggtitle("Variable importance for all treatments") +
# theme(text = element_text(size = 8))
#
#gsave("02_results/01_graphs/var_imp_alltreatments.png", width = 8, height = 6)
#
#ar_imp_plot

## best linear projection

# Best linear projection of the conditional average treatment effect on covariates
blp_cf <- best_linear_projection(cf, X[, -which(names(X) %in% c("VERL_1", "circumstances_0"))])

blp_cf_df <- as.data.frame(blp_cf[,])
blp_cf_df <- cbind(Variable = rownames(blp_cf_df), blp_cf_df)
write_csv(blp_cf_df, '02_results/02_tables/blp_cf.csv' )

blp_subset <- best_linear_projection(cf, A = X[, -which(names(X) %in% c("VERL_1", "circumstances_0"))], subset = !(cf$W.hat <= 0.1 | cf$W.hat >= 0.9))
blp_subset_df <- as.data.frame(blp_subset[,])
blp_subset_df  <- cbind(Variable = rownames(blp_subset_df ), blp_subset_df)
write_csv(blp_subset_df, '02_results/02_tables/blp_subset.csv' )


## 3.3 Heterogeneity across subgroups

### Heterogeneity across subgroups ###

# Manually creating subgroups of CATE 
num_tiles <- 4  # ntiles = CATE is above / below the median
df$ntile <- factor(ntile(df$cate, n=num_tiles))

## histogram soilquality ###

ggplot() +
  geom_histogram(data= df, aes(x = soilquality), bins = 100, alpha = 0.8) +
  xlim(0,100) +
  facet_wrap(~as.factor(ERWL), scales = "free")
ggsave("02_results/01_graphs/histogram_soilquality.png")


### Histogram of Size ###
ggplot() +
  geom_histogram(data= df, aes(x = FLAC), bins = 100) +
  xlim(0,30) 
ggsave("02_results/01_graphs/histogram_size.png")


### histogram distance autobahnausfahrt ###

ggplot() +
  geom_histogram(data= df, aes(x = DISTANCE_ausfahrt), bins = 100) +
  xlim(0,50000) +
  facet_wrap(~as.factor(ERWL), scales = "free")
ggsave("02_results/01_graphs/histogram_dist.png")


ggplot() +
  geom_histogram(data= df, aes(x = DISTANCE_ausfahrt), bins = 100) +
  geom_vline(aes(xintercept=2),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=4),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=8),color="red", linetype="dashed", size=0.8, alpha = 1) +
  geom_vline(aes(xintercept=16),color="red", linetype="dashed", size=0.8, alpha = 1) +
  xlim(0,40)


# Compute the quintiles in df_ATE
breaks_size <- quantile(df_ATE$FLAC, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_sq <- quantile(df_ATE$soilquality, probs = seq(0, 1, by = 0.2), na.rm = TRUE)
breaks_dist <- quantile(df_ATE$DISTANCE_ausfahrt, probs = seq(0, 1, by = 0.2), na.rm = TRUE)

# Extend the breaks
breaks_size[c(1, length(breaks_size))] <- c(-Inf, Inf)
breaks_sq[c(1, length(breaks_sq))] <- c(-Inf, Inf)
breaks_dist[c(1, length(breaks_dist))] <- c(-Inf, Inf)

### df for all treatments ##
df$size_interval <- cut(df$FLAC, breaks = breaks_size, labels = 1:5, include.lowest = TRUE)
df$sq_interval <- cut(df$soilquality, breaks = breaks_sq, labels = 1:5, include.lowest = TRUE)
df$dist_interval <- cut(df$DISTANCE_ausfahrt, breaks = breaks_dist, labels = 1:5, include.lowest = TRUE)


# Create list of data frames
breaks_list <- list(
  data.frame(Name = "breaks_size", Percentile = names(breaks_size), Value = breaks_size),
  data.frame(Name = "breaks_sq", Percentile = names(breaks_sq), Value = breaks_sq),
  data.frame(Name = "breaks_dist", Percentile = names(breaks_dist), Value = breaks_dist)
)

# Combine data frames into one
breaks_combined_df <- do.call(rbind, breaks_list)

# Reset row names
row.names(breaks_combined_df) <- NULL

write_csv(breaks_combined_df, "02_results/02_tables/GATE_breaks.csv")


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


### calculating group ate for all treatments ###
gate_size_all <-    group_ate_prop_relative(cf, df$size_interval, cf$W.hat, df$Y)
gate_sq_all <-      group_ate_prop_relative(cf, df$sq_interval, cf$W.hat, df$Y)
gate_dist_all <-    group_ate_prop_relative(cf, df$dist_interval, cf$W.hat, df$Y)

gate_size_all_total <-    group_ate_prop_relative_total(cf, df$size_interval, cf$W.hat, df$Y)
gate_sq_all_total <-      group_ate_prop_relative_total(cf, df$sq_interval, cf$W.hat, df$Y)
gate_dist_all_total <-    group_ate_prop_relative_total(cf, df$dist_interval, cf$W.hat, df$Y)

# jahr factor
df$JahrFactor <- as.factor(df$Jahr)

gate_year_all <-    group_ate_prop_relative_total(cf, df$JahrFactor, cf$W.hat, df$Y)
sink("02_results/03_processinformation/results_process.txt", append = TRUE)
cat("GATE relative to total mean price:", gate_year_all, "\n")
sink()

gate_year_all$lower_ci= gate_year_all$estimate - signif.level * gate_year_all$std.err
gate_year_all$upper_ci= gate_year_all$estimate + signif.level * gate_year_all$std.err

write_csv(gate_year_all, "02_results/02_tables/gate_year_all.csv")


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

ggsave("02_results/01_graphs/gates_year.png", width = 14, height =8)


#### 3.4.1 Quintile GATEs

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

# Function to compute the propensity scores for filtering propensity scores
# so that only observations with propensity scores between 0.1 and 0.9 are included 
# in the group ATE estimation
propensity_score_function <- function(forest) {
  W <- forest$W.orig
  X <- forest$X.orig
  return(predict(forest, X_covariate, estimate.variance = FALSE)[, 1] / W)
}

# Calculate propensity scores
pscores_cf <- cf$W.hat

# Iterate through each feature in X and estimate group ATE for each quintile
results_quint <- lapply(X_covariate, function(feature) {
  # Calculate quintiles for the current feature
  df$quintile_group <- ntile(df[[feature]], 5)
  
  # Estimate group ATE for each quintile using the group_ate_prop function
  group_ate_quintile <- group_ate_prop_relative_total(cf, df$quintile_group, pscores_cf, df$Y)
  group_ate_quintile$feature <- feature
  
  return(group_ate_quintile)
})

# Combine results into a single data frame
results_quint_df <- do.call(rbind, results_quint)


results_quint_df$lower_ci= results_quint_df$estimate - signif.level * results_quint_df$std.err
results_quint_df$upper_ci= results_quint_df$estimate + signif.level * results_quint_df$std.err

write_csv(results_quint_df,'02_results/02_tables/results_quint_df.csv')

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

  ggsave("02_results/01_graphs/gates_quint.png", width = 14, height =8)

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
                          "circumstances_8") 

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

results_bin_df$lower_ci= results_bin_df$estimate - signif.level * results_bin_df$std.err
results_bin_df$upper_ci= results_bin_df$estimate + signif.level * results_bin_df$std.err

write_csv(results_bin_df,'02_results/02_tables/results_bin_df.csv')

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
  

ggsave("02_results/01_graphs/gates_bin.png", width = 14, height =8)

### creating data frame 
all_gates <- rbind(
gate_size_all    = gate_size_all    ,
gate_sq_all      = gate_sq_all      ,
gate_dist_all    = gate_dist_all    
  )

all_gates <- data.frame(all_gates)
all_gates$method <- rownames(all_gates)
rownames(all_gates) <- NULL

all_gates$lower_ci= all_gates$estimate - signif.level * all_gates$std.err
all_gates$upper_ci= all_gates$estimate + signif.level * all_gates$std.err

### creating data frame 
all_gates_total <- rbind(
gate_size_all    = gate_size_all_total    ,
gate_sq_all      = gate_sq_all_total      ,
gate_dist_all    = gate_dist_all_total    
  )

all_gates_total <- data.frame(all_gates_total)
all_gates_total$method <- rownames(all_gates_total)
rownames(all_gates_total) <- NULL


all_gates_total$lower_ci= all_gates_total$estimate - signif.level * all_gates_total$std.err
all_gates_total$upper_ci= all_gates_total$estimate + signif.level * all_gates_total$std.err

### arranging data set

all_gates_total$method <- substr(all_gates_total$method,1,nchar(all_gates_total$method)-2)
all_gates_total$set <- sapply(strsplit(all_gates_total$method, "_"),"[[",3)
all_gates_total$group <- sapply(strsplit(all_gates_total$method, "_"),"[[",2)
all_gates_total$interval <- as.factor(all_gates_total$interval)

all_gates_total$set <- replace(all_gates_total$set, all_gates_total$set == "all", "Non-farmers")

### arranging data set

all_gates$method <- substr(all_gates$method,1,nchar(all_gates$method)-2)
all_gates$set <- sapply(strsplit(all_gates$method, "_"),"[[",3)
all_gates$group <- sapply(strsplit(all_gates$method, "_"),"[[",2)
all_gates$interval <- as.factor(all_gates$interval)

all_gates$set <- replace(all_gates$set, all_gates$set == "all", "Non-farmers")


### renaming treatment for graphs ##

write_csv(all_gates, '02_results/02_tables/all_gates.csv')

write_csv(all_gates_total, '02_results/02_tables/all_gates_total.csv')


#### 3.4.2 Plots

### plots for each size gates

size_gates <- filter(all_gates, group == "size")

p <- ggplot(size_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  #geom_vline(aes(xintercept=rct_lower),color="gray", linetype="dashed", size=1, alpha = 0.5) +
  #geom_vline(aes(xintercept=rct_upper),color="gray", linetype="dashed", size=1, alpha = 0.5) +
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
ggsave("02_results/01_graphs/size_gates.png", width = 7, height =4)


### one plot for all categories of gates
#
allnonfarmers_gates <- filter(all_gates, set == "Non-farmers")

allnonfarmers_gates$interval <- as.numeric(allnonfarmers_gates$interval)

## breaks_data dataframe for replacement values
breaks_data <- breaks_combined_df

# create category column based on Name column with sub (breaks_)

breaks_data$category <- gsub("breaks_", "", breaks_data$Name)

# round the values in Value column 1 decimal place
breaks_data$Value <- round(breaks_data$Value, 1)

library(purrr)

create_labels <- function(values, category) {
  labels <- character(length(values) - 1)
  for (i in seq_along(labels)) {
    # Check for Lower Bound
    if (values[i] == -Inf) {
      labels[i] <- paste0("(", i, ") <", values[i + 1])
    }
    # Check for Upper Bound
    else if (values[i + 1] == Inf) {
      labels[i] <- paste0("(", i, ") >", values[i])
    }
    # Middle Bound (Normal Interval)
    else {
      labels[i] <- paste0("(", i, ") ", values[i], "-", values[i + 1])
    }
  }

  # Append units (ha or km) as per category
  if (category == "size") labels <- paste0(labels, " ha")
  if (category == "dist") labels <- paste0(labels, " km")

  # Assign names to labels
  names(labels) <- paste0(category, seq_along(labels))
  labels
}


# Process dataframe to create replacement values
replacement_values <- breaks_data %>% 
  group_by(category) %>% 
  summarize(
    ranges = list(Value),
    .groups = 'drop'
  ) %>% 
  mutate(
    replacement = map2(ranges, category, ~create_labels(.x, .y))
  ) %>% 
  pull(replacement) %>% 
  unlist()

replacement_values

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


## create plot for each intervall and for type of group gates relative to total mean price
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
ggsave("02_results/01_graphs/allnonfarmers_gates.png", width = 7, height =4)
ggsave('02_results/01_graphs/allnonfarmers_gate.tiff', dpi = 300, width = 6, height =4)


## create plot for each intervall and for type of group gates relative to total mean price
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
ggsave("02_results/01_graphs/allnonfarmers_gates_total.png", width = 7, height =4)
ggsave('02_results/01_graphs/allnonfarmers_gates_total.tiff', dpi = 300, width = 6, height =4)


### plots for each distance gates

dist_gates <- filter(allnonfarmers_gates, group == "(B) Distance to highway exit")

p <- ggplot(dist_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_all["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("02_results/01_graphs/dist_gates.png", width = 7, height =4)
ggsave('02_results/01_graphs/dist_gates.tiff',dpi = 300, width = 7, height =4)


### plots for each soilquality gates

sq_gates <- filter(allnonfarmers_gates, group == "(C) Soil quality")

p <- ggplot(sq_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_all["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("02_results/01_graphs/soilquality_gates.png", width = 7, height =4)
ggsave('02_results/01_graphs/soilquality_gates.tiff',dpi = 300, width = 7, height =4)


### plots for size gates
size_gates <- filter(allnonfarmers_gates, group == "(A) Parcel size")

p <- ggplot(size_gates, aes(y = estimate, x = interval))
p + geom_point() +
  geom_errorbar(aes(ymax = upper_ci, ymin = lower_ci)) +
  geom_hline(aes(yintercept=0),color="black", size=0.3, alpha = 1) +
  geom_hline(aes(yintercept= tauhat_rf_aipw_all["ATE"]),color="blue", linetype="dashed", size=0.3, alpha = 1) +
  ggthemes::theme_few() +
  scale_color_viridis_d() +
  labs(y = "GATE as % of average sales price") +
  labs(x = "") +
  scale_y_continuous(labels = percent_format())  +
  theme(legend.position = "none") +
  theme(text = element_text(size = 10)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("02_results/01_graphs/size_gates.png", width = 7, height =4)
ggsave('02_results/01_graphs/size_gates.tiff',dpi = 300, width = 7, height =4)


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

### wald test
wald_gate_size_all      <- wald(gate_size_all   , df$size_interval)
wald_gate_sq_all        <- wald(gate_sq_all     , df$sq_interval) 
wald_gate_dist_all      <- wald(gate_dist_all   , df$dist_interval)

all_walds <- rbind(
wald_gate_size_all   = wald_gate_size_all   ,
wald_gate_sq_all     = wald_gate_sq_all     ,
wald_gate_dist_all   = wald_gate_dist_all  )

all_walds <- data.frame(all_walds)
all_walds$method <- rownames(all_walds)
rownames(all_walds) <- NULL
write_csv(all_walds, '02_results/02_tables/wald_results.csv')


#### 3.4.3 Group statistics


# clean data frames for propensity scores < 0.1 and > 0.9

df_prop <- filter(df, e.hat >= 0.1 & e.hat <= 0.9)
# grouped summarise for years for non-farmers

df_prop %>% group_by(JahrFactor) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
) %>% write_csv('02_results/02_tables/jahr_descriptive.csv')



# grouped summarise for soil quality intervals for non-farmers

stats_sq <- df_prop %>% group_by(sq_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
) %>% write_csv('02_results/02_tables/sq_descriptive.csv')

# grouped summarise for distance intervals for non-farmers
df_prop %>% group_by(dist_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W)
) %>% write_csv('02_results/02_tables/dist_descriptive.csv')

# grouped summarise for size intervals for private non-farmers
df_prop %>% group_by(size_interval, W) %>% summarise(
  mean_price = mean(Y),
  mean_area = mean(FLAC),
  total_area = sum(FLAC),
  number_trans = n(),
  share_treated = mean(W),
  mean_volume = mean(FLAC*Y),
  share_VERL1 = mean(VERL_1),
  share_VERL2 = mean(VERL_2),
  share_VERL3 = mean(VERL_3)
) %>% write_csv('02_results/02_tables/size_descriptive.csv')

## 3.5 Correlation plot


Xcor = cor(X_ATE)

corrplot(Xcor, method = "color", diag = FALSE)



### data backup ###
df_stat <- subset(df, select=(c(colnames(X_ATE), "W", "Y")))
df_stat_ATE <- subset(df_ATE, select=(c(colnames(X_ATE), "W", "Y")))

write_csv(df_stat_ATE,'02_results/02_tables/des_all_prop.csv')
write_csv(df_stat,'02_results/02_tables/des_all.csv')

# raise error
stop("Error message")













# 4. Shapley Values


library("fastshap")
library(iml)
library(parallel)
library(pbapply)







features_ATE <- subset(df_ATE, select=(colnames(X_ATE)))

write_csv(features_ATE, '02_results/02_tables/features_ATE.csv')



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



# Convert the list of Shapley values to a data frame
SHAP_df <- do.call(rbind, SHAP_ATE)

# Assign column names to the SHAP_df data frame (excluding the intercept)
colnames(SHAP_df) <- colnames(features_ATE)

# Remove the intercept column from the SHAP_df data frame
SHAP_df_no_intercept <- SHAP_df

# Write the Shapley values to a CSV file
write.csv(SHAP_df_no_intercept, "02_results/02_tables/shap_values_all_20230725.csv", row.names = FALSE)





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



write.csv(merged_df, "02_results/02_tables/shap_values_merged_20230725.csv", row.names = FALSE)



head(merged_df)


# 4.1 Partial dependence plots


variables_of_interest <- c("FLAC", "DISTANCE_ausfahrt", "soilquality", 'hog_share')




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

ggsave("02_results/01_graphs/partial_dependence.tiff", dpi = 300, width = 297, height = 210, units = "mm")






# 5. Robustness Checks

### 5.1 Pseudo outcomce approach


# first filter for control group #
pseudo_treatment_df <- filter(df, W == 0)

pseudo_treatment_df$W_pseudo <- ifelse(pseudo_treatment_df$ERWL == 1, 0, 1)
X_pseudo_treatment <- subset(pseudo_treatment_df, select = covariates_names) # covariates
Y_pseudo_treatment<- pseudo_treatment_df$Y # outcome


library(vcd)
table_pseudo <- table(pseudo_treatment_df$W_pseudo, pseudo_treatment_df$circumstances_8)
assocstats(table_pseudo)

sink("02_results/03_processinformation/robustnesschecks.txt", append = FALSE)
cat('Sum of pseudo treatment:', sum(pseudo_treatment_df$W_pseudo),"\n")
sink()


cf_pseudo_treatment <- causal_forest(X = as.matrix(X_pseudo_treatment),
                    Y = Y_pseudo_treatment,
                    W = pseudo_treatment_df$W_pseudo,
                    tune.parameters = "all")


sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE psuedo treatment:', average_treatment_effect(cf_pseudo_treatment, target.sample = "all", subset = !(cf_pseudo_treatment$W.hat <= 0.1 | cf_pseudo_treatment$W.hat >= 0.9)) ,"\n")
cat('Relative ATE psuedo treatment:', relative_average_treatment_effect(cf_pseudo_treatment, subset = !(cf_pseudo_treatment$W.hat <= 0.1 | cf_pseudo_treatment$W.hat >= 0.9), signif.level),"\n")
sink()


### 5.2 Random W variable

X_randomW_all <- subset(df, select = covariates_names) # covariates
W_randomW_all <- sample(c(0, 1), size = length(df$W), replace = TRUE) # treatment 
Y_randomW_all <- df$Y # outcome

cf_randomw_all <- causal_forest(X = as.matrix(X_randomW_all),
                    Y = Y_randomW_all ,
                    W = W_randomW_all,
                    tune.parameters = "all")

tau.hat_all_randomw <- predict(cf_randomw_all )$predictions

# Create a random treatment variable
W_random <- sample(c(0, 1), size = length(df$W), replace = TRUE)

# Fit the causal forest
cf_randomW <- causal_forest(X = as.matrix(df[covariates_names]),
                            Y = df$Y,
                            W = W_random,
                            tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE random W:', average_treatment_effect(cf_randomW, target.sample = "all", subset = !(cf_randomW$W.hat <= 0.1 | cf_randomW$W.hat >= 0.9)) ,"\n")
cat('Relative ATE random W:', relative_average_treatment_effect(cf_randomW, subset = !(cf_randomW$W.hat <= 0.1 | cf_randomW$W.hat >= 0.9), signif.level),"\n")
sink()

# Predict
tau.hat_randomW <- predict(cf_randomW)$predictions

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



### 5.3 Random Y


# Create a random outcome variable
Y_random <- rnorm(n = length(df$Y), mean = mean(df$Y), sd = sd(df$Y))

# Fit the causal forest
cf_randomY <- causal_forest(X = as.matrix(df[covariates_names]),
                            Y = Y_random,
                            W = df$W,
                            tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE random Y:', average_treatment_effect(cf_randomY, target.sample = "all", subset = !(cf_randomY$W.hat <= 0.1 | cf_randomY$W.hat >= 0.9)) ,"\n")
cat('Relative ATE random Y:', relative_average_treatment_effect(cf_randomY, subset = !(cf_randomY$W.hat <= 0.1 | cf_randomY$W.hat >= 0.9), signif.level),"\n")
sink()

# Predict
tau.hat_randomY <- predict(cf_randomY)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_randomY <- lm(tau.hat_randomY ~ 0 + tau.hat_all , data = data.frame(tau.hat_all, tau.hat_randomY))
rho_randomY <- cor(tau.hat_all, tau.hat_randomY)

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
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_randomY), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_randomY, 3)), parse = TRUE)

print(robust_randomY)


### 5.4 Random confounder U


# Generate random confounders for each dataset
random_confounder_df <- rnorm(n = nrow(df))

# Add the random confounders to each dataset
df_with_confounder <- cbind(df, random_confounder = random_confounder_df)

# Update the covariates_names to include the random confounder
covariates_names_with_confounder <- c(covariates_names, "random_confounder")

# Create a new subset of each dataset with the added confounder
X_with_confounder <- subset(df_with_confounder, select = covariates_names_with_confounder)

# Train causal forests for each dataset with random confounders
cf_with_confounder <- causal_forest(X = as.matrix(X_with_confounder),
                                    Y = df$Y,
                                    W = df$W,
                                    tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE random confounder:', average_treatment_effect(cf_with_confounder, target.sample = "all", subset = !(cf_with_confounder$W.hat <= 0.1 | cf_with_confounder$W.hat >= 0.9)) ,"\n")
cat('Relative ATE random confounder:', relative_average_treatment_effect(cf_with_confounder, subset = !(cf_with_confounder$W.hat <= 0.1 | cf_with_confounder$W.hat >= 0.9), signif.level),"\n")
sink()


# Get predictions for all three causal forests
tau.hat_with_confounder <- predict(cf_with_confounder)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_with_confounder <- lm(tau.hat_with_confounder  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_with_confounder))
rho_with_confounder <- cor(tau.hat_all, tau.hat_with_confounder)



# Plot the result
robust_with_confounder <- ggplot(data.frame(tau.hat_all, tau.hat_with_confounder), aes(y = tau.hat_with_confounder, x = tau.hat_all)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.2, size = 0.3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_abline(slope = coef(lin_model_with_confounder), intercept = 0, color = "red", linetype = "dashed") + # nolint: line_length_linter.
  coord_cartesian(xlim = c(-30000, 30000), ylim = c(-30000, 30000)) +
  labs(y = "CATE (random confounder)", x = "CATE", title = "(C) Random confounder") +
  theme_classic() +
  theme(text = element_text(size = 10)) +
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_with_confounder), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_with_confounder, 3)), parse = TRUE)

### 5.5 Leave out most important feature


# Calculate variable importance for each causal forest
importance_cf <- variable_importance(cf)
# Identify the most important feature for each causal forest
most_important_feature_cf <- covariates_names[which.max(importance_cf)]

# Create new subsets without the most important feature
X_no_most_important_cf <- subset(df, select = covariates_names[!covariates_names %in% most_important_feature_cf])


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
ggsave("02_results02_results/01_graphs/importance_plot.tiff", dpi = 300, width = 8, height = 6)
ggsave("02_results/01_graphs/importance_plot.png", width = 8, height = 6)




# Train causal forests without the most important feature
cf_no_most_important <- causal_forest(X = as.matrix(X_no_most_important_cf),
                                      Y = df$Y,
                                      W = df$W,
                                      tune.parameters = "all")


sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE no most important feature:', average_treatment_effect(cf_no_most_important, target.sample = "all", subset = !(cf_no_most_important$W.hat <= 0.1 | cf_no_most_important$W.hat >= 0.9)) ,"\n")
cat('Relative ATE no most important feature:', relative_average_treatment_effect(cf_no_most_important, subset = !(cf_no_most_important$W.hat <= 0.1 | cf_no_most_important$W.hat >= 0.9), signif.level),"\n")
sink()

# Get predictions for all three causal forests without the most important feature
tau.hat_no_most_important <- predict(cf_no_most_important)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_no_most_important <- lm(tau.hat_no_most_important  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_no_most_important))
rho_no_most_important<- cor(tau.hat_all, tau.hat_no_most_important)



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
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_no_most_important), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_no_most_important, 3)), parse = TRUE)


### 5.6 Leave out group of confounders: Land use variables


# Specify the features to leave out
features_to_remove <- c("agrar_share", "Freizeitflaeche_share", "Industrieflaeche_share", "Strassenflaeche_share", "Wohnbauflaeche_share")

# Create new subsets without the specified features
X_no_features <- subset(df, select = covariates_names[!covariates_names %in% features_to_remove])



# Train causal forests without the specified features
cf_no_features <- causal_forest(X = as.matrix(X_no_features),
                                Y = df$Y,
                                W = df$W,
                                tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE no features:', average_treatment_effect(cf_no_features, target.sample = "all", subset = !(cf_no_features$W.hat <= 0.1 | cf_no_features$W.hat >= 0.9)) ,"\n")
cat('Relative ATE no features:', relative_average_treatment_effect(cf_no_features, subset = !(cf_no_features$W.hat <= 0.1 | cf_no_features$W.hat >= 0.9), signif.level),"\n")
sink()


tau.hat_no_features <- predict(cf_no_features)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_no_features <- lm(tau.hat_no_features  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_no_features))
rho_no_features<- cor(tau.hat_all, tau.hat_no_features)




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
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_no_features), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_no_features, 3)), parse = TRUE)

### 5.7 Target common cause variable Z


# Create target common cause variable 
Z_common <- 5*df$W + df$Y / 2 + rnorm(n = length(df$Y), mean = 0, sd = 1)

df_Z <- df

df_Z$Z_common <- Z_common


# Update the covariates_names to include the common cause
covariates_names_Z_common <- c(covariates_names, "Z_common")

# Create a new subset of each dataset with the added confounder
X_with_Z_common <- subset(df_Z, select = covariates_names_Z_common)

# Train causal forests with common cause
cf_Z_common <- causal_forest(X = as.matrix(X_with_Z_common),
                                Y = df$Y,
                                W = df$W,
                                tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE common cause:', average_treatment_effect(cf_Z_common, target.sample = "all", subset = !(cf_Z_common$W.hat <= 0.1 | cf_Z_common$W.hat >= 0.9)) ,"\n")
cat('Relative ATE common cause:', relative_average_treatment_effect(cf_Z_common, subset = !(cf_Z_common$W.hat <= 0.1 | cf_Z_common$W.hat >= 0.9), signif.level),"\n")
sink()


tau.hat_Z_common <- predict(cf_Z_common)$predictions

# Fit a linear regression model (without intercept) and calculate R2
lin_model_Z_common <- lm(tau.hat_Z_common  ~ 0 + tau.hat_all, data = data.frame(tau.hat_all, tau.hat_Z_common))
rho_Z_common <- cor(tau.hat_all, tau.hat_Z_common)

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
  annotate("text", x = -20000, y = 25000, label = paste("beta == ", round(coef(lin_model_Z_common), 3)), parse = TRUE, colour = "red") #+  annotate("text", x = -20000, y = 21500, label = paste("rho == ", round(rho_Z_common, 3)), parse = TRUE)

robustness_plots <- ggarrange(robust_randomY, robust_randomW, robust_with_confounder, robust_no_most_important, robust_no_features, robust_Z_common, ncol = 2, nrow = 3)
ggsave("02_results/01_graphs/robustness_plots.tiff", dpi = 300, height = 297, width = 210, units = "mm")
ggsave("02_results/01_graphs/robustness_plots.png", height = 297, width = 210, units = "mm")


## 5.8 Unregulated vs. regulated plots

#We want to investigate potential differences between unregulated and regulated plot sales. 
#For this purpose we do descriptive statistics for two separate groups. 
#For plots smaller one hectare and larger one hectare. We first introduce a dummy variable to distinguish regulated and unregulated plots sales. Then we do a descriptive statistics for each group.


# making a dummy variable indicated if regualted or not
df$regulated <- ifelse(df$FLAC > 1, 1, 0)
# making it as factor
df$regulated <- as.factor(df$regulated)


# For estimating the possible effect of the regulation practice we split up the data 
#into two parts and estimate the average treatment effect for each part separrately. 
# Then we compare those separate estimation effects with group average treatment effects for the same parts. 


# creating two data frames
df_regulated <- filter(df, regulated == 1)
df_unregulated <- filter(df, regulated == 0)

cf_regulated <- causal_forest(X = as.matrix(df_regulated[covariates_names]),
                              Y = df_regulated$Y,
                              W = df_regulated$W,
                              tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE regulated:', average_treatment_effect(cf_regulated, target.sample = "all", subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9)) ,"\n")
cat('Relative ATE regulated:', relative_average_treatment_effect(cf_regulated, subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9), signif.level),"\n")
sink()


cf_unregulated <- causal_forest(X = as.matrix(df_unregulated[covariates_names]),
                              Y = df_unregulated$Y,
                              W = df_unregulated$W,
                              tune.parameters = "all")

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE unregulated:', average_treatment_effect(cf_unregulated, target.sample = "all", subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9)) ,"\n")
cat('Relative ATE unregulated:', relative_average_treatment_effect(cf_unregulated, subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9), signif.level),"\n")
sink()

aipw_average_treatment_effect(cf_regulated, subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9), signif.level = signif.level)


aipw_average_treatment_effect(cf_unregulated, subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9), signif.level = signif.level)


# group average treatment effect

df %>% group_by(regulated) %>% summarise(
  mean_aipw = mean(aipw.scores$p_cf)
)

relative_average_treatment_effect(cf_regulated, subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9), signif.level)

relative_average_treatment_effect(cf_unregulated, subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9), signif.level)

sink("02_results/03_processinformation/robustnesschecks.txt", append = TRUE)
cat('ATE unregulated:', average_treatment_effect(cf_unregulated, target.sample = "all", subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9)) ,"\n")
cat('Relative ATE unregulated:', relative_average_treatment_effect(cf_unregulated, subset = !(cf_unregulated$W.hat <= 0.1 | cf_unregulated$W.hat >= 0.9), signif.level),"\n")
cat('ATE regulated:', average_treatment_effect(cf_regulated, target.sample = "all", subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9)) ,"\n")
cat('Relative ATE regulated:', relative_average_treatment_effect(cf_regulated, subset = !(cf_regulated$W.hat <= 0.1 | cf_regulated$W.hat >= 0.9), signif.level),"\n")
sink()

# We can also combine the histograms of cate and see if the patterns are more or less similar if we assign different colours if there are regulated or not.
# get predictions of regulated and unregulated forest

cate_regulated <- cf_regulated$predictions
cate_regulated <- as.data.frame(cate_regulated)
cate_regulated$regulated <- "1"

cate_unregulated <- cf_unregulated$predictions
cate_unregulated <- as.data.frame(cate_unregulated)
cate_unregulated$regulated <- "0"

# do r bind

cate_reg <- rbind(cate_regulated, cate_unregulated)
# Histogram of CATEs for each treatment group

hist_reg <- ggplot(cate_reg, aes(V1, fill = as.character(regulated))) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-8000,15000) +
  ylim(0,3000)+
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  ggtitle("Separate causal forest") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Unregulated", "Regulated")) + theme(legend.position = "top")

ggsave("02_results/01_graphs/hist_cate_regulated_sep.png", width = 8, height = 6)

# Histogram of CATEs for each treatment group

hist_reg_com <- ggplot(df, aes(cate, fill = as.character(regulated))) +
  geom_histogram(bins = 100, alpha = 0.5) +
  xlim(-8000,15000) +
  ylim(0,3000)+
  theme_classic() +
  geom_vline(aes(xintercept=0),color="black", linetype="dashed", size=0.3, alpha = 1) +
  xlab("Estimated CATE in EUR per ha") +
  ylab("Frequency") +
  ggtitle("Combined causal forest") +
  theme(text = element_text(size = 8)) +
  scale_fill_discrete(name = "", labels = c("Unregulated", "Regulated")) + theme(legend.position = "top")

ggsave("02_results/01_graphs/hist_cate_regulated_combined.png", width = 8, height = 6)

hist_regulated <- ggarrange(hist_reg, hist_reg_com, nrow = 2, common.legend = TRUE)
hist_regulated

ggsave("02_results/01_graphs/hist_cate_regulated.png", width = 8, height = 6)

