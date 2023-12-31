# Do non-farmers pay more for land than farmers?
**Lorenz Schmidt, Martin Odening and Matthias Ritter**

This repository is for the work "Do non-farmers pay more for land than farmers?" and contains the code for the paper.

**Abstract**: The rise in farmland prices in many parts of the world over the past decade has sparked extensive discussion about whether non-farm investors pay higher prices for farmland. This study uses a causal machine learning approach to quantify the potential price premium paid by non-farmers on a rich dataset of land transactions in Germany. By applying the causal forest method, we uncover the heterogeneity of price premiums and reveal substantial moderating effects of covariates. In particular, we find that the average positive price premium decreases with parcel size and increasing distance to a highway exit.

**Method**: Causal forest, SHAP values

**Code structure**:

renv-non-farmers.lock contains the environment configuration file

code-non-farmers.md contains the code for all calculations including data cleaning and robustness checks

non-farmers-shapley-plots.R contains the code for the SHAP value graphs in the paper

non-farmers-descriptive-stats.R contains the code for the descriptive statistics
