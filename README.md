# How much homework is too much 

## Study Introduction 

The TIMSS study collects information about school math and science achievement around the world, alongside a wealth of information about students, schools, and classroom practices. A recent analysis of the [Irish TIMSS data by Nathan McJames](https://doi.org/10.1016/j.learninstruc.2024.101968) suggests short daily homework is best for mathematics achievement.  This analysis uses Bayesian Causal Forests to account for both treatment heterogeneity and propensity to end up in the treatment group. We will repeat this analysis for the 2023 and 2019 NZ data, and also think about how it can/should be modified to be most relevant in the New Zealand context. 

## Data Source

Data for the 2023 TIMSS data can be found [here](https://timss2019.org/international-database/) and data for the 2019 TIMSS data can be found [here](https://timss2023.org/data/). 

## Code Scripts and Description 

compare_country_proportions.R - Codes for comparing the proportions of responses for categorical variables across any 2 countries that are available in the TIMSS international database 
TIMSS2019_NZ_analysis.R - Code from Nathan McJames study with slight modification to be more relevant in the New Zealand context
TIMSS2023_data_cleaning.R - Code for cleaning the 2023 TIMSS data 
VarianceCalculations.R - Code from [Nathan McJames study](https://github.com/Nathan-McJames/Homework-Paper/blob/main/VarianceCalculationsGitHub.R)
HomeworkModel.cpp - Source code from [Nathan McJames study](https://github.com/Nathan-McJames/Homework-Paper/blob/main/HomeworkModelGitHub.cpp) which might requires some modification to increase the number of treatment variables 

