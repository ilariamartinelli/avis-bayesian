# avis-bayesian

## Libraries that are needed:
- library(tidyverse)
- library(lubridate)
- library(ggplot2)
- library(rstan)
- library(coda)
- library(tidyr)
- library(dplyr)
- library(purrr)
- library(ggsci)
- library(bayesplot)
- library(rstanarm)
- require(gplots)
- require(ggpubr)
- library(arulesViz)

# READ_ME:
## Information about our scripts:

1. Our datasets has been created using:
- donazioni_full.RData
- time_dependent.RData


2. In our code we use the following datasets:
- avis.RData            -> Used for the scripts names 01_descriptive_analysis.R and 02_fit500_posterior_analysis.R
- avis_clean_plus.RData -> Used for the script 03_time_dep_qualitative_analysis.R
 

3. We fitted four models and we saved it with the following names:
- fit-500.RData   -> fit with only the first 500 donors
- fit_all.RData   -> fit with all the donors and all variables
- fit.RData       -> fit with all donors and only significant variables
- fit-fixed.RData -> fit of dott. Spinelli with only significant variables


4. We created our model in STAN with a time-dependent log-likelihood:
- time-dependent.stan


5. In the file 03_time_dep_qualitative_analysis.R we draw the matplots for our time-dependent variables


6. The key-part of our code is the file 04_the_script, in which we obtain:
- fit_all.RData
- fit.RData
- fit-fixed.RData
  
Some plots for the posterior analysis are present in the file just to see if our results were ok. 
A more detailed posterior analysis is performed in another script.


7. In the file 05_post_processing.R you can find all the posterior analysis of the fit obtained in the previous script.


# BRIEF OVERVIEW OF OUR PROJECT:
Our project consists in modeling recurrent events through time-dependent covariates.
In particular our starting point is the Thesis of Dott.Spinelli that worked on the AVIS batabases in order to
analyse the process of blood donation in the collection centre of Lambrate. 
In his thesis he worked only with time-fixed covariates, our goal is to extend the dataset including also 4 new
time-dependent covariates: heart rate, min and max pressure and hemoglobine.
After fitting our model we compare our results with dott.Spinelli's one.
We'll report here the most significant plots obtained in our scripts.

# Some significant plots:

