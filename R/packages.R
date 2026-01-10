# load in packages
library(discrim)      # naive Bayes
library(dplyr)        # For Data Processing
library(DT)           # For pretty markdown tables
library(doParallel)    # For parallel processing
library(forcats)      # For categorical variables
library(furrr)        # For parallelization
library(future)       # For parallelization
library(fastDummies)  # For one-hot coding
library(forcats)      # For one-hot coding
library(ggplot2)      # For Visualizing in ggplots
library(ggpubr)       # For arranging plots
library(glmnet)       # For lasso
library(hardhat)      # For extracting model objects
#library(kableExtra)   # For Table formatting
#library(kernlab)      # For ML algorithms
library(magrittr)     # For Piping
#library(mice)         # For amputing missing data values
library(naivebayes)   # For naive bayes ML algorithm
library(naniar)       # For missing data
library(parsnip)      # Framework to put model through
library(psych)        # Functions for data descriptions
library(purrr)        # For mapping functions
library(ranger)       # For random forest algorithm
library(Rcpp)         # Supporting package
library(reshape2)     # Supporting package
#library(renv)         # For package version stability
library(tidymodels)   # For Tidy Model framework
#library(vip)          # For Variable Importance calculation and plots
  # no longer found at CRAN???
library(visdat)       # For visualizing missingness
library(wesanderson)  # For plotting
library(xgboost)      # For xgboost algorithm
library(yardstick)    # For model results

bank <- read.csv2("/Users/kristinporter/Documents/NCSU/my_dbt/seeds/bank-full.csv", sep=";")  # read with semicolon
write.csv(bank, "/Users/kristinporter/Documents/NCSU/my_dbt/seeds/bank-full.csv", row.names=FALSE)