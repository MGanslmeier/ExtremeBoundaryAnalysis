# DEFINE WORKING DIRECTORY
setwd("/Users/michaelganslmeier/Desktop/R_projects/lastRA/_2010_AmericanEconomicJournal_Macroeconomics_Obstfeld_MauriceShambaugh_Jay_CTaylor_Alan_M")
#.libPaths(c("/lustre/home/tjmsmga/Scratch/packinstall/libraries",.libPaths()))
pacman::p_load(plyr, dplyr, readxl, plm, parallel, tidyr, ggplot2, lmtest, multiwayvcov, foreign, haven, gtools, jsonlite, purrr)
rm(list = ls())


############################

# LOAD DATA AND RESHAPE
df <- read_dta("dataset/OST3datatoshare_temp.dta") %>%
  subset(., samp2 == 1 & emerging == 1) %>%
  mutate(unit = ifscode)

# DEFINE PARAMETERS
dep <- 'lnresM0'
fe_vars <- c('unit')
controls <- c('lnm2tomoney', 'peg', 'softpeg', 'newkopen2', 'lntradegdp', 'lpop', 'evol', 'lngdppercap')
free <- c("")
maxregN <- length(controls)
tablename <- 'table6_col1_col2_col3'

# LABEL VARIABLES
labels <- cbind(varname = c(dep, controls),
                label = c(dep, controls)) %>% 
  data.frame(., stringsAsFactors = F) %>%
  mutate(labeltext = paste(label, ' (', varname, ')', sep=''))

# STORE MODEL SPECIFICATIONS
param <- list(dep = dep, fe_vars = fe_vars, controls = controls, free = free, 
              maxregN = maxregN, labels = labels, tablename = tablename)

############################
############################

# EBA ESTIMATION
# Load Helper functions
source('../eba/1run_eba/0helper.R')

# Calculate Time for Estimations
sampleN <- 10
source('../eba/1run_eba/4calculateTime.R')

# Create Formulas
source('../eba/1run_eba/1formulaCreate.R')

# Run Model Estimations
source('../eba/1run_eba/2modelEstimate.R')

# Extract results
source('../eba/1run_eba/3results_extraction.R')
eba_results <- res_final %>% mutate(result_type = 'Estimation from EBA')

############################
############################

# PAPER ESTIMATION
# Create Formulas
formula_df <- c('lnresM0~lpop+lntradegdp+evol+lngdppercap',
                'lnresM0~lnm2tomoney+peg+softpeg+newkopen2+lntradegdp',
                'lnresM0~lnm2tomoney+peg+softpeg+newkopen2') %>% 
  data.frame(formula = ., stringsAsFactors = F)

# Run Model Estimations
source('../eba/1run_eba/2modelEstimate.R')

# Extract results
source('../eba/1run_eba/3results_extraction.R')
paper_results <- res_final %>% mutate(result_type = 'Estimation from Paper')

############################
############################

# STORE RESULTS
results <- list(results = rbind.fill(eba_results, paper_results), param = param)
filename <- paste('est_out/results_', tablename, '.RData', sep='')
save(results, file = filename)