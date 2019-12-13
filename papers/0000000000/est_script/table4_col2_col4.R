# DEFINE WORKING DIRECTORY
setwd("/Users/michaelganslmeier/Desktop/R_projects/lastRA/_2010_AmericanEconomicJournal_Macroeconomics_Obstfeld_MauriceShambaugh_Jay_CTaylor_Alan_M")
#.libPaths(c("/lustre/home/tjmsmga/Scratch/packinstall/libraries",.libPaths()))
pacman::p_load(plyr, dplyr, readxl, plm, parallel, tidyr, ggplot2, lmtest, multiwayvcov, foreign, haven, gtools, jsonlite, purrr)
rm(list = ls())


############################

# LOAD DATA AND RESHAPE
df <- read_dta("dataset/OST3datatoshare_temp.dta") %>%
  subset(., year>1979 & emerging == 1) %>%
  mutate(unit = ifscode)

# DEFINE PARAMETERS
dep <- 'lnresgdp'
fe_vars <- c('unit')
controls <- c('newkopen2', 'peg', 'softpeg', 'lnm2gdp', 'lntradegdp', 'sin1', 'lnforcurbngdp', 'FXAGG_no_FXR')
free <- c("")
maxregN <- length(controls)
tablename <- 'table4_col2_col4'

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
formula_df <- c('lnresgdp~newkopen2+peg+softpeg+lnm2gdp+lntradegdp+sin1+lnforcurbngdp',
                'lnresgdp~newkopen2+peg+softpeg+lnm2gdp+lntradegdp+FXAGG_no_FXR') %>% 
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
