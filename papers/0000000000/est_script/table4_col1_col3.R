# DEFINE WORKING DIRECTORY
setwd("~/Desktop/R_projects/lastRA/ExtremeBoundaryAnalysis/papers/0000000000")
rm(list = ls())
source('../../src/1run_eba/0helper.R')

############################

# LOAD DATA AND RESHAPE
df <- read_dta("dataset/OST3datatoshare_temp.dta") %>%
  subset(., year>1979) %>%
  mutate(unit = ifscode)

# DEFINE PARAMETERS
dep <- 'lnresgdp'
fe_vars <- c('unit')
controls <- c('newkopen2', 'peg', 'softpeg', 'lnm2gdp', 'lntradegdp', 'advanced', 'sin1', 'lnforcurbngdp', 'FXAGG_no_FXR')
free <- c("")
maxregN <- length(controls)
tablename <- 'table4_col1_col3'

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
source('../../src/1run_eba/0helper.R')

# Calculate Time for Estimations
sampleN <- 10
source('../../src/1run_eba/4calculateTime.R')

# Create Formulas
source('../../src/1run_eba/1formulaCreate.R')

# Run Model Estimations
source('../../src/1run_eba/2modelEstimate.R')

# Extract results
source('../../src/1run_eba/3resultsExtraction.R')
eba_results <- res_final %>% mutate(result_type = 'Estimation from EBA')

############################
############################

# PAPER ESTIMATION
# Create Formulas
formula_df <- c('lnresgdp~newkopen2+peg+softpeg+lnm2gdp+lntradegdp+advanced+sin1+lnforcurbngdp',
                'lnresgdp~newkopen2+peg+softpeg+lnm2gdp+lntradegdp+advanced+FXAGG_no_FXR') %>%
  data.frame(formula = ., stringsAsFactors = F)

# Run Model Estimations
source('../../src/1run_eba/2modelEstimate.R')

# Extract results
source('../../src/1run_eba/3resultsExtraction.R')
paper_results <- res_final %>% mutate(result_type = 'Estimation from Paper')

############################
############################

# STORE RESULTS
results <- list(results = rbind.fill(eba_results, paper_results), param = param)
filename <- paste('est_out/results_', tablename, '.RData', sep='')
save(results, file = filename)
