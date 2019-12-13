# Create Formulas
source('../eba/1run_eba/1formulaCreate.R')
totalN <- nrow(formula_df)
formula_df <- sample_n(formula_df, sampleN)

# Run Model Estimations
start_time <- Sys.time()
source('../eba/1run_eba/2modelEstimate.R')
print(paste('3a: Model Estimation finished: ', Sys.time()))
duration_model <- ((Sys.time() - start_time)/sampleN)

# Extract results
source('../eba/1run_eba/3results_extraction.R')
print(paste('3b: Model Extraction finished: ', Sys.time()))

duration_model <- ((Sys.time() - start_time)/sampleN)
print(paste(round(as.numeric(duration_model), 2), " Seconds per Model", sep=''))
duration_total <- totalN*duration_model
print(paste(round(as.numeric(duration_total)/60, 2), " Minutes in Total", sep=''))

# Remove objects
rm(list = ls()[ls() %in% c("formula_df", "cluster", "combos", "drop", "res_final", 
                           "results", "sampleN", "start_time", "totalN")])
