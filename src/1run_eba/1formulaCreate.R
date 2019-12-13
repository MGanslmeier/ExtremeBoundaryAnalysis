# Create combinations
combos <- list()
if(fe_vars!=''){
  controls <- c(controls, paste('factor(',fe_vars,')',sep='')) %>% unique(.)
}
for(i in 1:maxregN){
  combos[[i]] <- combinations(n=length(controls[!(controls %in% free)]), r=i, v=controls[!(controls %in% free)], repeats.allowed=FALSE)
}

print(paste('2a. finished combination creation:',as.character(Sys.time())))

############

# Create formula strings with parallelism
cluster <- parallel::makeCluster(detectCores())
parallel::clusterExport(cluster, c("combos"))
formula_df <- parallel::parLapply(cluster, c(1:length(combos)), formulaCreate)
formula_df <- data.frame(formula = as.character(flatten(formula_df)),stringsAsFactors = F) %>% 
  mutate(formula = paste(dep,'~', paste(free, collapse='+', sep=''), '+', formula,sep=''))
parallel::stopCluster(cluster)
if(free==''){
  formula_df$formula <- gsub('~\\+', '~', formula_df$formula) 
}

print(paste('2b. finished formula creation:',as.character(Sys.time())))