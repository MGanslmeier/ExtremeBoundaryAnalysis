# Run Extraction with parallelism
cluster <- parallel::makeCluster(detectCores())
parallel::clusterExport(cluster, c("results", "fe_vars"))
res_final <- parallel::parLapply(cluster, c(1:length(results)), transformRegOutput)
res_final <- bind_rows(res_final)
parallel::stopCluster(cluster)

# Drop models
drop <- res_final %>% subset(.,(var=='Std. Error' & value=='NaN') | (var=='z value' & value=='Inf'))
res_final <- res_final %>% subset(.,!modelID %in% drop$modelID)