# Execute parallel function
cluster <- parallel::makeCluster(detectCores())
parallel::clusterExport(cluster, c("df", "formula_df", "transformRegOutput_se", "fe_vars"))
results <- parallel::parLapply(cluster, c(1:nrow(formula_df)), modelEstimate)
parallel::stopCluster(cluster)