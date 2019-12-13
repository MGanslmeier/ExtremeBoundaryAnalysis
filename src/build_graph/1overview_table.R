#overall result table
overall_table_builder <- function(reg_results, dependent, controls, focus, model_type, se_type,
                                  r2_min, vif_max, bp_min, ramsey_min, dw_min, ks_max, labels){
  
  #create list
  plot_data <- reg_results %>% 
    subset(var=='Estimate') %>% 
    filterResults(., focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max)
  
  overall <- list(
    'Model Type' = 'OLS',
    'Dependent Variable' = labels %>% subset(., varname == dependent) %>% select(labeltext) %>% as.character(.),
    'Variable of Interest' = labels %>% subset(., varname == focus) %>% select(labeltext) %>% as.character(.),
    'Controls' = labels %>% subset(., varname %in% controls & varname != focus) %>% select(labeltext) %>% .[[1]] %>% paste(., collapse='; ',sep=''),
    'Total Number of Regressions' = nrow(plot_data) %>% as.character(.)
  ) 
  overall <- do.call(rbind,lapply(overall,data.frame)) %>% mutate('desc' = row.names(.)) %>% .[,c(2,1)] %>% 
    `colnames<-`(c('','')) %>% `row.names<-`(NULL)
  return(overall)
}
