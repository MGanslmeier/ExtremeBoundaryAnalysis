#result table of parameters
metastat_plot <- function(reg_results,focus,model_type,se_type,stat,bandwidth,r2_min,vif_max,
                          bp_min, ramsey_min, dw_min, ks_max){
  
  # Create dataframe for plots
  plot_data <- reg_results %>% 
    subset(var=='Estimate') %>% 
    filterResults(., focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max) %>%
    .[,colnames(.) %in% c('modelID',stat,'result_type')] %>% 
    select(statistics = stat, result_type) %>% 
    mutate(statistics = as.numeric(statistics)) %>%
    subset(.,!is.na(statistics))
  plot_data_paper <- plot_data[plot_data$result_type=="Estimation from Paper",]
  
  statname <- switch(stat, "vif" = 'Variance Inflation Factor', "r2" = "R-Squared", "loglh" = 'Log-Likelihood', 
                     "aic" = "Akaike Information Criterion", "bic" = "Baysian Information Criterion")
  
  if(stat=='r2'){
    metadist_plot <- ggplot(plot_data, aes(x=statistics)) + 
      geom_density(adjust = bandwidth) + xlab(statname) + ylab('Density') + xlim(0,1) + 
      {if(nrow(plot_data_paper)!=0) geom_vline(data = plot_data_paper, aes(xintercept=statistics, color=result_type), lty=3,lwd=1)} +
      scale_colour_manual(values=c("brown2")) + theme(legend.title=element_blank())
  } else if(stat=='vif'){
    metadist_plot <- ggplot(plot_data, aes(x=statistics)) + 
      geom_density(adjust = bandwidth) + xlab(statname) + ylab('Density') + xlim(0,vif_max) +
      {if(nrow(plot_data_paper)!=0) geom_vline(data = plot_data_paper, aes(xintercept=statistics, color=result_type), lty=3,lwd=1)} +
      scale_colour_manual(values=c("brown2")) + theme(legend.title=element_blank())
  }
  else {
    metadist_plot <- ggplot(plot_data, aes(x=statistics)) + 
      geom_density(adjust = bandwidth) + xlab(statname) + ylab('Density') +
      {if(nrow(plot_data_paper)!=0) geom_vline(data = plot_data_paper, aes(xintercept=statistics, color=result_type), lty=3,lwd=1)} +
      scale_colour_manual(values=c("brown2")) + theme(legend.title=element_blank())
  }
  dist_plot <- ggplotly(metadist_plot, tooltip='text') %>% 
    layout( legend = list(orientation = 'h', xanchor = 'auto'))
  print(metadist_plot)
  
}