#result table of parameters
distribution_plot=function(reg_results,focus,model_type,se_type,bandwidth,r2_min,vif_max,
                           bp_min, ramsey_min, dw_min, ks_max){
  
  #create dataframe for plots
  plot_data <- reg_results %>% subset(., var %in% c('Estimate')) %>% 
    filterResults(., focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max)
  plot_data_paper <- plot_data[plot_data$result_type=="Estimation from Paper",]

  dist_plot <- ggplot() + 
    geom_density(data = plot_data, aes(x=value), adjust = bandwidth) + 
    geom_vline(xintercept=0,lty=2,lwd=1,colour="grey50") + xlab('Coefficient Size') + ylab('Density') + 
    {if(nrow(plot_data_paper)!=0) geom_vline(data = plot_data_paper, aes(xintercept=value, color=result_type), lty=3,lwd=1)} +
    scale_colour_manual(values=c("brown2"), labels='Results from Paper') + theme(legend.title=element_blank())
  dist_plot <- ggplotly(dist_plot, tooltip = c('value')) %>% 
    layout( legend = list(orientation = 'h', xanchor = 'auto'))
  print(dist_plot)
  
}