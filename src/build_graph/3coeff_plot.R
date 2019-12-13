coeff_plot=function(reg_results,focus,model_type,se_type,r2_min,vif_max,ci, bp_min, ramsey_min, dw_min, ks_max, labels){
  
  #create dataframe for plots
  plot_data <- reg_results %>% 
    subset(var %in% c('Estimate','Std. Error')) %>% 
    filterResults(., focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max) %>%
    dplyr::select(modelID,var,value,formula,dfree,result_type) %>% spread(.,var,value) %>%
    dplyr::select(modelID="modelID", coef="Estimate", se="Std. Error",formula,dfree,result_type) %>%
    mutate(ci_lower = coef-se*qt(1-(((100-ci)/2))/100, dfree), 
           ci_upper = coef+se*qt(1-(((100-ci)/2))/100, dfree)) %>% 
    arrange(-coef)
  
  #create bar chart
  coeff_chart <- ggplot() + 
     geom_segment(data = plot_data, aes(color = result_type, x = ci_lower, xend = ci_upper, y = as.numeric(row.names(plot_data)), 
                                       yend=as.numeric(row.names(plot_data))),  alpha=0.5) + 
     geom_point(data = plot_data, aes(x = coef, y = as.numeric(row.names(plot_data)), color=result_type,
                                       text=formula),stat='identity',size=1) + scale_color_manual(values=c('black','brown2')) +
     geom_vline(xintercept=0,lty=2,lwd=1,colour="grey50") + 
     labs(title=paste('Coefficient Plot of ', labels$labeltext[labels$varname==focus], ' (',ci,'% CI-Levels)',sep=''),
          x='Coefficient Size',y='Model ID') +
     theme(legend.position = "bottom", legend.title=element_blank())
  coeff_chart <- ggplotly(coeff_chart, tooltip="text") %>% 
    layout( legend = list(orientation = 'h', xanchor = 'auto'))
  print(coeff_chart)
}