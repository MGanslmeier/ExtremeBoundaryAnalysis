#piechart
barpie_plot=function(reg_results,focus,model_type,se_type,r2_min,vif_max, bp_min, ramsey_min, dw_min, ks_max){
  
  #create dataframe for plots
  fake <- data.frame(level=c('1%','5%','10%','not'),stringsAsFactors = F)
  plot_data <- reg_results %>% 
    subset(.,var %in% c('Estimate','Pr(>|t|)')) %>% 
    filterResults(., focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max) %>%
    subset(., result_type == 'Estimation from EBA') %>%
    dplyr::select(modelID,var,value) %>% spread(.,var,value) %>%
    dplyr::select(modelID="modelID", coef="Estimate", pval="Pr(>|t|)") %>%
    mutate(level=case_when(pval <= 0.01 ~ '1%',pval > 0.01 & pval <0.05 ~'5%',
                           pval > 0.05 & pval <0.1 ~'5%',pval > 0.1 ~'not')) %>%
    dplyr::select(level) %>% group_by(level) %>% dplyr::summarize(freq=n()) %>%
    merge(fake, ., by='level', all.x=T) %>% mutate(freq = replace_na(freq, 0)) %>%
    mutate(level = factor(level, levels = c('1%','5%','10%','not'))) %>%
    mutate(perc = paste(round((freq/sum(freq))*100,2),'%',sep=''))

  #create pie chart
  pie_chart <- ggplot(plot_data,aes(x="",y=freq,fill=level)) + 
    geom_bar(width=1,stat="identity",size=1,color="white") + coord_polar("y") + 
    scale_fill_manual(values=c('skyblue','dodgerblue','dodgerblue4','coral1')) + 
    labs(title='Percentage of Models',subtitle='(by Significance Level)') + 
    geom_text(data=plot_data[plot_data$freq!=0,],aes(label=perc,x=1.2),position=position_stack(vjust = 0.5)) +
    theme(axis.text=element_blank(),axis.ticks=element_blank(),panel.grid=element_blank(),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank())
  
  #create bar chart
  bar_chart <- ggplot(plot_data,aes(x=level,y=freq,fill=level)) + 
    geom_bar(stat="identity",size=1,color="white") + 
    geom_text(aes(label=freq),vjust=-0.1) + 
    labs(title='Number of Models',subtitle='(by Significance Level)') + 
    scale_fill_manual(values=c('skyblue','dodgerblue','dodgerblue4','coral1')) + 
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),legend.title=element_blank(),legend.position="none")
  
  robustness_plots <- list(pie_chart,bar_chart)
  return(robustness_plots)
}

