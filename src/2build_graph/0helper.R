# further plotting helper functions
filterResults <- function(dataset, focus, model_type, se_type, vif_max, r2_min, bp_min, ramsey_min, dw_min, ks_max){
  temp <- dataset %>%
    subset(IV==focus) %>% 
    filter(if (model_type!="all") model_type_fe==model_type else model_type_fe!=model_type) %>%
    filter(if (se_type!="all") sterror_type==se_type else sterror_type!=se_type) %>%
    filter(as.numeric(vif)<=vif_max | vif=='Error') %>% 
    filter(r2>=r2_min & bp>=bp_min & ramsey>=ramsey_min & dw>=dw_min & ks<=ks_max)
  return(temp)
}