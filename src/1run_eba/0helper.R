# load packages
pacman::p_load(plyr, dplyr, readxl, plm, parallel, tidyr, ggplot2, lmtest, multiwayvcov, 
               foreign, haven, gtools, jsonlite, purrr, car)

# Function to create formula strings
formulaCreate <- function(formulaID){
  library(dplyr)
  temp <- apply(combos[[formulaID]], 1, function(r) {
    paste(r,collapse='+',sep='') %>% as.character()
  })
  return(temp)
}

# Function to estimate the models
modelEstimate <- function(ID){
  
  # Load libraries
  library(plyr)
  library(dplyr)
  library(lmtest)
  library(multiwayvcov)
  library(sandwich)
  
  # Run regression
  form <- as.formula(formula_df$formula[ID])
  reg <- do.call(lm, list(data = df, formula=as.formula(form)))
  
  #############
  
  # Calculate different standard errors
  reg_stand <- coeftest(reg, vcov = vcovHC(reg, "const")) %>% transformRegOutput_se(.)
  reg_rob <- coeftest(reg, vcov = vcovHC(reg, "HC1")) %>% transformRegOutput_se(.)
  if(fe_vars!=''){
    reg_clu <- coeftest(reg, cluster.vcov(reg, df$unit)) %>% transformRegOutput_se(.)
  }
  
  #############
  
  # Store results in list with meta data
  temp <- summary(reg)
  r2 <- temp$r.squared
  loglh <- logLik(reg)
  aic <- AIC(reg)
  bic <- BIC(reg)
  nobs <- nobs(reg)
  dfree <- summary(reg)$df[[2]]
  vif_obj <- try(vif(reg))
  #shapiro <- shapiro.test(reg$residuals)[['p.value']]
  ks <- ks.test(as.numeric(reg$residuals), "pnorm")[['p.value']]
  bp <- as.numeric(bptest(reg)[['p.value']])
  ramsey <- resettest(reg)[['p.value']]
  dw <- dwtest(reg)[['p.value']]
  
  if(fe_vars!=''){
    res <- list(reg_stand=reg_stand, reg_rob=reg_rob, reg_clu=reg_clu, 
                meta = list(form=formula_df$formula[ID], sum=temp, r2=r2, loglh=loglh, 
                            aic=aic, bic=bic, nobs=nobs, dfree=dfree, vif_obj=vif_obj, 
                            ks=ks, bp=bp, ramsey=ramsey, dw=dw))
  } else {
    res <- list(reg_stand=reg_stand, reg_rob=reg_rob,
                meta = list(form=formula_df$formula[ID], sum=temp, r2=r2, loglh=loglh, 
                            aic=aic, bic=bic, nobs=nobs, dfree=dfree, vif_obj=vif_obj, 
                            ks=ks, bp=bp, ramsey=ramsey, dw=dw))
  }

  return(res)
}

# Create function for regression output extraction
transformRegOutput <- function(ID){
  
  library(plyr)
  library(dplyr)
  library(tidyr)
  
  coefObject <- results[[ID]]
  meta <- coefObject$meta
  final <- data.frame(stringsAsFactors = F)
  
  if(fe_vars!=''){
    standard_error_types <- c('reg_stand','reg_rob','reg_clu')
  } else{
    standard_error_types <- c('reg_stand','reg_rob')
  }
  
  for(k in standard_error_types){
    
    class(coefObject[[k]]) <- 'matrix'
    temp <- coefObject[[k]] %>% as.data.frame(.) %>%
      subset(.,!grepl('factor',row.names(.))) %>% 
      mutate(IV = row.names(.)) %>%
      mutate(modelID = paste(ID, k ,sep='_')) %>%
      gather(., var, value, -c(IV, modelID)) %>%
      mutate(r2=meta$r2, 
             loglh=as.numeric(meta$loglh), 
             aic=meta$aic, 
             bic=meta$bic,
             formula=meta$form, 
             sterror_type=gsub(".*reg_","",modelID),
             nobs=meta$nobs, 
             dfree=meta$dfree,
             vif=as.numeric(substr(meta$vif_obj,1,5)[1]),
             bp=meta$bp, 
             ramsey=meta$ramsey,
             dw=meta$dw,
             ks=meta$ks) %>%
      mutate(vif=replace_na(vif,0)) %>% 
      mutate(fe_unit=grepl('factor\\(unit',formula), 
             fe_time=grepl('factor\\(time',formula)) %>%
      mutate(model_type_fe=case_when(fe_unit==T & fe_time==F ~ "fe_unit",
                                     fe_unit==F & fe_time==T ~ "fe_time",
                                     fe_unit==F & fe_time==F ~ "none",
                                     fe_unit==T & fe_time==T ~ "both")) %>%
      dplyr::select(-c(fe_unit, fe_time))
    final <- rbind.fill(final,temp)
  }
  return(final)
}

# Create helper function for reshaping of regression output
transformRegOutput_se <- function(coefObject){
  class(coefObject) <- 'matrix'
  coefObject <- coefObject %>% subset(.,!grepl('factor',row.names(.)))
  return(coefObject)
}




