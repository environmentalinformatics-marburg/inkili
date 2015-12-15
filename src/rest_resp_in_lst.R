#lst_rsp = list response
#lst_prd = list predict
#lst_cmb = list combined

###getting response and predicted values from "models"
lst_cmb <- lapply(models, function(a){
  lst_per_prd <- lapply(a, function(b){
    rsp <- b$testing$RESPONSE
    prd <- as.numeric(b$testing$PREDICTED)
    return(list(rsp=rsp, prd=prd))
  })
  return(lst_per_prd)
})

###creating list with one entry with all response values for each 
#response variable
lst_rsp_lst <- lapply(lst_cmb, function(a){
  rsp <- lapply(a, function(b){
    r <- b$rsp
  })
  return(rsp)
})

lst_rsp <- lapply(seq(lst_rsp_lst), function(a){
  rsp <- unlist(lst_rsp_lst[[a]], recursive=F)
})

###creating list with one entry with all prediction values for each 
#response variable

lst_prd_lst <- lapply(lst_cmb, function(a){
  prd <- lapply(a, function(b){
    r <- b$prd
  })
  return(prd)
})

lst_prd <- lapply(seq(lst_prd_lst), function(a){
  prd <- unlist(lst_prd_lst[[a]], recursive=F)
})

###plotten
R2_all_prd <- lapply(seq(lst_rsp), function(a){
  smmry <- summary(lm(lst_prd[[a]] ~ lst_rsp[[a]]))
  #plot(lm(lst_prd[[a]] ~ lst_rsp[[a]]), main = response[a])
  plot((lst_prd[[a]] ~ lst_rsp[[a]]), main = response[a])
  #R2 <- smmry$r.squared
})

