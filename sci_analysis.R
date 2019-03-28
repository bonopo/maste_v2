
# SCI Analysis ------------------------------------------------------------


#### cross correlation of SCI and SSI  ####
ccf_spi <- sci_ccf(sci= c(1,2,3,6,12,24),sci_namex = "spi_v2_", sci_namey="ssi_1")


ccf_spei <- sci_ccf(sci= c(1,2,3,6,12,24), sci_namex="spei_v2_", sci_namey="ssi_1")





####predictibility of droughts through correlation of SSI with SCI 
for(i in agg_month){
  int = which(ssi_1[,i] < 0)
  if (int[1] == 1) {
    int= int[2:length(int)]
  }
  res= sapply(1:catch_n, function(x) cor(x= get(paste0("spei_v2_",i))[(int-1),x], y=ssi_1[int,x],method = "s" ,use = "na.or.complete")) # -1 to correlate the SSI with the SCI of the previous month
  assign(paste0("spei_",i,"_predict1"),res)
  
}




# seasonal correlation values for SPI/SPEI####



#monthly correlation ####
feb_sci_cor = monthly_sci(month=2, threshold = 0) 
mar_sci_cor = monthly_sci(month=3, threshold = 0) 
apr_sci_cor = monthly_sci(month=4, threshold = 0) 
mai_sci_cor = monthly_sci(month=5, threshold = 0)
jun_sci_cor = monthly_sci(month=6, threshold = 0) 
aug_sci_cor = monthly_sci(month=8,threshold = 0) 
sep_sci_cor = monthly_sci(month=9, threshold = 0) 
oct_sci_cor = monthly_sci(month=10, threshold = 0) 
nov_sci_cor = monthly_sci(month=11, threshold = 0) 



#difference Between SPI and SPEI
spi_spei_mean = matrix(nrow=40, ncol=6)
spi_spei_sd = matrix(nrow=40, ncol=6)
  for (a in 1:length(drought_sci_0)){
    for(y in 1970:2009){
    temp= drought_sci_0[[a]] %>% 
      filter(year(yr_mt) == y)
    
    spi_spei_mean[(y-1969),a] = ((temp$spi) - (temp$spei ))%>% mean(.,na.rm=T)
    spi_spei_sd[(y-1969),a] = (temp$spi - temp$spei )%>% sd(.,na.rm=T)
    }
  }

#change of the correlation between ssi and sci over time?

cor_spi_time = matrix(nrow=40, ncol=length(drought_sci_0))
cor_spei_time = matrix(nrow=40, ncol=length(drought_sci_0))

  for (a in 1:length(drought_sci_0)){
    for (y in 1970:2009){
    temp= drought_sci_0[[a]] %>% 
      filter(year(yr_mt)== y)
    cor_spi_time[(y-1969), a] = cor(y= temp$ssi , x= temp$spi, method = "spearman",use="na.or.complete") 
    cor_spei_time[(y-1969), a] = cor(y= temp$ssi , x= temp$spei, method = "spearman", use="na.or.complete")
    }
  }

#trend calculation with MK
#spi
p_value = c()
tau =  c()
  for( i in 1:5){
    res_t = mk.test(cor_spi_time[,i])
    tau[i] = res_t$estimates[3]
    p_value[i]  = res_t$p.value
  }

#trend spei

p_value_spei = c()
tau_spei =  c()
  for( i in 1:5){
    res_t = mk.test(cor_spei_time[,i])
    tau_spei[i] = res_t$estimates[3]
    p_value_spei[i]  = res_t$p.value
}
