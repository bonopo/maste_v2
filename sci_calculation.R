
# Distribution free calculation -------------------------------------------

#SSI
sci_np(sci_data="mt_mn_q_wide", agg_n=1, sci_name="ssi")  # ssi calculation
ssi_1_long = ssi_1 %>%  #convert ssi into wide format
gather(key=gauge, value=ssi, -yr_mt) %>% 
  as.tbl()

#calculate SPI with spei package and gamma

for (n in agg_month){ 
res <- SPEI::spi(data= mt_sm_p_wide, scale=n) #calculate spi with the default (it's what I want) the result is a time series in the wide format (columns are the catchments)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F) #convert into  matrix
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA} # replacing INF with NAs
m1 %<>% as.data.frame()
colnames(m1)=1:catch_n
assign(paste0("spi_v2_",n),m1)
}

 remove(m1,res)

#with spei package and log logistic

for (n in c(1,2,3,6,12,24)){ 
res <- SPEI::spei(data= spei_data_mat, scale=n)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F)
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA}
m1 %<>% as.data.frame()
colnames(m1)=1:catch_n
assign(paste0("spei_v2_",n),m1)
}


 remove(m1,res)



