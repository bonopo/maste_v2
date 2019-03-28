# Clustering --------------------------------------------------------------


#SAAR #### (referred to ARS in the Master thesis)
#standart climate period 1971 bis 2000 (see DWD) or just the whole period as the climate normal period 1991 - 202 & 1961 bis 1990.

# standart period averae annual rainfall
saar <- precip_long %>% 
  group_by(gauge) %>% 
  summarise(sum_mm_yr = sum(sum_mm)/30)

gauges$saar <- saar$sum_mm_yr

#creating average seaosanal rainfall sum too be able to calculate trends in reference to their seasonal sums
#summer
su_sm_p = seasonal_data(lb_season = 5, ub_season = 11, dat= mt_sm_p, funx="sum", value="month_sum")
gauges$su_sm_p = colMeans(su_sm_p)

#winter
wi_sm_p = seasonal_data(lb_season = 12, ub_season = 4, dat= mt_sm_p, funx="sum", value="month_sum")
gauges$wi_sm_p = colMeans(wi_sm_p)
remove(saar,su_sm_p, wi_sm_p)

#mean temperature####

mean_t = mt_mn_temp %>% 
  filter(year(yr_mt) >1970 & year(yr_mt) < 2001) %>% 
  group_by(gauge) %>% 
  summarise(mn_t = mean(temp_m))

gauges$mn_t = mean_t$mn_t
remove(mean_t)

#seasonality ratio (SR)####
#after Laaha et al 2006

# creating two time series one winter one summer
#calculating q95 for both parts
q_sr_w <- q_long %>% #winter
  mutate(month = month(date)) %>%
  filter(month > 11 | month <5) %>%  #changed definitin of winter from Laaha et al 2006 to stahl so it is consistent for all further calculations
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>% #calcualting the 5% quantile
  summarise(q95_w = mean(qt))

#summer
q_sr_s <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month < 12 & month > 4) %>%
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_s = mean(qt))

q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter 

q_sr$sr_value_new = NA
q_sr$sr_value_new[which(q_sr$sr <= 1)] <- 0 #summer
q_sr$sr_value_new[which(q_sr$sr > 1)] <- 2 #winter NAs are produced in 9 time series that are very altered or have no clear seasonality

gauges$sr_new <- as.numeric(q_sr$sr_value_new) # 2= winter 12- 0= summer low flow

remove(q_sr, q_sr_s, q_sr_w)

#creating catchment size classes
gauges$ezggr_class <- cut(gauges$Enzgsg_, breaks=c(0,50,100,150,Inf), labels=c("<50", "50-100", "100-150", "150-200"))


# BFI ---------------------------------------------

#create a data frame that can be converted to an lf object
bfi=c()
for (i in 1:catch_n){
lf_obj <- q_long %>% 
  filter(gauge == i) %>% 
  mutate( flow= q,day = day(date), month=month(date), year = year(date)) %>% #needs exact these names
  dplyr::select(-date, -gauge, -q) %>% 
  as.data.frame()
 
basefl <- createlfobj(x= lf_obj, hyearstart = 1, baseflow = T)#creata an lf obj that can be read by the package lfstat
bfi[i] <- BFI(basefl) #calculates the BFI
}

gauges$bfi <- bfi
remove(bfi, lf_obj, basefl)
gauges$bfi_class = cut(gauges$bfi, breaks=c(0,.4,.6,.8,1), labels=c("<.4", ".4-.6", ".6-.8", ".8-1"))




#mean discharge####
gauges$mn_q= apply(q_wide, 2, mean)


#total number of drought events####

n_events = c()
  for (g in 1:catch_n){
    n_events[g] = dsi_0[[g]]$event_n %>% max()
  }

gauges$n_events = n_events
remove(n_events)

#alpine rivers####
#one can see big gap between all winter catchments: further definition with catchments with higher precipitation than 1200mm (alpine vs Harz/Blackforest)

gauges$alpine = 0
gauges$alpine[which(gauges$sr_new==2)] = 1
#after visual check removing following catchments: 221 238 42 305
gauges$alpine[c(42,221,238,305)] = 0


#longterm (lt) memory effect of catchments####
#defining long term memory as the spearman correlation between ssi and spi_12
gauges$lt_memoryeffect = cor_sci_ssi(sci_n = c(12), sci="spi_v2_", cor_met = "s", ssi="ssi_1")[,1]




#corellation SSI (during drought) with SPI/SPEI-n ####

drought_sci_0 = dr_corr(threshhold = 0)


#correlation of ssi with spi-/spei-n in drought periods####

cor_spi = matrix(nrow=catch_n, ncol=length(drought_sci_0))
cor_spei = matrix(nrow=catch_n, ncol=length(drought_sci_0))

for (a in 1:length(drought_sci_0)){
for (g in 1:catch_n){
temp= drought_sci_0[[a]] %>% 
  filter(gauge== g)
cor_spi[g, a] = cor(y= temp$ssi , x= temp$spi, use="c", method = "spearman") 
#spearman correlation SSI ~ SPI
cor_spei[g, a] = cor(y= temp$ssi , x= temp$spei, use="c", method = "spearman")
}
}

best_spi = c()
  value_spi = c()
best_spei = c()
  value_spei = c()

for(r in 1:catch_n){
 best_spi[r] = cor_spi[r,] %>% which.max() 
 value_spi[r] = cor_spi[r,] %>% max()}

for(r in 1:catch_n){
 best_spei[r] = cor_spei[r,] %>% which.max() 
 value_spei[r] = cor_spei[r,] %>% max()}

#writing to the shapefile
gauges$cor_spei_n_dr = best_spei
gauges$cor_spi_n_dr  = best_spi
gauges$cor_spi_dr    = value_spi
gauges$cor_spei_dr   = value_spei

remove(best_spei,best_spi,value_spi,value_spei,cor_spi,cor_spei)

  

#mean deficit per drought event as clusterin method of the catchments ####
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
mean_intensity =c()
mean_deficit =c()
mean_length = c()
n_events=c()
for (i in 1:catch_n){
  temp1 =  drought_q %>% 
    filter(catchment== i)
  mean_intensity[i] = mean(temp1$threshhold - temp1$mn_q) #deviation of the mean discharge during the drought event and the varying threshhold that defines the drought (80th percentile method van Loon 2015)
  mean_length[i] = mean(as.numeric(ymd(temp1$dr_end) - ymd(temp1$dr_start))) %>% round(.,0) #mean drought length similar to van Loon 2015
  mean_deficit[i] = mean(as.numeric(ymd(temp1$dr_end) - ymd(temp1$dr_start))*temp1$def_vol) #same calculation method as van Loon 2015 # mean(number of days * deficit)
  n_events[i]= nrow(temp1)
  
}



gauges$mn_deficit = log(mean_deficit)
gauges$mn_length = mean_length
gauges$mn_intensity = mean_intensity
gauges$n_events80 = n_events
remove(mean_deficit,mean_length, mean_intensity,temp1, drought_q, output, n_events)


# End of clustering -------------------------------------------------------

