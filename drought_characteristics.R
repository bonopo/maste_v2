#number of months in a year affected by drought ####


dr_length_1 <- dr_n()


#drought severity & intensity####


#severity: sum of differences between ssi indicator and threshold, it indicates a cumulative deficiency of a drought parameter below the critical level. 
#intensity: it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.


dsi_0<- dr_severity(severity = 0)
#returns all drought events by counting consecutive month affected by drought

dsi_0_yearly = list()
for (i in 1:catch_n){
dsi_0_yearly[[i]] = dsi_0[[i]] %>% 
  mutate(year = year(dr_start)) %>% # defining the year of the drought beginning to the year in which the drought happened
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())
}


#80th percentile approach of van loon &laaha (calculation) ####
#for discharge droughts ####
mov_mn_q = rollapply(q_wide, width=30, by.column=T, align= "center", FUN=mean, fill=NA) %>% as.data.frame %>% as.tbl() # making 30 day moving average

mov_mn_q_long = mov_mn_q %>% #convert 30day moving average into long format
  mutate(date=date_seq_long) %>% 
  gather(key=gauge, value=mov_mn_q, -date) %>% 
  mutate(gauge = as.numeric(gauge)) %>% 
  as.tbl()

output = matrix(nrow=0, ncol=7) %>% as.data.frame()
for (c in 1:catch_n){ 
lf_obj <- mov_mn_q_long %>% 
  filter(gauge == c) %>% 
  mutate( flow= mov_mn_q,day = day(date), month=month(date), year = year(date)) %>% 
  #convert data frame into lfstat compatible format
  dplyr::select(-date, -gauge, -mov_mn_q) %>% 
  createlfobj(., baseflow=F, hyearstart=1) #creating low flow object to calulatoe later

flowunit(lf_obj)<-'m³/s' #defining unit for cumulative deficit calculation

res= find_droughts(lf_obj, threshold = "Q80", varying="daily") #same as laaha approach saying the 80th percentile of the flow duration curve, with daily varying threshold. Comparison to own threshold calculation gives the same result see commented out part above

#problem: droughts of less than 4 days are still defined as drought: But I am intested in droughts that have a long lasting effect with it's deficit in water

for (i in 1:max(res$event.no)){
  if(length(which(res$event.no == i)) <= 3 ){ #removing droughts of less than 4 days
    res$event.no[res$event.no == i] = 0
  }  
}
new.drought.no = 1:(length(unique(res$event.no))-1) #because 0 is not an event -1
n=1

for (i in unique(res$event.no)[-1]){#because 0 is not an event -1
  res$event.no[res$event.no == i] = new.drought.no[n]
  n=n+1
}

#creating a matrix with drought event results
res %<>% as.data.frame()
drought_t = matrix(nrow = max(res$event.no), ncol=7) %>% as.data.frame()
drought_t[,1]=as.numeric(c)
for (i in 1:max(res$event.no)){
drought_t[i,2]= rownames(res)[res$event.no == i][1] #drought start
drought_t[i,3]=tail(rownames(res)[res$event.no == i], n=1) #drought end
drought_t[i,4] = sum(res$def.increase[res$event.no == i]) # deficit vol
drought_t[i,5] = mean(res$threshold[res$event.no == i]) #mean threshhold
drought_t[i,6] = mean(res$discharge[res$event.no == i], na.rm=T) #mean disscharge
drought_t[i,7] = i #event no
}

output = rbind(output, drought_t)

cat(100*round(c/catch_n,2),"%", "\n")

}
#warings are due to assuming the default in the deficit unit (default is correct) and because of Na valus that exist because the mean deficit is calculated from 30 day moving centere average
colnames(output) = c("catchment", "dr_start", "dr_end", "def_vol", "threshhold", "mn_q","event_no")

save(output, file="./output/drought_q.Rdata")

remove(lf_obj, res,  drought_t, new.drought.no, mov_mn_q, mov_mn_q_long)


#for precipitation ####

  mov_sm_p = rollapply(precip, width=30, by.column=T, align= "center", FUN=sum, fill=NA) %>% as.data.frame() %>% as.tbl()

mov_sm_p_long = mov_sm_p %>% 
  mutate(date=date_seq_long) %>% 
  gather(key=gauge, value=sum_mm, -date) %>% 
  mutate(gauge = as.numeric(gauge)) %>% 
  as.tbl()



output = matrix(nrow=0, ncol=7) %>% as.data.frame()
for (c in 1:catch_n){ 
lf_obj <- mov_sm_p_long %>% 
  filter(gauge == c) %>% 
  mutate( flow= sum_mm,day = day(date), month=month(date), year = year(date)) %>% 
  dplyr::select(-date, -gauge, -sum_mm) %>% 
  createlfobj(., baseflow=F, hyearstart=1) 
  
flowunit(lf_obj)<-"l/d" #default is m³/s so new default definition is needed. since unit is mm/day it can be set to l/d

res= lfstat::find_droughts(lf_obj, threshold = "Q80", varying="daily") 
#daily is not really daily. It is actually the 30day moving sum calculated two steps before

#same as laaha approach saying the 80th percentile of the flow duration curve, with daily varying threshold. Comparison to own threshold calculation gives the same result see commented out part above

#problem: droughts of less than 4 days are still defined as drought: But I am intested in droughts that have a long lasting effect with it's deficit in water

for (i in 1:max(res$event.no)){
  if(length(which(res$event.no == i)) <= 3 ){ #removing droughts of less than 4 days
    res$event.no[res$event.no == i] = 0
  }  
}
new.drought.no = 1:(length(unique(res$event.no))-1) #because 0 is not an event -1
n=1

for (i in unique(res$event.no)[-1]){#because 0 is not an event -1
  res$event.no[res$event.no == i] = new.drought.no[n]
  n=n+1
}

#creating a matrix with drought event results
res %<>% as.data.frame()
drought_t = matrix(nrow = max(res$event.no), ncol=7) %>% as.data.frame()
drought_t[,1]=as.numeric(c)
for (i in 1:max(res$event.no)){
drought_t[i,2]= rownames(res)[res$event.no == i][1] #drought start
drought_t[i,3]=tail(rownames(res)[res$event.no == i], n=1) #drought end
drought_t[i,4] = sum(res$def.increase[res$event.no == i]) # deficit vol
drought_t[i,5] = mean(res$threshold[res$event.no == i]) #mean threshhold
drought_t[i,6] = mean(res$discharge[res$event.no == i], na.rm=T) #mean disscharge
drought_t[i,7] = i #event no
}

output = rbind(output, drought_t)

cat(100*round(c/catch_n,3),"%", "\n")

}
#warings are due to Na values. NA values exist because the mean deficit is calculated from 30 day moving centered average (creating 14 NAs at the beginning at at the end of each time series).

colnames(output) = c("catchment", "dr_start", "dr_end", "def_vol", "threshhold", "mn_sm_p","event_no")

save(output, file="./output/drought_p.Rdata")

remove(lf_obj, res,  drought_t, new.drought.no)


#80th percentile summarising ####

#loading data from previous step
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
load("./output/drought_p.Rdata", verbose = TRUE)
  drought_p = output
remove(output)



# summarising result (per year and month) 
# function takes a while and runs on all cores -1
q_seas= seasonal_80th(data = drought_q) # if it takes too long you can load the result (see line 182)
p_seas= seasonal_80th(data = drought_p)

# returns list with 3 df for each catchment [[1]] = mat_days (= days of drought per year per month) [[2]] = mat_def (=deficit volume per month (of drought) per year) [[3]] = number of events
  
#saving result for next step
save(q_seas,file="./output/seasonal_q.Rdata")
save(p_seas,file="./output/seasonal_p.Rdata")

load("./output/seasonal_q.Rdata", verbose = T)
load("./output/seasonal_p.Rdata", verbose = T)


#converting ouput into readable and usable dataframes aus output is list ####

#days of drought
p_days_of_drought_list = lapply(p_seas, function(x) x[[1]])  # precipitation 
q_days_of_drought_list = lapply(q_seas, function(x) x[[1]])# discharge
p_days_of_drought_df <- lapply(p_seas, function(x) x[[1]]) %>% do.call("rbind", .)  #as df
q_days_of_drought_df <- lapply(q_seas, function(x) x[[1]]) %>% do.call("rbind", .) #as df

p_days_of_drought_yr = apply(p_days_of_drought_df,1, sum )%>% cbind(days_dr=.,year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40))%>%as.data.frame() %>%  spread(key=gauge, value = days_dr)%>% dplyr::select(-year) # precipitation yearly values to calculate trends wide format (columns are catchments and rows are the yearly sums) 

q_days_of_drought_yr = apply(q_days_of_drought_df,1, sum )%>% cbind(days_dr=., year = rep(1970:2009,catch_n),gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = days_dr) %>% dplyr::select(-year) # discharge yearly values to calculate trends wide format (columns are catchments and rows are the yearly sums) 


#cumulative deficit

p_sum_def_list = lapply(p_seas, function(x) x[[2]]) #precipitation
q_sum_def_list = lapply(q_seas, function(x) x[[2]]) #discharge

p_sum_def_df = lapply(p_seas, function(x) x[[2]]) %>% do.call("rbind", .)
q_sum_def_df<- lapply(q_seas, function(x) x[[2]]) %>% do.call("rbind", .)

p_sum_def_yr = apply(p_sum_def_df,1, sum ) %>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_sum_def_yr = apply(q_sum_def_df,1, sum )%>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

#number of events

p_n_df = lapply(p_seas, function(x) x[[3]]) %>% do.call("rbind", .)
q_n_df<- lapply(q_seas, function(x) x[[3]]) %>% do.call("rbind", .)

p_n_events_yr = apply(p_n_df,1, sum ) %>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_n_events_yr = apply(q_n_df,1, sum )%>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

#calculating drought trends for a subset ####

#for the years 1970 - 1999
q_sum_def_sub1 = q_sum_def_yr[1:30,] 
p_sum_def_sub1 = p_sum_def_yr[1:30,] 

#for the years 1979 - 2009
q_sum_def_sub2 = q_sum_def_yr[11:40,] 
p_sum_def_sub2 = p_sum_def_yr[11:40,] 

mmky_par(c("q_sum_def_sub1","p_sum_def_sub1","q_sum_def_sub2","p_sum_def_sub2"))




#seasonal drought characteristics

summer_dy_drought_p = seasonal_80th_trend(month = 5:11, datax= p_days_of_drought_list)
summer_dy_drought_q = seasonal_80th_trend(month = 5:11, datax= q_days_of_drought_list)
summer_sm_def_p = seasonal_80th_trend(month = 5:11, datax= p_sum_def_list) 
summer_sm_def_q = seasonal_80th_trend(month = 5:11, datax= q_sum_def_list) 

winter_dy_drought_p = seasonal_80th_trend(month = c(12,1,2,3,4), datax= p_days_of_drought_list)
winter_dy_drought_q = seasonal_80th_trend(month = c(12,1,2,3,4), datax= q_days_of_drought_list)
winter_sm_def_p = seasonal_80th_trend(month = c(12,1,2,3,4), datax= p_sum_def_list) 
winter_sm_def_q = seasonal_80th_trend(month = c(12,1,2,3,4), datax= q_sum_def_list) 

spring_dy_drought_p = seasonal_80th_trend(month = 3:5, datax= p_days_of_drought_list)
spring_dy_drought_q = seasonal_80th_trend(month = 3:5, datax= q_days_of_drought_list)
spring_sm_def_p = seasonal_80th_trend(month = 3:5, datax= p_sum_def_list) 
spring_sm_def_q = seasonal_80th_trend(month = 3:5, datax= q_sum_def_list)

#total deficit of the catchments####

tot_deficit = apply(q_sum_def_yr, 2, sum)
#boxplot(log10(apply(q_sum_def_yr, 2, sum)) ~ gauges$hydrogeo_simple)