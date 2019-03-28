

# Discharge ---------------------------------------------------------------

# q seasonal data ####
#7 day moving average in summer####
#calculate
  # min for every year
  # date of min
#summer = mai to november

#selecting only summer and then convert into wide format
q_wide_summer = q_long %>% 
    filter(month(date) >= 5 & month(date)<= 11) %>% 
    group_by(gauge, year(date)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q) %>% 
  as.tbl() %>% 
  dplyr::select( -`year(date)`)

ms7 = rollapply(dplyr::select(q_wide_summer,as.character(1:catch_n)), FUN = mean, width = 7, align="right", by.column=TRUE, fill=NA) #making the 7 day moving average 

date_summer= dplyr::select(q_wide_summer, date) 
date_summer = date_summer$date #dates to calculate integers`


date_seq_long_1 =  ymd(paste0("1972-",month(date_summer), "-",day(date_summer))) 
## 1972 to be able to filter in the next step over all years (could also be an imaginary year)
int = which(date_seq_long_1 <= ymd("1972-05-06"))
#where the rollapply result hast to be NA because it calculates moving average continues (ie end of 1970 will be partially used to calculate moving average for beginning 1971), but end november can not be used partially to calculate rolling mean for beginning of mai

ms7[int,] = NA 
ms7_df = as.data.frame(ms7)

# retrieve date from the original data frame
ms7_min_temp = q_long %>% 
    filter(month(date) >= 5 & month(date)<= 11) %>% 
    group_by(gauge, year(date)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q) %>% 
  as.data.frame() %>% 
  dplyr::select(`date`, `year(date)`) 

#retrieveing the yearly minima and the date of the minima
#resulting in a table with the minimum value and the day of the year when the minimum value accured (looking at summer only)
ms7_min = cbind(ms7_min_temp, ms7) %>%  
  gather(key=gauge, value=ms7, -`date`, -`year(date)`) %>% 
  group_by( gauge= as.integer(gauge),year = year(date)) %>% 
  summarise(ms7_min = min(ms7, na.rm=T), ms7_date = yday(date[which.min(ms7)]) , ms7_date_long =date[which.min(ms7)]) %>% 
  ungroup() %>% 
  as.data.frame()

remove(ms7_min_temp)

#ms7 date data frame wide format refered to as T_Q_7 in the master thesis
ms7_date = ms7_min %>% 
  dplyr::select(gauge, ms7_date, year) %>% 
  spread(key=gauge, value=ms7_date) %>% 
  dplyr::select(-year)

#ms7 minima data frame wide format refered to as Q_min_7 in the master thesis
ms7_min %<>%  dplyr::select(gauge, ms7_min, year) %>% 
  spread(key=gauge, value=ms7_min) %>% 
  dplyr::select(-year) 
remove(ms7_df, ms7,  int, date_seq_long_1,date_summer)

save(list= c("ms7_min", "ms7_date"), file="./output/ms7.Rdata")

#7day minima in the winter for the nival catchments####

#since there are catchments that have their lowflow in winter the ms7 and ms7 date will relate to their non-snowfall caused drought. 


q_wide_winter = q_long %>% 
    filter(month(date) < 5 | month(date)>11) %>% 
    group_by(gauge, year(date)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q) %>% 
  as.data.frame() 

date_winter= dplyr::select(q_wide_winter, `date`) %>% mutate(date = ymd(date)) 
date_winter = date_winter$date #dates to calculate integers
q_wide_winter=  dplyr::select(q_wide_winter, -`date`, -`year(date)` ) # winter streamflow

mw7 = rollapply(dplyr::select(q_wide_winter,which(gauges$sr_new==2)), FUN = mean, width = 7, align="right", by.column=TRUE, fill=NA) #calculating rolling mean for all catchments that are winter low flow
#to look at the regime minima would mean to calculate the 7 day minima for the wintermonths for sr == 2 ; 29 catchments

date_seq_long_1 =  ymd(paste0("1972-",month(date_winter), "-",day(date_winter))) # 1972 to be able to filter in the next step over all years (could also be an imaginerary year)
int = which(date_seq_long_1 >= ymd("1972-12-01") & date_seq_long_1 <= ymd("1972-12-06"))
#where the rollapply result hast to be NA because it calculates moving average continues (ie end of 1970 will be partially used to calculate moving average for beginning 1971), but end november can not be used partially to calculate rolling mean for beginning of mai

mw7[int,] = NA 
mw7_df = as.data.frame(mw7)

mw7_min_temp = q_long %>% 
   filter(month(date) < 5 | month(date)>11) %>% 
    group_by(gauge, year(date)) %>% 
    ungroup() %>% 
    spread(key=gauge, value=q) %>% 
    as.data.frame() %>% 
    dplyr::select(`date`, `year(date)`) 

mw7_min = cbind(mw7_min_temp, mw7) %>% 
  gather(key=gauge, value=mw7, -`date`, -`year(date)`) %>% 
  group_by( gauge= as.integer(gauge),year = year(date)) %>% 
  summarise(mw7_min = min(mw7, na.rm=T), mw7_date = yday(date[which.min(mw7)]) , mw7_date_long =date[which.min(mw7)]) %>% #resulting in a table with the minimum value and the day of the year when the minimum value accured (looking at winter only)
  ungroup() %>% 
  as.data.frame()

remove(mw7_min_temp, int, date_seq_long_1,date_winter)

mw7_date = mw7_min %>% 
  dplyr::select(gauge, mw7_date, year) %>% 
  spread(key=gauge, value=mw7_date) %>% 
  dplyr::select(-year)

#to calculate trend data needs to be adjusted so that december has lower values thatn january (an not higher)
mw7_date[which((mw7_date) <= yday("2008-04-30"), arr.ind = T)] = mw7_date[
    which((mw7_date) <= yday("2008-04-30"), arr.ind = T)] +31 #since 1.december is 0

 mw7_date[which(mw7_date > yday("2008-06-30"), arr.ind = T)] = mw7_date[which((mw7_date) > yday("2008-06-30"), arr.ind = T)] -as.numeric(yday("2008-12-01"))
 #setting "01-12" as zero and counting upwards from that day, choosin 06-30 because i just added 31 to the other dates before, calculating now with not real dates but just surrogates
  

mw7_min %<>%  dplyr::select(gauge, mw7_min, year) %>% 
  spread(key=gauge, value=mw7_min) %>% 
  dplyr::select(-year) 


remove(mw7_df, q_wide_winter)

#30 day moving average ####
#calculates min for every year


ms30_df = rollapply(dplyr::select(q_wide_summer,as.character(1:catch_n)), FUN = mean, width = 30, align="center", by.column=TRUE, fill=NA) %>% as.data.frame() #rolling mean

ms30_df$date = ymd(q_wide_summer$date) # date

ms30_min = ms30_df %>% 
  filter(ymd(paste0("1960-",month(date),"-",day(date))) >= ymd("1960-5-15") & ymd(paste0("1960-",month(date),"-",day(date))) <= ymd("1960-11-15")) %>%  #filtering only from 15th onwards to make sure that the mean of november is not "carried" on to may and vice versa
  mutate(year= year(date)) %>% 
  as.tbl() %>% 
  gather(key=gauge, value=ms30, -year,-date) %>% 
  group_by(gauge, year) %>% 
  summarise(ms30_min = min(ms30, na.rm=T))%>%   #selecting the min of every year
  ungroup() %>% 
  mutate(gauge= as.integer(gauge)) %>% 
  dplyr::select(gauge, ms30_min, year) %>% #converting into df that can be read by MK function
  spread(key=gauge, value=ms30_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame() 


remove(ms30_df,q_wide_summer)


#10% quantile
su_q10 =  q_long %>% 
  filter(month(date) >= 5, month(date)<= 11) %>% 
  mutate(year=year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(q10 = quantile(q, .1)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q10) %>% 
  dplyr::select(-year ) %>% 
  as.data.frame()


#Q yearly data 
#yearly mean
yearly_mn_q = q_long %>% 
  mutate(year= year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_mean = mean(q)) %>% 
  spread(key=gauge, value=yearly_mean) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_q10 = q_long %>% 
  mutate(year= year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_min = round(quantile(q,.1),4)) %>% 
  spread(key=gauge, value=yearly_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

# regime minam vs minima in summer 
#regime minima (see stahl et al 2010) in every hydrological year

#calculating with hydrologic year
#starting time series after 1.4.1970 since before is not a complete hydrologic year
data_temp1 = q_long %>%
   filter(date >= "1970-04-01") %>% 
   spread(key=gauge, value=q) %>% 
  dplyr::select(-date)


mn_7day = rollapply(data_temp1, FUN = mean, width = 7, align="right", by.column=TRUE, fill=NA) # 7 day mean

#selecting the minima in every hydrological year
dates =  q_long %>%
   filter(date >= "1970-04-01") %>% 
   spread(key=gauge, value=q) %>% 
   dplyr::select(date) 

hydro_year_s <- hydro_year[-c(1:which(date_seq_long == "1970-03-31"))] #add the hydrologic year

yearly_7_t = mn_7day %>% 
   as.data.frame() %>%
   as.tbl() %>% 
     mutate(hy_year = hydro_year_s, date=ymd(dates$date)) %>%  
   gather( key=gauge, value=mn_7day, -hy_year, -date)  %>% 
   mutate( gauge= as.integer(gauge)) %>% 
   group_by(gauge,hy_year) %>% 
   summarise(min_7day=min(mn_7day, na.rm=T),date_min= yday(date[which.min(mn_7day)])) 
 
 
yearly_7_min = yearly_7_t %>% 
  dplyr::select(gauge, min_7day, hy_year) %>% 
  spread(key=gauge, value=min_7day) %>% 
  dplyr::select(-hy_year) %>% 
  as.data.frame()

yearly_7_date = yearly_7_t %>% 
  dplyr::select(gauge, date_min, hy_year) %>% 
  spread(key=gauge, value=date_min) %>% 
  dplyr::select(-hy_year) %>% 
  as.data.frame()
 
remove(yearly_7_t, yearly_7day_min, dates, date, data_temp1)
 
#comparison shows that the summer low flow reflect the regime minima

#q monthly data ####
#calculating monthly (calender based) mean


monthly_mean_q = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(q)) %>% 
  mutate(month= month(yr_mt))

res =  plyr::dlply(monthly_mean_q, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
for(i in 1:12){ #starting with january
   df=  Reduce(rbind,res[int]) %>%
     spread(key=gauge,value= monthly_mean) %>%
     dplyr::select(-yr_mt, -month)
   assign(paste0(str_to_lower(month.abb[i]),"_mn_q"),df)
   int = int+1 # through int+1 the next month will be selected (feb,march....)
   }
remove(res, int, monthly_mean_q, df)

#monthly median
monthly_med_q = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_med = median(q)) %>% 
  mutate(month= month(yr_mt))

res =  plyr::dlply(monthly_med_q, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
for(i in 1:12){ #starting with january
   df=  Reduce(rbind,res[int]) %>% 
     spread(key=gauge,value= monthly_med) %>%
     dplyr::select(-yr_mt, -month)
   assign(paste0(str_to_lower(month.abb[i]),"_med_q"),df)
   int = int+1 # through int+1 the next month will be selected (feb,march....)
   }
remove(res, int, monthly_med_q, df)


#low flwo quantile q10
monthly_q10 = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_q10 = quantile(q,.1)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_q10, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){ #starting with january
   df=  Reduce(rbind,res[int]) %>%
     spread(key=gauge,value= monthly_q10) %>%
     dplyr::select(-yr_mt, -month)
   assign(paste0(str_to_lower(month.abb[i]),"_q10"),df)
   int = int+1 # through int+1 the next month will be selected (feb,march....)
   }
remove(res, int, monthly_q10, df)

#low flwo quantile q35
monthly_q35 = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_q35 = quantile(q,.35)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_q35, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){ #starting with january
     df=  Reduce(rbind,res[int]) %>%
       spread(key=gauge,value= monthly_q35) %>%
       dplyr::select(-yr_mt, -month)
     assign(paste0(str_to_lower(month.abb[i]),"_q35"),df)
     int = int+1 # through int+1 the next month will be selected (feb,march....)
     }
remove(res, int, monthly_q35, df)

#high flow quantile q80####
monthly_q80 = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_q80 = quantile(q,.8)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_q80, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){ #starting with january
     df=  Reduce(rbind,res[int]) %>% 
       spread(key=gauge,value= monthly_q80) %>% 
       dplyr::select(-yr_mt, -month)
     assign(paste0(str_to_lower(month.abb[i]),"_q80"),df)
     int = int+1 # through int+1 the next month will be selected (feb,march....)
     }
remove(res, int, monthly_q80, df)


#flood analysis####
q_wide_flood = q_long %>% 
    group_by(gauge, year(date)) %>% 
  summarise(high_flow = quantile(q,.9)) %>% 
  spread(key=gauge, value=high_flow) %>% 
  as.tbl() %>% 
  dplyr::select( -`year(date)`) %>% 
  set_colnames(1:catch_n)

#temp monthly####
#mean
monthly_mean_t = temp_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(temp)) %>% 
  mutate(month= month(yr_mt))

res =  plyr::dlply(monthly_mean_t, c("gauge", "month")) #produce data frames by gauge and month (so 377 * 12 data frames)
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){#starting with january
   df=  Reduce(rbind,res[int]) %>%
     spread(key=gauge,value= monthly_mean) %>%
     dplyr::select(-yr_mt, -month)
   assign(paste0(str_to_lower(month.abb[i]),"_mn_t"),df)
   int = int+1 # through int+1 the next month will be selected (feb,march....)
   }
remove(res, int, monthly_mean_t, df)

#median temperature monthly
monthly_med_t = temp_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = median(temp)) %>% 
  mutate(month= month(yr_mt))

res =  plyr::dlply(monthly_med_t, c("gauge", "month")) #produce data frames by gauge and month (so 377 * 12 data frames)
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){#starting with january
     df=  Reduce(rbind,res[int]) %>%
       spread(key=gauge,value= monthly_mean) %>%
       dplyr::select(-yr_mt, -month)
     assign(paste0(str_to_lower(month.abb[i]),"_med_t"),df)
     int = int+1 # through int+1 the next month will be selected (feb,march....)
     }
remove(res, int, monthly_med_t, df)


#temperature seasonal####
sp_mn_t= mt_mn_temp %>% 
  filter(month(yr_mt) >= 3 & month(yr_mt) <= 5) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

su_mn_t = mt_mn_temp %>% 
  filter(month(yr_mt) >= 5 & month(yr_mt) <= 11) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

wi_mn_t = mt_mn_temp %>% 
  filter(yr_mt < "2009-12-15") %>% #last december can not be used since one month is too short to base mean for whole winter on this month
  filter(month(yr_mt) < 5 | month(yr_mt) > 11) %>% 
  mutate(hydro_year = rep(hydro_year_wi, catch_n)) %>% 
  group_by(gauge, hydro_year) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-hydro_year) %>% 
  as.data.frame()

gauges$wi_mn_t = colMeans(wi_mn_t)


#days below 0 ####

#in winter
wi_days_below_0 = temp_long %>% 
    filter(date < "2009-12-01") %>% #last december can not be used since one month is too short to base mean for whole winter on this month
    mutate(hydro_year = rep(hydro_year[-c(1:31)]-1, catch_n)) %>% #-31 because we kicked out december and -1 because we are starting one year before so 1.1.1970-30.4.1970 counts to the year 1970
    filter(month(date) <= 4 | month(date) >= 12) %>% 
    group_by(gauge, hydro_year) %>% 
    filter(temp < 0) %>% 
    summarise(n_days = n()) %>% 
    spread(key=gauge, value=n_days) %>% 
    dplyr::select(-hydro_year) %>% 
    as.data.frame()



sp_days_below_0 = temp_long %>% 
  filter(month(date) >=3 & month(date) < 6) %>% 
  group_by(gauge, year(date)) %>% 
  filter(temp < 0) %>% 
  summarise(n_days = n()) %>% 
  spread(key=gauge, value=n_days) %>% 
  dplyr::select(-`year(date)`) %>% 
  as.data.frame()

yr_days_below_0 = temp_long %>%  #yearly number of days below 0
  group_by(gauge, year(date)) %>% 
  filter(temp < 0) %>% 
  summarise(n_days = n()) %>% 
  spread(key=gauge, value=n_days) %>% 
  dplyr::select(-`year(date)`) %>% 
  as.data.frame()


#temp yearly####
yearly_mn_t = mt_mn_temp %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

gauges$mean_t = colMeans(yearly_mn_t)


#yearly max and min temperature
#calculated from the 30day mean
d30_mn_t = rollapply(data = da_temp_wide,FUN= mean, width=30, by.column=TRUE, fill=NA, align="center") %>% as.data.frame()


yearly_max_t = d30_mn_t %>%  #yearly max temp based on 30 day moving average
  mutate(date = date_seq_long) %>% 
  gather(key=gauge, value=d30_mn_t, -date) %>% 
  mutate(gauge=as.integer(gauge), year= as.integer(year(date))) %>% 
  group_by(gauge, year) %>% 
  summarise(max_temp = max(d30_mn_t, na.rm=T)) %>% 
  spread(key=gauge, value=max_temp) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_min_t = d30_mn_t %>%  #yearly min temp based on 30 day moving average
  mutate(date = date_seq_long) %>% 
  gather(key=gauge, value=d30_mn_t, -date) %>% 
  mutate(gauge=as.integer(gauge), year= as.integer(year(date))) %>% 
  group_by(gauge, year) %>% 
  summarise(min_temp = min(d30_mn_t, na.rm=T)) %>% 
  spread(key=gauge, value=min_temp) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()



remove(d30_mn_t)




# precipitation yearly ####

yearly_sm_p = mt_sm_p %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(sum_mm = sum(month_sum)) %>% 
  spread(key=gauge, value=sum_mm) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

# precipitation seasonal ####

su_sm_p = seasonal_data(lb_season = 6, ub_season = 8, funx = "sum") #summer
sp_sm_p = seasonal_data(lb_season = 3, ub_season = 5, funx = "sum") #spring
wi_sm_p = seasonal_data(lb_season = 12, ub_season = 2, funx = "sum") #winter
at_sm_p = seasonal_data(lb_season = 9, ub_season = 11, funx = "sum") #autumn
#dry spells

  #selecting only summer month
su_ds_t = precip_long %>% 
    filter(month(date) >= 5 & month(date) <= 11) %>% 
    mutate(rain = 0)  
  
  #which days is there no rain
  su_ds_t$rain[which(su_ds_t$sum_mm > 0.5)] = 1
  
   su_ds = su_ds_t%>% 
      group_by(gauge, year(date)) %>% 
      mutate(cum_precip = cumsum(rain)) %>%  # cumulative count of the days where there is no rain
      summarise(dry_spell = getmode(cum_precip) ) %>%  #get the mode of the cumsum it is the longest dry spell in that year
      spread(key=gauge, value = dry_spell) %>%  #wide format
      dplyr::select(-`year(date)`) %>% 
      set_colnames(1:catch_n)

yr_ds_t = precip_long %>% 
    mutate(rain = 0) 

  yr_ds_t$rain[which(yr_ds_t$sum_mm > 0.5)] = 1
    yr_ds = yr_ds_t%>% 
      group_by(gauge, year(date)) %>% 
      mutate(cum_precip = cumsum(rain)) %>% 
      summarise(dry_spell = getmode(cum_precip) ) %>% 
      spread(key=gauge, value = dry_spell) %>% 
      dplyr::select(-`year(date)`) %>% 
      set_colnames(1:catch_n)




#days without rain ####
#during the summer months
su_dwr = precip_long %>% 
  filter(month(date) >= 5 & month(date) <= 11) %>% 
  group_by(gauge, year(date)) %>% 
  #defining days with less than 0.5mm as no rain day
  summarise(days_no_rain= length(which(sum_mm < 0.5))) %>% 
  spread(value=days_no_rain, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)

#during the winter months
wi_dwr = precip_long %>% 
  filter(month(date) < 5 | month(date) > 11) %>% 
  group_by(gauge, year(date)) %>% 
  summarise(days_no_rain= length(which(sum_mm < 0.5))) %>% 
  spread(value=days_no_rain, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)

#yearly 
yearly_dwr = precip_long %>% 
  group_by(gauge, year(date)) %>% 
  summarise(days_no_rain= length(which(sum_mm < 0.5))) %>% 
  spread(value=days_no_rain, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)


#severe precipitation events

#during summer
su_ext_p = precip_long %>% 
  filter(month(date) >= 5 & month(date) <= 11) %>% 
  group_by(gauge, year(date)) %>% 
  #rolling sum over three days summing the precipitation and getting the yearly max value
  summarise(severe_events= max(rollapply(data = sum_mm, FUN= sum, width = 3, align="right"))) %>% 
  spread(value=severe_events, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)

#during winter
wi_ext_p = precip_long %>% 
  filter(month(date) < 5 | month(date) > 11) %>% 
  group_by(gauge, year(date)) %>% 
  summarise(severe_events= max(rollapply(data = sum_mm, FUN= sum, width = 3, align="right"))) %>% 
  spread(value=severe_events, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)

#yearly
yearly_ext_p = precip_long %>% 
  group_by(gauge, year(date)) %>% 
  summarise(severe_events= max(rollapply(data = sum_mm, FUN= sum, width = 3, align="right"))) %>% 
  spread(value=severe_events, key=gauge) %>% 
  dplyr::select(-`year(date)`) %>% 
  set_colnames(1:catch_n)


#precip - PET ####
#calculating winter and summer mean 

wi_p_pet= seasonal_data(lb_season = 12, ub_season = 2, dat=spei_data, funx="mean", value="p_pet")

su_p_pet= seasonal_data(lb_season = 6, ub_season = 8, dat=spei_data, funx="mean", value="p_pet")


#precipitation - pet monthly monthly ####

monthly_p_pet = spei_data %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = p_pet) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_p_pet, c("gauge", "month")) #produce data frames by gauge and month
 int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){ #starting with january
     df =  Reduce(rbind,res[int]) %>% 
         spread(key=gauge,value= monthly_mean) %>% 
         dplyr::select(-yr_mt, -month)
     assign(paste0(str_to_lower(month.abb[i]),"_p_pet"),df)
     int = int+1 # through int+1 the next month will be selected (feb,march....)
    
     }
remove(res, int, monthly_p_pet, df)

#precipitation monthly

mt_sm_p$month = month(mt_sm_p$yr_mt) %>% as.integer


res =  plyr::dlply(mt_sm_p, c("gauge", "month"))  #produce data frames by gauge and month (so 377 * 12 data frames)
int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
for(i in 1:12){ #starting with january
   df=  Reduce(rbind,res[int]) %>% 
       spread(key=gauge,value= month_sum) %>%
       dplyr::select(-yr_mt, -month)
   assign(paste0(str_to_lower(month.abb[i]),"_sm_p"),df)
   int = int+1 # through int+1 the next month will be selected (feb,march....)
   }
 
remove(res, int, df)


#monthly PET

monthly_pet = spei_data %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(pet = pet_th) %>% 
  mutate(month= month(yr_mt) %>% 
  as.integer())

res =  plyr::dlply(monthly_pet, c("gauge", "month")) #produce data frames by gauge and month
int = seq(1,length(res),12) # producing a sequence to select all january of every catchment 
 for(i in 1:12){ #starting with january
   df=  Reduce(rbind,res[int]) %>% 
       spread(key=gauge,value= pet) %>% 
       dplyr::select(-yr_mt, -month)
     assign(paste0(str_to_lower(month.abb[i]),"_pet"),df)
     int = int+1 # through int+1 the next month will be selected (feb,march....)
     }
remove(res, int, monthly_pet, df)

