
# User defined functions --------------------------------------------------

#data formatations functions ####

load_file <- function(file, #time series in wide format. Colums are catchments rows are daily time steps
                      value_name, #how the variable should be named
                      origin="1970-1-1" #date format an beginning
                      ){
  output <- melt(file, varnames = c("date", "gauge"), value.name = value_name ) #creates a long format from the wide format
  seq_date <- seq.Date(from= as.Date(origin),by=1, length.out = diff(range(output$date))+1) %>% 
  rep(., times=length(unique(output$gauge))) #adds a column with the date
  output %<>%
  mutate(date = seq_date) %>% 
  mutate(gauge = parse_number(gauge)) %>% #since catchments are not numbered but have a name before the character must be dropped 
  mutate(gauge =as.integer(gauge)) %>% 
  as.tibble()
  return(output)
}

seasonal_data = function(
  lb_season=1, # when does the season begin
  ub_season=12,  # when does the season end
  dat = mt_sm_p, # data frame in wide format
  value="month_sum", # which variable to summarise
  funx = "max" # how should the value be summarised (max, sum , mean..)
  ){
  
  
  # for the winter season a seperate calucation method has to  be done as the december of the previous year has to be atttributed to the remaing winter months of the following year (i.e. dec. - jan.)
  if(lb_season > ub_season){ 
    rg_year = range(dat$yr_mt) %>% year
    # produce a new column for the "winter problem"
    hydro_year = c(rep(rg_year[1],time=ub_season), 
                   rep((rg_year[1]+1):rg_year[2], each=(ub_season+13-lb_season)),
                   rep((rg_year[2]+1),time=13-lb_season)) 
    res = dat %>% 
        filter(month(yr_mt) <=ub_season|month(yr_mt) >=lb_season) %>%  # filter the considered months of the season
        mutate(hy_year = rep(hydro_year, times=catch_n)) %>% #add hydro year
        group_by(hy_year, gauge) %>% #calculate the summaising function for every year and every catchment seperatly
        summarise(mean = get(funx, mode="function")(get(value))) %>%  #summarising funtion
        spread(key=gauge, value=mean) %>%  # wide format for the mann-kendall test
        as.data.frame() 
    
  }else{ # same es above for all other seasons other than winter
        res = dat %>% 
        filter(between(month(yr_mt),lb_season,ub_season)) %>%
        group_by(year(yr_mt), gauge) %>% 
        summarise(mean = get(funx, mode="function")(get(value))) %>% 
        spread(key=gauge, value=mean) %>% 
        as.data.frame() 
       }  
  ts_data = res[,2:(catch_n+1)]
  
return(ts_data)  
}


# non parametric sci calculation ------------------------------------------

sci_np <- function(
    sci_data="mt_mn_q_wide", #monthly data from which the ranks should be calculated
    agg_n=1, # aggregation period
    sci_name="spi"  # how should the variable be names in the global environment
          ){
        for (a in agg_n){ # for every aggregation period
          erg <- matrix(nrow=480, ncol=(catch_n+1))
          data <- get(sci_data)
               if(a>1){ #aggregate over the required months
               data <- rollapply(data, width=a, FUN=mean, 
                                 by.column =TRUE, align="right", fill=NA) %>%
                          as.data.frame()
               }
        data$yr_mt <- date_seq # create date column from date_seq from global environment
            for(i in 1:catch_n){ #for every catchment
                for (m in 1:12) { #for every month calculate the ranks
                      data_temp <- data %>%
                        filter(month(yr_mt) == m) %>% #select all the required months from the time series
                        dplyr::select(i) # select the specific catchment
                         data_temp_na <- data_temp[which(!is.na(data_temp)),] #removing NA, produced by moving average
                  erg[(40*m-39):(40*m),i] <-  c(rep(NA,times=length(which(is.na(data_temp)))),qnorm(trunc(rank(data_temp_na))/(length(data_temp_na)+1))) # fitting data to follow normal inverse cumulative function
                  erg[(40*m-39):(40*m),(catch_n+1)] <- data$yr_mt[month(data$yr_mt)==m] # adding date to sort later
                } # closing  the months for loop
              } # closing the catchment loop
       erg <- erg[order(as.Date(erg[,(catch_n+1)])),] %>% as.data.frame() #sorting series according to the dates
       colnames(erg) <- c(1:catch_n,"yr_mt")
       erg$yr_mt = as.Date(erg$yr_mt, origin = "1970-01-01")
        cat(paste0(sci_name,"_",a)," finished")
       assign(paste0(sci_name,"_",a), erg, envir = .GlobalEnv) #save into the global environment
      
        } # closing the aggregation for loop
}#end of function, non parametric data frame is saved into global environment



# correlation ####

#calculate cross correlation between SCI (of a certain aggregation period) and SSI
sci_ccf <- function(
  sci= c(1,2,3,6,12,24), # the aggregation month to consider
  sci_namex="spei_v2_", # x = SCI the name of the SPEI data frame it should retrieve from the global environment
  sci_namey="ssi_1" # y= SSI the name of the ssi data frame it should retrieve from the global environment
  ){
  ccf_tot <- list()
  ccf_acf <- data.frame()
  ccf_lag <- data.frame()
  y <- get(sci_namey)
  i=1
  for (a in sci){ #for every aggregation month
    sci_data <- get(paste0(sci_namex,a))
      for(g in 1:catch_n){ # for every catchment
        ccf_temp <- ccf(x= sci_data[,g], y= y[,g], 
                        na.action = na.pass, plot = FALSE)
        ccf_acf[g,i] <- max(ccf_temp$acf)
        ccf_lag[g,i] <- ccf_temp$lag[which.max(ccf_temp$acf),1,1]
      }# finished all catchments
  
    i= i+1}# finished all aggregation months
  
    colnames(ccf_lag) <- as.character(sci)
    colnames(ccf_acf) <- as.character(sci)
    ccf_tot[[2]] <- ccf_lag #returns the lag at which the highest cross correlation is found between SCI and SSI (expected optimal lag should be 0 for all catchments)
    ccf_tot[[1]] <- ccf_acf # the correlation value
  return(ccf_tot)
}

#correlation of SCI and SSI
cor_sci_ssi <- function(
  sci_n= c(1,2,3,6,12,24), # the aggregation month to consider
  cor_met="s", # the correlation type it should calculate, default: spearman
  sci="spi_v2_", # the name of the SCI data frame it should retrieve from the global environment
  ssi="ssi_1" # the name of the ssi data frame it should retrieve from the global environment
  ){
   mat <- matrix(ncol=length(sci_n), nrow=catch_n)
   i <- 1
  for (n in sci_n){ # for all aggreagtion periods
    x_data <- get(paste0(sci,n))
    y_data <- get(ssi)
       for (g in 1:catch_n){ # for all catchment numbers
          mat[g,i] <- cor(x= x_data[,g], y_data[,g], 
                          method = cor_met, use="na.or.complete" )
           } #catchments
     i = i+1} # aggregation months
  df = mat %>% as.data.frame() #result is a data frame with the correlation values of every catchment at every aggreagtion month considered
  colnames(df) = sci_n
return(df)

}


#monthly sci analysis ####
#calculates the correlation of every month with the SCI and SSI at different aggreagation months to see changes over the year i.e. in autumn higher correlation with longer aggreagation periods than during spring
monthly_sci = function(
  month=3, # which month should be looked at (1:12)
  threshold = 0 # what is the drought threshold default : 0
  ){
  res.list = list()  
  for (c in 1:catch_n){ # for loop for all catchments
    
      int= which(month(date_seq)==month) # selecting the specific month entered as an argument in the function
      df= cbind.data.frame(ssi = ssi_1[c(int),c], spi_v2_1[c(int),c], spi_v2_2[c(int),c], spi_v2_3[c(int),c], spi_v2_6[c(int),c], spi_v2_12[c(int),c], spi_v2_24[c(int),c], spei_v2_1[c(int),c], spei_v2_2[c(int),c], spei_v2_3[c(int),c], spei_v2_6[c(int),c], spei_v2_12[c(int),c], spei_v2_24[c(int),c]) # produce data frame with all spi and spei values of the specific months where there was a drought
      df_drought = df %>% filter(ssi < threshold)
      colnames(df_drought) = c("ssi", "spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24", "spei_1","spei_2","spei_3", "spei_6", "spei_12","spei_24")
      res.list[[c]] =df_drought
  
  }#  close loop of all catchments
  mat = matrix(nrow=catch_n, ncol=12)
  
  for (n in 2:13)
    {
  mat[,(n-1)]= sapply(1:catch_n, function(x) cor(x= res.list[[x]]$ssi,y= res.list[[x]][,n], use="na.or.complete", method = "spearman"))# calculate the spearman correlation of the sci with the ssi
  }
  mat_cor = mat %>% as.data.frame()
  colnames(mat_cor) = c("spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24", "spei_1","spei_2","spei_3", "spei_6", "spei_12","spei_24")
  mat_cor = cbind(mat_cor,gauge= 1:catch_n,sr=  gauges$sr_new,saar= gauges$saar, hydro_geo = gauges$hydrogeo_simple, landuse= gauges$landuse) %>% as.tbl()
  #add meta information for easier grphic analysis in a seperate next step
  mat_cor_long = gather(mat_cor, key=sci_type, value=cor, -gauge,-landuse, -sr, -saar, -hydro_geo, factor_key = TRUE) %>% as.data.frame()  
return(mat_cor_long)
}


# drought characteristics -------------------------------------------------

#correlating discharge drought periods with climate indicators (SCI)

dr_corr = function(
  threshhold = 0, # which ssi value is seen as drought
  agg_month = c(1,2,3,6,12,24) # the aggregation months to consider
  ){
  try(if(threshhold < min(ssi_1_long$ssi)) stop ("Too low threshhold. Choose higher SSI Value!"))
  result_part= matrix(nrow = 0, ncol=6)
  result = list()
  

droughts_q =ssi_1_long %>% 
  filter(ssi<=threshhold) %>% 
  as.data.frame() #retrieving the ssi data from the global environment

spi_d = lapply(agg_month, FUN = function(x)  cbind(get(paste0("spi_v2_", x)), ymd(date_seq)) %>% as.data.frame() %>% gather(key=gauge, value=spi,-`ymd(date_seq)`))
 # producing workable dataframes with the spi value of every month under hydrologic drought
spei_d = lapply(agg_month, FUN = function(x)  cbind(get(paste0("spei_v2_", x)), ymd(date_seq)) %>% as.data.frame() %>% gather(key=gauge, value=spei,-`ymd(date_seq)`))
 # producing workable dataframes with the spei value of every month under hydrologic drought

for(i in 1:length(agg_month)){ #for every aggregation month 
  for (g in 1:catch_n){ #for every catchment
    
    spi_d_catch = spi_d[[i]] %>%  #spi
        filter(gauge == g) #filtering every aggregation month and catchment
   
    spei_d_catch = spei_d[[i]] %>%  #spei
        filter(gauge == g) #filtering every aggregation month and catchment
    
    droughts_q_catch = droughts_q %>% #ssi values of the months under drought
      filter(gauge==g)
    

  int= pmatch(droughts_q_catch$yr_mt,spei_d_catch$`ymd(date_seq)`) #getting the value of spi or spei of every month with drought (discharge) 

catch_res = droughts_q_catch %>%  
    mutate(spi= spi_d_catch$spi[int], spei= spei_d_catch$spei[int], date_sci= spei_d_catch$`ymd(date_seq)`[int]) 
 #creating dataframe with the spei and spi values to every drought month according to the ssi threshold
  
  result_part = rbind(result_part, catch_res)

  
  }
  result[[i]] = result_part
  result_part= matrix(nrow = 0, ncol=6)
  cat(round(i/(length(agg_month)),2)*100,"%" ,"\n")

  }
return(result)
}

#computing of drought deficit and days of drought per month per catchment per year from the gathered drought information through the 80th method

seasonal_80th = function(
  data= drought_q, # data frame of all drought according to the 80th definition
  year_ta= 1970:2009){ #years to consider
  
  #creating subfunction for parallel computing
  
sub_80th =  function(i) {
  
  temp1 = data %>%
    filter(catchment == i) #parallelized for every catchment
      mat_def = matrix(nrow=length(year_ta), ncol=12,data=0)
      mat_days = matrix(nrow=length(year_ta), ncol=12,data=0)
      mat_n = matrix(nrow=length(year_ta), ncol=12,data=0)

  for (e in 1:max(temp1$event_no)){
  #retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought (in days)
     mt_yr = NULL
     mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month") #create sequence of the months effected by drought to attribute the drought information to the specific months
     
     
           if(day(ymd(temp1$dr_end[e])) <  day(ymd(temp1$dr_start[e]))){
             mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
           }#problem: the seq leaves out the last month if the day (in the month) of drought end is before the day (in the month) of the drought start therefore this if statement
     
    year_y = year(mt_yr) - (min(year_ta)-1) #to set row indice in matrix
    month_x = month(mt_yr) # to set column indice in matrix
  
  #writing to matrix the cumulative deficit and days of drought
    
    if (length(month_x) == 1 ){
      #if drought starts and ends in the same month
      
      #deficit
      mat_def[cbind(year_y,month_x)] = sum(mat_def[cbind(year_y,month_x)],temp1$def_vol[e]*as.numeric(ymd(temp1$dr_end[e])-ymd(temp1$dr_start[e]))) #sum to include the information of a possible previous event in the same month, if for example there was a drought already at the beginning of the month
      #def.vol * number of days = total deficit of the drought event
      
      #drought length
      mat_days[year_y[1],month_x[1]] = sum(mat_days[cbind(year_y,month_x)],as.numeric(ymd(temp1$dr_end[e])-ymd(temp1$dr_start[e])))
      # drought end - drought beginning = number of days with drought 
    }else { #if drought was longer than 1 month
      
    # if there was already a drought in that month the new drought event needs to be added to the drought (that already occured in that month). Happens where there are two short droughts right after each other. 
     
       if(length(month_x)>2){  #if drought is longer than two months
         
         #drought deficit 
         mat_def[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = temp1$def_vol[e]*as.numeric(days_in_month(month_x[2:(length(month_x)-1)]))
         
         #drought length
        mat_days[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = days_in_month(month_x[2:(length(month_x)-1)]) %>% as.numeric()
       }
      
      #drought deficit (in the first month effected)
      mat_def[year_y[1],month_x[1]] = sum(mat_def[year_y[1],month_x[1]],temp1$def_vol[e]*(30-day(ymd(temp1$dr_start[e]))))
      
       #drought deficit (in the last month effected)
      mat_def[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_def[year_y[length(year_y)],month_x[length(month_x)]],temp1$def_vol[e]*day(ymd(temp1$dr_end[e])))
      #drought length (in the first month effected)
      mat_days[year_y[1],month_x[1]] = sum(mat_days[year_y[1],month_x[1]],(days_in_month(month_x[1])+1)-day(ymd(temp1$dr_start[e])))#retrieving the days of in the first month of drought, +1 because the first day of the drought count's as drought
      
       #drought length (in the last month effected)
      mat_days[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_days[year_y[length(year_y)],month_x[length(month_x)]],day(ymd(temp1$dr_end[e])))
    }
  
    #writing to mat_n number of events
      mat_n[cbind(year_y, month_x)] = mat_n[cbind(year_y, month_x)] +1
    
      }
   
  
return(list(mat_days, mat_def, mat_n))#returns list with three dataframes for every catchment (the rows are the years, the columns are the months January - December): one df with the days of drought in every month, one df with the drought deficit, one with the number of events.
}

cl<-makeCluster(no_cores-1) # it is 4 times faster than the sequential loop!
registerDoSNOW(cl) #create the workers
res=list()
pb <- txtProgressBar(max = catch_n, style = 3) #progress bar
progress <- function(n) setTxtProgressBar(pb, n) #progress bar
opts <- list(progress = progress) #progress bar
res <- foreach::foreach(c = 1:catch_n, # elements for the subfunction
                        .packages = c("tidyverse", "lubridate"), #loading externam packages
                        .options.snow = opts #for the progress bar
                        )%dopar%{  #parralel
   sub_80th(i=c) #call the subfunction 
                        }
close(pb) #close progress bar
stopCluster(cl) #stop the workers
return(res)
}


#seasonal 80th data preperation for seasonal trend analysis of the detected droughts

seasonal_80th_trend = function(month = c(1,2,3), #which month to analyse
                               datax= p_days_of_drought_list # input must be a list of one of the drought charachteristics created by the seasonal_80th funtion
                               ){
      res= lapply(1:catch_n, function(x) apply(datax[[x]][,month],1,sum)) %>%
        do.call("cbind", .) %>%
        as.data.frame() %>% 
        set_colnames(1:catch_n)
      return(res)
}

#counting how many month in a year are affected by drought for every year
dr_n <- function(severity = -1 #defining the threshold
                 )  {
try(if(severity < min(ssi_1_long$ssi)) stop ("Too low severity. Choose higher SSI Value!"))
 res<- list()
    for (i in 1:catch_n){ #goes through all the catchments
      ssi_temp <- ssi_1_long %>% 
       mutate(year= year(yr_mt))%>% 
       filter(ssi < (severity)) %>% 
       as.tbl() #filters all the month below the selected severity (i.e threshhold)
      
      res[[i]] <- ssi_temp %>% 
        filter(gauge== i) %>% 
        group_by(year) %>%  # groups by year
        summarise(occurences = n())  #counts number of occurences below severity in every year
    }#closes for loop of the catchments
 return(res)
}

#counting number of events depending on severity threshhold
#unlike dr_n this functions counts events, they begin if the ssi is below the treshold and the event is terminated if the ssi is above the treshold again. the funtion is used in the next step to calculate severity....
dr_count <- function(severity = -1, datax=ssi_1_long){
  try(if(severity < min(datax[,3], na.rm = T)) stop ("Too low severity. Choose higher SSI Value!"))
 res<- list()
 for (g in 1:catch_n){ #goes through all catchments
s1 <- datax %>% 
  filter(gauge == g , .[,3] < severity) %>%  #filters all month with below the threshold
   mutate(date_diff = c(diff.Date(yr_mt),0)) #calculates the number of days that are between two months that are below the treshold. 

n <- 1 # the first event gets the number 1 assigned
for (i in 1: length(s1$yr_mt)){
  s1$event_n[i] <- n
if(s1$date_diff[i] > 31) { #if two months are affected by drought but they are not chronologically after each other they are seen as seperate events
  n <- n+1
}}
res[[g]] <- s1}
 return(res) #returns list with data frames for every catchment with the month affected by drought and what number of event it is (i.e. is it a continues event from the previous month or a new event)
}

#sum of severity per event
dr_severity <- function(severity = -1, #drought defining threshold
                        datax=ssi_1_long # data source
                        ){
  try(if(severity < min(datax[,3], na.rm=T)) stop ("Too low severity. Choose higher SSI Value!"))
  res_list <- list()
  raw_data = dr_count(severity = severity, datax = datax) #runs the number of events function
      for (g in 1:catch_n){ #goes through all catchments
          data  <- raw_data[[g]]
          res   <- matrix(nrow = max(data$event_n), ncol=6) 
          
              for (d in 1:max(data$event_n)){ #goes through every event and calculated drought parameters
                res[d,1]        <- sum(filter(data, event_n == d) %>%  dplyr::select(3)-severity) # summing the deviation from the threshhold as the drought drought severity index (dsi)
                res[d,2]        <- d # event number
                res[d,3]        <- data$yr_mt[data$event_n == d][1]  # drought beginning
                res[d,4]        <- tail(data$yr_mt[data$event_n == d],1) # drought end
               if(res[d,4] - res[d,3] == 0){
                   res[d,5] =  days_in_month(as.Date(res[d,3], origin = "1970-01-01"))}else{
                     res[d,5] =  res[d,4] - res[d,3] # drought length
                   }
                res[d,6] = res[d,1]/((month(res[d,4]) - month(res[d,3]))+1) #drought intensity: dsi/drought length. basically the mean deviation from the threshold
        }
          
          colnames(res) <- c("dsi", "event_n", "dr_start", "dr_end", "dr_length", "dr_intens")
          res <- as.data.frame(res)
          res$dr_start<- as.Date(res$dr_start, origin = "1970-01-01")
          res$dr_end<- as.Date(res$dr_end, origin = "1970-01-01")
          res_list[[g]] <- res #returns list with a data frame for every catchment with the calculated drought parameter for every drought event ordered chronologically
          }
return(res_list)
}





# trend calculaton --------------------------------------------------------



#calculating man kandell trend with correction for serial correlated data using Yue and Wang's (2004) approach

mmky_par = function(raw_data =c("ms7_min", "ms30_min") #the data must be in wide format, columns are the catchments, rows are the time steps (i.e. mean yearly temperature) ordered chronologically from 1970 -> 2009
                    ){

  for(d in raw_data){
      ts_data = get(d)
      #modified mk test
      res_mmky = t(sapply(c(ts_data[,1:ncol(ts_data)]), FUN =mmky_edit))
      colnames(res_mmky) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")
      assign(paste0("mmky_", d), as.data.frame(res_mmky), envir = .GlobalEnv )
  }
}


mmky_edit = function(x)
{
    x = x
    z = NULL
    z0 = NULL
    pval = NULL
    pval0 = NULL
    S = 0
    Tau = NULL
    essf = NULL
    if (is.vector(x) == FALSE) {
        stop("Input data must be a vector")
    }
    if (any(is.finite(x) == FALSE)) {
        x <- x[-c(which(is.finite(x) == FALSE))]
        warning("The input vector contains non-finite numbers. An attempt was made to remove them")
    }
    n <- length(x)
    V <- rep(NA, n * (n - 1)/2)
    k = 0
    for (i in 1:(n - 1)) {
          for (j in (i + 1):n) {
              k = k + 1
              V[k] = (x[j] - x[i])/(j - i) #calculate the differences between the data points according to mann - kendall formula
          }
    }
    slp <- median(V, na.rm = TRUE) #calculate sen's slope
    t = 1:length(x)
    xn = (x[1:n]) - ((slp) * (t)) #detrending the time series for auto correlation calculation   #xn = new time series 
 
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            S = S + sign(x[j] - x[i]) #calculates S statistic from Mann kendall
        }
    }
    ro <- acf(xn, lag.max = (n - 1), plot = FALSE)$acf[-1] #calculates autocorrelation at n-1 lags therefore the result is a n-1 vector with the autocorrelation values
    rof <- rep(NA, length(ro))
    for (i in 1:(length(ro))) {
        rof[i] <- ro[i]
    } # rof = ro this step is pointless, but maybe a safety check
    ess = 0 #effective sample size
    for (k in 1:(n - 1)) {
        ess = ess + (1 - (k/n)) * rof[k] #calculates effective sample size
    }
    essf = 1 + 2 * (ess) #calculates effective sample size
    var.S = n * (n - 1) * (2 * n + 5) * (1/18) #calculates not corrected variance
    if (length(unique(x)) < n) { #correction of variance for ties
        aux <- unique(x)
        for (i in 1:length(aux)) {
            tie <- length(which(x == aux[i]))
            if (tie > 1) {
                var.S = var.S - tie * (tie - 1) * (2 * tie + 
                  5) * (1/18)
            }
        }
    }
    VS = var.S * essf #variance correction of the original variance with ess
    if (S == 0) { # calculates S  of Mann Kendall test
        z = 0 # corrected z statistic
        z0 = 0 #  non corrected z statistic
    }
    if (S > 0) {
        z = (S - 1)/sqrt(VS) # corrected z statistic
        z0 = (S - 1)/sqrt(var.S) #  non corrected z statistic
    }
    else {
        z = (S + 1)/sqrt(VS)
        z0 = (S + 1)/sqrt(var.S)
    }
    pval = 2 * pnorm(-abs(z)) # calculates corrected p value
    pval0 = 2 * pnorm(-abs(z0)) #  calculates non corrected p value
    Tau = S/(0.5 * n * (n - 1))
    return(c(`Corrected Zc` = z, `new P-value` = pval, `N/N*` = essf, 
        `Original Z` = z0, `old P.value` = pval0, Tau = Tau, 
        `Sen's slope` = slp, old.variance = var.S, new.variance = VS,s_stat = S))
}

#mmky for subset of data set
#to see how large the influence is of the time period considered
mmky_sbst = function(raw_data =ms7_min, #data set in the wide format (same as for mmky_par function)
                     width = 30, # how many years should a trend be calculated for (max = 40 since that is the length of the time series)
                     start_y=1970 #  when to start to calculate the trends on the subset
                     ){
  n=nrow(raw_data)
  mat = matrix(nrow=catch_n, ncol=(n-width))
  for ( i in 1:(n-width)){
      sbst = raw_data[i:(i+width-1),]
      #modified mk test
      mat[,i] = t(sapply(c(sbst[,1:ncol(sbst)]), FUN =mmky))[,7] #only interested in sen's slope sigificance is not important since the amount of data is too little to be significant
 }
  mat %<>% as.data.frame()
  colnames(mat) = c(start_y:(start_y+n-width-1))
  return(mat)
}

#calculate seasonal trends and (possibly) create latex compatible tables

seasonal_trends = function(
  lb_season=1, # when does the season begin
  ub_season=12,  # when does the season end
  dat = mt_sm_p, # data frame in wide format
  value="month_sum", # which variable to summarise
  funx = "max", # how should the value be summarised (max, sum , mean..)
  xtable = T, # should the output be the ranges and the % of catchments with positive/negative trends
  px=0.03, #  p value
  ref = T # should the ranges be referenced to something e.g. mean discarge
  ){
  
  
  # for the winter season a seperate calucation method has to  be done as the december of the previous year has to be atttributed to the remaing winter months of the following year (i.e. dec. - jan.)
  if(lb_season > ub_season){ 
    rg_year = range(dat$yr_mt) %>% year
    # produce a new column for the "winter problem"
    hydro_year = c(rep(rg_year[1],time=ub_season), 
                   rep((rg_year[1]+1):rg_year[2], each=(ub_season+13-lb_season)),
                   rep((rg_year[2]+1),time=13-lb_season)) 
    res = dat %>% 
        filter(month(yr_mt) <=ub_season|month(yr_mt) >=lb_season) %>%  # filter the considered months of the season
        mutate(hy_year = rep(hydro_year, times=catch_n)) %>% #add hydro year
        group_by(hy_year, gauge) %>% #calculate the summaising function for every year and every catchment seperatly
        summarise(mean = get(funx, mode="function")(get(value))) %>%  #summarising funtion
        spread(key=gauge, value=mean) %>%  # wide format for the mann-kendall test
        as.data.frame() 
    
  }else{ # same es above for all other seasons other than winter
        res = dat %>% 
        filter(between(month(yr_mt),lb_season,ub_season)) %>%
        group_by(year(yr_mt), gauge) %>% 
        summarise(mean = get(funx, mode="function")(get(value))) %>% 
        spread(key=gauge, value=mean) %>% 
        as.data.frame() 
       }  
  ts_data = res[,2:(catch_n+1)] #removing the date column
 
  #mann kendall test of the created seasonal data frame
  res_mmky = t(sapply(c(ts_data[,1:ncol(ts_data)]), FUN =mmky_edit)) 
  colnames(res_mmky) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")
   
  res_2 = as.data.frame(res_mmky)
     
  if(xtable == T){ 
       
        if(ref ==T){ #reference for latex compatible table
          reference_t = apply(ts_data,2,mean) 
        }else{
          reference_t = NULL
        }
       
  mat = matrix(nrow=1, ncol=6, data=c( #creating matrix with the results of the mk test
           pos.neg(dat = res_2, p=NULL, positive = T), 
           # number of positive non signifcant trends
           pos.neg(dat = res_2, p=px, positive = T), 
           # number of positive  signifcant trends
           pos.neg(dat = res_2, p=NULL, positive = F), 
           # number of negative non signifcant trends 
           pos.neg(dat = res_2, p=px, positive = F), 
           # number of negative signifcant trends 
           magnitude(dat= res_2, p=NULL, reference = reference_t ) 
           #range of the mk trends
       )
     ,byrow=T)
    #should the ouput be a latex compatible table of thhe summary of the mann -kendall test or should the output be rather a dataframe with the mann kendall results for every catchment (every row  is one catchment)   
    return(mat)
    }else{
      return(res_2)
          }
   
}


#drought attribution####
 
#attributing monthly flow to aggregated SCI
flow.attribution = function(
  datx = "spei_v2_", # the SCI data frame
  daty= ssi_1_long, # the SSI data frame
  threshold = NULL # what should be attributed? All flow SSI or only discarge values during drought
  ){
  cor_monthly = matrix(nrow=length(agg_month), ncol=12, data=NA)
  n=1
  for(a in agg_month){ # go through all aggregation periods
      dat_x = get(paste0(datx, a)) %>%  # get the SCI data frame 
      mutate(yr_mt = date_seq) %>%  #add date column to be able to do monthly SSI ~ SCI correlations
      gather(key=catchment, value=sci, -yr_mt) #make into long format
      for(m in 1:12){
        temp_x = filter(dat_x, month(yr_mt) == m) %>% #filter every months
          mutate(catchment=as.integer(catchment)) %>%  
          as.tbl
        temp_y= daty%>% 
          set_colnames(c("yr_mt","catchment","ssi")) %>% 
          mutate(catchment=as.integer(catchment)) %>% 
          filter( month(yr_mt) == m) %>% as.tbl #filter by every months
        temp_cor = merge(x=temp_x, y=temp_y)  #merge the SCI and the SSI to one data frame
        if(is.numeric(threshold)){ #to be able to calculate only SSI~SCI correlation during hydrologic drought events
          temp_cor = filter(temp_cor, ssi<threshold)
        }
        cor_monthly[n,m] = cor(temp_cor$ssi, temp_cor$sci, method="s", use="na.or.complete")      #spearman correlation
        
        
        }
      n=n+1 # to save result of next aggregation period in the next line
  }
  return(cor_monthly)
}


#are there trends of monthly discarge attribution to the SCI
#e.g. is autumn getting more sensitive for SPEI of long aggregation periods
  
flow.attribution.trends = function(
  datx = spei_v2_3, # the SCI data frame
  daty= ssi_1_long, # the SSI data frame
  threshold = NULL # what should be attributed? All flow SSI or only discarge values during drought
  ){
  cor_trend = data.frame()
  
      dat_x = datx %>% 
      mutate(yr_mt = date_seq) %>%  #add date column to be able to do monthly
      gather(key=catchment, value=sci, -yr_mt)
      for(m in 1:12){ # go through all months
        temp_x = filter(dat_x, month(yr_mt) == m) %>% 
          mutate(catchment=as.integer(catchment)) %>% #filter every months
          as.tbl
        temp_y= daty%>% 
          set_colnames(c("yr_mt","catchment","ssi")) %>% 
          mutate(catchment=as.integer(catchment)) %>% 
          filter( month(yr_mt) == m) %>% as.tbl #filter every months
        temp_cor = merge(x=temp_x, y=temp_y) # merging the data frames
        if(is.numeric(threshold)){#to be able to calculate only SSI~SCI correlation during hydrologic drought events
          temp_cor = filter(temp_cor, ssi<threshold)
        }
        trend_temp = temp_cor %>% 
          group_by(year(yr_mt)) %>% #calculate spearman correlation of all catchments between SSI and SCI in every month and every year seperatly 
          summarise(cor = cor(ssi, sci, 
                              method="s", use="na.or.complete") )
        cor_trend = rbind(cor_trend, mmky_edit(x=trend_temp$cor)) # mann kendall trend calculation
         
      }
     colnames(cor_trend) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")
    return(cor_trend)    #returns data frame with the trends of the monthly discarge correlation
    }


#statistics####
#field significance after renard 2008 and Burn et al 2002
#burn et al worked with local and global significance levels at 0.1
field.significance = function(
  loc_sig = 0.05, # local significance level
  data_x= ms30_min, # data frame in wide format (columns are catchments, rows are time steps)
  global_sig= 0.05,  # global significance
  nsim=600 # number of simulations
  ){ 
  catch_nx = ncol(data_x)
  n_x=nrow(data_x)
  set.seed(1) #set seed
  loc_sig_dist = c()
  
  simulation = function(x){ #sub function for parralel computing
  for (i in x){ #go through all catchments
  resample_mat = sapply(1:catch_nx, function(x) sample(data_x[,x], size= n_x,  replace=T)) #take a sample with putting back
  mkttest_res = apply(resample_mat,2,mkttest) #calculate the mann kendall test
  loc_sig_dist = length(which(mkttest_res[5,]< loc_sig))/catch_nx # %of catchments that are significant at local significance level
  }
  return(loc_sig_dist) # returns %of catchments that are significant at local significance level
  }
  
cl<-makeCluster(no_cores-1) #create workers
registerDoSNOW(cl) # register workers
pb <- txtProgressBar(max = catch_n, style = 3) #progress bar
progress <- function(n) setTxtProgressBar(pb, n) #progress bar
opts <- list(progress = progress) #progress bar
res <- foreach::foreach(c = 1:nsim, #the bootstrapping simulations
                        .packages = "modifiedmk", #extra package that needs to be laoded
                        .options.snow = opts, #progress bar
                        .inorder = F, #must not be in order, is faster
                        .combine = "c" # the result should be cbinded
                        )%dopar%{  #parralel
   simulation(x=c) # call the sub function, runs the subfuntion in all registered workers for the n number of simulations (nsim)
                        }
close(pb) #progress bar
stopCluster(cl) #stop workers (they can go home)
return(quantile(res,global_sig)) #return the global significance quantile of the local significance distribution created through the sub function
}


#wilcox test modified to test for difference between the two regime types (pluvial and nival)
#e.g. test if the precipitation trends of pluvial and nival regimes differ signifcantly

wilcox.test.modified = function(
  x_m="mmky_wi_mn_t", # which variable to test
  y_m= NULL, # the y variable
  fs_x=NULL, # the p value of the x variable to be able to do the wilcox test for only significant trens
  fs_y = NULL # the p value of the y variable to be able to do the wilcox test for only significant trens 
  ){
  
  if(is.null(y_m)) y_m=get(x_m) # make x = y, if the wilcox test test the differnces within a variable 
  
  if(is.null(fs_x)){
  fs_x= get(paste0("fs_",substr(x_m,6, str_count(x_m)))) #get the field significance value from the global Environment
  }
  
  if(is.null(fs_y)) fs_y = fs_x
  wilcox.test(x = get(x_m)$sen_slope[get(x_m)$new_p < fs_x  & gauges$sr_new == 0], y= y_m$sen_slope[y_m$new_p < fs_y& gauges$sr_new == 2]) # wilcox test is done if the assumtion of the t.test can not be fulfilled (ie. not normal distributed)

}

#sens slope error bars
# after Gilbert 1987

error.bar = function(data= "mar_mn_q"){
  dat = get(data)
  sen_dat= get(paste0("mmky_", data)) #get the sen's slope of the already calculated variable
  res = apply(dat, 2, EnvStats::kendallTrendTest) # calculating the sens slope confidence intervals with the EnvStats package; default is exactly what I want to calculate
  low_lim = sapply(1:catch_n, function(x) res[[x]]$interval$limits[1]) #extracting the lower limit
  upp_lim = sapply(1:catch_n, function(x) res[[x]]$interval$limits[2]) # upper limit
  return(
    cbind.data.frame(
    low_lim, upp_lim, sen_dat=sen_dat$sen_slope)
  )#returns data frame with the lower & upper limit and the "real" sen's slope
}

#how many catchments have positve ore negative trends

pos.neg = function(p=NULL, #to be able to test how many catchments exhibit a significant trend, the user can state a significance level
                   dat=mmky_yr_days_below_0, # date frame of a mann kendall result 
                   positive=T # should it calculate hom many positive or hom many nagative trends are present
                   ){
  
  if(is.null(p)) p = 10 #if no p value is set then p is set to 10 to include all trends (incl. non significant ones)
  if(positive == T) #how many trends exhibit a positive trend
    {
    res = length(which(dat$sen_slope > 0 & dat$new_p < p ))/NROW(dat) %>% round(.,2)*100 
    }else{#how many trends exhibit a negative trend
   res= length(which(dat$sen_slope < 0 & dat$new_p < p ))/NROW(dat) %>% round(.,2)*100 
    }
  
  return(round(res,0)) #returns % of catchments that exhibit a positve or negative trend 
}

#calculate trend ranges
magnitude = function(
  dat = mmky_su_mn_t, # raw data
  p=NULL, # p value
  reference = NULL # should the ranges be references to something e.g. average rainfall sum (saar/ARS) or mean discharge
  ){
  if(is.null(p)) p = 10 #if no p value is set then p is set to 10 to include all trends (incl. non significant ones)
  if(is.null(reference)){ # ranges is not referenced
    res = dat$sen_slope[dat$new_p < p] %>% range %>% round(.,2)
  }else{ #range is referenced to "reference" 
     res = range((dat$sen_slope*40)/reference)*100 %>% round(.,2)
  }
  return(res) #returns the ranges
}


#count number of occurences (for ggplots)
give.n <- function(x){
  return(c(y= -2.4, label = length(x))) 
}


#mode

getmode <- function(v) { #get the mode of vector for dry spell length calculation
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}


