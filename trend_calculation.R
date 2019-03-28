
# Trend calculation -------------------------------------------------------
#this is just an excerpt

#su = summer
#wi = winter
#sp = spring
#at= autumn
#p = precipitation
#ms7 = summer 7 day mean low flow
# sm= sum
#mn =mean
#t= temperature

#yearly trends
mmky_par(raw_data = c( "ms7_date", "ms7_min", "ms30_min", "yearly_q10","yearly_mn_q","su_q10", "su_mn_t", "yearly_mn_t", "yearly_max_t", "yearly_sm_p",   "sp_sm_p","at_sm_p", "su_sm_p", "wi_sm_p", "sp_sm_p",  "su_p_pet", "wi_p_pet","p_days_of_drought_yr" ,"q_days_of_drought_yr","p_sum_def_yr","q_sum_def_yr", "p_n_events_yr", "q_n_events_yr"))


#monthly trends
monthly_q10=c()
for ( i in 1:12) monthly_q10[i] =paste0(str_to_lower(month.abb[i]),"_q10")
mmky_par(raw_data = monthly_q10)

monthly_q35=c()
for ( i in 1:12) monthly_q35[i] =paste0(str_to_lower(month.abb[i]),"_q35")
mmky_par(raw_data = monthly_q35)

monthly_q80 = c()
for ( i in 1:12) monthly_q80[i] =paste0(str_to_lower(month.abb[i]),"_q80")
mmky_par(raw_data = monthly_q80)

monthly_q=c()
for ( i in 1:12) monthly_q[i] =paste0(str_to_lower(month.abb[i]),"_mn_q")
mmky_par(raw_data = monthly_q)
remove(monthly_q)


