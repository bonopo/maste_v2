
# Statistical -------------------------------------------------------------

#field significance ####
#see renard 2008 and Burn et al 2002


fs_wi_sm_p=  field.significance(loc_sig = 0.05, data_x= wi_sm_p, global_sig=0.05, nsim=600)



#monthly field significance
fs_mt_q= c()
for ( i in 1:12) {
fs_mt_q[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_mn_q")), global_sig=0.05, nsim=600)
}
#this is just an excerpt
#for nearly all variables the lower bound of the p value is 0.03!


#statistic tests
#this is just an excerpt

wilcox.test.modified(x_m="mmky_wi_sm_p")

t.test(x = mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 0])

anova(lm(mmky_su_mn_t$sen_slope ~ gauges$sr_new)) %>% summary()