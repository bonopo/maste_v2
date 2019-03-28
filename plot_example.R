
# plot example -----------------------------------------------------


#storage transfer march - june ####

# hist(gauges$sr_new) # normal
# hist(mmky_mar_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)][gauges$sr_new==0]) #not normal
# hist(mmky_mar_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)][gauges$sr_new==2]) # not normal

y= mmky_jun_mn_q # mmky_jun_mn_q
x1= mmky_mar_mn_q # mmky_mar_mn_q 
x2= gauges$sr_new #or gauges$sr

lm_y = y$sen_slope[which(x1$new_p<fs_mt_q[6] & y$new_p< fs_mt_q[3])] 
# retreiveing only the ones that are significant
lm_x1 = x1$sen_slope[which(x1$new_p<fs_mt_q[6] & y$new_p< fs_mt_q[3])]
lm_x2 = x2[which(x1$new_p<fs_mt_q[6] & y$new_p<fs_mt_q[3])]

data_plot = cbind.data.frame(lm_y,lm_x1, lm_x2)
sig_points = which(x1$new_p<fs_mt_q[6] & y$new_p< fs_mt_q[3])
#transformation to normal
lm_x1_w = lm_x1[which(lm_x2 == 2)] #winter lf
lm_x1_s = lm_x1[which(lm_x2 != 2)] #not winter low flows
lm_y_norm = abs(min(lm_y))+lm_y # adding minim
lm_x1_w_norm= (lm_x1_w+abs(min(lm_x1_s))) 
lm_x1_s_norm= (lm_x1_s+abs(min(lm_x1_s))) #adding the minima of summer (!) to both summer and winter
lm_x1_norm =  abs(min(lm_x1)-0.0000001)+lm_x1 # adding the minima +0.0000001 to enable log transform of the data to be able normalize



hist(log(lm_x1_s_norm)) #normal
hist(log(lm_x1_w_norm))#normal
hist((lm_y_norm)^5) #normal


data_lm = cbind.data.frame(lm_y_norm,lm_x1_norm, lm_x2)


fm = lm((lm_y_norm)^5 ~ log(lm_x1_norm)*lm_x2, data=data_lm)


#removing influencial points
cook_d = influence.measures(fm) #with threshold 4/n (dorman)
table= cooks.distance(fm)
threshold = 4/length(data_lm$lm_y_norm)
high_leverage = which(table >threshold) %>% as.numeric()

#new linear regression without the points with high leverage
fm2= lm((lm_y_norm)^5 ~ log(lm_x1_norm)*lm_x2, data=data_lm[-high_leverage,]) 

##after visual check removing row 154
fm3= lm((lm_y_norm)^5 ~ log(lm_x1_norm)*lm_x2, data=data_lm[-c(high_leverage,154),]) 


data_plot = data_plot[-c(threshold,154),]

#error bars for june
error_jun_mn_q = error.bar("jun_mn_q")
 
p=ggplot(data= data_plot, aes(y=lm_y, x=lm_x1, col=as.factor(lm_x2)))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
 geom_point(data=  data_plot[data_plot$alp==0 & lm_x2 == 2,] , aes(x=lm_y, y=lm_x1), col="blue", show.legend = F)+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -2.5, label=paste("n = ", length(data_plot$lm_y)))+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -1, label=paste(paste("p =", round(fs_mt_q[3],3))))+
  annotate(geom="text", -Inf, -Inf,  hjust =-0.2, vjust = -4, label=paste("r²=",round(summary(fm2)$adj.r.squared,2)))+
  ylab(paste("June median q trend (slope) [m³/s/a]"))+
  xlab(paste("March median q trend (slope) [m³/s/a]"))+
 geom_linerange(alpha= .4,aes( ymin=low_lim, ymax = upp_lim ), data = error_jun_mn_q[sig_points,], lwd=.8)+
  scale_color_discrete("Regime", label=c("pluvial", "nival"))+
  theme(legend.direction = "horizontal", legend.position = "bottom")+
  theme_bw()


# drought ssi spi/spie correlation heatmaps -------------------------------

drought_sci_0 = dr_corr(threshhold = 0)

cor_spi = matrix(nrow=catch_n, ncol=length(drought_sci_0))
cor_spei = matrix(nrow=catch_n, ncol=length(drought_sci_0))

for (a in 1:length(drought_sci_0)){
for (g in 1:catch_n){
temp= drought_sci_0[[a]] %>% 
  filter(gauge== g)
cor_spi[g, a] = cor(y= temp$ssi , x= temp$spi, use="c", method = "spearman") 
cor_spei[g, a] = cor(y= temp$ssi , x= temp$spei, use="c", method = "spearman")
}
}

# plotly::plot_ly(x= 1:6, y=1:337, z=cor_spei[bfi_id,], type="heatmap")
# plotly::plot_ly(x= 1:6, y=1:337, z=cor_spi[bfi_id,], type="heatmap")
