
# Preambel -------------------------------------------

setwd("C:/Users/Menke/Dropbox/masterarbeit/R/r_CD")

#install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "SPEI", "eha","reliaR", "PearsonDS","FAdist","trend", "Kendall","mgcv", "modiscloud", "Hmisc", "scales", "sn", "randomForest", "gridExtra", "foreach",  "doSNOW", "snow", "itertools","beeswarm"))
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI",  "lubridate", "SPEI", "lmomco",  "evd", "reliaR", "PearsonDS", "FAdist","trend","Kendall", "mgcv", "lmtest","lfstat", "modifiedmk", "climtrends", "boot", "parallel","modiscloud", "Hmisc","car", "scales", "sn", "gridExtra",  "foreach", "doSNOW", "snow","beeswarm"), require, character.only = T)

# User defined constants --------------------------------------------------
#date sequence of the time series on monthly basis
date_seq <- seq.Date(from= ymd("1970-01-15"), to = ymd("2009-12-15"), by="month")  
#date sequence of the time series on daily basis
date_seq_long =seq.Date(from= ymd("1970-01-01"), to = ymd("2009-12-31"), by= "day")
catch_n <- 337 # number of catchments; originally 338 but last catchment has been altered, the water gauge has not been mantained which caused it to be filled up over the time
no_cores = detectCores() #for parallel computing
agg_month =c(1, 2, 3, 6, 12, 24)



# Running the scripts -----------------------------------------------------
source("./code/functions.R") #reading all user defined functions


source("./code/data_import_formating.R",echo=T) #importing the data set and formating it  

source("./code/sci_calculation.R",echo=T) # SCI + SSI calculation

source("./code/drought_characteristics.R",echo=T) # drought characteristics

source("./code/clustering.R",echo=T) # clustering catchments according to catchment characteristics

source("./code/hydroclimatic_characteristics.R",echo=T) # hydroclimate characteristics (i.e. low flow, monthly mean, quantile flow)

source("./code/sci_analysis.R",echo=T) # SCI analsis

source("./code/trend_calculation.R",echo=T) # trend calculation

source("./code/statistical.R",echo=T) #statistics calculated (example)

source("./code/plot_example.R",echo=T) # plot example (needs all codes to be run before except sci_calculation, sci_analysis and drought characteristics)
