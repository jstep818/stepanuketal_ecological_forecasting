### Monthly cross validation of GAM model built for weeks 10 to 34 of each year.
# In the humpback dataset: number of segments, total area observed, number of observations, and aggregated humpback sightings per unit area for each month
# In the predicted outputs for satellite SST, a forecast 1 week in advance, and a forecast 2 weeks in advance: the aggregated humpback density for each month, and the ratio of the SPUA in the dataset to the modeled density using each SST product
# For the model that was built from the training data in each month: deviance explained and R2

if (!require(mgcv)) install.packages("mgcv")
library(mgcv) 

##by month CV
mn_data_tt_mon <- NULL
mndata_gamout_mon <- list()
for (i in 3:8){
  # Divide dataset into test data (one month between March (3) and August (8)), and training data (the other months)
  train <- mn_data_season[mn_data_season$monthnum != i,]
  test <- mn_data_season[mn_data_season$monthnum == i,]
  
  # Run GAM on the training data that has the same criteria as the original full model (environmental covariates, spline function, and number of knots)
  gammn_tt <- gam(Abundance ~  s(sst, bs="ts", k=5) + s(logbath, bs="ts", k=5) +  s(distfromsh, bs="ts", k=5) + str_yn_6f, data=train, offset=log(Area), family=tw())
  pos <- i - 2
  mndata_gamout_mon[[pos]] <- gammn_tt #save GAM to list
  
  # Predict on SST satellite rasters
  # This will predict onto the test dataset, representing one candidate month
  z <- data.frame(predict(gammn_tt, test, type="response", se=TRUE))
  test <- cbind(test,z)
  
  # Now, we need to predict the GAM when the SST is represented by the SubX forecast, week 1.
  # To do this, rename SST to represent the SubX hindcast week 1 and predict
  test2 <- test
  test2$sst <- test2$hindcast
  o <- data.frame(predict(gammn_tt, test2, type="response", se=TRUE))
  test2 <- cbind(test2, o)
  
  # Now, we need to predict the GAM when the SST is represented by the SubX forecast, week 2.
  # To do this, rename SST to represent the SubX hindcast week 2 and predict  
  test3 <- test2
  test3$sst <- test$hindcast2
  w <- data.frame(predict(gammn_tt, test3, type="response", se=TRUE))
  test3 <- cbind(test3, w)
  
  # Now there are added columns onto the test dataset: model prediction and standard error for satellite SST, SubX week 1 SST, and SubX week 2 SST. Name columns accordingly and bind total dataset together.
  colnames(test3)[89:94] <- c("fit_sat", "se_sat", "fit_wk1", "se_wk1", "fit_wk2", "se_wk2")
  mn_data_tt_mon <- rbind(mn_data_tt_mon, test3)
  
  rm(gammn_tt)
  print(i)
}


# [[[[[[This code creates Table 1]]]]]]

mon_dev <- unlist(lapply(mndata_gamout_mon, function(x) summary(x)$dev.expl)) #model dev exp for each month
mon_r2 <- unlist(lapply(mndata_gamout_mon, function(x) summary(x)$r.sq)) #model R2 for each month

dat_segs <- table(mn_data_tt_mon$monthnum) #number of segments per month
dat_area <- aggregate(Area ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #area covered per month
dat_sgts <- aggregate(Observations ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #sightings per month
dat_obsd <- aggregate(SPUE ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #density per month

dat_dsat <- aggregate(fit_sat ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #density predicted from satellite sst per month
dat_fwk1 <- aggregate(fit_wk1 ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #density predicted from a 1 week forecast per month
dat_fwk2 <- aggregate(fit_wk2 ~ monthnum, data=mn_data_tt_mon, FUN = sum)[,2] #density predicted from a 2 week forecast per month
dat_satrat <- dat_obsd / dat_dsat #ratio of observed to predicted for satellite SST
dat_wk1rat <- dat_obsd / dat_fwk1 #ratio of observed to predicted for forecast wk 1
dat_wk2rat <- dat_obsd / dat_fwk2 #ratio of observed to predicted for forecast wk 2

tbl1 <- data.frame(dat_segs, dat_area, dat_sgts, dat_obsd, dat_dsat, dat_satrat, dat_fwk1, dat_wk1rat, dat_fwk2, dat_wk2rat, mon_dev, mon_r2)

