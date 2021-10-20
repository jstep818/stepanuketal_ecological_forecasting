# [[[[[This code will predict the GAM model at the end of "04_mn_GAM_model.R" onto a raster stack of all relevant environmental variables. The first set of code is for the SubX downloaded forecasts and hindcasts. Because it is reading in .rds files which represent weekly stacks of SST, we need to first read them in and then choose the week of interest for predicting. In this case, we predict on the first hindcasted/forecasted week. The second set of code is very similar, but it reads in satellite SST rasters and spring transition rasters (in .tif format). These rasters are then stacked and predicted on in a loop.]]]]]

library(raster)
library(rgdal)
library(lubridate)


#Read in .tif files from ArcMap processing (see README for more information)
setwd("~/Documents/PhD/envr/aug2017/maskclip/") #location of .tif files

slope <- raster("slope_10km.tif")
logbath <- raster("logbath2_10km.tif")
bathym <- raster("bathym_10km.tif")
dist_shore <- raster("distfromshore_10kmkm.tif")
rugosity <- raster("rugosity_prj_10km_clip.tif")
dblslope <- raster("dblslop_10km_prj_clip.tif")

#----------------
# How to read in and set up forecasts and hindcasts that have been downscaled
#----------------

##bring in rasters for spring transition calculation (from 02_sptr_calc.R)
setwd("~/Documents/PhD/envr/Hindcasts/L1_GEFS_rds_sptr/") #Location of sptr rds file
fc_sptr_nday <- readRDS("GEFS_weekly_sptr_6_ndays.rds") # List of 5-week raster stacks, one per model initialization, where each cell represents the number of days in a week that the cell exceeds the threshold temperature

fc_sptr_nday_2 <- stack(lapply(fc_sptr_nday, function(x) x[[1]])) # Choose the week 1 hindcast

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
fc_sptr_nday_2 <- projectRaster(fc_sptr_nday_2, slope, res=res(slope), crs=proj4string(slope))
fc_sptr_nday_2 <- mask(crop(fc_sptr_nday_2, slope), slope)

#To convert the number of days a cell exceeds the threshold temperature to a boolean, we need to reclassify. Any cell value from 0 - 6.5 becomes 0, and any value from 6.5 to infinity becomes 1
mmm <- matrix(c(0,6.5,0,6.5,Inf,1), ncol=3, byrow=TRUE)
fc_sptr_fac <- reclassify(fc_sptr_nday_2, mmm)
names(fc_sptr_fac) <- names(fc_sptr_nday_2)


##Next, we need to read the forecasted SST from the downsampled .rds values
setwd("~/Documents/PhD/envr/Hindcasts/L1_GEFS_rds_downsamp_weekly/")
fc_sst <- list.files(pattern=".rds$", recursive = F)
fc_sst <- lapply(fc_sst, function(x) readRDS(x))

fc_sst_2 <- stack(lapply(fc_sst, function(x) x[[1]])) #choose the "week 1" forecast and stack these rasters

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
fc_sst_2 <- projectRaster(fc_sst_2, ssha, res=res(ssha), crs=proj4string(ssha))
fc_sst_2 <- mask(crop(fc_sst_2, ssha), ssha)


#-------------
# Other option: How to read in satellite SST and satellite spring transition rasters
#-------------
##bring in rasters for spring transition calculation (from 02_sptr_calc.R)
setwd("~/Documents/PhD/envr/springtrans_ndays/springtrans_ndays_6") #Location of weekly sptr .tif files
sat_sptr_nday <- list.files(pattern=".tif$", recursive = F) # List of files, each file is one week of one year where cells indicate the number of days that a cell exceeded the spring transition threshold temperature
sat_sptr_nday <- stack(lapply(sat_sptr_nday, raster))

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
fc_sptr_nday_2 <- projectRaster(fc_sptr_nday_2, slope, res=res(slope), crs=proj4string(slope))
fc_sptr_nday_2 <- mask(crop(fc_sptr_nday_2, slope), slope)

#As with the hindcasts/forecasts, to convert the number of days a cell exceeds the threshold temperature to a boolean, we need to reclassify. Any cell value from 0 - 6.5 becomes 0, and any value from 6.5 to infinity becomes 1
mmm <- matrix(c(0,6.5,0,6.5,Inf,1), ncol=3, byrow=TRUE)
sat_sptr_fac <- reclassify(sat_sptr_nday, mmm)
names(sat_sptr_fac) <- names(sat_sptr_nday)


##Next, we need to read the satellite daily SST 
setwd("~/Documents/PhD/SST_download/SST_CMC02_CMC01/yrs/2000")
sat_sst <- list.files(pattern=".tif$", recursive = T)
sat_sst <- stack(lapply(sat_sst, function(x) raster(x)))
sat_names <- data.frame(names(sat_sst))
sat_names$date <- ymd(substr(sat_names$names.sat_sst., 4,12))
sat_names$yr_wk <- paste(year(sat_names$date), week(sat_names$date), sep="_")

sat_sst_wk <- stackApply(sat_sst, indices=sat_names$yr_wk, fun=mean)

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
sat_sst_wk <- projectRaster(sat_sst_wk, ssha, res=res(ssha), crs=proj4string(ssha))
sat_sst_wk <- mask(crop(sat_sst_wk, ssha), ssha)






#Now we will use a custom function to predict on rasters. We will name our stack of SST forecast rasters and representative SST spring transition rasters and can predict on those in a loop
sstsource <- fc_sst_2 #name the SST stack (either the SubX data or the satellite data)
sstsptr <- fc_sptr_fac #name the spring transition stack (either the SubX data or the satellite data)

sstsptrnm <- "str_yn_6" #name the spring transition column you used to build the original GAM

whalepred_stack_ts_diff <- list() #create an empty list

#For each SST layer, apply the function written above, crop the output by the standard grid (represented by slope), and write out into the empty list
for(i in 1:nlayers(sstsource)){
  temp <- fun_stack_gam_predict(sstsource[[i]], sstsptr[[i]], sstsptrnm, agammn_reg_sptr)
  temp <- crop(temp, slope)
  whalepred_stack_ts_diff[[i]] <- temp
}
whalepred_stack_ts_diff <- stack(whalepred_stack_ts_diff) #stack the output

#set names for the output based on the SST date in the names in the sstsource list
names(whalepred_stack_ts_diff) <- paste("gammn_sptr6_", substr(names(sstsource), 6, 13), sep="")