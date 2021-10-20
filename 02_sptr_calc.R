# [[[[[This code will pull either rasters (.tif) or raster stacks (saved as .rds files from the SubX downloading and downscaling code) to calculate spring transition]]]]]
# Spring transition has been defined as whether a region exceeds a temperature threshold for a given number of days. In Henderson et al. (2017), this was defined as whether a region exceeds a threshold for an 8-day period. To temporally match the weekly SST outputs from the SubX forecast, we calculated spring transition for 2, 4, 6, 8, and 10 degrees C for a one week period.

library(raster)

#--------------------------------------------------------------------------------------
##### First, calculation for folders of daily .tif SST files, where each folder represents one year -----
# Stepanuk et al. (XXXX) used this code to calculate spring transition for the CMC 0.2 decimal degree daily SST files that were downloaded in ArcMap and the Marine Geospatial Ecology Toolbox. For more information, see README.
# Note: the fun_sptr_tifs calculates the number of days that a cell exceeded the threshold value. We will later calculate the boolean rasters (whether the cell exceeded the threshold value for all seven days or not)
#--------------------------------------------------------------------------------------
sptr_ndays <- NULL
for(i in 1999:2016){
  #directory w/ rasters you want to average
  setwd(paste("F:/D_drive_backup/PhD/SST_download/SST_CMC02_CMC01/yrs/", i, "/", sep="")) 
  
  #the first item in the function is the threshold in degrees C for spring transition.
  #The second item is the pattern of YMD that is read from the .tif filename to correctly parse dates
  sptr_calced <- fun_sptr_tifs(6, "sst%Y%m%d_CMC") 
  
  #Store the rasterstack output in a list where the elements start at 1 (i.e. 1999 - 1998 = 1)
  sptr_ndays[[i - 1998]] <- sptr_calced
  
  #Option to save each day as separate .tif files
  setwd("D:/PhD/envr/springtrans_ndays/springtrans_ndays_6")
  writeRaster(sptr_ndays, paste("st_ndays_", thresval, "_", substr(names(sptr_ndays), 2, 9), ".tif", sep=""), bylayer=TRUE)
}

#--------------------------------------------------------------------------------------
##### Another option is to calculate spring transition for the outputs of the demonstration downscaling code (03_SubX_download_downscale.R). These files are saved as .rds file where each file represents a model initialization. Within each stack, there are 35 days of rasters that have been downscaled to match the resolution of your original satellite SST data. We will read those .rds files in from their directory and will process for spring transition at a 6 degree C threshold.
#--------------------------------------------------------------------------------------
sptr_ndays2 <- NULL
for(i in 1999:2016){
  #directory w/ rasters you want to average
  setwd(paste("F:/D_drive_backup/PhD/SST_download/SST_CMC02_CMC01/yrs/", i, "/", sep="")) 
  
  #The function call is the temperature in degrees C to calculate spring transition
  #Note: unlike the raster code above, there is no need to define the pattern. This is because the rasters are already read in as .rds files
  sptr_calced <- fun_sptr_rds(6) 
  
  #Store the rasterstack output in a list where the elements start at 1 (i.e. 1999 - 1998 = 1)
  sptr_ndays2[[i - 1998]] <- sptr_calced
  
  #Option to save each day as separate .tif files
  setwd("D:/PhD/envr/SubX/L1_spring_transition/springtrans_ndays_6")
  writeRaster(sptr_ndays2, paste("st_ndays_", thresval, "_", substr(names(sptr_ndays2), 2, 9), ".tif", sep=""), bylayer=TRUE)
}