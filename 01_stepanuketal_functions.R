# [[[[[FUNCTIONS]]]]]
# Run this code first for functions to predict the GAM onto raster stacks and to calculate spring transition.
# In addition, all packages and versions are listed below

if (!require(RNetCDF)) install.packages("RNetCDF") #v2.4-2
library(RNetCDF)
if (!require(lubridate)) install.packages("lubridate") #V1.7.10
library(lubridate) 
if (!require(mgcv)) install.packages("mgcv") #V1.8-35
library(mgcv) 
if (!require(raster)) install.packages("raster") #V.3.4-10
library(raster) 
if (!require(rgdal)) install.packages("rgdal") #V1.5-23
library(rgdal) 
if (!require(sp)) install.packages("sp") #V1.4-5
library(sp) 
if (!require(ggplot2)) install.packages("ggplot2") #V3.3.5
library(ggplot2) 
if (!require(rnaturalearth)) install.packages("rnaturalearth") #V0.1.0
library(rnaturalearth)
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata") #V0.1.0
library(rnaturalearthdata) 
if (!require(devtools)) install.packages("devtools") #V2.4.2
library(devtools)
if (!require(rnaturalearthhires)) devtools::install_github("ropensci/rnaturalearthhires") #v0.2.0
library(rnaturalearthhires)
if (!require(dplyr)) install.packages("dplyr") #V1.0.6
library(dplyr) 
if (!require(sf)) install.packages("sf") #V1.0-0
library(sf) 
if (!require(lattice)) install.packages("lattice") #V0.20-44
library(lattice) 
if (!require(latticeExtra)) install.packages("latticeExtra") #V0.6-29
library(latticeExtra) 
if (!require(rasterVis)) install.packages("rasterVis") #V0.50.2
library(rasterVis) 
if (!require(kableExtra)) install.packages("kableExtra") #V1.3.4
library(kableExtra)
if (!require(cowplot)) install.packages("cowplot") #V1.1.1
library(cowplot)
if (!require(reshape2)) install.packages("reshape2")
library(reshape2)

##### Predict GAM onto stack of rasters ----
#To predict onto the forecast surface, we need to write a function that reads in an iterative SST layer, spring transition layer, spring transition name, and GAM model name.

fun_stack_gam_predict <- function(sstlayer, sptr, sptrnm, modname){
  s1 <- stack(slope, logbath, sstlayer, dist_shore, sptr) #stack the rasters we want to use
  
  #list the names of the variables in s1 in order. sptrnm is the name of the spring transition column in the humpback whale dataset that was used to build the model.
  #NOTE: these names need to match the column names included in the building of the GAM.
  var_names <- c("slope", "logbath", "sst", "distfromsh", sptrnm) 
  names(s1) <- var_names #set stack names
  
  #Predict the GAM (represented by "modname") onto the raster stack, using a constant area of 100 to represent the 10km by 10km grids (100km squared)
  predmod <- predict(s1, modname, type="response", const=data.frame(Area =100))
}


##### Spring transition for folder of raster .tif files -----
# This was for the satellite SST 
fun_sptr_tifs <- function(thresval, formatt){
  #list rasters in a folder defined by setwd()
  raster_data <- list.files(path = getwd(), pattern = "*(...)tif$", recursive=TRUE)
  
  #apply raster function to list of files
  s <- lapply(raster_data, raster)
  
  #create stack of the rasters in list
  stackedtemps <- stack(s)
  
  #set the "z" for the stack as the week of year (could also do month, etc, depending on what aggregation you're aiming for)
  sdate <- setZ(stackedtemps, week(as.Date(names(stackedtemps), format = formatt)))
  
  #For all rasters, do a monthly average. Remember "sdate" is the original stack of rastes above with the indices set as the "z" value
  overthres <- calc(sdate, function(x) x>thresval)
  overthres <- setZ(overthres, getZ(sdate))
  ndays_weekly <- stackApply(overthres, indices = getZ(overthres), fun = sum)
  
  #re-name rasters
  names(ndays_weekly) <- paste(i, sprintf("%02d",unique(getZ(sdate))), sep="_")
  
  sptr_ndays <- ndays_weekly
}


##### Spring transition for folder of .rds raster stacks -----
# This was for SubX spring transition calculations
fun_sptr_rds <- function(thresval){
  #list .rds files. These are .rds files that contain a stack of 35 rasters in each. Each .rds file represents a different model initialization date and each raster represents a different day.
  raster_data <- list.files(path = getwd(), pattern = "*(...)rds$", recursive=FALSE)
  
  #apply readRDS function to list of files
  s <- lapply(raster_data, readRDS)
  
  # Create a vector that will be used to sum the raster stacks across, condensing 35 days into 5 weeks
  sdate <- rep(1:5, each=7)
  
  #For each raster stack in the list, calculate whether the threshold temperature is exceeded on each day. This results in a list of stacks of the same dimension as the initial inputs, but each raster cell is now a 0 or 1 (depending on whether the cell exceeds the threshold temperature)
  overthres <- lapply(s, function(y){calc(y, function(x) x>thresval)})
  
  #For the above list of raster stacks, set the Z so each of the 35 rasters is assigned to weeks 1 through 5
  overthres1 <- lapply(overthres, function(y) {setZ(y, sdate)})
  
  # For each raster stack, run an apply where the index is the assigned week and the function is sum. This results in a list where each element is a raster stack with 5 weeks. Each raster contains values from 0 to 7, representing the number of days that each cell exceeeded the threshold temperature.
  ndays_weekly <- lapply(overthres1, function(y) {stackApply(y, indices = sdate, fun = sum)})
}

