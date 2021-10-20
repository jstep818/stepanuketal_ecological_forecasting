# This document is a tutorial to walk through the download and downscale process for SubX forecasts from the SubX forecasting project: http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/

# The downscale process is based on Hare et al., 2012: https://academic.oup.com/icesjms/article/69/10/1753/624341?login=true doi:https://doi.org/10.1093/icesjms/fss160 

# The outline is as follows:
# 1) Download forecasts/hindcasts for given time frame and spatial extent
# 2) Average all ensemble members to get one forecast per initialization date per lead time. Create list of model runs where each element is a raster brick comprised of model runs for each lead day
# 3) Download model climatology from SubX site. Select relevant dates and subtract climatology from step 2 to get a forecast anomaly
# 4) Read in climatologies for satellite SST data (see TBD for code on how to calculate this). Downscale forecast anomaly and add to satellite climatology
#--------------------------------------------------------------------------------------

#Start by loading required libraries
library(RNetCDF)
library(lubridate)
library(raster)
library(kableExtra)

#--------------------------------------------------------------------------------------
# Setting up storage, model, and spatiotemporal variables

# Now set directory to save files, variable names of interest, missing value, and forecast or hindcast. Most of this information is on the SubX site and can be tailored to the specific model, environmental variable, and height at which the variable is recorded. In addition, this is where you choose whether you want a forecast or a hindcast. Note, there will be different parameters below (i.e. different dimensions of the array) depending on whether you choose forecast or hindcast.
#--------------------------------------------------------------------------------------
outPath="D:/PhD/forecasts/tests/" #where to save your models
dimname="ts"   # Variable names
plevstrs="sfc"  # Must be same size as varname; for variables with no plevels, use sfc or 10m. This is for whether you're using a surface variable (we are doing this for temperature) or using some atmospheric variable at a height above the surface
groups="EMC"  # Modeling Groups (must be same # elements as models below)
models= "GEFS"     # Model Name (must be same # of elements as groups above)
dfv=-9.99e-8  # Default missing_value or FillValue if not specified in input data file
type="hindcast"  # hindcast or forecast


#-------------------------------------------------------------------------------------
# Next, we choose limits in space to constrain the global models to our area of study. The spatial covariates are a typical xmin, xmax, ymin, and ymax formatted as XXN. Direction based on meridians is denoted with "N" "S" "E" or "W", not negative or positive numbers.
#--------------------------------------------------------------------------------------
xxmin <- "90W" #min longitude - formatted like "90W"
xxmax <- "50W" #max longitude - formatted like "50W"
yymin <- "20N" #min latitude - formatted like "20N"
yymax <- "50N" #max latitude - formatted like "50N"

#--------------------------------------------------------------------------------------
#Next, we choose limits in time to constrain the global models to the time frame of interest. This is inclusive, so it will include an model runs where the initial date occurs between the start date and end date denoted below. This usually takes some guess work to figure out model start dates. I try to download one year at a time (i.e. N=53 runs of 35 day forecasts). 
#Variable explanations are as follows:

text_tbl <- data.frame(
  Items = c("yr1", "dd1", "mon1", "date1", "date_elapsed","date2", "yr2", "dd2", "mon2"),
  Features = c(
    "Year of date 1 - earliest date to begin searching for forecasts/hindcasts (numeric, year)",
    "Day of date 1 - earliest day of the calendar month to begin searching for forecasts/hindcasts (numeric, 1-31)", 
    "Month of date 1 - earliest month of the calendar year to start searchign for forecasts (text of length 3, capital letter first. example: Dec",
    "Calculated: Parsed start date. Using package lubridate, parse a date string indicating the earliest date to start searching for available forecasts/hindcasts",
    "Calculated: Number of days from start date to desired end date. This will be added to the start date (date1) to obtain an end date",
    "Calculated: Parsed end date. Using package lubridate, parse a date string indicating the last date beyond which searches for available forecasts/hindcasts will not be conducted",
    "Calculated: Year of date 2 - last date to look for forecasts/hindcasts (numeric, year)",
    "Calculated: Day of date 2 - last day of the calendar month to search for forecasts/hindcasts (numeric, 1-31)",
    "Calculated: Month of date 2 - last month of the calendar year to search for forecasts/hindcasts (text of length 3, capital letter first. example: May"
  )
)

kbl(text_tbl) %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "50em")
#--------------------------------------------------------------------------------------

dateelapsed <- 30 #How many days elapsed from your start date do you want to download forecasts for?
yr1 <- 2018 #Start date year
dd1 <- 23 #Start date day (needs to be formatted with 2 digits - like 01)
mon1 <- "Dec" #Start date month (needs to be formatted as "Jan" "Feb" Mar" etc)
date1 <- ymd(paste(yr1, mon1, dd1)) #Cobble together dates
yr2 <- year(date1 + dateelapsed) #End date year
dd2 <- day(date1 + dateelapsed) #End date day
mon2 <- month(date1 + dateelapsed, label=TRUE, abbr=TRUE) #End date month formatted correctly
date2 <- date1 + dateelapsed #End date total

#--------------------------------------------------------------------------------------
## Downloading and Opening .nc File

# We will now take all the above variables and paste them into the opendap server URL to access all forecasts/hindcasts within our date range and cropped to the spatial scale of interest
#--------------------------------------------------------------------------------------

URL <- paste("http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.", type, "/.ts/", "Y/%28", yymin, "%29%28", yymax, "%29RANGEEDGES/S/%280000%20", dd1, "%20", mon1, "%20", yr1, "%29%280000%20", dd2, "%20", mon2, "%20", yr2, "%29RANGEEDGES/X/%28", xxmin, "%29%28", xxmax, "%29RANGEEDGES/dods/", sep="") #Everything above put into the URL to get the .nc files available for the model/date range/extent/etc that you called above
nc <- open.nc(URL) #Open the nc files above

dimname <- "ts" #ts is for surface temperature. you can find 

dlname <- att.get.nc(nc, dimname, "long_name")
dunits <- att.get.nc(nc, dimname, "units")
fillvalue <- att.get.nc(nc, dimname, "_FillValue")


#--------------------------------------------------------------------------------------
## Checking the nc file dimensions
#We will now print a summary of the nc file and will check each dimension available in the nc file. Dimensions will give info on the length of each variable in the nc file. Variables include data like the latitude, longitude, number of ensemble members, lead time, and start dates.

#Additional information on the variables is available below the dimensions description. This provides names, units, and lengths of these variables. We first want to convert dates to "real" date strings, longitude to non-negative values, temperature from Kelvin to Celsius, and the lead time to integer values (values were 0.5 to 35.5, for example. We want them to be from 0 to 35).
#--------------------------------------------------------------------------------------
print(nc)

nc.lon <- var.get.nc(nc, "X")
nc.lat <- var.get.nc(nc, "Y")
nc.date <- var.get.nc(nc, "S")
nc.ens <- var.get.nc(nc, "M")
nc.lead <- var.get.nc(nc, "L")
nc.ts <- var.get.nc(nc, "ts")

nc.date <- ymd(19600101) + nc.date
nc.lon <- -1*nc.lon
nc.ts <- nc.ts - 273.15
nc.lead <- nc.lead - 0.5

dim(nc.ts)

tmp.slice <- nc.ts[,,1,1,8]
image(nc.lon, nc.lat, tmp.slice)

#--------------------------------------------------------------------------------------
### Converting to raster bricks
#Our goal now is to convert the nc file to raster bricks. For each initial model run date (i.e. datelen), we loop through each ensemble date (i.e. leadlen), and brick "nc.ts" from the previous chunk of code. We define the projection (WGS84 i.e. EPSG 4326). This results in a brick of rasters where X = longitude, Y = latitude, and the brick is all of the ensemble members. 

#But to obtain an ensemble model, we want to average all ensemble members. So we collapse this ens_1_d1 raster brick by averaging it. We then assign it to a position in the list named "meanbrick". We iterate through this inner for-loop for all lead days in a single model run, and the meanbrick result is a brick of all lead days, where each lead day is the ensemble (i.e. averaged) raster. 

F#or the outer for loop, a different raster brick is created for each model initialization date. These are stored in the datebricklist list. In summary, this chunk of code creates an ensemble model from each member, separates the nc file into each model initialization date, and creates a single raster for each lead day of each model initialization.

#Even though these forecasts are released weekly, for some reason the nc file downloads as a daily product. So there is one rasterbrick followed by 6 blank list elements. the last line of code here filters the list and removes all na elements. The result should be a list with an element representing each initialization date, which is comprised of a rasterbrick where each raster is one day.
#--------------------------------------------------------------------------------------

datelen <- length(nc.date)
leadlen <- length(nc.lead)

meanbrick <- list()
datebricklist <- list()
for(j in seq(1,datelen,1)){
  meanbrick <- list()
  for(i in seq(1,leadlen,1)){
    ens_1_d1 <- t(flip(brick(nc.ts[,,i,,j], xmn=25, xmx=50, ymn=-90, ymx=-50), direction='x'))

    proj4string(ens_1_d1) <- CRS("+init=epsg:4326")
    ens_1_d1_mean <- mean(ens_1_d1)

    meanbrick[[i]] <- ens_1_d1_mean
  }
  meanbrick <- brick(meanbrick)
  names(meanbrick) <- paste(nc.date[j], "+", seq(1, leadlen, 1), sep="")
  datebricklist[[j]] <- meanbrick
  names(datebricklist)[[j]] <- paste("mod", as.character(nc.date[j]))
}

datebricklist <- (Filter(function(e){!all(is.na(values(e)[[1]]))},datebricklist))

#Demonstration plot of the first 8 days of the first model run
levelplot(datebricklist[[1]][[1:8]])


#--------------------------------------------------------------------------------------
## Downscale forecasts to match other SST data

### Download Climatologies
#We have our datebricklist above where each list element is a different model run (denoted by the start date of the run) and within each element we have a rasterbrick of N days of the model run. Next, we need to calculate an anomaly for these rasters. Fortunately, SubX products have many calculated products available on the data download site. Climatologies are the daily average for a given time period (most in the SubX project are the daily averages between 1999-2014). It is important to note that there is model drift in the forecast model which affects the forecast of a specific date at varying lead times. Therefore, the climatology files will have an average temperature for each day of the year, but each day will serve as an initialization date from which the lead days are forecasted. For example, for NCEP-GEFS there is one climatology hindcast created for each day of the year, and each climatology date has a forecast for the next 35 days. Links for surface temperature are available at:

text_tbl2 <- data.frame(
  Items = c("CESM", "ECCC", "EMC", "ESRL", "GMAO", "NCEP", "NRL", "observed", "RSMAS"),
  Features = c(
    "NA",
    "Multiple available. See http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.ECCC/.GEM/.hindcast/.dc9915/ for example",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.dc9915/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.ESRL/.FIMr1p1/.hindcast/.dc9915/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.GMAO/.GEOS_V2p1/.hindcast/.dc9915/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.NCEP/.CFSv2/.hindcast/.dc9915/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.NRL/.NESM/.hindcast/.dc9915/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.observed/.climatologies/.CPC/",
    "http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.RSMAS/.CCSM4/.hindcast/.dc9915/"
    
  )
)

kbl(text_tbl2) %>%
  kable_paper(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "50em")
#*CESM, GMAO, and EMC were used in Stepanuk et al. (YYYY)

#--------------------------------------------------------------------------------------

#Download the relevant nc files and read into R as follows
ncloc1 <- "C:/Users/jstepanuk/Downloads/"
ncfname1 <- paste(ncloc1,"climatology_GEFS.nc", sep="")
#Open NetCDF file
ncin1 <- nc_open(ncfname1)
print(ncin1)

dname <- "ts"
climatol <- ncvar_get(ncin1,dname)

#--------------------------------------------------------------------------------------
# Based on the printout, there is one fewer dimensions than in the forecast download. This is because there are no ensemble members here. There is a lead time, a start date (length = 366 because there is one initialization date for each day of the year), latitude, and longitude. 

### Calculate Forecast Anomaly
# The next step is to isolate the climatologies that match up with the dates of the forecast we downloaded above. We can do this by determining the day of year of the forecast initialization date(s) we downloaded in the first step. From there, like we did with the forecast, we brick the lead days and define a projection. We add another two steps here. First, we resample the climatology to match the forecast (just because there are some inconsistencies in the projection process). Second, for a given forecast initialization date, we subtract the climatology from the respective forecast to obtain a forecast anomaly. This gets saved into "ano_list".
#--------------------------------------------------------------------------------------

#Start day of climatology (Just Jan 1 of any year)
datestart <- yday(ymd(20100101))

climsub <- t(flip(brick(climatol[,,,datestart], xmn=25, xmx=50, ymn=-90, ymx=-50), direction='x'))
climsub <- climsub - 273.15
proj4string(climsub) <- CRS("+init=epsg:4326")

#Subtract acutal daily SST (calculated in step above) from climatology to get anomaly
ano_list <- list()
for(i in seq(1,length(datebricklist),1)){
  mindate <- min(ymd(substr((names(datebricklist[[i]])), 2, 11)))
  #is it a leap year?
  #if(leap_year(mindate) == TRUE){
  #  loop_climatols <- climsub[[-X]]
  #}
  
  ymindate <- yday(mindate)
  
  climbrick <- t(flip(brick(climatol[,,,ymindate], xmn=25, xmx=50, ymn=-90, ymx=-50), direction='x'))
  climbrick <- climbrick - 273.15
  proj4string(climbrick) <- CRS("+init=epsg:4326")
  climbrick <- resample(climbrick, datebricklist[[i]])
  
  ano_list[[i]] <- datebricklist[[i]] - climbrick
  
}

# Demo plot of the forecast anomaly for the first 8 days of the first forecast run
levelplot(ano_list[[1]][[1:8]])

#--------------------------------------------------------------------------------------
### Read in satellite SST climatologies
# For the final step of the downscale process, we add the coarser forecast anomaly that we just calculated in the above code chunk to a satellite-derived SST anomaly that is at a finer spatial resolution. To calculate the SST climatology, please refer to the XXYY. We will read this in from a location on a hard drive, which will be the output location from the satellite climatology script.
#--------------------------------------------------------------------------------------

setwd("D:/PhD/SST_download/SST_cmc0201_climatology/")
raster_data <- list.files(path = getwd(), pattern = "*(...)tif$", recursive=FALSE)
s <- lapply(raster_data, raster)
cmcclimstack <- stack(s)

#--------------------------------------------------------------------------------------
# Next, we need to resample the coarse forecast anomalies to match the fine scale satellite SST climatologies to be able to add them together. This is an artificial downsampling process. It does not actually change the values of the coarser raster, it just appears to match the fine scale SST so the anomaly and satellite raster bricks can be added seamlessly.
#--------------------------------------------------------------------------------------

ds_ano <- lapply(ano_list, function(x) resample(x, cmcclimstack[[1]], method='ngb'))

plot(ano_list[[1]][[1]]) #demo plot of the first element in the first forecast date at the coarser scale
plot(ds_ano[[1]][[1]]) #demo plot of the first element in the first forecast date at the finer scale

#--------------------------------------------------------------------------------------
# Lastly, we need to add the forecast anomalies to the climatologies. This should be a simple process, but it becomes more difficult when forecasts overlap between years (i.e. a forecast encompasses December-January. There is a fair amount of date parsing in the first half of this code chunk: we first calculate the initialization date of our first anomaly brick, and calculate the day of year of that initialization. The next if-else statement states that:
# If initialization day of year + the number of days of the forecast is <= 365, the lead date (i.e. last day of the forecast period) is what we would expect. It's just the initialization day of year + the lead times (minus 1 because the start day counts as day 1).
# If the initialization day of year + the number of days of the forecast is > 365, this means the forecast period crosses into the next year. Therefore, the last day of the forecast period is actually the initialization day of year + the lead times (minus 1 as above) - 365.

# For example: The forecast begins on December 30, 2018. The next 35 days (if we use an NCEP-GEFS forecast), would bring us to Feb 3, 2019. So because we cross the new year we have the day of year of Dec 30 (364) + the 35 day lead time - 1 = 398. We then subtract 365 to get 33, which is the day of year of Feb 3.

# The second if-else statements create stacks of satellite climatology that match the forecast anomaly time frame. if the initial day of year is less than the last day of year (i.e. the forecast period is within the same year), you can just stack the satellite climatologies for those lead days with no additional steps. Else, if the day of year of the model start is greater than the model end (like the example above), we create a stack that goes from the start day to 365, and from 1 to the final date.

# We then add the anomaly to the climatology, and stack each initialization date. This is the final downscaled product.
#--------------------------------------------------------------------------------------

ds_complete <- list()
for (k in 1:length(ds_ano)){
  daate <- min(ymd(substr((names(ds_ano[[k]])), 2, 11)))
  daateday <- yday(daate) 
  if(daateday + length(nc.lead) -1 <=365){
    daatelead <- daateday + length(nc.lead) - 1
  }else{
    daatelead <- daateday + length(nc.lead) - 1 - 365
  }
  
  if(daateday < daatelead){
    cmcanostack <- stack(cmcclimstack[[daateday:daatelead]])
  }else{
    cmcanostack <- stack(cmcclimstack[[daateday:365]], cmcclimstack[[1:daatelead]])
  }
  
  completeadd <- ds_ano[[k]] + cmcanostack
  
  ds_complete[[k]] <- stack(completeadd)
}

levelplot(ds_complete[[1]][[1:8]])
