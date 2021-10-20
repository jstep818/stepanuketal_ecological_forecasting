# [[[[[This plot reads in line transect 10km segment centroids and extracts static and dynamic variables at the centroids. The segments are then merged with the humpback whale estimated abundance (see Roberts et al., 2016 for process of fitting detection functions and estimating abundance). Environmental covariates are assessed for collinearity using a pairs plot and the variance inflation factor (VIF; Zuur et al., 2009). The final model determined by backwards selection and AIC is demonstrated at the end of the code.]]]]]

# To run this code, users should have downloaded relevant environmental variables as raster files (see README for details on this process in ArcMap). In addition, both spring transition and change in SST should be calculated with the 02_sptr_calc.R file.

# After this code is run, the GAM at the end of this code can be applied to either satellite derived SST or SubX SST.

library(rgdal)
library(lubridate)
library(raster)
library(sp)

# Define custom projection (from Roberts et al., 2016) and general WGS projection

prj_cust <-"+proj=aea +lat_0=34 +lon_0=-78 +lat_1=27.3333333333333 +lat_2=40.6666666666667 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

prj_wgs <- "+proj=longlat +datum=WGS84 +no_defs"

##### Read in segments, Read in static variables -----
setwd("~/Documents/PhD/roberts_data/v2")
segments_shp <- readOGR(".", "segments_pts")
segments_shp <- spTransform(segments_shp, CRSobj = crs(prj_cust))

setwd("/Volumes/JES TOSHIBA/ENVR_VARS/bathym/")
bathym <- raster("GEBCO2014_-82.1359_22.5243_-58.7379_49.1262_30Sec_Geotiff.tif")
bathym_prj <- projectRaster(bathym, crs = prj_cust)

# All static variables below were processed in ArcMap based on analyses outlined in Web Table 1.
aspect <- raster("aspect.tif")
curvature <- raster("curvature_prj.tif")
rugosity <- raster("rug_SA_PA.tif")
slope <- raster("slope.tif")
slopeslope <- raster("doubleslope.tif")
dshore <- raster("distfromshore.tif")
iso_200 <- raster("iso200_dist.tif")

segments_shp$bathym <- extract(bathym_prj, segments_shp)
segments_shp$aspect <- extract(aspect, segments_shp)
segments_shp$curvature <- extract(curvature, segments_shp)
segments_shp$rugosity <- extract(rugosity, segments_shp)
segments_shp$slope <- extract(slope, segments_shp)
segments_shp$dblslope <- extract(slopeslope, segments_shp)
segments_shp$dshore <- extract(dshore, segments_shp)
segments_shp$distfrom200 <- extract(iso_200, segments_shp)

segments_shp$dshore <- segments_shp$dshore / 1000.0
segments_shp$distfrom200 <- segments_shp$distfrom200 / 1000.0

# Name new column for dynamic variable extraction
segments_shp$sst <- NULL
segments_shp$date <- ymd(segments_shp$t1)
segments_shp$week <- week(segments_shp$date)
segments_shp$Month_ <- as.numeric(as.character(segments_shp$Month_))
segments_shp$Day_ <- as.numeric(as.character(segments_shp$Day_))


# Dynamic variables -----
# Extract SST to points
setwd("F:/D_drive_backup/PhD/SST_download/SST_CMC02_CMC01/yrs/")
patt <- paste(i, "/", "sst", i, month(dat), day(dat), "_CMC", ".tif", sep="")

#name new column
segments_shp$sst <- NULL
segments_shp$DateTime <- ymd_hms(ext_file_init$DateTime)
segments_shp$week <- week(ext_file_init$DateTime)

amempty <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]
for(i in c(1995,1998:2016)){ #for all the years
  l <- segments_shp[segments_shp$Year_ == i,] #subset shp by yrs
  for(j in unique(l$DayOfYear)){ #for all the unique weeks in that year (this way you avoid the try_catch issue)
    o <- l[l$DayOfYear == j,] #subset previous subset by wk
    dat <- ymd(20190101) + j - 1
    r <- raster(patt) #select raster based on name
    o$sst <- extract(r, o) #extract to column
    amempty <- bind(amempty, o) #bind to empty shp
  }
}


# Do same for spring transition (calculated from 02_sptr_code.R)
amempty2 <- SpatialPoints(data.frame(x = 0, y = 0))[-1,]

for(i in c(1995, 1998:2016)){ #for all the years
  l <- amempty[amempty$Year_ == i,] #subset shp by yrs
  for(j in unique(l$week)){ #for all the unique weeks in that year (this way you avoid the try_catch issue)
    o <- l[l$week == j,] #subset previous subset by wk
    
    setwd(paste("D:/PhD/envr/springtrans_ndays/springtrans_ndays_", "10", "/", sep=""))
    r2 <- raster(paste("st_ndays_", "10", "_", i, "_", sprintf("%02d", j), ".tif", sep="")) #select raster based on name
    o$str_no_10 <- extract(r2, o) #extract to column
    
    setwd(paste("D:/PhD/envr/springtrans_ndays/springtrans_ndays_", "8", "/", sep=""))
    r2 <- raster(paste("st_ndays_", "8", "_", i, "_", sprintf("%02d", j), ".tif", sep="")) #select raster based on name
    o$str_no_8 <- extract(r2, o) #extract to column
    
    setwd(paste("D:/PhD/envr/springtrans_ndays/springtrans_ndays_", "6", "/", sep=""))
    r2 <- raster(paste("st_ndays_", "6", "_", i, "_", sprintf("%02d", j), ".tif", sep="")) #select raster based on name
    o$str_no_6 <- extract(r2, o) #extract to column
    
    setwd(paste("D:/PhD/envr/springtrans_ndays/springtrans_ndays_", "4", "/", sep=""))
    r2 <- raster(paste("st_ndays_", "4", "_", i, "_", sprintf("%02d", j), ".tif", sep="")) #select raster based on name
    o$str_no_4 <- extract(r2, o) #extract to column
    
    setwd(paste("D:/PhD/envr/springtrans_ndays/springtrans_ndays_", "2", "/", sep=""))
    r2 <- raster(paste("st_ndays_", "2", "_", i, "_", sprintf("%02d", j), ".tif", sep="")) #select raster based on name
    o$str_no_2 <- extract(r2, o) #extract to column
    
    amempty2 <- bind(amempty2, o) #bind to empty shp
  }
}


### Now read in segment abundances for humpback whales, as well as time information on the segments.
setwd("~/Documents/PhD/roberts_data/v2")
whales_shp <- readOGR(".", "segments_pts11_prj")
whales <- data.frame(whales_shp@data, whales_shp@coords)
seg_abund <- read.csv("~/Documents/PhD/roberts_data/v2/SegmentAbundances.csv")

timee <- read.csv("~/Documents/PhD/roberts_data/v2/segments_time.csv")

#segment abundances read in w/ read_access.R
mn_shp <- amempty3
mn_shp@data <- merge(whales_shp@data, seg_abund[seg_abund$ModelName == "Humpback_whale",], by="SegmentID")

#make dataframes
mn_data <- data.frame(cbind(mn_shp@data, mn_shp@coords))

#abundances to binary
mn_data$presabs <- as.factor(ifelse(mn_data$Abundance == 0, "Absent", "Present"))
mn_data$logbath <- log(abs(mn_data$bathym))


mn_data$Distfrom200 <- abs(mn_data$inoff)
mn_data$Distfromcoast <- abs(mn_data$distfromsh)

mn_data <- merge(mn_data, timee, "SegmentID")
mn_data$DateTime <- mdy_hms(mn_data$DateTime)



# Correlation tests for collinearity - Select relevant variables
mn_data_corr <- mn_data[,c(17,20,22:24,30,31,33,66)]

# Custom scatter panel - this takes a long time to run
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}
# Create the plots
pairs(mn_data_corr, 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# CORVIF function. From Zuur et al (2009)
corvif <- function(dataz) {
  dataz <- as.data.frame(dataz)
  #correlation part
  cat("Correlations of the variables\n\n")
  tmp_cor <- cor(dataz,use="complete.obs")
  print(tmp_cor)
  
  #vif part
  form    <- formula(paste("fooy ~ ",paste(strsplit(names(dataz)," "),collapse=" + ")))
  dataz   <- data.frame(fooy=1,dataz)
  lm_mod  <- lm(form,dataz)
  
  cat("\n\nVariance inflation factors\n\n")
  print(myvif(lm_mod))
}

corvif(mn_data_corr)

# Model building
# Select weeks 10 to 34 for all years to model the spring migratory period exclusively. Convert number of days for each spring transition to a boolean: cells are assigned "1" when the SST in the cell exceeds the threshold for all seven days of the week. Cells are assigned "0" otherwise.
mn_data_season <- mn_data[mn_data$week >= 10 & mn_data$week <= 34,]
mn_data_season$str_yn_2f <- as.factor(ifelse(mn_data_season$str_no_2 >=7, 1, 0))
mn_data_season$str_yn_4f <- as.factor(ifelse(mn_data_season$str_no_4 >=7, 1, 0))
mn_data_season$str_yn_6f <- as.factor(ifelse(mn_data_season$str_no_6 >=7, 1, 0))
mn_data_season$str_yn_8f <- as.factor(ifelse(mn_data_season$str_no_8 >=7, 1, 0))
mn_data_season$str_yn_10f <- as.factor(ifelse(mn_data_season$str_no_10 >=7, 1, 0))

mn_data_season$str_no_2f <- as.factor(mn_data_season$str_no_2)
mn_data_season$str_no_4f <- as.factor(mn_data_season$str_no_4)
mn_data_season$str_no_6f <- as.factor(mn_data_season$str_no_6)
mn_data_season$str_no_8f <- as.factor(mn_data_season$str_no_8)
mn_data_season$str_no_10f <- as.factor(mn_data_season$str_no_10)

mn_data_season$str_yn_2 <- ifelse(mn_data_season$str_no_2 >=7, 1, 0)
mn_data_season$str_yn_4 <- ifelse(mn_data_season$str_no_4 >=7, 1, 0)
mn_data_season$str_yn_6 <- ifelse(mn_data_season$str_no_6 >=7, 1, 0)
mn_data_season$str_yn_8 <- ifelse(mn_data_season$str_no_8 >=7, 1, 0)
mn_data_season$str_yn_10 <- ifelse(mn_data_season$str_no_10 >=7, 1, 0)

# SPUE is the abundance per area. This will be used to create the ratios of observed to predicted values in Table 1.
mn_data_season$SPUE <- mn_data_season$Abundance / mn_data_season$Area

# This is the gam that was used in text as a result of backwards selection
agammn_reg_sptr <- gam(Abundance ~ offset(log(Area)) + s(sst, bs="ts", k=5)  + s(logbath, bs="ts", k=5) +  s(distfromsh, bs="ts", k=5) + str_yn_6, data=mn_data_season, family=tw())

