# [[[[[This code reads in downsampled forecast values and predicts the humpback density GAM onto them using raster::predict()]]]]]

if (!require(lattice)) install.packages("lattice")
library(lattice) 
if (!require(latticeExtra)) install.packages("latticeExtra")
library(latticeExtra) 
if (!require(rasterVis)) install.packages("rasterVis")
library(rasterVis) 
if (!require(raster)) install.packages("raster")
library(raster) 
if (!require(rgdal)) install.packages("rgdal")
library(rgdal) 
if (!require(sf)) install.packages("sf")
library(sf) 
if (!require(rnaturalearth)) install.packages("rnaturalearth")
library(rnaturalearth)
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
library(rnaturalearthdata) 
if (!require(devtools)) install.packages("devtools")
library(devtools)
if (!require(rnaturalearthhires)) devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)

#Read in .tif files from ArcMap processing (see README for more information)
setwd("~/Documents/PhD/envr/aug2017/maskclip/") #location of .tif files

slope <- raster("slope_10km.tif")
logbath <- raster("logbath2_10km.tif")
bathym <- raster("bathym_10km.tif")
dist_shore <- raster("distfromshore_10kmkm.tif")
rugosity <- raster("rugosity_prj_10km_clip.tif")
dblslope <- raster("dblslop_10km_prj_clip.tif")

##bring in rasters for spring transition calculation (from 02_sptr_calc.R)
setwd("~/Documents/PhD/envr/Forecasts/L1_GEFS_rds_sptr/") #Location of sptr rds file
sptr_nday <- readRDS("GEFS_weekly_sptr_6_ndays.rds") # List of 5-week raster stacks, one per model initialization, where each cell represents the number of days in a week that the cell exceeds the threshold temperature

sptr_nday_2 <- stack(lapply(sptr_nday, function(x) x[[2]])) # Choose the week 2 forecast

testsstsptr <- sptr_nday_2 #rename for integration with predict function

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
testsstsptr <- projectRaster(testsstsptr, slope, res=res(slope), crs=proj4string(slope))
testsstsptr <- mask(crop(testsstsptr, slope), slope)

#To convert the number of days a cell exceeds the threshold temperature to a boolean, we need to reclassify. Any cell value from 0 - 6.5 becomes 0, and any value from 6.5 to infinity becomes 1
mmm <- matrix(c(0,6.5,0,6.5,Inf,1), ncol=3, byrow=TRUE)
testsstsptr_fac <- reclassify(testsstsptr, mmm)
names(testsstsptr_fac) <- names(testsstsptr)


##Next, we need to read the forecasted SST from the downsampled .rds values
setwd("~/Documents/PhD/envr/Forecasts/L1_GEFS_rds_downsamp_weekly/")
fc_files <- list.files(pattern=".rds$", recursive = F)
fc_files <- lapply(fc_files, function(x) readRDS(x))

fc_files_2 <- stack(lapply(fc_files, function(x) x[[2]])) #choose the "week 2" forecast and stack these rasters

#To be sure everything is the same projection and resolution, project and mask based on the standard grid (the slope layer was created on the standard grid in ArcMap so we can use this for reference)
fc_files_2 <- projectRaster(fc_files_2, ssha, res=res(ssha), crs=proj4string(ssha))
fc_files_2 <- mask(crop(fc_files_2, ssha), ssha)


#Now we will use a custom function to predict on rasters. We will name our stack of SST forecast rasters and representative SST spring transition rasters and can predict on those in a loop
sstsource <- fc_files_2 #name the SST stack
sstsptr <- testsstsptr_fac #name the spring transition stack

sstsptrnm <- "str_yn_6" #name the spring transition column you used to build the original GAM

whalepred_stack_ts_diff <- list() #create an empty list

#For each sst forecast layer, apply the function written above, crop the output by the standard grid (represented by slope), and write out into the empty list
for(i in 1:nlayers(sstsource)){
  temp <- fun_stack_gam_predict(sstsource[[i]], sstsptr[[i]], sstsptrnm, agammn_reg_sptr)
  temp <- crop(temp, slope)
  whalepred_stack_ts_diff[[i]] <- temp
}
whalepred_stack_ts_diff <- stack(whalepred_stack_ts_diff) #stack the output

#set names for the output based on the SST date in the names in the sstsource list
names(whalepred_stack_ts_diff) <- paste("gammn_sptr6_", substr(names(sstsource), 6, 13), sep="")


###### PLOTTING #####

#download world polygon, coast line for clipping later
usa_coast <- ne_countries(scale = 10, returnclass = "sf", country=c("united states of america", "canada"))
neus_coast <- st_crop(usa_coast, c(xmin =-78, xmax =-65,ymin= 35, ymax=48))

cols <- colorRampPalette(c("#59919f", "#92b9d0", "#f3de96", "#fcdf75","#f98f8d", "#e51d50", "#820338"))

my.at <- c(seq(0,0.55,0.05), seq(0.6, 2.0, 0.1), 2.5)
mycolorkey <- list(at=my.at, ## where the colors change
                   labels=list(
                     at=my.at ## where to print labels
                   ))

whalepred_stack_ts_diff_select <- projectRaster(whalepred_stack_ts_diff[[c(20:22,33:35)]], crs= proj4string(as(neus_coast, "Spatial")))
plt_2c <- levelplot(whalepred_stack_ts_diff_select, 
                    at=my.at, 
                    colorkey=list(mycolorkey, labels=list(cex=0.7, col="black"), space="bottom"),
                    col.regions=cols,
                    between=list(x=1.2, y=0),
                    margin=FALSE, 
                    names.attr=c("Apr 17", "Apr 24", "May 1",rep("", 3)),
                    par.strip.text=list(cex=0.7),
                    scales=list(cex=c(0.7,0.7), alternating=1, col="black",
                                x=list(at=c(-71, -70, -69, -68), 
                                       labels= c("-71", "-70", "-69", "-68")),
                                y=list(at = c(40, 41, 42, 43, 44),
                                       labels=c("40", "41", "42", "43", "44"))),
                    xlab= list("", cex=0),
                    ylab = list(c("2020", "2019"), cex=0.7),
                    xlim = c(-71,-68),
                    ylim=c(41,44),
                    layout=c(3,2),
                    main=list(expression(bold("Humpback Whale Density (Animals per 100km"^2*")" )), cex=0.7),
                    par.settings=list(axis.line=list(col="black"),
                                      strip.background = list(col = "transparent"), 
                                      strip.border = list(col = "transparent"),
                                      panel.background=list(col="white")))

plt_2c <- plt_2c + latticeExtra::layer(sp.polygons(as(neus_coast, "Spatial"), fill="gray20"))

setwd("~/Documents/PhD/Figures/20210916/")
pdf("mn_forecast_compareyears_20210916.pdf",width=8,height=4, bg="white")
plt_2c
dev.off()

