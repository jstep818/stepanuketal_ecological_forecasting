#### Code for figures 2a and 2b. This code reads in a shapefile containing the Seasonal Management Areas in the Northwest Atlantic as well as all saved predicted surfaces from the GAM prediction process in 05_mn_raster_predict.R. This code points to a folder containing all weekly .tif predictions of humpback whale density, crops the rasters by each SMA polygon, and averages the density value in each SMA for each month of each year. We then have a weekly average density in each SMA region, which can then be averaged over years or plotted annually, as is demonstrated at the bottom of this code. The end result is Figures 2a and 2b.

if (!require(rgdal)) install.packages("rgdal")
library(rgdal) 
if (!require(sf)) install.packages("sf")
library(sf) 
if (!require(sp)) install.packages("sp")
library(sp) 
if (!require(raster)) install.packages("raster")
library(raster) 
if (!require(ggplot2)) install.packages("ggplo2")
library(ggplot2) 
if (!require(dplyr)) install.packages("dplyr")
library(dplyr) 
if (!require(lubridate)) install.packages("lubridate")
library(lubridate) 
if (!require(rnaturalearth)) install.packages("rnaturalearth")
library(rnaturalearth)
if (!require(rnaturalearthdata)) install.packages("rnaturalearthdata")
library(rnaturalearthdata) 
if (!require(devtools)) install.packages("devtools")
library(devtools)
if (!require(rnaturalearthhires)) devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)
if (!require(cowplot)) install.packages("cowplot")
library(cowplot)
if (!require(reshape2)) install.packages("reshape2")
library(reshape2)
if (!require(ggpubr)) install.packages("ggpubr")
library(ggpubr)

# Download country polygon, coast line for plotting later
usa_coast <- ne_countries(scale = 10, returnclass = "sf", country=c("united states of america", "canada"))
neus_coast <- st_crop(usa_coast, c(xmin =-78, xmax =-65,ymin= 35, ymax=48))

# Project to custom projection
neus_coast_prj <- neus_coast %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
usa_coast_prj <- usa_coast %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

# Shipping lanes: downloaded from https://www.fisheries.noaa.gov/inport/item/39986
setwd("~/Dropbox/shippinglanes/")
shipln <- readOGR(".", "shippinglanes")
shipln <- crop(shipln, extent(-79, -59, 35, 45))
shipln <- spTransform(shipln, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
shipln <- st_as_sf(shipln)

# Select objects labeled as right whale management zones (OBJL = 112), number the zones, and isolate the seasonal management areas. This takes some trial and error to figure out which is which.
narwzone <- shipln[shipln$OBJL == 112,]
narwzone$number <- 1:15
narwzone_SMAs <- narwzone[11:15,]
narwzone_SMAs$name <- c("Chesapeake", "Massachusetts", "Block Island Sound", "New York Harbor", "Delaware Bay")
narwzone_sf <- st_as_sf(narwzone)

narwzone_SMAs <- spTransform(as(narwzone_SMAs, "Spatial"), "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")

# Now read in the raster layers that were saved out of the 04_mn_raster_predict.R script. These layers represent weekly humpback whale density across the study area
setwd("~/Documents/PhD/envr/Hindcasts/L1_GEFS_weekly_agammnsptr/")
raster_data <- list.files(".", "*.tif$") #list the raster files

s_rast <- lapply(raster_data, raster) #convert to raster
s_rast <- stack(s_rast) #stack the humpback density rasters

fileorder <- data.frame("yr" = as.numeric(substr(names(s),32,35)), "wk" = as.numeric(substr(names(s), 37,38))) #ensure the files are in correct year-date order by creating a data frame where one column is year and one is week, from the file name
s_rast <- subset(s_rast, order(fileorder$yr, fileorder$wk)) #order rasters by year, by week

# Create vector of dates based on file names
tempnms <- substr(names(s_rast), 22,29)
tempnms <- ymd(tempnms)
tempnms <- data.frame(tempnms)
tempnms$newdate <- tempnms$tempnms + weeks(1)
tempnms$name <- paste("WkOf", month(tempnms$newdate, label=T), day(tempnms$newdate), year(tempnms$newdate), sep="_")
names(s_rast) <- tempnms$name

#For each SMA, loop through the rasters and mask the raster stack. Calculate the mean value in each SMA and write out to the avg_SMA matrix. transpose and convert to a data frame. avg_sma_t is now a data from where rows represent each week-year and columns represent each SMA. Melt this to prepare for plotting.
avg_SMA <- NULL
for(i in seq(1,length(narwzone_SMAs), 1)){
  blorp <- mask(s_rast, narwzone_SMAs[i,])
  temp22 <- cellStats(blorp, function(x,...){mean(x, na.rm=T)})
  outt <-  c(temp22)
  avg_SMA <- rbind(avg_SMA, outt)
  print(i)
  beep(5)
}
beep()

avg_SMA_t <- data.frame(t(avg_SMA)) 
colnames(avg_SMA_t) <- narwzone_SMAs$name
avg_SMA_t$xid <- rownames(avg_SMA_t)

avg_SMA_melt <- melt(avg_SMA_t)

# Collect date information for plotting and axes
nameinfo <- data.frame(matrix(unlist(strsplit(avg_SMA_t$xid, "_", fixed=TRUE)), nrow=nrow(avg_SMA_t), byrow=T))
nameinfo <- nameinfo[,-1]
colnames(nameinfo) <- c("Month", "Day", "Year")
nameinfo$merger <- rownames(avg_SMA_t)
nameinfo$date <- ymd(paste(nameinfo$Year, nameinfo$Month, nameinfo$Day))
nameinfo$ydayy <- yday(nameinfo$date)
nameinfo$mon_day <- paste(nameinfo$Month, nameinfo$Day, sep="_")
nameinfo$week <- week(nameinfo$date)

avg_SMA_merge <- merge(avg_SMA_melt, nameinfo, by.x="xid", by.y="merger")

avg_SMA_melt2 <- merge(avg_SMA_melt, nameinfo, by.x="xid", by.y="merger")

SMA_avg <- aggregate(value ~ variable + week, data=avg_SMA_merge, mean) 
SMA_avg$fakedate <- ymd(20200101) + weeks(SMA_avg$week - 1)
SMA_avg$ydayy <- yday(SMA_avg$fakedate)

# Calculate the week of maximum humpback density per year in the Massachusetts SMA 
# First, for each year, calculate a smooth spline. Predict on the values of the smooth spline weekly for 365 days. Bind to a data frame.
# Next, identify the column names (i.e. week number) with the maximum value for each year during weeks 10 to 34, representing the spring migratory period.
# Lastly create data frame and add 9 to match the output of max_vec (which starts on week 10) with the overall dataset for plotting (which starts on week 1). This essentially lines up the maximum weeks, which are represented by a vertical line in the ggplot 2a, with the x-axis in the ggplot.
years_preds <- NULL
for (i in 1999:2016){
  ttt <- df_2A[df_2A$Year == i,]
  ss <- smooth.spline(ttt$ydayy, ttt$value, spar=0.2)
  ppp <- predict(ss, seq(1,365,7))
  years_preds <- rbind(years_preds, ppp[[2]])
  
}
years_preds <- data.frame(years_preds)

max_vec <- colnames(years_preds)[apply(years_preds[,10:34], 1, which.max)]
max_wks <- data.frame(Year = 1:18,
                         MaxWk = rev(max_vec))
max_wks$MaxWk <- max_wks$MaxWk + 9 #add 9 for plotting to match up week of year

#### Plotting -----
# A - Plot all years of the Massachusetts SMA
df_2A <- avg_SMA_melt2[avg_SMA_melt2$variable == "Massachusetts",]
df_2A$yrfac <- factor(df_2A$Year, levels = 2016:1999, ordered=T)
df_2A$Year <- as.integer(as.character(df_2A$Year))
df_2A$Month <- as.integer(as.character(df_2A$Month))
df_2A$week <- as.integer(as.character(week(df_2A$date)))


ridge_2A <- ggplot(df_2A[df_2A$week >= 10 & df_2A$week <=34,]) + 
  geom_density_ridges(aes(x=week, y=yrfac, fill=yrfac, height=value), stat="identity", scale=1.2, alpha=0.65, size=0.8, fill="gray") +
  geom_segment(data = max_wks, aes(x = MaxWk, xend = MaxWk, y = as.numeric(Year) ,
                                      yend = as.numeric(Year) + 1.2), size=0.8) +
  theme_minimal() + 
  theme(
    text=(element_text(size=10)),
    axis.text=element_text(size=10),
    axis.text.x = element_text(angle=34, vjust=1, hjust=1),
    panel.grid.minor.x = element_blank(),
    legend.position = "", 
    legend.key.width= unit(2, "cm"),
    axis.text.y = element_text(vjust=0.5)) +
  scale_x_continuous(breaks=seq(10,34,1),labels=paste(month(ymd(20190101) + weeks(seq(10,34,1) -1), label=TRUE), day(ymd(20190101) + weeks(seq(10,34,1) -1)), sep=" ")) +
  labs(x="", y="Mean Humpback Density")
ridge_2A


### B - Aggregate by week and SMA over all years to plot one smooth per SMA
df_2B <- avg_SMA_melt2
df_2B <- aggregate(value ~ week + variable, data=df_2B, mean)
df_2B$SMA <- factor(df_2B$variable, levels= c( "Block Island Sound","Massachusetts", "New York Harbor", "Delaware Bay", "Chesapeake"))
df_2B$yrfac <- as.factor(df_2B$Year)
df_2B$Year <- as.integer(as.character(df_2B$Year))
df_2B$Month <- as.integer(as.character(df_2B$Month))
df_2B$week <- as.integer(as.character(week(df_2B$date)))


ridge_2b <- ggplot(df_2B[df_2B$week >= 10 & df_2B$week <= 34,]) + 
  geom_density_ridges(aes(x=week, y=1, height=value, fill=SMA), stat="identity", scale=1.5, size=0.8, alpha = 0.85) +
  theme_minimal() + 
  coord_cartesian(xlim=c(10,34)) +
  scale_fill_manual(name = "", values=rev(c("#CC0000", "#fed049", "#0e49b5", "#382933", "blueviolet"))) +
  guides(color=guide_legend(nrow=3,byrow=TRUE)) +
  theme(
    text=(element_text(size=10)),
    axis.text=element_text(size=10),
    axis.text.x = element_text(angle=34, vjust=1, hjust=1),
    panel.grid.minor.x = element_blank(),
    legend.position = "")  +
  scale_x_continuous(breaks=seq(10,34,1),labels=paste(month(ymd(20190101) + weeks(seq(10,34,1) -1), label=TRUE), day(ymd(20190101) + weeks(seq(10,34,1) -1)), sep=" ")) +
  labs(x="", y="Mean Humpback Density")
ridge_2b

#Pull out the legend code by adding "(legend.position= "right")" into the theme() call in the ggplot above and save it using get_legend() below. Then run the code above with no legend to obtain the final plot. This allows you to organize plot 2a, 2b, and the legend as separate grobs in cowplot when the file is saved.
#ridge_2b_leg <- ggpubr::get_legend(ridge_2b)

setwd("~/Documents/PhD/Figures/20210916/")
pdf("fig2b2c_20210916.pdf", width=12,height=6)
cowplot::plot_grid(ridge_2A,ridge_2b,  ridge_2b_leg, labels = c( "(b)",  "(c)","",""), rel_heights = c(3,1), ncol = 2)
dev.off()

