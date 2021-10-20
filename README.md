# stepanuketal_ecological_forecasting
Code repo for Stepanuk et al (in review) 

Workflow for Stepanuk et al. (in review) - Subseasonal forecasts provide a powerful tool for dynamic marine mammal management - Frontiers in Ecology and the Environment

Constant values:
	- ArcMap version (10.5.1)
	- ArcMap Spatial Analyst Extension 
	- Marine Geospatial Ecology Toolbox (https://mgel.env.duke.edu/mget/) 
	- Custom Projection: "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
	- Standard Grid: A standard 10km grid was created in the custom projection using the fishnet tool in the Data Management toolbox in ArcMap. This is the grid that all environmental covariates were resampled to match.

First, run 01_stepanuketal_functions.R to load all libraries and functions.

———— RASTER DATA ————
L0 Data Download:

- Sea Surface Temperature (SST) rasters were downloaded as .tif files in ArcMap using the Marine Geospaital Ecology Toolbox. The dataset was the CMC 0.2 decimal degree daily SST (https://podaac.jpl.nasa.gov/dataset/CMC0.2deg-CMC-L4-GLOB-v2.0 ). Spatial extent was restricted from 20N to 50N and 90W to 50W. All daily SST rasters were projected to the custom projection and resampled to the standard grid.

- Ocean floor depth was downloaded as a .tif file from www.gebco.net , the GEBCO Compilation Group (2020) GEBCO 2020 Grid (doi:10.5285/a29c5465-b138-234d-e053-6c86abc040b9. The spatial extent matched the SST rasters described above. Bathymetry was projected to the standard projection and resampled to the standard grid.

——
L1 Data Processing (ArcMap):

- Distance from shore and Distance from 200m isobath: These layers were calculated by first projecting the bathymetry file into the custom projection (See above). Next, isobaths were extracted using the Contour List tool in the ArcMap Spatial Analyst extension. Distance to isobaths was calculated using the Euclidean distance tool.

- Bottom Slope: calculated from the ocean floor depth raster file using the slope tool in ArcMap.

- Slope of the Bottom Slope: Calculated from the bottom slope layer using the slope tool in ArcMap.

- Aspect: Calculated from the ocean floor depth layer using the aspect tool in ArcMap.

- Rugosity: Calculated from the ocean floor depth layer NOAA’s bathymetric terrain modeler (V3.0) in ArcMap. Walbridge S, Slocum N, Pobuda M, and Wright DJ. 2018. Unified Geomorphological Analysis Workflows with Benthic Terrain Modeler. Geosciences 8: 94.

- Curvature: Calculated from the ocean floor depth layer using the curvature tool in ArcMap.

ALL rasters described above were projected into the custom projection and resampled to the standard grid.

L1 Data Processing (R)

- SST spring transition calculation: see “02_sptr_calc.R” for processing code.

———— FORECAST DATA ————
L0 - Download SubX Climatology for your given model of interest. For Stepanuk et al. (xxxx), we used the GEFS SubX model (http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.dc9916/)

L0 and L1 - Forecast data are downloaded and downscaled in “03_SubX_download_downscale.R”

L2 - Calculation of SST spring transition for forecasts/hindcasts in “02_sptr_calc.R”

———— HUMPBACK WHALE DATA ————
L0 Processed Data Download: Processed data were provided with permission from J. Roberts. For access please contact jason.roberts@duke.edu

L1 Data Processing and modeling: Code provided in Environmental covariates were extracted to the centroid of the 10km line transect segments that contain estimated abundances. This is detailed in “04_mn_GAM_model.R”

———— HUMPBACK MODEL AND FORECAST INTEGRATION ————
- Model and forecast validation - See Table 1 code “Table1.R”

- Predict GAM on historical and SubX rasters - “05_mn_raster_predict.R”

- Seasonal Management Areas shape file - Downloaded from https://www.fisheries.noaa.gov/inport/item/39986.

———— FIGURE AND TABLE CREATION ————
Figure 1 - “Fig1.R” for spatial predictions in 1a, globe creation in 1b, and map in 1c. Figure was created and adjusted in Adobe Illustrator (vxxx).
Figure 2 - “Fig2a2b.R” for the calculation of mean humpback density in each SMA region. Fig2c.R for the demonstration forecast output.
Table 1 - “Table1.R” for the calculation of all columns of Table 1.





