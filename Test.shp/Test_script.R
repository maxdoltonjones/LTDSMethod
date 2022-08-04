#Testing new package

#Load libraries
#library(devtools)
#Install package directly from github, which will also install all needed dependencies
#install_github("maxdoltonjones/LTDSMethod")
#Load new package and dependencies
library(LTDSMethod)
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)

#Set working directory
setwd("~/LTDS/R_packages/LTDS_R_package/LTDSMethod")

#Specify file where shapefiles can be found
wd.shp.r.400 <- paste0("./Test.shp/")
#Read in shapefile
#Below example is the core area of the Sakaerat Biosphere Reserve, Thailand
shp.test <- readOGR(dsn = paste0(wd.shp.r.400,
                                 "SBRcore.shp"))

#Use tran_place function from new package to place transects on site
tran_place(400, 0.6, "horz", shp.test, 47, plot = T)


