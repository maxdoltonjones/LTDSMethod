##R package training day##

##Max D. Jones and Elizabeth A. Hunter##

##Package name: LTDSMethod##

##January 2023##

####Introduction:installing and loading packages----

#The LTDSMethod package is currently open-access on a GitHub repository.
#To download and install the package, we first need the devtools package.

#Install devtools:
install.packages("devtools") #Ignore if previously installed
#Load devtools into the session:
library(devtools)

#We can now use the devtools package to install LTDSMethod from GitHub
install_github("maxdoltonjones/LTDSMethod",  build_vignettes = TRUE, force = TRUE)


#The installation will ask if any package updates are needed, we can skip these
#by running the number three (3):
3

#The installation of the package should also prompt the installation of all
#package dependencies (other packages which are needed for LTDSMethod to work).
#However, if this is not the case, run the following lines (this may take a
#few minutes or so):
#install.packages("raster")
#install.packages("rgeos")
#install("rgdal")
#install("dplyr")
#install.packages("readr")
#install.packages("htmltools")

packageurl <- "https://cran.rstudio.com//src/contrib/Archive/raster/raster_3.5-15.tar.gz"
install.packages(packageurl, repos=NULL, type="source", lib='~/R/R-4.1.2/library')

#Again, if these packages are already installed, the dependencies will be
#loaded at the same time as LTDSMethod is loaded. However, we can also load
#all of the dependencies separately:
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(readr)
library(LTDSMethod)
library(nimble)
library(terra)
library(ggplot2)

####Introduction:working directory and loading files----

#Now we have everything loaded in, we need to set the working directory. Try to
#designate a new folder for this, as several files will need to be loaded from,
#and saved to, this folder. Set the working directory by:
#Set working directory
setwd("~/LTDS/R_packages/LTDS_R_package/LTDSMethod")

#####Before sampling#####

#Let's remind ourselves what is needed to work through the LTDSMethod package.
#This information can be found on page XXX of the SOP:
#1. Shapefile of recipient site (only permitted, habitat area)
#2. Shapefile of additional acreage (only permitted, habitat area that is not
#already included in the original shapefile)
#3. The acreage of the recipient site (only permitted, habitat area which is to
#be surveyed)
#4. UTM zone of recipient site location (either 16 or 17, see Figure 3 of SOP)
#5. Location of soft-release enclosures at recipient site (either embedded
#into shapefiles, or on a separate map)

#Let's start with number 1.
#Read in our shapefile showing the recipient site to be surveyed:
#We can read in a shapefile with:
site.shp <- readOGR(dsn = "./Test.shp/site_6_new_poly.shp")

#We can check the site by:
plot(site.shp)

#For this training, we are going to create our own shapefile and raster.
#Specifically, we will create an irregular-shaped site (not square), with 100%
#available habitat for tortoises, that also has two 120 acre soft-release pens.
#Site size is length in meters of one square edge
site.size <- 1500
#Pixel size will be set to 5m per pixel
pixel.size <- 5
#We can create a raster based on those set sizes. Projection is for Florida.
new.crs <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
site <- raster(nrows=site.size/pixel.size, ncol=site.size/pixel.size, crs=new.crs)
#Set smallest Easting and Northing for site
xmin <- 300000
ymin <- 3350000
#Set the extent based on coordinates and size
extent(site) <- c(xmin, xmin+site.size, ymin, ymin+site.size)
#For now, set all pixel values to 0
values(site) <- 0
site

#We want to create a site that is more irregular than just a square:
area.1 <- extent(matrix(c(300050, 3350050, 301450, 3351450), nrow=2))
site[area.1][site[area.1] == 0] <- 1
area.2 <- extent(matrix(c(300050, 3350550, 300450, 3351050), nrow=2))
site[area.2][site[area.2] == 1] <- 0
area.3 <- extent(matrix(c(301150, 3350050, 301450, 3350510), nrow=2))
site[area.3][site[area.3] == 1] <- 0
#We can use the below code to check the acreage of the site
sum <- sum(site@data@values, na.rm = TRUE)
(sqrt(sum)*5)^2/4047

#We need to add our soft-release pens - just to help with the vignette example
pen.area <- site

pen.1 <- extent(matrix(c(300050, 3350050, 301150, 3350490), nrow=2))
pen.area[pen.1][pen.area[pen.1] == 1] <- 3
pen.2 <- extent(matrix(c(300600, 3350650, 301300, 3351350), nrow=2))
pen.area[pen.2][pen.area[pen.2] == 1] <- 3

#Remove rest of habitat to set boundaries
pen.area <- reclassify(pen.area, cbind(1, NA))
pen.area <- reclassify(pen.area, cbind(0, NA))
#Add fence to pen area to site
edge <- boundaries(pen.area, classes=TRUE, directions=8)
edge <- reclassify(edge, cbind(1, 2))
edge <- reclassify(edge, cbind(0, NA))
#Merge all three together to make site
new.site <- merge(edge, pen.area, site)
site.rast <- reclassify(new.site, cbind(0, NA))

#Convert and store shapefiles to crop rasters later
site.shp <- rasterToPolygons(site.rast)

#Plot raster to visualize site
temp<-as.data.frame(site.rast, xy = T)
temp <- temp %>% na.omit()
ggplot(temp) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#Now, number 2.
#We have now created a recipient site to work with. The second item on our list
#is any additional acreage to be added to the site. We will only work with the
#site we have made above moving forward, but we can create additional acreage to
#test package functionality later on.
#Create another site, just to test the first function.
site.size <- 400  #Site size is length in meters of one square edge
pixel.size <- 5
new.crs <- "+proj=utm +zone=17 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
site.2 <- raster(nrows=site.size/pixel.size, ncol=site.size/pixel.size, crs=new.crs)
xmin <- 301600
ymin <- 3350000
extent(site.2) <- c(xmin, xmin+site.size, ymin, ymin+site.size)
values(site.2) <- 0
site.2
#Convert and store shapefiles to crop rasters later
site.shp.2 <- rasterToPolygons(site.2)

temp.2<-as.data.frame(site.2, xy = T)
temp.2 <- temp.2 %>% na.omit()
ggplot(temp.2) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")


#For number 3.
#The last three requirements can be conveniently parsed from the first site we
#created. We have already checked the site acreage, but we do this again below.
#We also set the projection system, but this can easily be verified.
#Calculate site acreage
sum <- sum(site@data@values, na.rm = TRUE)
(sqrt(sum)*5)^2/4047

#For number 4, most recipient sites in Florida will be in UTM zone 17, but
#refer to Figure 3 of the SOP to check. We will use zone 17 for this tutorial.
#Check site CRS
site.rast

#For number 5, we have created our own raster with pen edges, so we know where
#they are. A shapefile or raster file with pen location would be the easiest
#method of visualizing pen locations. Otherwise, an aerial map or site visit
#would be another option.
#Plot raster as a reminder:
ggplot(temp) +
  geom_tile(aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

####Function:merging shapefiles - site_merge----

#If further information about any of the functions is needed, we can view the
#help file by using a question mark:
?site_merge

#We are not going to use our first function, as outlined in the SOP (Figure 10).
#The site_merge function will combine the two shapefiles. For this function to
#run, we need to provide specific information. Firstly, we need to provide the
#two shapefiles that need to be merged together (site1 and site2). Then we need
#to specify the UTM zone (17 in this case). Next, we need to let the function
#know if we would like the merged site to be plotted upon merging, and also
#if this shapefile needs to be saved to the working directory (TRUE(T)/FALSE(F))
new_site <- site_merge(site.shp, site.shp.2, 17, plot = F, save = F)

ggplot() +
  geom_polygon(data = new_site, aes(x = long, y = lat, group = group,
                                    fill = "light blue")) +
  scale_fill_viridis_d(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

####Function:placing transect lines - tran_place----

#Check function help documentation:
?tran_place

#We are going to use the next function, tran_place, to delineate transect lines
#across our recipient site depicting the lines to be walked for the pilot survey.
#We need to provide the function with some key information, depending on the
#size of the area to be surveyed (number 3 of checklist). We have a 400 acre site,
#so we can refer to figure 4 of the SOP to know what characteristics are needed.
#For this size, we need 50% of the overall transects to be surveyed for the pilot
#survey, with a distance of 50m between adjacent transects.

#Importantly, this is where we decide which direction the transect lines will be
#running. The options are either North-South ("N-S"), East-West ("E-W"), or
#Northeast-Southwest ("NE-SW). In an attempt to troubleshoot potential problems,
#we will run the function using a "N-S" orientation. Similar to the last
#function, we need to provide the site shapefile, UTM zone and if we want the
#lines to be plotted and saved to the working directory.

#Set a seed for reproducible results
set.seed(1235)

#The tran_place function produces a SpatialLines object
pilot.lines <- tran_place(50, 0.5, "N-S", site.shp, 17, plot = F, save = F)
#We can review information about the SpatialLines object by running:
pilot.lines

#Plot the raster and lines to assess position relative to pens
temp<-as.data.frame(site.rast, xy = T)
temp <- temp %>% na.omit()
#Need to convert transect lines for plotting in ggplot
data <- data.frame(lines=1:length(pilot.lines))
p.lines.new <- SpatialLinesDataFrame(pilot.lines, data, match.ID = FALSE)
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#We now have the first potential pilot lines that we could survey. However, when
#choosing transect direction, it is extremely important to consider the position
#of pen edges in relation to the transect lines. In our example, consider the
#following locations specifically:

#Make points for data visualization
point.1 <- data.frame(
  x = c(300050, 300050, 300000, 300100),
  y = c(3350050, 3350500, 3350250, 3350250)
)
point.2 <- data.frame(
  x = c(300600, 300600, 300550, 300650),
  y = c(3350650, 3351300, 3351000, 3351000)
)
point.3 <- data.frame(
  x = c(301300, 301300, 301250, 301350),
  y = c(3350650, 3351300, 3351000, 3351000)
)
point.4 <- data.frame(
  x = c(301150, 301150, 301100, 301200),
  y = c(3350050, 3350500, 3350250, 3350250)
)
test <- ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  #geom_point(data = point.1, aes(x = x, y = y), color = "black") +
  #geom_circle(aes(x0 = x0, y0 = y0, r = r, color = r), data = circles) +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

test + stat_ellipse(data = point.1, aes(x = x, y = y), linetype = 2, level = 0.7,
                    color = "red") +
  stat_ellipse(data = point.2, aes(x = x, y = y), linetype = 2, level = 0.7,
               color = "red") +
  stat_ellipse(data = point.3, aes(x = x, y = y), linetype = 2, level = 0.7,
               color = "blue") +
  stat_ellipse(data = point.4, aes(x = x, y = y), linetype = 2, level = 0.7,
               color = "blue")

#The dashed red lines show locations where the transect lines run directly on
#top of pen edge locations. Where this occurs, we are likely to overestimate the
#true density of tortoises. This is because translocated gopher tortoises are
#likely to cluster around these pen edges, because they typically offer built-up
#sandy habitat (perfect for burrowing), but also limit outward movement - which
#is typical for translocated reptiles. As a result of the transect line running
#along one of these highly clustered edges, the encounter rate of a survey is
#inflated and ultimately the estimates are highly overestimated.

#On the other hand, the blue dashed lines show where a pen edge has been
#completely missed by the placement of transect lines. We have also found that
#underestimations of density can occur due to an uneven sampling across pen edges,
#particularly where highly clustered pen edges are missed entirely.

#Due to the poor placement of transect lines shown in the above figure, we will
#now try running the _tran_place_ function again using a NE-SW direction. If we
#tried an E-W direction, we could easily encounter the same issue as N-S, due to
#the high linearity of our simulated study site. Let's see what the NE-SW looks
#like:

#Now trying the same parameters, but with NE-SW
pilot.lines <- tran_place(50, 0.5, "NE-SW", site.shp, 17, plot = F, save = T)
#We can review information about the SpatialLines object by running:
pilot.lines

#Plot the raster and lines to assess position relative to pens
temp<-as.data.frame(site.rast, xy = T)
temp <- temp %>% na.omit()
#Need to convert transect lines for plotting in ggplot
data <- data.frame(lines=1:length(pilot.lines))
p.lines.new <- SpatialLinesDataFrame(pilot.lines, data, match.ID = FALSE)
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")


#These new pilot lines give a better distribution of transects across pen edges.
#So we will be moving forward using these pilot transect lines.

####Function:getting start and end locations - start_end----

#Check function help documentation:
?start_end

#Now we have the transect lines needed for our pilot surveys, we are going to
#extract the start and end locations of transects. These will be exported to a
#GPS device so that we can adhere to line trajectories in the field.

#For the start_end function to work, we need to provide the pilot lines
#that we created using the tran_place function, and the utm zone as before.
#By setting save = T, this will save the points to the working directory.
se.pts <- start_end(pilot.lines, zone = 17, save = F, plot = F)

#Change to dataframe for plotting
se_loc <- data.frame(se.pts)
#We can also check the coordinated by plotting against the site and transects
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  geom_point(data = se_loc, aes(x = x, y = y), size = 3, color = "orange") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#The orange points now delineate either where a transect line should start or end
#during a survey. Observers should survey from one orange point, to the opposite
#orange point which is connected via the blak trnsect line.

####Function:delineate locations for vegetation sampling - veg_plot----

#Check function help documentation:
?veg_plot

#While doing the surveys, collecting vegetation obstruction data can be
#extremely important for accurate tortoise density estimation. In the next
#function, we are going to determine where these vegetation plots are going
#to be conducted.

#The locations are determined in a very similar manner to the start_end function
#where we need to provide the pilot lines. However, we need to also provide
#the set distance in meters between vegetation points.
veg.loc <- veg_plot(pilot.lines, intv = 75, save = F, plot = F)

#Change to dataframe for plotting
veg_loc <- data.frame(veg.loc)
#We can also check the coordinated by plotting against the site and transects
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  #  geom_point(data = se_loc, aes(x = x, y = y), size = 3, color = "orange") +
  geom_point(data = veg_loc, aes(x = x, y = y), size = 2, color = "red") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#The red points now show where observers will stop and collect vegetation
#obstruction data. Consistent with our function, these are set at 75m apart.

#We have come to the end of our "before sampling" section.
#Let's review our working directory folders to see if we have everything needed
#to get out in the field and perform our pilot survey.

#Below shows all the files we have in our working directory. We need the
#following files to continue:
#1. Transect_lines.shp
#2. Start_end_locations.shp
#3. Veg_plots.shp
list.files()

#####After pilot#####

####Function:calculate pilot effort - samp_effort----

#Check function help documentation:
?samp_effort

#Once the pilot survey has been conducted, as outlined in the SOP, we can take
#the start and end locations for all surveyed lines and calculate the effort.
#Our start_end function creates a csv file with start and end locations for
#each transect line. This mirrors the expected dataframe from an actual field
#survey. The order of the columns is the most important for the function.
#See the SOP for information on how to properly format the coordinate data
#during and following an LTDS survey.

#To calculate the effort (distance in meters), we provide the start and end
#locations of each transect line walked during the pilot survey. As before, we
#also need the utm zone. We also have the option to plot the transect lines.
#Since we are using lines that we created, we will not re-plot the lines, however,
#this is this could give us the opportunity to assess if there are any errors
#with the data collection or function. For example, if the lines do not look
#anything like what was actually walked (besides being perfectly straight), then
#there is likely an issue.

#The csv file will be in the working directory
start.end <- read_csv("./start_end_coord.csv")

effort <- samp_effort(start.end, zone = 17, plot = F)

#View resulting effort - this is another place to check if the distance is
#corresponding to the actual effort in the field.
effort

#As we can see from above, the effort for the pilot survey was 19869m (~19.9km).

####Function:Determine lines for full LTDS survey - samp_full ----

#Check function help documentation:
?samp_full

#Now we have the distance covered during the pilot survey, we can calculate the
#remaining effort needed to attain our target CV (default = 17%). The following
#function has a similar functionality to the _tran_place_ function used at the
#beginning of the tutorial. However, we do not want to duplicate effort, so we
#now build upon the pilot survey to add additional transect lines.

#There are several bits of information that we need to provide to get the function
#to work. Firstly, we need the effort from the _samp_effort_ function. We need to
#state the direction of the transects (which needs to match the direction of the
#pilot survey), which in this case is NE-SW. Next, we need the number of occupied
#burrows detected during the pilot survey. If there were a lot of occupied
#burrows discovered, we can pull this information from the data. However, for the
#purpose of this vignette, we will set the number of occupied burrows (nburr) to
#100. We then need to provide the desired CV from the survey. For our tutorial, we
#will aim for a CV of 17%. As with the other functions, we lastly need to provide
#the utm zone.

full.ltds <- samp_full(effort = effort, site.poly = site.shp, direction = "NE-SW",
                       nburr = 100, cv = 0.17, zone = 17, plot = F, save = F)

#We can now plot the lines over the pilot lines, to see how much more effort
#would be needed to attain our target cv.

data <- data.frame(lines=1:length(full.ltds))
full.lines <- SpatialLinesDataFrame(full.ltds, data, match.ID = FALSE)
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  geom_polygon(data = full.lines, aes(x = long, y = lat, group = group),
               color = "red") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#By assigning 100 discovered, occupied burrows we reached a CV of 17.32%. The
#function has suggested new lines (red), that need to be surveyed to attain a CV
#under 17%. Now, let's test the function again, but with 120 burrows:

full.ltds <- samp_full(effort = effort, site.poly = site.shp, direction = "NE-SW",
                       nburr = 120, cv = 0.17, zone = 17, plot = F, save = F)

#By changing the nburr to 120, the object is no longer created. In the console,
#we can see the statement "Target CV attained". This means that by finding 120
#burrows with the current amount of pilot effort, we have already achieved a CV
#that is lower than our target. In this case, the CV is 15.81%. Now, let's try
#the opposite, what if we only detected 50 occupied burrows?

full.ltds <- samp_full(effort = effort, site.poly = site.shp, direction = "NE-SW",
                       nburr = 50, cv = 0.17, zone = 17, plot = F, save = F)

data <- data.frame(lines=1:length(full.ltds))
full.lines <- SpatialLinesDataFrame(full.ltds, data, match.ID = FALSE)
ggplot() +
  geom_tile(data = temp, aes(x = x, y = y, fill = layer)) +
  geom_polygon(data = p.lines.new, aes(x = long, y = lat, group = group),
               color = "black") +
  geom_polygon(data = full.lines, aes(x = long, y = lat, group = group),
               color = "red") +
  scale_fill_viridis_c(option = "mako", begin = 0.3, end = 0.8) +
  xlab("Easting") +
  ylab("Northing") +
  theme_classic() +
  theme(legend.position = "none")

#With this much lower encounter rate, our CV was much higher at 24.49%. We can see
#from the accompanying plot that we would now need to survey all possible
#transects to achieve a more adequate CV.

#####After full LTDS#####

#We now presume that the full LTDS survey has been conducted. This survey will
#ultimately give us a dataframe with several important variables. Import the
#data by:

full.data <- read_csv("./Data/LTDS_example_data.csv")

#Let's view the data to see what we're now working with.
View(full.data)

#We can also summarize the data
summary(full.data)

####Function:truncate data - LTDS_crop ----

#Check function help documentation:
?LTDS_crop

#So that we can use the data to estimate the density of tortoises at the site,
#we are going to slightly format the data. Specifically, we are going to
#truncate the data so that any burrows found over half the distance between the
#lines are removed; and we are also going to remove the top distance outliers (5%).

#The function we are using simply needs two bits of information. We need the
#data from the full survey, and we also need to set the desired truncation
#distance - this will always be half the distance between transects. For our
#tutorial, that is 25m.
data.new <- LTDS_crop(full.data, 25)

#We can summarize the data again for comparison.
summary(data.new)

####Function:estimate the density/abundance - dens_est ----

#Check function help documentation:
?dens_est

#We are finally at the penultimate function of the LTDSMethod package. We are
#now ready to calculate the density of tortoises at the recipient site.

#For the function to work and estimate the density, we need to provide three
#main bits of information. We need the cropped ltds data from LTDS_crop, the
#effort resulting from samp_effort, and lastly we need the total area of the
#recipient site in acres.
tort.dens <- dens_est(ltds_data = data.new, effort = effort, area = 400)

#Let's look at the resulting density:
tort.dens

#This means that our total number of tortoises estimated at the site is:
tort.dens*400

#Our final density estimate at the site is 1.68 tortoises per acre, which is
#approximately 671 tortoises at our site. This is fabricated data to fit in with
#the vignette training, so the accuracy of this estimation is unknown. See the
#SOP for simulation results.

####Function:produce LTDS final report - LTDS_summary ----

#As always, let's check the function help file:
?LTDS_summary #default values set to "NA"

#Now we can produce our final report. There is slightly more manual input for
#the report, since we wanted to make this adaptable to changes in data and
#reuslts which cannot be captured by the LTDSMethod package. The parameters of
#the function and report are as follows:

#effort: the effort from the full LTDS survey. We will use the pilot effort that
#we calculated earlier for this example.
effort <- effort
#nburr: the total number of burrows found during the survey. We can axtract this
#from our example dataset, which will likely be applicable for future use.
nburr <- length(full.data$Burrow_width)
#burr.w.mn: the mean width of detected burrows.
burr.w.mn <- round(mean(full.data$Burrow_width), digits = 2)
#burr.w.min: the minimum width of detected burrows.
burr.w.min <- round(min(full.data$Burrow_width), digits = 2)
#burr.w.max: the maximum width of detected burrows.
burr.w.max <- round(max(full.data$Burrow_width), digits = 2)
#comm.comm: the name of the most common commensal species detected. We can
#extract the number of detections for the full dataset by:
table(full.data$Commensals)
#This shows that the most common species was the Florida mouse
comm.comm <- "Florida mouse"
#occ: the number of occupied burrows discovered.
#unocc: the number of unoccupied burrows discovered.
#unk: the number of unknown burrows discovered.
#We can use the same method as above to pull these numbers out of the data
table(full.data$Occupied)
occ <- 107
unocc <- 131
unk <- 25
#site.name: the name of the recipient site. We can set this to whatever we want.
site.name <- "Tortoise Wonderland"
#tort.dens: the estimated tortoise density at the site. We calculated this earlier.
tort.dens <- round(tort.dens, digits = 2)
#tort.pop: the estimated tortoise population at the site. We calculated this earlier.
tort.pop <- round(tort.dens*400)

#The above information is used for the text part of the documents, however, any
#figures produced in the report will pull data directly from the working directory.

#We can now input all of these into our function. As a warning, if you have
#previously run this function, and viewed the resulting pdf, the function will
#not run if you still have the pdf open.
LTDS_summary(effort = effort, nburr = nburr, burr.w.mn = burr.w.mn,
             burr.w.min = burr.w.min, burr.w.max = burr.w.max, comm.comm = comm.comm,
             occ = occ, unocc = unocc, unk = unk, site.name = site.name,
             tort.dens = tort.dens, tort.pop = tort.pop)


#That is the end of the LTDSMethod package training. Thank you for joining!
#Any questions, please contact me at maxdoltonjones@vt.edu


