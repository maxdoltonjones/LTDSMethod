#' Transect placement
#'
#' Place transects across site in preparation for LTDS surveys
#' @param tran.dist The required distance between transects in meters
#' @param pil.perc The decimal percentage of overall transects required for pilot surveys
#' @param direction The cardinal direction of the transects
#' @param site.poly A shape file used to clip the transect lines to fit the site
#' @param zone The UTM zone of the shapefile/study site
#' @param plot Do you want the transect placement to be plotted against the site?
#' @return A SpatialLines object delineating transect placement
#' @examples
#' #Read in site shapefile file
#' site.poly <- readOGR(dsn = paste0("./site.poly.shp"))
#'
#' #Select required transect parameters and run function
#' transect.lines <- tran_place(50, 0.5, "angle", site.poly, 1, plot = T)
#'
#' @export
tran_place <- function(tran.dist, pil.perc, direction, site.poly, zone, plot){
  #Get coordinate reference system from shapefile
  #CRS.shp <- site.poly@proj4string@projargs
  #Transform shapefile to SpatialPolygonsDataFrame
  site.p <- spTransform(site.poly, CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  #Create an empty raster using the same extent as the shapefile
  r <- raster(extent(site.p))
  #Set the resolution
  res(r) <- 5
  #Rasterize SpatialPolygonsDataFrame to new raster
  site.r <- rasterize(site.p, r)
  #Change all site values to equal 1 to simplify raster
  site <- reclassify(site.r, cbind(1, nrow(site.poly@data), 1), left=FALSE)
  site.size <- site@extent@ymax - site@extent@ymin
  if(direction == "vert"){
    diff.ang <- site.size * cos(0 * pi / 180)
    ext.ang <- site.size - diff.ang
    lt.start <- round(runif(1, 1, tran.dist))
    pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
    pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
    l <- vector("list", length(pilot.lts))
    for(p in 1:length(pilot.lts)){
      l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin-ext.ang+pilot.lts[p],
                                        site@extent@xmin+ext.ang+pilot.lts[p]),
                                      c(site@extent@ymin,
                                        site@extent@ymin+site.size)))), as.character(p))
    }
  }else if(direction == "angle"){
    diff.ang <- site.size * cos(45 * pi / 180)
    ext.ang <- site.size - diff.ang
    lt.start <- round(runif(1, 1, tran.dist))
    pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
    pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
    l <- vector("list", length(pilot.lts))
    for(p in 1:length(pilot.lts)){
      l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin-ext.ang+pilot.lts[p],
                                        site@extent@xmin+ext.ang+pilot.lts[p]),
                                      c(site@extent@ymin,
                                        site@extent@ymin+site.size)))), as.character(p))
    }
  } else if(direction == "horz"){
  lt.start <- round(runif(1, 1, tran.dist))
  pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
  pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
  l <- vector("list", length(pilot.lts))
  for(p in 1:length(pilot.lts)){
    l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin,
                                      site@extent@xmin+site.size),
                                    c(site@extent@ymin+pilot.lts[p],
                                      site@extent@ymin+pilot.lts[p])))), as.character(p))
    }
  }
  pilot.lines <- SpatialLines(l)
  crs(pilot.lines) <- crs(site)
  #Cropping pilot lines to fit within study area
  pilot.lines.crop <- raster::crop(pilot.lines, site.poly)
  if(plot == T){
    plot(site)
    plot(pilot.lines.crop, add = T)
  } else if(plot == F){

  }
  return(pilot.lines.crop)
}
