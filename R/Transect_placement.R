#' Transect placement
#'
#' Place transects across site in preparation for LTDS surveys
#' @param tran.dist The required distance between transects
#' @param pil.perc The percentage of overall transects required for pilot surveys
#' @param direction The cardinal direction of the transects
#' @param site A raster file of the site, with an assigned CRS
#' @param site.poly A shape file used to clip the transect lines to fit the site
#' @param plot Do you want the transect placement to be plotted against the site?
#' @return A SpatialLines object delineating transect placement
#' @examples 
#' #Read in site shapefile and raster file
#' site.poly <- readOGR(dsn = paste0("./site.poly.shp"))
#' site <- raster(paste0("./site.tif"))
#' 
#' #Select required transect parameters and run function
#' transect.lines <- tran_place(50, 0.5, "angle", site, site.poly, plot = T)
#' 
#' @export
tran_place <- function(tran.dist, pil.perc, direction, site, site.poly, plot){
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
  pilot.lines.crop <- raster::crop(pilot.lines, site.poly.3)
  if(plot == T){
    plot(site)
    plot(pilot.lines.crop, add = T)
  } else if(plot == F){
    
  }
  return(pilot.lines.crop)
}
