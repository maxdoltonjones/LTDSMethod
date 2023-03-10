#' Transect placement
#'
#' Place transects across site in preparation for LTDS surveys
#' @param tran.dist The required distance between transects in meters
#' @param pil.perc The decimal percentage of overall transects required for pilot surveys
#' @param direction The cardinal direction of the transects
#' @param site.poly A shape file used to clip the transect lines to fit the site
#' @param zone The UTM zone of the shapefile/study site
#' @param plot Do you want the transect placement to be plotted against the site?
#' @param save Do you want to save the transect lines to the working directory?
#' @return A SpatialLines object delineating transect placement
#' @examples
#' #Read in site shapefile file
#' site.poly <- readOGR(dsn = paste0("./site.poly.shp"))
#'
#' #Select required transect parameters and run function
#' transect.lines <- tran_place(50, 0.5, "angle", site.poly, 1, plot = T)
#'
#' @export
tran_place <- function(tran.dist, pil.perc, direction, site.poly, zone, plot, save){
    if(pil.perc < 0 | pil.perc > 100){
    return(print("pil.perc must be a value between 0-100"))
    }
  pil.perc <- pil.perc/100
  #Transform shapefile to SpatialPolygonsDataFrame
  site.p <- spTransform(site.poly, CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  #Create an empty raster using the same extent as the shapefile
  r <- terra::rast(ext(site.p))
  #Set the resolution
  res(r) <- 5
  #Convert polygon to spatvector
  site.p2 <- vect(site.p)
  #Rasterize SpatialPolygonsDataFrame to new raster
  site.r <- terra::rasterize(site.p2, r)
  #Change all site values to equal 1 to simplify raster
  site <- classify(site.r, cbind(1, nrow(site.poly@data), 1))
  site.size <- site@ptr[["extent"]][["vector"]][4]-site@ptr[["extent"]][["vector"]][3]
  #site.size <- site@extent@ymax - site@extent@ymin
  if(direction == "N-S"){
    diff.ang <- site.size * cos(0 * pi / 180)
    ext.ang <- site.size - diff.ang
    lt.start <- round(runif(1, 1, tran.dist))
    pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
    pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
    l <- vector("list", length(pilot.lts))
    for(p in 1:length(pilot.lts)){
      l[[p]] <- Lines(list(Line(cbind(c(site@ptr[["extent"]][["vector"]][1]-ext.ang+pilot.lts[p],
                                        site@ptr[["extent"]][["vector"]][1]+ext.ang+pilot.lts[p]),
                                      c(site@ptr[["extent"]][["vector"]][3],
                                        site@ptr[["extent"]][["vector"]][3]+site.size)))), as.character(p))
    }
  }else if(direction == "NE-SW"){
    diff.ang <- site.size * cos(45 * pi / 180)
    ext.ang <- site.size - diff.ang
    lt.start <- round(runif(1, 1, tran.dist))
    pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
    pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
    l <- vector("list", length(pilot.lts))
    for(p in 1:length(pilot.lts)){
      l[[p]] <- Lines(list(Line(cbind(c(site@ptr[["extent"]][["vector"]][1]-ext.ang+pilot.lts[p],
                                        site@ptr[["extent"]][["vector"]][1]+ext.ang+pilot.lts[p]),
                                      c(site@ptr[["extent"]][["vector"]][3],
                                        site@ptr[["extent"]][["vector"]][3]+site.size)))), as.character(p))
    }
  } else if(direction == "E-W"){
  lt.start <- round(runif(1, 1, tran.dist))
  pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
  pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
  l <- vector("list", length(pilot.lts))
  for(p in 1:length(pilot.lts)){
    l[[p]] <- Lines(list(Line(cbind(c(site@ptr[["extent"]][["vector"]][1],
                                      site@ptr[["extent"]][["vector"]][1]+site.size),
                                    c(site@ptr[["extent"]][["vector"]][3]+pilot.lts[p],
                                      site@ptr[["extent"]][["vector"]][3]+pilot.lts[p])))), as.character(p))
    }
  }else if(direction == "NW-SE"){
    diff.ang <- site.size * cos(45 * pi / 180)
    ext.ang <- site.size - diff.ang
    lt.start <- round(runif(1, 1, tran.dist))
    pot.lts <- seq(-site.size/2, (site.size+(site.size/2)), by = tran.dist) + lt.start
    pilot.lts <- sort(sample(pot.lts, round(length(pot.lts)*pil.perc)))
    l <- vector("list", length(pilot.lts))
    for(p in 1:length(pilot.lts)){
      l[[p]] <- Lines(list(Line(cbind(c(site@ptr[["extent"]][["vector"]][1]+ext.ang+pilot.lts[p],
                                        site@ptr[["extent"]][["vector"]][1]-ext.ang+pilot.lts[p]),
                                      c(site@ptr[["extent"]][["vector"]][3],
                                        site@ptr[["extent"]][["vector"]][3]+site.size)))), as.character(p))
    }
  }
  pilot.lines <- SpatialLines(l)
  crs(pilot.lines) <- crs(site)
  #Cropping pilot lines to fit within study area
  pilot.lines.crop <- terra::crop(pilot.lines, site.poly)
  data <- data.frame(lines=1:length(pilot.lines.crop))
  final.lines <- SpatialLinesDataFrame(pilot.lines.crop, data, match.ID = FALSE)
  #Save the full list of potential transect lines
  saveRDS(pot.lts, file="Potential_transects.RData")
  #and save the pilot lines r object too (needed for subsequent function)
  saveRDS(pilot.lts, file="Pilot_transects.RData")
  if(plot == T){
    plot(site)
    plot(pilot.lines.crop, add = T)
  } else if(plot == F){

  }
  if(save == T){
    writeOGR(final.lines, ".", paste0("Transect_lines"),
             driver = "ESRI Shapefile", overwrite = T)
  } else if(save == F){

  }
  return(pilot.lines.crop)
}
