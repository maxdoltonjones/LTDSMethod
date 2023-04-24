tran_place <- function(tran.dist, pil.perc, direction, site.poly, zone, plot, save){
  if(pil.perc < 0 | pil.perc > 100){
    return(print("pil.perc must be a value between 0-100"))
  }
  pil.perc <- pil.perc/100
  #site.poly <- shp.test
  #Transform shapefile to SpatialPolygonsDataFrame
  if(class(site.poly) == "SpatialPolygonsDataFrame"){
    site.p2 <- vect(site.poly)
  } else{
    site.p2 <- site.poly
  }
  crs(site.p2) <- paste("+proj=utm + zone=", 17, " ellps=WGS84", sep='')
  #site.p <- spTransform(site.poly, CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  #Create an empty raster using the same extent as the shapefile
  r <- terra::rast(ext(site.p2))
  #Set the resolution
  res(r) <- 5
  #Convert polygon to spatvector
  #site.p2 <- vect(site.p)
  #Rasterize SpatialPolygonsDataFrame to new raster
  site.r <- r
  #Change all site values to equal 1 to simplify raster
  site <- classify(site.r, cbind(1, nrow(site.p2), 1))
  #site.size.x <- site@ptr[["extent"]][["vector"]][4]-site@ptr[["extent"]][["vector"]][3]
  site.size <- site@ptr[["extent"]][["vector"]][2]-site@ptr[["extent"]][["vector"]][1]
  #if(site.size.x > site.size.y){
  #  site.size <- site.size.x
  #  } else if(site.size.y > site.size.x){
  #  site.size <- site.size.y
  #  }
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
    tran.dist <- tran.dist + (tran.dist*0.16)
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
    tran.dist <- tran.dist + (tran.dist*0.16)
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
  pilot.lines.new <- vect(pilot.lines)
  pilot.lines.crop <- terra::crop(pilot.lines.new, site.p2)
  #data <- data.frame(lines=1:length(pilot.lines.crop))
  #final.lines <- SpatialLinesDataFrame(pilot.lines.crop, data, match.ID = FALSE)
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
    #writeOGR(final.lines, ".", paste0("Transect_lines"),
    #         driver = "ESRI Shapefile", overwrite = T)
    writeVector(pilot.lines.crop, "./Transect_lines.shp",
                overwrite = T)
  } else if(save == F){
    
  }
  return(pilot.lines.crop)
}
