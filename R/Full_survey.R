#' Full LTDS survey effort
#'
#' Delineates additional transects needed for full LTDS survey
#' @param effort Distance in meters walked during the pilot survey
#' @param direction The cardinal direction of the transects
#' @param site.poly A shape file used to clip the transect lines to fit the site
#' @param nburr Number of occupied burrows detected during the pilot survey
#' @param cv The target coefficient of variation for full LTDS survey (e.g. 0.17 = 17%)
#' @param zone The UTM zone of the shapefile/study site
#' @param plot Do you want the transect placement to be plotted against the site?
#' @param save Do you want to save the transect lines to the working directory?
#' @return A SpatialLines object delineating additional transect placement
#' @examples
#' #Read in csv file showing transect lines
#' start.end <- read_csv("./Start_end_locations.csv")
#'
#' #Input coordinate file
#' effort <- samp_effort(start.end, zone = 17, plot = T)
#'
#' #Input resulting pilot effort into function
#' full_survey <- samp_full(effort, site.poly, direction = "N-S", nburr = 100, cv = 0.17, zone = 17, plot = T, save = T)
#'
#' #To view the effort needed, run:
#' gLength(full_survey)
#'
#'
#' @export
samp_full <- function(effort, site.poly, direction, nburr, cv, zone, plot, save){

  #cv = % of CV aiming for (0.17 = 17%)
  cv.val <- cv
  #Calculate effort for 17% CV - using this percentage based on conversations
  #with Lora
  enc.rate <- effort/nburr
  #v = approximate variance in density from pilot study
  #Keeping v as default from calculate.effort == 3
  v <- 3
  E <- v/cv.val^2 * enc.rate

  #load in pilot lines from tran_place function
  transect.lines <- readOGR(dsn = paste0("./Transect_lines.shp"))

  if(gLength(transect.lines) > E){
    x.val <- effort/enc.rate
    cv.new <- round(sqrt(v/x.val)*100, digits = 2)
    return(print(paste0("Target CV attained. CV = ", cv.new, "%")))
  }else if(gLength(transect.lines) < E){

    #Combine all (potential) lines and pilot lines
    pot.lts <- readRDS(file="Potential_transects.RData")
    pilot.lts <- readRDS(file="Pilot_transects.RData")
    com.lts <- c(pilot.lts, pot.lts)
    #We can then filter out any remaining lines for additional transects
    rem.lts <- com.lts[!com.lts %in% pilot.lts]
    perc <- 0.1


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

    while(TRUE)
    {
      #Select 10% of remaining lots to add on to survey effort
      if(length(rem.lts) > 1){
        add.lts <- sort(sample(rem.lts, ceiling(length(rem.lts)*perc)))
      } else if (length(rem.lts) == 1){
        add.lts <- rem.lts
      }

      if(direction == "N-S"){
        diff.ang <- site.size * cos(0 * pi / 180)
        ext.ang <- site.size - diff.ang
        l <- vector("list", length(add.lts))
        for(p in 1:length(add.lts)){
          l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin-ext.ang+add.lts[p],
                                            site@extent@xmin+ext.ang+add.lts[p]),
                                          c(site@extent@ymin,
                                            site@extent@ymin+site.size)))), as.character(p))
        }
      }else if(direction == "NE-SW"){
        diff.ang <- site.size * cos(45 * pi / 180)
        ext.ang <- site.size - diff.ang
        l <- vector("list", length(add.lts))
        for(p in 1:length(add.lts)){
          l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin-ext.ang+add.lts[p],
                                            site@extent@xmin+ext.ang+add.lts[p]),
                                          c(site@extent@ymin,
                                            site@extent@ymin+site.size)))), as.character(p))
        }
      } else if(direction == "E-W"){
        l <- vector("list", length(add.lts))
        for(p in 1:length(add.lts)){
          l[[p]] <- Lines(list(Line(cbind(c(site@extent@xmin,
                                            site@extent@xmin+site.size),
                                          c(site@extent@ymin+add.lts[p],
                                            site@extent@ymin+add.lts[p])))), as.character(p))
        }
      }
      add.lines <- SpatialLines(l)
      crs(add.lines) <- crs(site)
      #Cropping pilot lines to fit within study area
      add.lines.crop <- raster::crop(add.lines, site.poly)
      data <- data.frame(lines=1:length(add.lines.crop))
      final.add.lines <- SpatialLinesDataFrame(add.lines.crop, data, match.ID = FALSE)


      add.effort <- gLength(final.add.lines)
      pilot.effort <- gLength(transect.lines)
      comb.effort <- add.effort + pilot.effort
      #Increase percentage of pilot lines to survey
      perc <- perc + 0.1

      #So if the amount of effort given is below the effort needed, the
      #loop will start again with an additional 10% of overall transects
      if(comb.effort > E) break()
      if(perc > 1) break()
      if(length(pilot.lts) > length(pot.lts)) break()
    }
  if(plot == T){
    plot(site)
    plot(final.add.lines, add = T)
  } else if(plot == F){

  }
  if(save == T){
    writeOGR(final.add.lines, ".", paste0("Add_transect_lines"),
             driver = "ESRI Shapefile", overwrite = T)
  } else if(save == F){

  }
    x.val <- effort/enc.rate
    cv.new <- round(sqrt(v/x.val)*100, digits = 2)
    print(paste0("CV = ", cv.new, "%"))
    return(final.add.lines)
  }
}
