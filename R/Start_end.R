#' Start end
#'
#' Delineates start and end coordinates for LTDS transect lines
#' @param lines A SpatialLines object delineating transect lines
#' @param zone The UTM zone of the study site
#' @param plot Do you want to plot/visualize the start and end points?
#' @param save Do you want to save the SpatialPoints to the working directory?
#' @return A SpatialPoints object showing start and end locations for LTDS lines
#' @examples
#' #Read in SpatialLines object showing transect lines
#' transect.lines <- readOGR(dsn = paste0("./Transect_lines.shp"))
#'
#' #Input transect lines, and delineate UTM zone
#' transect.points <- start_end(transect.lines, 1, plot = T, save = T)
#'
#' @export
start_end <- function(lines, zone, plot, save){
  crd.full <- as.data.frame(crds(pilot.lines))
  coord.list <- vector(mode = "list", length = nrow(crd.full)/2)
  for (i in 1:length(lines)) {
    crd.test <- as.data.frame(crds(pilot.lines[i,]))
    line.list <- vector(mode = "list", length = nrow(crd.test))
      for (j in 1:nrow(crd.test)) {
        if(j %% 2 == 0){
          next
        }else {
        xy1 <- as.data.frame(crd.test[j,])
        xy2 <- as.data.frame(crd.test[j+1,])
        xy <- cbind(xy1, xy2)
        colnames(xy) <- c("Start_x", "Start_y", "End_x", "End_y")
        line.list[[j]] <- xy
        }
      }
    line.list <- line.list[lapply(line.list,length)>0]
    line.list <- do.call(rbind.data.frame, line.list)
    coord.list[[i]] <- line.list
  }
#  new.list <- vector(mode = "list", length = 2)
#  for(p in 1:length(new.list)){
#    coord.list.test <- coord.list[[p]]
#    #colnames(coord.list.test) <- c("Start_x", "Start_y", "End_x", "End_y")
#    new.list[[p]] <- coord.list.test
#  }
  #coord.list <- coord.list[lapply(coord.list,length)>0]
  coord.list <- do.call(rbind.data.frame, coord.list)
  #new.list.test <- do.call(rbind.data.frame, new.list)
  coord.list <- SpatialPoints(coord.list, proj4string = CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  coord.list <- SpatialPointsDataFrame(coord.list, data.frame(row.names=row.names(coord.list),
                                                              ID=1:length(coord.list)))

  #Create dataframe with coordinates from lines
  coords <- as.data.frame(coord.list@coords)
  #Add a transect number for each line
  coords$Transect_ID <- c(1:nrow(coords))
  #Save the csv to working directory
  write_csv(coords, "start_end_coord.csv")
  if(plot == T){
    plot(coord.list)
  } else if(plot == F){

  }
  if(save == T){
    writeOGR(coord.list, ".", paste0("Start_end_locations"),
             driver = "ESRI Shapefile", overwrite = T)
  } else if(save == F){

  }
  return(coord.list)
}
