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
  coord.list <- vector(mode = "list", length = length(lines@lines))
  for (i in 1:length(lines@lines)) {
    if(length(lines@lines[[i]]@Lines) > 1){
      ext.list <- vector(mode = "list", length = 1)
      coord.list <- c(coord.list, ext.list)
      for (j in 1:length(lines@lines[[i]]@Lines)) {
        xy1 <- as.data.frame(t(lines@lines[[i]]@Lines[[j]]@coords[1,]))
        xy2 <- as.data.frame(t(lines@lines[[i]]@Lines[[j]]@coords[2,]))
        xy <- rbind(xy1, xy2)
        if(j == 1){
          coord.list[[i]] <- xy
        } else if(j > 1){
          coord.list[[length(coord.list)]] <- xy
        }
      }
    } else if(length(lines@lines[[i]]@Lines) == 1){
      xy1 <- as.data.frame(t(lines@lines[[i]]@Lines[[1]]@coords[1,]))
      xy2 <- as.data.frame(t(lines@lines[[i]]@Lines[[1]]@coords[2,]))
      xy <- rbind(xy1, xy2)
      coord.list[[i]] <- xy
    }
  }
  coord.list <- do.call(rbind.data.frame, coord.list)
  coord.list <- SpatialPoints(coord.list, proj4string = CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  coord.list <- SpatialPointsDataFrame(coord.list, data.frame(row.names=row.names(coord.list),
                                                              ID=1:length(coord.list)))

  #Create dataframe with coordinates from lines
  coords <- as.data.frame(coord.list@coords)
  #Filter out every other coordinates for one side of lines
  coord2 <- coords %>%
    slice(which(row_number() %% 2 == 1))
  #Delete first row to apply same methodology
  temp.coord <- coords %>%
    filter(!row_number() %in% c(1))
  #Filter our every other row again
  coord3 <- temp.coord %>%
    slice(which(row_number() %% 2 == 1))
  #Combine data
  coord.com <- cbind(coord2, coord3)
  #Add a transect number for each line
  coord.com$Transect_ID <- c(1:nrow(coord.com))
  #Rename the columns
  colnames(coord.com) <- c("Start_x", "Start_y", "End_x", "End_y", "Transect_ID")
  #Save the csv to working directory
  write_csv(coord.com, "start_end_coord.csv")
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
