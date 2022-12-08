#' Survey effort
#'
#' Calculates the effort as distance in meters
#' @param points A csv file showing start and end locations for LTDS lines
#' @param zone The UTM zone of the study site
#' @param plot Do you want the actual lines walked to be plotted?
#' @return Distance in meters showing effort of pilot or full survey
#' @examples
#' #Read in csv file showing transect lines
#' start.end <- read_csv("./Start_end_locations.csv")
#'
#' #Input coordinate file
#' effort <- samp_effort(start.end, zone = 17, plot = T)
#' #View resulting effort
#' effort
#'
#' @export
samp_effort <- function(points, zone, plot){
  l <- vector("list", nrow(points))
  for(i in 1:nrow(points)){
    l[[i]] <- Lines(list(Line(cbind(c(as.numeric(points[i, 1]), as.numeric(points[i, 3])),
                                    c(as.numeric(points[i, 2]), as.numeric(points[i, 4]))))),
                    as.character(i))
  }
  lines <- SpatialLines(l)
  #zone <- 17
  crs(lines) <- crs(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep=''))
  effort <- gLength(lines)
  if(plot == T){
    plot(lines)
  } else if(plot == F){

  }
  return(effort)
}
