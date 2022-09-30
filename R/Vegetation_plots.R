#' Vegetation plots
#'
#' Delineates locations for collecting vegetation obstruction data
#' @param lines A SpatialLines object delineating transect lines
#' @param intv The interval/distance in meters between plots
#' @param plot Do you want to plot/visualize the start and end points?
#' @param save Do you want to save the SpatialPoints to the working directory?
#' @return A SpatialPoints object showing start and end locations for LTDS lines
#' @examples
#' #Read in SpatialLines object showing transect lines
#' transect.lines <- readOGR(dsn = paste0("./Transect_lines.shp"))
#'
#' #Input transect lines, and delineate distance between vegetation plots
#' veg.plot <- veg_plot(transect.lines, 75, plot = T, save = T)
#'
#' @export
veg_plot <- function(lines, intv, plot, save){
  n <- (gLength(lines))/intv
  veg.point <- spsample(pilot.lines, n, "regular")
  veg.point <- SpatialPointsDataFrame(veg.point, data.frame(row.names=row.names(veg.point),
                                                            ID=1:length(veg.point)))
  if(plot == T){
    plot(veg.point, pch = 1, cex= .5)
  } else if(plot == F){

  }
  if(save == T){
    writeOGR(veg.point, ".", paste0("Veg_plots"),
             driver = "ESRI Shapefile", overwrite = T)
  } else if(save == F){

  }
  return(veg.point)
}
