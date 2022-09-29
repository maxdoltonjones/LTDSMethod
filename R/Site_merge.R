#' Site merge
#'
#' Merge two shapefiles together
#' @param shape1 The first shapefile that needs merging
#' @param shape2 The second shapefile that needs merging
#' @param zone The UTM zone of the shapefile/study site
#' @param plot Do you want to plot/visualize the merged site?
#' @param save Do you want to save the merged site to the working directory?
#' @return A new shapefile resulting from combining two separate shapefiles
#' @examples
#' #Read in two site shapefiles needing to be merged
#' shape1 <- readOGR(dsn = paste0("./shape1.shp"))
#' shape2 <- readOGR(dsn = paste0("./shape2.shp"))
#'
#' #Select required transect parameters and run function
#' shape_merge <- site_merge(shape1, shape2, 1, plot = T, save = T)
#'
#' @export
site_merge <- function(shape1, shape2, zone, plot, save){
  #Transforming the shapefiles so that they have the same projection system
  #This will work even if the input shapes have UTM or LatLong projection
  site.1 <- spTransform(shape1, CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  site.2 <- spTransform(shape2, CRS(paste("+proj=utm + zone=", zone, " ellps=WGS84", sep='')))
  #Merge/bind the two shapefiles
  shape_merge <- bind(site.1, site.2)
  #Plot merged site if plot = TRUE
  if(plot == T){
    plot(shape_merge)
  } else if(plot == F){

  }
  #Save merged site into working directory if save = T
  if(save == T){
    writeOGR(shape_merge, ".", paste0("Merged_site"),
             driver = "ESRI Shapefile", overwrite = T)
  } else if(save == F){

  }
  return(shape_merge)
}


