#' LTDS crop
#'
#' Crops field data to remove observations outside of truncation distance and outliers
#' @param ltds_data Raw field data from a full LTDS survey
#' @param tran.dist The distance between transect lines
#' @param save Do you want to save the data to the working directory?
#' @return A csv file of truncated ltds data
#' @examples
#' #Read in csv file showing transect lines
#' ltds_data <- read_csv("./LTDS_example_data.csv")
#'
#' #Input raw data into function
#' new_ltds_data <- ltds_crop(ltds_data = ltds_data, tran.dist = 50, save = T)
#'
#' @export
LTDS_crop <- function(ltds_data, trunc, save = T){

  trunc <- tran.dist/2
  #Remove any distance greater than the input truncation distance (trunc)
  ltds_data <- ltds_data[!(ltds_data$Distance > trunc),]
  #Arrange the data in descending order and then remove the highest 5%
  #of values
  ltds_data <- ltds_data %>%
    arrange(desc(Distance)) %>%
    slice_tail(prop = 0.95)
  #Re-arrange data by transect id
  ltds_data <- ltds_data %>%
    arrange(Transect_ID)

  if(save == T){
    write_csv(file = "LTDS_truncated_data.csv", x = ltds_data)
  } else if(save == F){

  }

  return(ltds_data)
}
