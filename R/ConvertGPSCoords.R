#' GPS coordinate converter
#'
#' Converts GPS x and y coordinates to longitude and latitude based
#' on linear model
#'
#' @inheritParams PredictLabels
#'
#' @references
#'
#' @return The original dataframe with longitude and latitude coordinates added
#'
#' @examples
#'

ConvertGPSCoords <- function(gps.data) {

  load("Longitude_Conversion_Model.rda")  # long.mod
  load("Latitude_Conversion_Model.rda")  # lat.mod
  gps.data$lat <- gps.data$long <- rep(NA, nrow(gps.data))
  gps.data$long <- predict(long.mod, newdata=data.frame(avg.x=gps.data$X))
  gps.data$lat <- predict(lat.mod, newdata=data.frame(avg.y=gps.data$Y))

  return(gps.data)
}
