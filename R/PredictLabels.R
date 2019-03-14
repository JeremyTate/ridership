#' Detection/Prediction for Bus Passengers
#'
#' Predicts labels of passengers based on proximity (time) to ground-truth
#' stops, manufacturer, minimum speed, detection time and maximum transfer rate
#'
#' @import chron
#' @import dplyr
#' @import plyr
#' @import randomForest
#'
#' @param wifi.data A wifi dataset containing variables named "NetworkType",
#' "Channel", "Latitude", "Longitude", "Maxspeed", "Minspeed", "Firstdetection",
#' and "LastDetection"
#' @param gps.data a gps dataframe with a UTC column
#' @param stops.data a matching list of stops with longitude and latitude data
#' @param phones vector of phone manufacturers
#' @param dist.tol chron times object that determines how close to a stop a device
#' must be to be labelled a passenger
#' @param speed.tol speed in meters per second where observations with speed
#' greater than this will be classified as non-passengers
#' @param time.tol the number of seconds where observations that are
#' detected for less than this time will be classified as non-passengers
#' @param rate.tol maximum transfer rate tolerance in kbps?
#' @param gps.time.offset a chron times object that specifies how many hours the
#' gps time data is off from the wifi time data
#' (gps.time.offset = gps time - wifi time)
#'
#' @export
#'
#' @references
#'
#' @return PredictLabels returns the wifi dataset with labelled passengers. For
#' each detected passenger, the first and last stops are included as separate
#' columns (first.stop and last.stop respectively)
#'
#' @examples
#'

PredictLabels <- function(wifi.data, gps.data, stops.data, phones, dist.tol,
                          speed.tol, time.tol, rate.tol=6000, gps.time.offset) {
  load("RF_Manufacturer_Mod.rda")  # RF.mod

  stops.data <- SyncTimes(stops.data=stops.data, gps.data=gps.data,
                          gps.time.offset=gps.time.offset)
  wifi.data <- CleanWifiData(wifi.data)
  wifi.data$passenger <- 0
  wifi.data$last.stop <- wifi.data$first.stop <- rep(NA, nrow(wifi.data))
  # note: distance is in seconds
  last.dist <- first.dist <- rep(NA, nrow(stops.data))

  for (i in 1:nrow(wifi.data)) {
    wifi.data[i, ] <- EvalPass(obs=wifi.data[i, ], stops.data=stops.data,
                               phones=phones, dist.tol=dist.tol,
                               speed.tol=speed.tol, time.tol=time.tol,
                               rate.tol=rate.tol, pass.mod=RF.mod)
  }

  wifi.data <- rbind.fill(wifi.data)

  return(wifi.data)
}
