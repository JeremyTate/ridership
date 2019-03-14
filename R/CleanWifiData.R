#' Cleans variables in wifi data
#'
#' Changes appropriate variables to factors and corrects time-logging problem
#' (hours incorrectly specified as minutes and minutes incorrectly specified as seconds)
#'
#' @inheritParams PredictLabels
#'
#' @references
#'
#' @return A dataframe of the same size with cleaned wifi data
#'
#' @examples
#'

CleanWifiData <- function(wifi.data) {

  wifi.data$NetworkType <- suppressWarnings(factor(wifi.data$NetworkType))
  wifi.data$Channel <- suppressWarnings(factor(wifi.data$Channel))
  wifi.data$Latitude <- suppressWarnings(as.numeric(wifi.data$Latitude))
  wifi.data$Longitude <- suppressWarnings(as.numeric(wifi.data$Longitude))
  wifi.data$Maxspeed <- suppressWarnings(as.numeric(wifi.data$Maxspeed))
  wifi.data$Minspeed <- suppressWarnings(as.numeric(wifi.data$Minspeed))

  # Note: the hours were incorrectly specified as minutes and the minutes were
  # incorrectly specified as seconds and both were corrected below
  wifi.data$first.detection <- chron::times(substr(wifi.data$Firstdetection,
                                                   12, 19))
  wifi.data$last.detection <- chron::times(substr(wifi.data$Lastdetection,
                                                  12, 19))

  return(wifi.data)
}
