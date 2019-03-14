#' GPS times converter
#'
#' Function to convert GPS UTC to correct chron times vector
#'
#' @inheritParams PredictLabels
#'
#' @references
#'
#' @return A dataframe identical to the gps dataframe with the corrected time
#' (gps.data$time)
#'
#' @examples
#'

ConvertGPSTimes <- function(gps.data, gps.time.offset) {

  for (i in 1:nrow(gps.data)) {  # deal with incorrectly formatted times
    if (nchar(gps.data$UTC[i]) < 8) {
      gps.data$UTC[i] <- paste("00:", gps.data$UTC[i], sep="")
    }
  }

  gps.data$time <- chron::times("00:00:01")  # initialize times
  for (i in 1:nrow(gps.data)) {
    # 24 hour offset
    if (chron::times(gps.data$UTC[i]) - gps.time.offset <= 0) {
      gps.data$time[i] <- chron::times(gps.data$UTC[i]) +
        chron::times("12:00:00") - gps.time.offset +
        chron::times("12:00:00")
    } else {
      gps.data$time[i] <- chron::times(gps.data$UTC[i]) - gps.time.offset
    }
  }

  # deal with possible times misorderings due to going from midnight to next day
  if (sum(!(gps.data$time[order(gps.data$time)] == gps.data$time)) > 0) {
    for (i in 1:(nrow(gps.data) - 1)) {

      # NA in times problem (sometimes occurs at "00:00:00")
      if (is.na(gps.data$time[i + 1])) {
        gps.data$time[i + 1] <- chron::times("00:00:00")
      }

      # going to next day problem
      if (gps.data$time[i + 1] < 0) {
        gps.data$time[i + 1] <- gps.data$time[i + 1] + 1
      }

      # hour mishap problem (time logger jumps back an hour sometimes)
      if (gps.data$time[i] + chron::times("00:00:01") != gps.data$time[i + 1]) {
        gps.data$time[i + 1] <- gps.data$time[i] + chron::times("00:00:01")
        warning("Times were not in order. This was corrected (possibly wrongly).")
      }

    }
  }

  return(gps.data)
}
