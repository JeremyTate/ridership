#' Evaluate passengers according to cutoff values
#'
#' A function to calculate whether or not a single observation is a passenger
#' based on specified cutoff values
#'
#' @param obs A single observation from a wifi dataset that is to be classified
#' as a passenger or not.
#' @param pass.mod A random forest model to classify "unknown" manufacturers as
#' valid manufacturers or not.
#'
#' @inheritParams PredictLabels
#'
#' @import randomForest
#'
#' @references
#'
#' @return The same observation from a wifi dataset with a "passenger" variable
#' that indicates whether or not it was determined to be a bus passenger
#' (1: passenger, 0: not a passenger). If the observation is determined to be a
#' passenger the first and last stop (first.stop and last.stop respectively) are
#' added as new columns to the observation.
#'
#' @examples
#'
EvalPass <- function(obs, stops.data, phones, dist.tol, speed.tol, time.tol,
                     rate.tol, pass.mod) {

  last.dist <- first.dist <- rep(NA, nrow(stops.data))
  for (j in 1:nrow(stops.data)) {
    # distance is difference in times
    first.dist[j] <- abs(obs$first.detection - stops.data$time[j])
    last.dist[j] <- abs(obs$last.detection - stops.data$time[j])
  }
  first.closest <- which.min(first.dist)
  last.closest <- which.min(last.dist)

  if (obs$NetworkType == "probe" &
      obs$Channel == 0 &
      (is.na(obs$SSID) | obs$SSID == "#N/A" | obs$SSID == "") &
      obs$Maxrate <= rate.tol &
      obs$Manufacturer %in% phones &
      first.dist[first.closest] <= dist.tol &
      last.dist[last.closest] <= dist.tol &
      first.closest != last.closest &
      (is.na(obs$Minspeed) | obs$Minspeed <= speed.tol) &
      obs$Detectiontime >= time.tol) {

    if (obs$Manufacturer == "Unknown") {
      if (predict(pass.mod, obs[, c(14, 15, 21)]) == 0) {
        return(obs)  # not a predicted phone on bus
      }
    }

    obs$passenger <- 1
    obs$first.stop <- stops.data$name[first.closest]
    obs$last.stop <- stops.data$name[last.closest]
  }

  return(obs)
}
