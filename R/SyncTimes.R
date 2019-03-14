#' Synchronize Bus Stop Arrival Times
#'
#' Synchronize the time a bus reaches each stop based on topological principals
#'
#' @inheritParams PredictLabels
#'
#' @param chon.time.tol Chron times object which is the tolerance for times that are
#' determined to be on the same part of the route
#' @param dist.tol Radius of circle used to determine closeness of points
#' @param first.cutoff Chron times object where the first stop must occur within
#' this cutoff of the beginning of the route
#' @param last.cutoff Chron times object where the last stop must occur within
#' this cutoff of the end of the route
#'
#'
#' @references
#'
#' @return A modified stops.data dataframe with the estimated time that the bus reached each stop
#'
#' @examples
#'

SyncTimes <- function(gps.data, stops.data, gps.time.offset,
                      chon.time.tol=chron::times("0:05:00"), dist.tol=0.001,
                      first.cutoff=chron::times("0:05:00"),
                      last.cutoff=chron::times("0:05:00")) {

  gps.data <- ConvertGPSTimes(gps.data=gps.data, gps.time.offset=gps.time.offset)
  gps.data <- ConvertGPSCoords(gps.data=gps.data)
  stops.data$time <- chron::times("0:00:00")
  stops.data$anchor <- FALSE

  dist <- matrix(rep(NA, nrow(stops.data) * nrow(gps.data)),
                 ncol=nrow(stops.data))
  for (i in 1:nrow(stops.data)) {  # Compute distance matrix
    # Naive manhattan distance
    dist[, i] <- abs(gps.data$long - stops.data$longitude[i]) +
      abs(gps.data$lat - stops.data$latitude[i])
  }

  # 1: label first and last stop ###############################################
  first.obs <- gps.data$time - gps.data$time[1] < first.cutoff
  last.obs <- gps.data$time[nrow(gps.data)] - gps.data$time < last.cutoff
  # which.max retruns first matching value (first TRUE)
  start.last.obs <- which.max(last.obs)
  first.index <- which.min(dist[first.obs, 1])
  stops.data$time[1] <- gps.data$time[first.index]
  last.index <- which.min(dist[last.obs, nrow(stops.data)]) + start.last.obs - 1
  stops.data$time[nrow(stops.data)] <- gps.data$time[last.index]
  stops.data$anchor[nrow(stops.data)] <- stops.data$anchor[1] <- TRUE

  # 2: determine which stops are anchor points #################################
  for (i in 2:(nrow(stops.data) - 1)) {
    close.times <- gps.data$time[dist[, i] < dist.tol]
    closest.time <- gps.data$time[which.min(dist[, i])]
    time.diffs <- abs(close.times - closest.time)
    if (sum(!(time.diffs < chon.time.tol)) == 0) {
      stops.data$time[i] <- closest.time
      stops.data$anchor[i] <- TRUE
    }
  }

  # 3: classify remaining stops according to anchor points constraint ##########
  for (i in 1:nrow(stops.data)) {
    if (stops.data$anchor[i]) {
      next
    }

    # find times from closest anchor point before and closest anchor point after
    for (j in i:1) {
      if (stops.data$anchor[j]) {
        time.before <- stops.data$time[j]
        break
      }
    }
    for (j in i:nrow(stops.data)) {
      if (stops.data$anchor[j]) {
        time.after <- stops.data$time[j]
        break
      }
    }

    # select all points within distance tolerance
    close.points <- gps.data[dist[, i] < dist.tol, ]
    close.dists <- dist[dist[, i] < dist.tol, i]
    # discard points that are not between time before and time after
    discard.vec <- close.points$time > time.before &
      close.points$time < time.after
    close.points <- close.points[discard.vec, ]
    close.dists <- close.dists[discard.vec]

    # rerun #2 on remaining points.
    # If anchor: assign closest time and label anchor, if not move to step #4
    close.times <- close.points$time
    closest.time <- close.points$time[which.min(close.dists)]
    time.diffs <- abs(close.times - closest.time)
    if (sum(!time.diffs < chon.time.tol) == 0) {
      stops.data$time[i] <- closest.time
      stops.data$anchor[i] <- TRUE
    } else {
      # 4: If all constraints work, simply take closest point
      # Note: this does not incorporate time approximations
      stops.data$time[i] <- closest.time
      warning(paste("Warning: impossible to determine exact time of the",
                             stops.data$name[i], "stop. Time approximated."))
    }
  }
  stops.data <- stops.data[, !(colnames(stops.data) %in% "anchor")]

  return(stops.data)
}
