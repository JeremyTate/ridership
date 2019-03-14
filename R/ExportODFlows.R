#' Export origin-destination flows
#'
#' Exports o-d flows to a new folder with a full, compact, and aggregated
#' version.
#'
#' @inheritParams PredictLabels
#'
#' @param labelled.data A wifi dataset with passenger labels (from the
#' `PredictLabels` function)
#' @param folder.name The name of the folder to create (if it doesn't already
#' exist) where the exported o-d flows will be stored
#' @param o.d.flows.name The name of csv which the o-d flows will be stored in
#' @param compact.name The name of the csv which the compact o-d flows will be
#' stored in
#' @param aggregate.name The name of the csv which the aggregated o-d flows will
#' be stored in
#' @param remove.cols Removes irrelevant columns
#' @param rename.cols Renames (capitalizes) columns in labelled data
#'
#' @references
#'
#' @return
#'
#' @export
#'
#' @examples
#'

ExportODFlows <- function(labelled.data, stops.data, folder.name="Output",
                          o.d.flows.name="O-DFlows.csv",
                          compact.name="O-DFlowsCompact.csv",
                          aggregate.name="O-DFlowsAggregate.csv",
                          remove.cols=TRUE, rename.cols=TRUE) {

  pass <- labelled.data[labelled.data$passenger == 1, ]  # passengers
  stops.data$deboarded <- stops.data$boarded <- 0

  # calculating total boarded and deboarded at each stop
  iter <- 0  # index in loop (i is a name)
  for (i in stops.data$name) {
    iter <- iter + 1
    for (j in 1:nrow(pass)) {
      if (identical(pass$first.stop[j], i)) {
        stops.data$boarded[iter] <- stops.data$boarded[iter] + 1
      }
      if (identical(pass$last.stop[j], i)) {
        stops.data$deboarded[iter] <- stops.data$deboarded[iter] + 1
      }
    }
  }

  stops.data <- rbind(stops.data,
                      c(NA, NA, "Detected Total", sum(stops.data$boarded),
                        sum(stops.data$deboarded)))

  if (remove.cols) {# discarding irrelevant vars
    pass <- pass[, c(2, 5:7, 10:18, 21:23, 25:26)]
  }
  if (rename.cols) {
    colnames(pass) <- c("NetworkID", "NetworkType", "MACAddress", "Channel",
                        "Manufacturer", "MaxRate", "MaxSpeed", "MinSpeed",
                        "MaxSignalStrength", "MinSignalStrength", "SSID",
                        "Cloaked", "ConnectedClients", "DetectionTime",
                        "TimeOfFirstDetection", "TimeOfLastDetection",
                        "Boarded", "De-Boarded")
  }

  if(!dir.exists(folder.name)) {
    dir.create(folder.name)  # create output folder if it doesn't exist
  }

  write.csv(pass, file=file.path(folder.name, o.d.flows.name), row.names=FALSE)
  pass <- pass[, 17:18]  # Compact dataset with only bus stops
  write.csv(pass, file=file.path(folder.name, compact.name), row.names=FALSE)
  write.csv(stops.data[, -c(1, 2)], file=file.path(folder.name, aggregate.name),
            row.names=FALSE)
}
