#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param wolbachia_2023
#' @param draws
#' @return
#' @author njtierney
#' @export
greta_dharma <- function(wolbachia_2023, means) {

  # do posterior simulations (calculate size and prob, stick in rnbinom), summarise and plot as in INLA analysis
  y_preds <- means * NA
  y_preds[] <- rpois(prod(dim(means)), means[])

  # summarise these
  medians <- apply(means, 2, median)

  # get objects required by DHARMa

  dharma <- createDHARMa(simulatedResponse = t(y_preds),
                         observedResponse = wolbachia_2023$cases,
                         fittedPredictedResponse = medians,
                         integerResponse = TRUE)

  # annoyingly, these required objects aren't added by createDHARMa
  dharma$refit <- FALSE
  dharma$simulatedResponse <- t(y_preds)

  dharma

}
