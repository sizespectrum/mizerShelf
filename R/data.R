#' MizerParams object for the NW Mediterranean shelf model
#'
#'
#' @format A MizerParams object
#' @source Forthcoming paper
"NWMed_params"

#' Catch size distribution for the NW Mediterranean shelf around Blanes
#'
#' This contains size observations from landings of 9 species:
#' Shortfin squid, Angler fish, Poor cod, Hake, Blue whiting,
#' Striped red mullet, Horse mackerel, Red mullet and Starfish. 
#'
#' @format
#' A data frame with 208 rows and 4 columns:
#' \describe{
#'   \item{species}{Species name}
#'   \item{length}{Start of length bin in cm}
#'   \item{dl}{Width of length bin in cm. The bin goes from `length` to 
#'     `length + dl`.}
#'   \item{catch}{Number of fish observed in the catch in this size bin for this 
#'     species}
#' }
#' @source Silvia de Juan, IMEDEA (UIB-CSIC)
"NWMed_catch"