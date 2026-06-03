# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title:        Crop Coefficient (Kc) Time Series Expansion Engine
# Author:       Rutuja Chitra-Tarak
# Description:  Expands a 366-day localized crop coefficient (Kc) climatology 
#               profile across a continuous multi-year meteorological forcing timeline.
# Dependencies: dplyr, lubridate
#               (Note: Required libraries are handled and sourced externally)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 1: TIME SERIES EXPANSION ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Expand Crop Coefficient Climatology to Long-Term Meteorological Timeline
#'
#' @description Recycles a localized daily 366-day crop coefficient (Kc) profile 
#' by mapping it onto a continuous multi-year calendar timeline based on Day of Year (DOY).
#' Handles structural lists output by upstream interpolation tools or standalone data frames.
#'
#' @param kc.doy A data frame containing 'doy' and 'kc' columns, or a structured list 
#'   containing a daily profile data frame under the element 'ts' or 'clim'.
#' @param from Date object marking the beginning boundary of the target timeline (default: 1951-01-01).
#' @param to Date object marking the ending boundary of the target timeline (default: 2023-12-31).
#'
#' @return A structured list containing:
#'   \item{ts}{A comprehensive data frame containing columns 'date' (Date), 'doy' (numeric), and 'kc' (numeric).}
#'   \item{kc.key}{The original discrete parameter mapping inputs if provided.}
#'   \item{clim}{The parsed 366-day clean profile used during join operations.}
#'   \item{phase}{Preserved growth stage tracking vector metadata if available.}
#'   \item{actions}{Preserved scheduled management cycle metadata if available.}
#' @export
expand_kc <- function(kc.doy, 
                      from = as.Date("1951/01/01"),
                      to = as.Date("2023/12/31")) {
  
  # Error handling boundaries
  if (missing(kc.doy) || is.null(kc.doy)) {
    stop("The input tracking object 'kc.doy' is missing or evaluated as NULL.")
  }
  if (!inherits(from, "Date") || !inherits(to, "Date")) {
    stop("Timeline boundary limits ('from' and 'to') must be valid Date objects.")
  }
  if (from > to) {
    stop("The simulation starting date ('from') cannot be greater than the ending date ('to').")
  }
  
  # Initialize defensive variable tracking structures
  clim <- NULL
  kc.key <- NULL
  phase <- NULL
  actions <- NULL
  
  # Parse input structures dynamically depending on object class architecture
  if (is.list(kc.doy) && !is.data.frame(kc.doy)) {
    if (!is.null(kc.doy$clim)) {
      clim <- kc.doy$clim
    } else {
      clim <- kc.doy$ts
    }
    kc.key   <- kc.doy$kc.key
    phase   <- kc.doy$phase
    actions <- kc.doy$actions
  } else if (is.data.frame(kc.doy)) {
    clim   <- kc.doy
    kc.key <- kc.doy
  }
  
  # Structural compliance checks on extracted profile metrics
  if (is.null(clim) || !is.data.frame(clim)) {
    stop("Failed to extract or identify a valid data frame of daily profile coefficients from 'kc.doy'.")
  }
  if (!all(c("doy", "kc") %in% colnames(clim))) {
    stop("The baseline profile matrix must contain explicit structural tracking columns 'doy' and 'kc'.")
  }
  
  # Clean, typecast, and deduplicate to prevent unexpected row explosion during merge steps
  clim$doy <- as.numeric(clim$doy)
  clim$kc  <- as.numeric(clim$kc)
  clim     <- clim[, c("doy", "kc")]
  clim     <- clim[!duplicated(clim$doy), ]
  
  # Build complete daily calendar sequence timeline tracking table
  ts <- data.frame(date = seq(from = from, to = to, by = "1 day"))
  
  # Extract absolute Day of Year (DOY) markers via explicit lubridate scope
  ts$doy <- as.numeric(lubridate::yday(ts$date))
  
  # Recycle localized climatology profile to fit full forcing met dates sequence
  ts <- dplyr::left_join(ts, clim, by = "doy")
  
  # Standardize layout constraint orderings
  ts <- ts[, c("date", "doy", "kc")]
  
  return(list(
    ts      = ts, 
    kc.key  = kc.key, 
    clim    = clim, 
    phase   = phase, 
    actions = actions
  ))
}