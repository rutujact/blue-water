# Copyright (C) 2026 Rutuja-Chitra Tarak
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Lesser General Public License for more details.
# 
# You should have received a copy of the GNU Lesser General Public License
# along with this program. If not, see <https://gnu.org>.


# ==============================================================================
# title: "SCS Curve Number Effective Rainfall and Runoff Engine"
# author: "Rutuja Chitra-Tarak"
# description: Implements the USDA Soil Conservation Service (SCS) Curve Number
#              method to convert daily raw precipitation series into localized
#              surface runoff (Q) and effective moisture infiltration (Peff)
#              conditioned on explicit Curve Numbers and localized soil archetypes.
# references: Reddy K. S. et al. (2012). Farm Ponds: A Climate Resilient Technology.
# input: (1) precip - A data frame containing 'date' and numeric 'p' or 'P' rows.
#        (2) CN - An atomic numeric Curve Number parameter (default: 85).
#        (3) soil - An integer scalar flag: 1 for Red soil, 2 for Black soil.
# output: A structured data frame mapping chronological date, raw rainfall (P),
#         computed surface runoff (Q), and remaining effective rainfall (Peff).
# ==============================================================================

#' Compute Daily Effective Rainfall and Runoff via SCS Curve Number Method
#' 
#' @param precip A data.frame containing daily rainfall with at least \code{date} (Date or Character) and \code{p} (or \code{P}, numeric rainfall in mm).
#' @param CN A numeric scalar specifying the SCS Curve Number coefficient. Defaults to \code{85}.
#' @param soil An integer specifying the structural soil classification framework. Use \code{1} for Red soil (initial abstraction threshold = 0.3*S) or \code{2} for Black soil (initial abstraction threshold = 0.2*S). Defaults to \code{1}.
#' 
#' @return A data.frame with explicit chronological columns: \code{date} (class \code{Date}), \code{P} (raw precipitation), \code{Q} (computed runoff), and \code{Peff} (effective precipitation).
#' @export
#' 
#' @examples
#' # rain_data <- data.frame(date = seq(as.Date("2020-01-01"), by="day", length.out=3), p = c(5.0, 45.2, 0.0))
#' # effective_rain <- get_Peff(precip = rain_data, CN = 85, soil = 1)
get_Peff <- function(precip, CN = 85, soil = 1) {
  
  # 1. Input Guarding and Structural Validations
  if (!is.data.frame(precip)) {
    stop("Argument 'precip' must be a structured data.frame object.")
  }
  
  colnames(precip) <- tolower(colnames(precip))
  
  if (!"date" %in% colnames(precip)) {
    stop("Input data must contain an explicit 'date' column vector.")
  }
  if (!any(c("p", "p") %in% colnames(precip))) {
    # Handle implicit 'p' variants gracefully
    if ("p" %in% colnames(precip)) {
      colnames(precip)[colnames(precip) == "p"] <- "p"
    } else {
      stop("Input data must contain an explicit numeric precipitation column named 'p' or 'P'.")
    }
  }
  
  if (!soil %in% c(1, 2)) {
    stop("Invalid 'soil' argument parameter specified. Select 1 (Red Soil) or 2 (Black Soil).")
  }
  
  # 2. Type Mapping and Time Isolation
  ts <- data.frame(
    date = as.Date(precip$date),
    P    = as.numeric(precip$p),
    stringsAsFactors = FALSE
  )
  
  # 3. Derive Maximum Soil Water Retention Capacity Parameter (S)
  # Formula: S = (25400 / CN) - 254
  S <- (25400 / CN) - 254
  
  # 4. Set Initial Abstraction Wetting Thresholds (lambda * S) Based on Soil Type
  if (soil == 2) { 
    # Black soil archetype (Typically 0.2)
    thresh <- 0.2 * S
  } else if (soil == 1) { 
    # Red soil loamy archetype (Adjusted to 0.3 based on regional calibration text)
    thresh <- 0.3 * S
  }
  
  # Initialize runoff vector tracking
  ts$Q <- stats::setNames(numeric(nrow(ts)), NULL)
  
  # 5. Execute Core SCS-CN Segment Splitting Matrix Math
  # Case A: Precipitation is below or equal to the initial abstraction threshold (Q = 0)
  P_below_thresh <- ts$P <= thresh
  ts$Q[P_below_thresh] <- 0
  
  # Case B: Precipitation exceeds threshold. Compute empirical runoff curve: Q = (P - thresh)^2 / (P - thresh + S)
  P_above_thresh <- !P_below_thresh
  if (any(P_above_thresh)) {
    numerator <- (ts$P[P_above_thresh] - thresh)^2
    denominator <- (ts$P[P_above_thresh] - thresh) + S
    ts$Q[P_above_thresh] <- numerator / denominator
  }
  
  # 6. Final Hydrological Balance Equation
  ts$Peff <- ts$P - ts$Q
  
  # 7. Write standard output asset tracking record
  dir.create(here("data/processed"), showWarnings = FALSE, recursive = TRUE)
  utils::write.csv(ts, file = here(paste0("data/processed/11_met.forcing_soil_", soil, ".csv")), row.names = FALSE)
  
  return(ts)
}