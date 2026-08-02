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
# along with this program. If not, see [https://gnu.org](https://gnu.org).

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title:       Effective Rainfall & Meteorological Forcing Generators
# Description: Standardized SCS Curve Number calculations and ET0 formatting.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Compute Daily Effective Rainfall and Runoff via SCS Curve Number Method
#' 
#' @param precip A data.frame containing daily rainfall with at least `date` and `p`.
#' @param CN A numeric scalar specifying the SCS Curve Number coefficient. Defaults to 85.
#' @param lambda A numeric scalar specifying the Initial Abstraction ratio. Defaults to 0.2.
#' 
#' @return A data.frame with explicit chronological columns: `date`, `P`, `Q`, and `Peff`.
#' @export
get_Peff <- function(precip, CN = 85, lambda = 0.2) {
  
  # 1. Input Guarding and Structural Validations
  if (!base::is.data.frame(precip)) base::stop("Argument 'precip' must be a structured data.frame object.")
  base::colnames(precip) <- base::tolower(base::colnames(precip))
  
  if (!"date" %in% base::colnames(precip)) base::stop("Input data must contain an explicit 'date' column.")
  if (!base::any(c("p", "p") %in% base::colnames(precip))) {
    base::stop("Input data must contain an explicit numeric precipitation column named 'p' or 'P'.")
  }
  base::colnames(precip)[base::colnames(precip) == "p"] <- "p"
  
  # 2. Type Mapping and Time Isolation
  ts <- base::data.frame(
    date = base::as.Date(precip$date),
    P    = base::as.numeric(precip$p),
    stringsAsFactors = FALSE
  )
  
  # 3. Derive Maximum Soil Water Retention Capacity Parameter (S)
  S <- (25400 / CN) - 254
  
  # 4. Set Initial Abstraction Wetting Threshold (lambda * S)
  thresh <- lambda * S
  
  # Initialize runoff vector tracking
  ts$Q <- 0
  
  # 5. Execute Core SCS-CN Segment Splitting Matrix Math
  P_above_thresh <- ts$P > thresh
  
  if (base::any(P_above_thresh)) {
    numerator <- (ts$P[P_above_thresh] - thresh)^2
    denominator <- (ts$P[P_above_thresh] - thresh) + S
    ts$Q[P_above_thresh] <- numerator / denominator
  }
  
  # 6. Final Hydrological Balance Equation
  ts$Peff <- ts$P - ts$Q
  
  return(ts)
}

#' Merge Environmental Forcing Filters
#'
#' @description Combines precipitation (effective rainfall) and reference evapotranspiration (ET0) 
#' into a single continuous daily forcing data frame.
#'
#' @param p A list containing parameter specifications and forcing time series.
#' @param soil_id The numeric or character identifier for the target soil.
#'
#' @return A data frame containing structured daily environmental forcing columns.
#' @export
get_peff_et0 <- function(p, soil_id) {
  
  if (!base::is.list(p)) base::stop("Argument 'p' must be a valid structured parameter list.")
  if (!"soil.param" %in% base::names(p)) base::stop("Parameter object 'p' is missing 'soil.param'.")
  
  # Extract soil-specific variables
  soil_props <- p$soil.param[p$soil.param$soil == soil_id, ]
  if (base::nrow(soil_props) == 0) base::stop(base::paste("Soil ID", soil_id, "not found in soil parameters."))
  
  # Generate soil-specific effective rainfall
  ts <- get_Peff(precip = p$precip, CN = soil_props$CN, lambda = soil_props$lambda)
  
  # Format ET0
  if (p$et0.timestep == "weekly") {
    et0 <- get_et0(p$et0)
  } else if (p$et0.timestep == "daily") {
    et0 <- p$et0
  } else {
    base::stop("Invalid 'et0.timestep' specified. Must be 'daily' or 'weekly'.")
  }
  
  base::colnames(et0) <- base::tolower(base::colnames(et0))
  et0$date <- base::as.Date(et0$date)
  
  # Merge and return
  ts <- dplyr::left_join(ts, et0[, c("date", "et0")], by = "date")
  return(ts)
}