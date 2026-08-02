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
# Title:        Crop Coefficient (Kc) Generation & Climatology Utilities
# Author:       Rutuja Chitra-Tarak
# Description:  A comprehensive suite of functions to prepare, interpolate,
#               and simulate dynamic daily Crop Coefficient (Kc) time series.
#               Explicitly designed to match legacy agricultural model baselines.
# Dependencies: Base R only
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# TABLE OF CONTENTS
# 1. build_fao_curve        - Generates standard FAO-56 segmented crop curves
# 2. generate_fruit_step    - Generates monthly block curves for fruit trees
# 3. generate_polynomial_kc - Generates continuous polynomial curves
# 4. generate_grazed_legacy - Generates grazed pasture curves matching legacy rules
# 5. generate_napier        - Generates cyclical harvesting/regrowth curves
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. STANDARD FAO-56 CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate Standard FAO-56 Segmented Curve
#'
#' @description Builds a continuous 366-day daily Kc curve from discrete FAO-56 
#' growth stage parameters using hyper-fast vectorized calculations.
#'
#' @param row A one-row data frame or named list containing crop parameters.
#' @param base_val Numeric baseline value for the off-season (default 0).
#' @return A data frame containing columns 'crop', 'doy', and the computed 'kc'.
#' @export
build_fao_curve <- function(row, base_val = 0) {
  crop_name <- row$crop
  Sow <- row$Sow
  
  if (crop_name == "Grasslands") base_val <- 0.01
  
  # Calculate absolute durations
  dur_ini  <- if(row$E_ini < Sow) row$E_ini + 365 - Sow else row$E_ini - Sow
  dur_dev  <- if(row$E_dev < Sow) row$E_dev + 365 - Sow else row$E_dev - Sow
  dur_mid  <- if(row$E_mid < Sow) row$E_mid + 365 - Sow else row$E_mid - Sow
  dur_harv <- if(row$Harv < Sow) row$Harv + 365 - Sow else row$Harv - Sow
  if (Sow == row$Harv) dur_harv <- 365
  
  wrap_crops <- c("Grapes", "Banana 2", "Horsegram", "Rice Long")
  is_wrapping <- crop_name %in% wrap_crops
  
  # 1. Generate the entire DOY vector instantly
  doy <- 1:365
  d_eff <- if(is_wrapping) {
    ifelse(doy >= Sow, doy - Sow, doy + 365 - Sow)
  } else {
    doy - Sow
  }
  
  # 2. Vectorized segment generation (no loops!)
  out_kc <- rep(base_val, 365)
  
  cond_ini      <- d_eff >= 0 & d_eff <= dur_ini
  cond_dev      <- d_eff > dur_ini & d_eff <= dur_dev
  cond_mid      <- d_eff > dur_dev & d_eff <= dur_mid
  cond_harv_eq  <- d_eff > dur_mid & d_eff <= dur_harv & dur_harv == dur_mid
  cond_harv_neq <- d_eff > dur_mid & d_eff <= dur_harv & dur_harv != dur_mid
  
  out_kc[cond_ini] <- row$K_ini
  out_kc[cond_dev] <- row$K_ini + ((d_eff[cond_dev] - dur_ini) / (dur_dev - dur_ini)) * (row$K_mid - row$K_ini)
  out_kc[cond_mid] <- row$K_mid
  out_kc[cond_harv_eq] <- row$K_mid
  out_kc[cond_harv_neq] <- row$K_mid + ((d_eff[cond_harv_neq] - dur_mid) / (dur_harv - dur_mid)) * (row$K_end - row$K_mid)
  
  # Leap Year Drag-Down
  out_kc <- c(out_kc, out_kc[365])
  
  return(data.frame(crop = crop_name, doy = 1:366, kc = out_kc, stringsAsFactors = FALSE))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2. FRUIT TREE CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate Fruit Tree Kc Curve (Monthly Step Function)
#'
#' @description Maps monthly discrete crop coefficient thresholds onto a continuous 
#' day-of-year (DOY) timeline using explicit calendar-month groupings.
#'
#' @param crop_name Character string matching the column name of the target crop.
#' @param raw_params A data frame containing a 'Month' column (1-12) and Kc values.
#' @return A structured data frame with columns 'crop', 'doy', and 'kc'.
#' @export
generate_fruit_step <- function(crop_name, raw_params) {
  
  if (!is.character(crop_name) || length(crop_name) != 1) {
    stop("Parameter 'crop_name' must be a single character string.")
  }
  if (!is.data.frame(raw_params)) {
    stop("Parameter 'raw_params' must be a data frame object.")
  }
  if (!"Month" %in% colnames(raw_params)) {
    stop("Data frame 'raw_params' must contain an explicit 'Month' tracking column.")
  }
  if (!crop_name %in% colnames(raw_params)) {
    stop(sprintf("Target crop column '%s' not found within 'raw_params'.", crop_name))
  }
  
  # Generate chronological month assignments for 366 days
  date_seq <- as.Date(0:365, origin = "2023-01-01")
  months <- as.numeric(format(date_seq, "%m"))
  
  doy_df <- data.frame(doy = 1:366, Month = months, stringsAsFactors = FALSE)
  merged_df <- merge(doy_df, raw_params[, c("Month", crop_name)], by = "Month", all.x = TRUE)
  merged_df <- merged_df[order(merged_df$doy), ]
  
  # Convert crop name to Title Case
  s <- strsplit(crop_name, " ")[[1]]
  crop_title <- paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " ")
  
  out_df <- data.frame(
    crop = crop_title,
    doy = merged_df$doy,
    kc = merged_df[[crop_name]],
    stringsAsFactors = FALSE
  )
  
  return(out_df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. POLYNOMIAL CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate Polynomial Curve Parameters
#'
#' @description Models smooth, continuous dynamic trajectories of crop coefficients 
#' (e.g., for Groundnuts) using a pre-calculated 4th-order polynomial equation.
#'
#' @param crop_name Character string identifier for the crop type.
#' @param c4 Numeric coefficient for the fourth-order term (doy^4).
#' @param c3 Numeric coefficient for the third-order term (doy^3).
#' @param c2 Numeric coefficient for the second-order term (doy^2).
#' @param c1 Numeric coefficient for the first-order term (doy).
#' @param intercept Numeric intercept constant.
#' @return A data frame containing evaluated 'doy', 'crop', and computed 'kc'.
#' @export
generate_polynomial_kc <- function(crop_name, c4, c3, c2, c1, intercept) {
  
  if (!is.numeric(c(c4, c3, c2, c1, intercept))) {
    stop("All polynomial curve coefficients and the baseline intercept must be numeric.")
  }
  
  doy <- 1:366
  kc <- c4*(doy^4) + c3*(doy^3) + c2*(doy^2) + c1*doy + intercept
  
  out_df <- data.frame(
    doy = doy,
    crop = crop_name,
    kc = kc,
    stringsAsFactors = FALSE
  )
  
  return(out_df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. GRAZED PASTURE CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate Grazed Pasture Curve (Legacy Match)
#'
#' @description Generates a grazed grassland curve utilizing an optimized sequential loop.
#'
#' @param crop_name Character string name tag for the grass category.
#' @param graze_doys Numeric vector of Days of Year when grazing events occur.
#' @param base_kc Numeric vector (length 366) representing the uncut biological baseline.
#' @param g_rate Numeric daily regrowth rate.
#' @param g_drop Numeric Kc value to drop to immediately after grazing (default 0.20).
#' @param m_bound Numeric maximum upper bound for regrowth (default 0.75).
#' @param sow_doy Numeric Day of Year representing the start of the season (default 122).
#' @param base_val Numeric baseline value for the off-season (default 0.01 for dry season).
#' @return A data frame containing columns 'crop', 'doy', and the computed 'kc'.
#' @export
generate_grazed_legacy <- function(crop_name, graze_doys, base_kc, g_rate, g_drop = 0.20, m_bound = 0.75, sow_doy = 122, base_val = 0.01) {
  kc_array <- numeric(366)
  
  # Initialize a tracker so we don't have to search arrays inside the loop
  last_graze <- -999 
  
  for (i in 1:365) {
    if (i < sow_doy) {
      kc_array[i] <- base_val
    } else {
      
      # Did a grazing event happen today?
      if (i %in% graze_doys) last_graze <- i
      
      if (last_graze == -999) {
        kc_array[i] <- base_kc[i]
      } else {
        if ((i - last_graze) < 30) {
          kc_array[i] <- g_drop
        } else {
          if (kc_array[i-1] == base_val) { 
            kc_array[i] <- base_kc[i]
          } else {
            kc_array[i] <- min(m_bound, kc_array[i-1] + g_rate)
          }
        }
      }
    }
  }
  
  kc_array[366] <- kc_array[365]
  
  return(data.frame(doy = 1:366, crop = crop_name, kc = kc_array, stringsAsFactors = FALSE))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 5. NAPIER GRASS CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate Napier Grass Harvesting Cycles
#'
#' @description Generates step-and-decay cyclical sequences representing continuous 
#' automated harvesting cutting schedules and subsequent multi-phase vegetative regrowth.
#'
#' @param crop_name Character string name tag for the grass category.
#' @param start_doy Numeric representation of the initial calendar day harvesting begins.
#' @param decline_rate_1 Numeric slope parameter for Phase 1 biomass or senescence decay.
#' @param decline_rate_2 Numeric slope parameter for subsequent recurring harvesting cycles.
#' @return A data frame containing columns 'doy', 'crop', and the computed timeline 'kc'.
#' @export
generate_napier <- function(crop_name, start_doy, decline_rate_1, decline_rate_2) {
  
  if (!is.numeric(c(start_doy, decline_rate_1, decline_rate_2))) {
    stop("Parameters 'start_doy', 'decline_rate_1', and 'decline_rate_2' must be purely numeric.")
  }
  
  doy <- 1:366
  kc <- numeric(366)
  
  # Baseline correction before start
  kc[doy < start_doy] <- 0
  
  # Phase 1 sequence conditions
  cond1 <- (start_doy == 1) & (doy <= 50) & (doy >= start_doy)
  if (any(cond1)) {
    d_cond1 <- doy[cond1]
    kc[cond1] <- ifelse(d_cond1 <= 11, 0.5,
                        ifelse(d_cond1 <= 45, 0.9, 0.9 - decline_rate_1 * (d_cond1 - 45)))
  }
  
  # Subsequent cyclical sequences
  cond2 <- doy >= start_doy & !cond1
  if (any(cond2)) {
    cycle_start <- ifelse(start_doy == 1, 51, start_doy)
    c_day <- (doy[cond2] - cycle_start) %% 50
    kc[cond2] <- ifelse(c_day < 11, 0.4,
                        ifelse(c_day < 45, 0.95, 0.95 - decline_rate_2 * (c_day - 44)))
  }
  
  out_df <- data.frame(
    doy = doy,
    crop = crop_name,
    kc = kc,
    stringsAsFactors = FALSE
  )
  
  return(out_df)
}