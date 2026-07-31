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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title:        Crop Coefficient (Kc) Generation & Climatology Utilities
# Author:       Rutuja Chitra-Tarak
# Description:  A comprehensive suite of functions to prepare, interpolate,
#               and simulate dynamic daily Crop Coefficient (Kc) time series.
#               Explicitly designed to match legacy agricultural model baselines.
# Dependencies: dplyr, lubridate, stringr
#               (Note: Required libraries are handled and sourced externally)
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
#' growth stage parameters. Replicates cell-by-cell "Days Since Sowing" logic to 
#' natively handle cross-year wrapping, leap years, and perennial crops exactly 
#' as standard spreadsheet models do.
#'
#' @param row A one-row data frame or named list containing crop parameters: 
#' `crop`, `Sow`, `E_ini`, `E_dev`, `E_mid`, `Harv`, `K_ini`, `K_mid`, `K_end`.
#' @param base_val Numeric baseline value for the off-season (default 0).
#' @return A tibble containing columns 'crop', 'doy', and the computed 'kc'.
#' @export
build_fao_curve <- function(row, base_val = 0) {
  crop_name <- row$crop
  Sow <- row$Sow
  
  # Scientific correction: Grasslands drop to 0.01 in the dry season
  if (crop_name == "Grasslands") {
    base_val <- 0.01
  }
  
  # Calculate absolute durations from the sowing day to mimic Excel formulas
  dur_ini  <- if(row$E_ini < Sow) row$E_ini + 365 - Sow else row$E_ini - Sow
  dur_dev  <- if(row$E_dev < Sow) row$E_dev + 365 - Sow else row$E_dev - Sow
  dur_mid  <- if(row$E_mid < Sow) row$E_mid + 365 - Sow else row$E_mid - Sow
  dur_harv <- if(row$Harv < Sow) row$Harv + 365 - Sow else row$Harv - Sow
  
  # Perennial override: if Sow == Harv, duration is a full 365-day cycle
  if (Sow == row$Harv) dur_harv <- 365
  
  # Explicitly define the crops that span across Jan 1 and need to wrap
  wrap_crops <- c("Grapes", "Banana 2", "Horsegram", "Rice Long")
  is_wrapping <- crop_name %in% wrap_crops
  
  out_kc <- numeric(366)
  
  for (doy in 1:365) {
    # Calculate the "Effective Day" (Days since sowing)
    if (is_wrapping) {
      d_eff <- if(doy >= Sow) doy - Sow else doy + 365 - Sow
    } else {
      d_eff <- doy - Sow
    }
    
    # Mathematical IF/AND Simulation for Segmented Growth
    if (d_eff < 0 || d_eff > dur_harv) {
      out_kc[doy] <- base_val # Apply the baseline here
    } else if (d_eff <= dur_ini) {
      out_kc[doy] <- row$K_ini
    } else if (d_eff <= dur_dev) {
      frac <- (d_eff - dur_ini) / (dur_dev - dur_ini)
      out_kc[doy] <- row$K_ini + frac * (row$K_mid - row$K_ini)
    } else if (d_eff <= dur_mid) {
      out_kc[doy] <- row$K_mid
    } else if (d_eff <= dur_harv) {
      if (dur_harv == dur_mid) {
        out_kc[doy] <- row$K_mid
      } else {
        frac <- (d_eff - dur_mid) / (dur_harv - dur_mid)
        out_kc[doy] <- row$K_mid + frac * (row$K_end - row$K_mid)
      }
    }
  }
  
  # Match standard legacy leap year drag-down
  out_kc[366] <- out_kc[365]
  
  return(dplyr::tibble(crop = crop_name, doy = 1:366, kc = out_kc))
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
  
  doy_df <- data.frame(doy = 1:366) %>%
    dplyr::mutate(
      date = as.Date(doy - 1, origin = "2023-01-01"),
      Month = lubridate::month(date)
    )
  
  out_df <- doy_df %>%
    dplyr::left_join(raw_params %>% dplyr::select(Month, !!dplyr::sym(crop_name)), by = "Month") %>%
    dplyr::rename(kc = !!dplyr::sym(crop_name)) %>%
    dplyr::mutate(crop = stringr::str_to_title(crop_name)) %>%
    dplyr::select(crop, doy, kc)
  
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
  
  out_df <- data.frame(doy = 1:366) %>%
    dplyr::mutate(
      crop = crop_name,
      kc = c4*(doy^4) + c3*(doy^3) + c2*(doy^2) + c1*doy + intercept
    )
  
  return(out_df)
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 4. GRAZED PASTURE CURVES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Generate Grazed Pasture Curve (Legacy Match)
#'
#' @description Generates a grazed grassland curve that explicitly mimics the discrete 
#' monthly step, linear regrowth logic, and New Year boundaries used in historical 
#' spreadsheet baselines.
#'
#' @param crop_name Character string name tag for the grass category.
#' @param graze_doys Numeric vector of Days of Year when grazing events occur.
#' @param base_kc Numeric vector (length 366) representing the uncut biological baseline.
#' @param g_rate Numeric daily regrowth rate.
#' @param g_drop Numeric Kc value to drop to immediately after grazing (default 0.20).
#' @param m_bound Numeric maximum upper bound for regrowth (default 0.75).
#' @param sow_doy Numeric Day of Year representing the start of the season (default 122).
#' @param base_val Numeric baseline value for the off-season (default 0.01 for dry season).
#' @return A tibble containing columns 'crop', 'doy', and the computed 'kc'.
#' @export
generate_grazed_legacy <- function(crop_name, graze_doys, base_kc, g_rate, g_drop = 0.20, m_bound = 0.75, sow_doy = 122, base_val = 0.01) {
  kc_array <- numeric(366)
  
  for (i in 1:365) {
    if (i < sow_doy) {
      # Phase 1: STRICT lock to baseline before sowing
      kc_array[i] <- base_val
    } else {
      # Find grazing history
      past_grazes <- graze_doys[graze_doys <= i]
      
      if (length(past_grazes) == 0) {
        # Phase 2: Track biological base exactly before the first cut
        kc_array[i] <- base_kc[i]
      } else {
        last_graze <- max(past_grazes)
        
        # Phase 3: Static 30-day block representing manual monthly scale
        if ((i - last_graze) < 30) {
          kc_array[i] <- g_drop
        } else {
          # Phase 4: Daily linear regrowth capped at max bound
          if (kc_array[i-1] == base_val) { # Update to check against baseline
            kc_array[i] <- base_kc[i]
          } else {
            kc_array[i] <- min(m_bound, kc_array[i-1] + g_rate)
          }
        }
      }
    }
  }
  
  # Phase 5: Leap Year Drag-Down 
  kc_array[366] <- kc_array[365]
  
  return(dplyr::tibble(doy = 1:366, crop = crop_name, kc = kc_array))
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
#' @return A tibble containing columns 'doy', 'crop', and the computed timeline 'kc'.
#' @export
generate_napier <- function(crop_name, start_doy, decline_rate_1, decline_rate_2) {
  
  if (!is.numeric(c(start_doy, decline_rate_1, decline_rate_2))) {
    stop("Parameters 'start_doy', 'decline_rate_1', and 'decline_rate_2' must be purely numeric.")
  }
  
  out_df <- dplyr::tibble(doy = 1:366, crop = crop_name) %>%
    dplyr::mutate(
      kc = dplyr::case_when(
        doy < start_doy ~ 0, # Apply the baseline correction here
        start_doy == 1 & doy <= 50 ~ dplyr::case_when(
          doy <= 11 ~ 0.5,
          doy <= 45 ~ 0.9,
          TRUE ~ 0.9 - decline_rate_1 * (doy - 45)
        ),
        TRUE ~ {
          cycle_start <- dplyr::if_else(start_doy == 1, 51, start_doy)
          c_day = (doy - cycle_start) %% 50
          dplyr::case_when(
            c_day < 11 ~ 0.4,
            c_day < 45 ~ 0.95,
            TRUE ~ 0.95 - decline_rate_2 * (c_day - 44)
          )
        }
      )
    )
  
  return(out_df)
}