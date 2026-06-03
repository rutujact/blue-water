# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Title:        Crop Coefficient (Kc) Generation & Climatology Utilities
# Author:       Rutuja Chitra-Tarak
# Description:  A comprehensive suite of functions to prepare, interpolate,
#               and simulate dynamic daily Crop Coefficient (Kc) time series,
#               including specialized curves for fruit trees, grazing, and grasses.
# Dependencies: dplyr, lubridate, stringr, stats
#               (Note: Required libraries are handled and sourced externally)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 1: CORE KC INTERPOLATION ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Interpolate and Fold Crop Coefficient (Kc) Climatology
#'
#' @description Cleans, sorts, and interpolates a given set of discrete growth phase
#' parameters (day of year) into a continuous daily 366-day profile. Handles wrap-around
#' conditions where cropping growth cycles cross calendar boundaries.
#'
#' @param kc.key A data frame containing at least 'doy' (Day of Year) and 'kc' columns.
#' @param from Date object marking the beginning simulation date boundary (default: 2000-01-01).
#' @param to Date object marking the ending simulation date boundary (default: 2000-12-31).
#'
#' @return A list containing a single element 'ts', which is a data frame with columns 
#' 'doy' (1:366) and 'kc' (interpolated numeric crop coefficients).
#' @export
get_kc <- function(kc.key, from = as.Date("2000-01-01"), to = as.Date("2000-12-31")) {
  
  # Error handling boundaries
  if (!is.data.frame(kc.key)) {
    stop("Input 'kc.key' must be a valid data frame structure.")
  }
  if (!all(c("doy", "kc") %in% colnames(kc.key))) {
    stop("Input 'kc.key' must contain structural columns 'doy' and 'kc'.")
  }
  
  # 1. Clean and sort key, adding a tiny epsilon to prevent duplicate DOY ties (e.g., End of Mid == Harvest)
  kc.key$doy <- kc.key$doy + seq_len(nrow(kc.key)) * 1e-6
  kc.key <- kc.key[order(kc.key$doy), ]
  
  # 2. Base interpolation function using stats namespace
  f <- stats::approxfun(x = kc.key$doy, y = kc.key$kc, rule = 1)
  
  # 3. Check for Wrap-Around (crops crossing the new year > 366)
  is_wrap_around <- any(kc.key$doy > 366)
  
  if (is_wrap_around) {
    # Interpolate into the next calendar year
    max_doy <- ceiling(max(kc.key$doy))
    kc_seq <- f(1:max_doy)
    kc_seq[is.na(kc_seq)] <- 0
    
    kc_out <- numeric(366)
    duration <- max_doy - min(kc.key$doy)
    
    if (duration <= 366) {
      # Single-year crops that cross the winter boundary (e.g., Grapes, Banana 2)
      # Fold using a 365-day step to match Excel's calendar math
      for(i in 1:366) {
        idx <- seq(i, length(kc_seq), by = 365)
        kc_out[i] <- max(kc_seq[idx], na.rm = TRUE)
      }
    } else {
      # Multi-year crops explicit to Year 1 (e.g., Banana 1 Beg). Do not fold!
      kc_out <- kc_seq[1:366]
    }
  } else {
    kc_out <- f(1:366)
    kc_out[is.na(kc_out)] <- 0
  }
  
  return(list(ts = data.frame(doy = 1:366, kc = kc_out)))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 2: HELPER FUNCTIONS FOR COMPLEX SPECIALIZED TRACKS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Generate Fruit Tree Kc Curve (Monthly Step Function)
#'
#' @description Maps monthly discrete crop coefficient thresholds onto a continuous 
#' day-of-year (DOY) timeline using explicit calendar-month groupings.
#'
#' @param crop_name Character string matching the column name of the target crop in raw_params.
#' @param raw_params A data frame containing a 'Month' column (1-12) and columns for crop Kc values.
#'
#' @return A structured data frame with columns 'crop', 'doy', and 'kc'.
#' @export
generate_fruit_step <- function(crop_name, raw_params) {
  
  # Error handling boundaries
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

#' Generate Polynomial Curve Parameters
#'
#' @description Models smooth, continuous dynamic trajectories of crop coefficients 
#' (e.g., for Groundnuts) using a pre-calculated 4th-order polynomial equation evaluated across DOYs.
#'
#' @param crop_name Character string identifier for the crop type.
#' @param c4 Numeric coefficient for the fourth-order term (doy^4).
#' @param c3 Numeric coefficient for the third-order term (doy^3).
#' @param c2 Numeric coefficient for the second-order term (doy^2).
#' @param c1 Numeric coefficient for the first-order term (doy).
#' @param intercept Numeric intercept constant.
#'
#' @return A data frame containing evaluated 'doy', 'crop', and computed polynomial 'kc' values.
#' @export
generate_polynomial_kc <- function(crop_name, c4, c3, c2, c1, intercept) {
  
  # Error handling boundaries
  if (!is.numeric(c(c4, c3, c2, c1, intercept))) {
    stop("All polynomial curve coefficients and the baseline intercept must be numeric values.")
  }
  
  out_df <- data.frame(doy = 1:366) %>%
    dplyr::mutate(
      crop = crop_name,
      kc = c4*(doy^4) + c3*(doy^3) + c2*(doy^2) + c1*doy + intercept
    )
  
  return(out_df)
}

#' Generate Managed Grazing Rotations
#'
#' @description Simulates sudden structural drop-downs and trailing fractional recovery 
#' loops in baseline grassland crop coefficients caused by scheduled livestock rotational grazing events.
#'
#' @param crop_name Character string descriptor for the specific management regime.
#' @param grazing_starts Numeric vector specifying the calendar DOY values when 30-day grazing cuts begin.
#' @param grassland_base Numeric vector of length 366 detailing the baseline untouched grassland curve.
#' @param growth_rate Numeric scalar representing the constant daily fractional recovery velocity after grazing stops.
#'
#' @return A tibble object containing structured 'crop', 'doy', and calculated 'kc' rows.
#' @export
generate_grazed <- function(crop_name, grazing_starts, grassland_base, growth_rate) {
  
  # Error handling boundaries
  if (!is.numeric(grazing_starts)) {
    stop("Parameter 'grazing_starts' must be parsed as a numeric sequence vector.")
  }
  if (!is.numeric(grassland_base) || length(grassland_base) < 366) {
    stop("Parameter 'grassland_base' must be a numeric vector tracking at least 366 daily items.")
  }
  if (!is.numeric(growth_rate) || length(growth_rate) != 1) {
    stop("Parameter 'growth_rate' must evaluate to a single numeric scalar.")
  }
  
  kc_grazed <- grassland_base
  for (start_doy in grazing_starts) {
    end_doy <- start_doy + 30
    for (d in 1:366) {
      if (d >= start_doy & d <= end_doy) {
        kc_grazed[d] <- 0.2
      } else if (d > end_doy) {
        kc_grazed[d] <- min(0.75, 0.2 + (d - end_doy) * growth_rate)
      }
    }
  }
  
  return(dplyr::tibble(crop = crop_name, doy = 1:366, kc = kc_grazed))
}

#' Generate Napier Grass Harvesting Cycles
#'
#' @description Generates step-and-decay cyclical sequences representing continuous 
#' automated harvesting cutting schedules and subsequent multi-phase biomass vegetative regrowth.
#'
#' @param crop_name Character string name tag for the grass category.
#' @param start_doy Numeric representation of the initial calendar day harvesting begins.
#' @param decline_rate_1 Numeric slope parameter for Phase 1 biomass or senescence decay.
#' @param decline_rate_2 Numeric slope parameter for subsequent recurring harvesting cycles.
#'
#' @return A tibble containing columns 'doy', 'crop', and the computed timeline 'kc'.
#' @export
generate_napier <- function(crop_name, start_doy, decline_rate_1, decline_rate_2) {
  
  # Error handling boundaries
  if (!is.numeric(c(start_doy, decline_rate_1, decline_rate_2))) {
    stop("Parameters 'start_doy', 'decline_rate_1', and 'decline_rate_2' must be purely numeric vectors.")
  }
  
  out_df <- dplyr::tibble(doy = 1:366, crop = crop_name) %>%
    dplyr::mutate(
      kc = dplyr::case_when(
        doy < start_doy ~ 0,
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