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
# Title:       Main Function to Compute Dynamic Soil Water Budgets & Water Potentials
# Author:      Rutuja Chitra-Tarak
# Date:        2024-08-12
# Modified:    2025-12-20 to 2026-06-03
# Description: Simulates forward-running daily water balances, crop actual ET (AET),
#              groundwater recharge fluxes, and net crop irrigation requirements.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 1. Input-Output Data Structure Specifications ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# p: Parameter definition list object structured as:
#    p <- list(
#      precip        = data.frame(date=Date, p=numeric),       # Rainfall time series (mm)
#      et0           = data.frame(date=Date, et0=numeric),     # Reference ET series (mm)
#      et0.timestep  = "daily",                                # Temporal format: "daily" or "weekly"
#      soil.input    = "P",                                    # "P" (raw precipitation) or "Peff" (effective rainfall)
#      sm.init       = 0.5,                                    # Initial fractional soil moisture [0-1]
#      soil.param    = data.frame(soil, soil.type, whc.mmpercm, raw, CN, lambda),
#      crop.param    = data.frame(crop.ID, crop, irrigation, rooting_depth_cm),
#      kc_no_crop    = list(dry_season = 0.005, kharip = 0.2) # Fallow baseline ET coefficients
#    )
#
# cs: Crop system rotation structural data frame mapping:
#     doy, kc, crop, ncrop, irrigation, rooting_depth_cm, crop.system, rotation, soil
#
# ts: Forcing list object generated via get_peff_et0()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 3. Primary Estimation Model: get_budget ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Primary Soil Water Budget Estimation Model
#'
#' @description Simulates forward-running daily water balances, crop actual evapotranspiration (AET),
#' groundwater recharge fluxes, and net crop irrigation requirements for a specific crop system rotation and soil.
#'
#' @param cs A data frame of crop system rotation data containing columns: `doy`, `kc`, `crop`, `ncrop`, `irrigation`, `rooting_depth_cm`, `crop.system`, `rotation`, `soil`.
#' @param p A list containing parameter definitions (e.g., `soil.param`, `crop.param`, `kc_no_crop`, `soil.input`, `sm.init`).
#' @param ts A named list of data frames containing daily environmental forcing time series for each soil type.
#'
#' @return A list containing five processed water balance tracking matrix elements:
#' \item{ts}{Detailed continuous daily water budget simulation time series.}
#' \item{sum.yr}{Annual sum and mean summary data frame.}
#' \item{summary.yr}{Long-term annual summary statistics and quantiles.}
#' \item{sum.mo}{Monthly aggregated water budget summary.}
#' \item{clim.mo}{Long-term monthly climatology summary with quantiles.}
#' @export
get_budget <- function(cs, p, ts) {
  
  # Structural Input Boundary Validation Checks
  if (!base::is.data.frame(cs)) base::stop("Argument 'cs' must be a valid structured data frame.")
  if (!base::is.list(p)) base::stop("Argument 'p' must be a valid parameters list object.")
  if (!base::is.list(ts)) base::stop("Argument 'ts' must be a valid environmental forcing list object.")
  
  required_cs_cols <- c("soil", "doy", "kc", "crop", "ncrop", "irrigation", "rooting_depth_cm", "crop.system", "rotation")
  if (!base::all(required_cs_cols %in% base::colnames(cs))) {
    base::stop(base::paste("The crop system table 'cs' lacks required parameters:", 
                           base::paste(base::setdiff(required_cs_cols, base::colnames(cs)), collapse = ", ")))
  }
  
  # 3.1. Isolate the target meteorological forcing matched to current soil type ----
  soil <- cs$soil[1]
  if (!soil %in% base::names(ts)) base::stop(base::paste("Specified soil identifier", soil, "is absent from environmental list 'ts'."))
  ts <- ts[[soil]]
  
  # 3.2. Flag active crop phases dependent on irrigation triggers ----
  cs.ncrop <- cs[, c("doy", "kc", "crop", "ncrop", "irrigation")]
  cs.ncrop <- cs.ncrop %>% 
    dplyr::mutate(kc.irri = dplyr::if_else(irrigation == "irrigated" & kc > 0, TRUE, FALSE))
  
  # 3.3. Sum active daily crop evapotranspiration coefficients (kcr) ----
  cs.kcr.irri <- cs.ncrop %>%
    dplyr::group_by(doy) %>%         
    dplyr::summarise(irri.needed = base::any(kc.irri),
                     kcr = base::sum(kc), .groups = "drop") %>%
    dplyr::ungroup()
  
  # 3.4. Fill fallow periods with baseline land evaporative loss coefficients ----
  dry_season.kc <- p$kc_no_crop[["dry_season"]]
  kharip.kc <- p$kc_no_crop[["kharip"]]
  
  cs.kcr.irri$sum.kc <- cs.kcr.irri$kcr
  cs.kcr.irri$kcr[cs.kcr.irri$kcr == 0 & cs.kcr.irri$doy < 120] <- dry_season.kc
  cs.kcr.irri$kcr[cs.kcr.irri$kcr == 0 & cs.kcr.irri$doy >= 120] <- kharip.kc
  
  # 3.5. Join temporal coefficients onto continuous simulation timeline via DOY ----
  ts$doy <- base::as.numeric(base::format(base::strptime(ts$date, format = "%Y-%m-%d"), format = "%j"))
  ts <- dplyr::left_join(ts, cs.kcr.irri, by = "doy", relationship = "many-to-many")
  
  # 3.6. Calculate daily Potential Crop Evapotranspiration (ETc in mm) ----
  ts$etc <- ts$et0 * ts$kcr 
  
  # 3.7. Extract specific soil physical limits to calculate Water Holding Capacity (WHC) ----
  soil.param <- p$soil.param
  psoil <- base::split(soil.param, soil.param$soil)
  if (!soil %in% base::names(psoil)) base::stop(base::paste("Soil key", soil, "is absent from parameters list context 'p$soil.param'."))
  
  max.rooting.depth <- base::max(cs$rooting_depth_cm, na.rm = TRUE)
  whc <- psoil[[soil]]$whc.mmpercm * max.rooting.depth
  raw <- psoil[[soil]]$raw 
  
  soil.type <- soil.param$soil.type[soil.param$soil == soil]
  
  # 3.8. Configure active incoming water boundaries (Raw vs effective precipitation) ----
  if (p$soil.input == "P") {
    ts$Peff.used <- ts$P
    ts$Q <- 0
  } else {
    ts$Peff.used <- ts$Peff
  }
  
  # 3.9. Initialize sequential tracking matrices for simulation loops ----
  ts$sm.hold <- NA
  ts$recharge <- NA
  ts$sm.mid <- NA
  ts$ks <- NA
  ts$aet <- NA
  ts$sm.end <- NA
  ts$irrigation <- NA
  
  # 3.10. Execute Daily Forward Dynamic Water Budget Simulation ---- 
  for(i in 1:base::nrow(ts)) {
    
    # 3.10.1 Compute temporary soil water content after current day's precipitation ----
    if (i == 1) {
      ts$sm.hold[i] <- whc * p$sm.init
    } else {
      ts$sm.hold[i] <- ts$sm.end[i-1] + ts$Peff.used[i]
    }
    
    # 3.10.2 Route any accumulated moisture above profile capacity threshold (WHC) to Recharge ----
    if(ts$sm.hold[i] > whc) {
      ts$recharge[i] <- ts$sm.hold[i] - whc
      ts$sm.mid[i] <- whc
    } else {
      ts$recharge[i] <- 0
      ts$sm.mid[i] <- ts$sm.hold[i]
    }
    
    # 3.10.3 Compute moisture reduction stress factor (ks) based on RAW limit boundaries ----
    if(ts$sm.mid[i] >= (whc * raw)) { 
      ts$ks[i] <- 1
    } else {
      ts$ks[i] <- ts$sm.mid[i] / (whc * raw)
    }
    
    if (ts$sm.mid[i] == 0) {
      ts$ks[i] <- 0
    }
    
    # 3.10.4 Quantify Actual Evapotranspiration (AET) ----
    ts$aet[i] <- ts$etc[i] * ts$ks[i]
    
    # 3.10.5 Apply ET depletion and evaluate dynamic structural irrigation triggers ----
    if(ts$irri.needed[i]) {
      irri.need.test <- (ts$sm.mid[i] - ts$aet[i]) < (whc * raw)
      
      if((!base::is.na(irri.need.test) || !base::is.null(irri.need.test)) & irri.need.test) {
        ts$sm.end[i] <- whc 
      } else {
        ts$sm.end[i] <- base::max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
      }
    } else {
      ts$sm.end[i] <- base::max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
    }
  }
  
  # 3.11. Post-Simulation Aggregations and Diagnostics ----
  ts$irrigation <- ts$sm.end - (ts$sm.mid - ts$aet)
  ts$sm.percent <- (ts$sm.end / whc) * 100
  
  # 3.11.1 Segment specific potential and actual ET metrics for active vegetative seasons ----
  ts$etc.crop <- ts$etc
  ts$aet.crop <- ts$aet
  ts$aet.to.etc.crop <- NA
  
  rows.crop.kc.zero <- base::which(ts$sum.kc == 0)
  if (base::length(rows.crop.kc.zero) != 0) {
    ts$etc.crop[rows.crop.kc.zero] <- 0
    ts$aet.crop[rows.crop.kc.zero] <- 0
  }
  
  valid.rows <- ts$etc.crop != 0 & !base::is.na(ts$etc.crop)
  ts$aet.to.etc.crop[valid.rows] <- ts$aet.crop[valid.rows] / ts$etc.crop[valid.rows]
  
  # 3.11.2 Calculate supplementary agricultural balance indexes ----
  ts$aet.to.Peff <- ts$aet / ts$Peff
  ts$aet.to.Peff[ts$Peff == 0] <- NA
  
  ts$runoff.percent <- (ts$Q / ts$P) * 100
  ts$runoff.percent[ts$P == 0] <- NA
  
  ts$balance <- ts$recharge - ts$irrigation 
  ts$year <- base::as.numeric(base::format(ts$date, format = "%Y"))
  ts$month <- base::as.numeric(base::format(ts$date, format = "%m"))
  
  ts <- ts %>% 
    dplyr::select(date, year, month, doy, P, Q, Peff, Peff.used,
                  et0, etc, aet, etc.crop, aet.crop, 
                  recharge, irrigation, balance, runoff.percent, aet.to.Peff, dplyr::everything())
  
  # 3.12. Compile Aggregated Data Overviews ----
  # 3.12.1 Annual/Yearly Summary ----
  sum.yr <- ts %>% 
    dplyr::group_by(year) %>%
    dplyr::summarise(dplyr::across(P:balance, \(x) base::sum(x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::mutate(aet.to.Peff = aet / Peff, runoff.percent = Q / P * 100)
  
  mean.yr <- ts %>% 
    dplyr::group_by(year) %>%
    dplyr::summarise(dplyr::across(c(sm.percent, aet.to.etc.crop), \(x) base::mean(x, na.rm = TRUE)), .groups = "drop")
  sum.yr <- sum.yr %>% dplyr::left_join(mean.yr, by = "year")
  
  # 3.12.2 Long-Term Annual Summary Climatology ----
  quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
    tibble::tibble(quantile = probs, value = stats::quantile(x, probs, na.rm = TRUE))
  }
  
  summary.yr <- sum.yr %>%
    tidyr::pivot_longer(-year, names_to = "var", values_to = "value") %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(mean = base::mean(value, na.rm = TRUE), 
                     sd = stats::sd(value, na.rm = TRUE), .groups = "drop") %>% 
    dplyr::arrange(var)
  
  # 3.12.3 Monthly Summary ----
  sum.mo <- ts %>% 
    dplyr::group_by(year, month) %>%
    dplyr::summarise(dplyr::across(P:balance, \(x) base::sum(x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::ungroup() %>%
    dplyr::mutate(aet.to.Peff = aet / Peff, runoff.percent = Q / P * 100)
  
  # 3.12.4 Long-Term Monthly Climatology Overviews ----
  mean.mo <- ts %>% 
    dplyr::group_by(year, month) %>%
    dplyr::summarise(dplyr::across(c(sm.percent, aet.to.etc.crop), \(x) base::mean(x, na.rm = TRUE)), .groups = "drop") %>%
    dplyr::ungroup()
  
  clim.mo <- sum.mo %>%
    dplyr::select(-year) %>%
    tidyr::pivot_longer(-month, names_to = "var", values_to = "value") %>%
    dplyr::group_by(var, month) %>%
    dplyr::reframe(quantile_df(value)) %>%
    base::as.data.frame() %>%
    tidyr::pivot_wider(names_from = "quantile", values_from = "value")
 
  return(base::list(ts = ts, sum.yr = sum.yr, summary.yr = summary.yr, 
                    sum.mo = sum.mo, clim.mo = clim.mo))
}