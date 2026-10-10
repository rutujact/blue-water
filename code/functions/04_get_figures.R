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
# Title:        Water Budgeting & Visualization Utility Functions
# Author:       Rutuja Chitra-Tarak
# Description:  A comprehensive suite of functions to process, aggregate, and 
#               visualize Blue Water Potential (Recharge - Irrigation) across 
#               crop systems, farm types, and landscape scales.
# Dependencies: ggplot2, dplyr, tidyr, forcats, stringr, scales, patchwork, networkD3, htmlwidgets
#               (Note: Required libraries are handled and sourced externally)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# GLOBAL COLOR PALETTE CONFIGURATION (OKABE-ITO, SURFACE-FIRST MENTAL MODEL) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BWP_PALETTES <- list(
  # 1. Climate Profile (Supply vs. Demand)
  climate_fill    = c("Rainfall" = "#56B4E9", "ET0" = "#D55E00"),         
  climate_color   = c("Rainfall" = "#0072B2", "ET0" = "#D55E00",          
                      "Temperature" = "#CC79A7"),                    
  climate_ribbon  = "#CC79A7",                                            
  
  # 2. Irrigation Types (Annual Summary)
  irrigation      = c("Rainfed" = "#E69F00", "Irrigated" = "#56B4E9"),    
  
  # 3. Monthly Blue Water Potential (BWP) Envelopes
  bwp_fill        = c("Below axis" = "#F0E442", "Above axis" = "#56B4E9"), 
  bwp_color       = c("TRUE" = "#0072B2", "FALSE" = "#D55E00"),           
  
  # 4. Socio-Economic Land Use Contributions (Fig 5 & 8)
  land_use        = c("irrigated cropped area" = "#56B4E9",               
                      "rainfed cropped area"   = "#E69F00",               
                      "grazed area"            = "#009E73"),              
  
  # 5. Farm Disparity Dashboard (Fig 6)
  disparity_count = "#999999",                                            
  disparity_bwp   = c("+BWP (Surplus)" = "#0072B2", "-BWP (Deficit)" = "#CC79A7"), 
  
  # 7. Village Flow Charts / Water Tank (Fig 7)
  flow_box_line   = "#0072B2",                              
  flow_box_fill   = "#0072B2",                              
  flow_box_diff   = "#E5F5FA",                              
  flow_drop_text  = "#CC79A7",                              
  flow_in_p1      = c("#0072B2", "#56B4E9"),                
  flow_in_p2      = c("#0072B2", "#56B4E9"),                
  flow_out_p2     = c("#CC79A7", "#EBB9D3"),                
  flow_out_single = c("#CC79A7", "#EBB9D3"),
  
  # 8. Interactive Sankey Flow Charts (Fig 8)
  sankey_domain   = '["Rain", "Irrigated", "Rainfed", "Grazed", "Unused", "BWP", "Flow_Rain"]',
  sankey_range    = '["#56B4E9", "#56B4E9", "#E69F00", "#009E73", "#CC79A7", "#0072B2", "#E8E8E8"]'
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 1: GLOBAL GRAPHICS THEMES ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Standardized Base Theme for ggplot2 Visualizations
#' 
#' @description A minimalistic white-background theme without gridlines, 
#' providing a clean canvas for primary visualizations.
#' @return A `ggplot2` theme object.
#' @export
theme_baseR <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_rect(fill = "white", colour = "black"), 
    strip.background = ggplot2::element_blank()  
  )

#' Graphics Theme for Journal Manuscripts
#' 
#' @description A refined theme designed for high-resolution print manuscripts, 
#' featuring distinct axes, bold labeling, and standard horizontal gridlines.
#' @param base_size Numeric. Base font size for text elements (default is 11).
#' @return A `ggplot2` theme object.
#' @export
theme_manuscript <- function(base_size = 11) {
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.2, color = "#e0e0e0"),
      plot.title    = ggplot2::element_blank(), 
      axis.title    = ggplot2::element_text(face = "bold", size = 12, color = "black"),
      axis.text     = ggplot2::element_text(size = 9, color = "black"),
      legend.position = "top",
      legend.box      = "horizontal",
      legend.title    = ggplot2::element_blank(),
      legend.text     = ggplot2::element_text(size = 10, color = "black"),
      legend.margin   = ggplot2::margin(b = -5),
      strip.background = ggplot2::element_rect(fill = "#f8f9fa", color = "black"),
      strip.text       = ggplot2::element_text(face = "bold", size = 9, color = "black")
    )
}

#' Graphics Theme for Digital Presentations
#' 
#' @description Enlarged, high-contrast theme optimized for screen projection 
#' and digital presentations.
#' @param base_size Numeric. Base font size for text elements (default is 14).
#' @return A `ggplot2` theme object.
#' @export
theme_presentation <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(linewidth = 0.3, color = "#eaeaea"),
      plot.title    = ggplot2::element_text(face = "bold", size = 18, hjust = 0.5, margin = ggplot2::margin(b = 12)),
      axis.title    = ggplot2::element_text(face = "bold", size = 14, color = "black"),
      axis.text     = ggplot2::element_text(size = 12, color = "black"),
      legend.position  = "top",
      legend.box       = "horizontal",
      legend.title     = ggplot2::element_blank(),
      legend.text      = ggplot2::element_text(size = 14, face = "bold", color = "black"),
      legend.spacing.x = ggplot2::unit(0.4, "cm"),
      strip.text       = ggplot2::element_text(face = "bold", size = 12, color = "black")
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 2: BASELINE AGRO-CLIMATOLOGY & CROP PARAMETERS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot Historical Climate Profile
#' 
#' @description Generates a dual-axis climate profile comparing monthly supply (Rainfall) 
#' and demand (ET0) alongside temperature trends.
#' @param climate_summary Dataframe containing monthly climate variables (Rainfall, ET0, Temperature).
#' @param scale_factor Numeric. Multiplier to scale temperature to fit on the secondary Y-axis (default is 7.5).
#' @param show_temperature Logical. Whether to overlay the temperature curve (default is TRUE).
#' @return A `ggplot2` object displaying the climate profile.
#' @export
plot_climate_profile <- function(climate_summary, scale_factor = 7.5, show_temperature = TRUE) {
  
  # _________________________________________________________________________
  # STEP 1: Format Month Labels
  # _________________________________________________________________________
  # Convert numeric months (1-12) to ordered factor abbreviations (Jan-Dec)
  climate_summary <- climate_summary %>%
    dplyr::mutate(Month_Name = forcats::fct_relevel(factor(month.abb[Month], levels = month.abb)))
  
  # _________________________________________________________________________
  # STEP 2: Pivot Volumes (Bars)
  # _________________________________________________________________________
  # Isolate the main volume metrics (Rainfall and ET0) and pivot them into a 'long' format
  # so ggplot can easily map them side-by-side in grouped bar charts.
  climate_summary_long <- climate_summary %>%
    dplyr::select(Month_Name, Rainfall_avg, ET0_avg) %>%
    tidyr::pivot_longer(cols = c(Rainfall_avg, ET0_avg), names_to = "Water_Metric", values_to = "Volume") %>%
    dplyr::mutate(Water_Metric = dplyr::recode(Water_Metric, "Rainfall_avg" = "Rainfall", "ET0_avg" = "ET0"))
  
  # _________________________________________________________________________
  # STEP 3: Pivot Error Bounds (Whiskers)
  # _________________________________________________________________________
  # Isolate the standard deviations (SD) and pivot them to match the volumetric data.
  water_errors <- climate_summary %>%
    dplyr::select(Month_Name, Rainfall_sd, ET0_sd) %>%
    tidyr::pivot_longer(
      cols = -Month_Name, 
      names_to = "Water_Metric", 
      names_pattern = "(Rainfall|ET0)_sd",
      values_to = "SD"
    )
  
  # Merge volumes and errors back together into one master dataframe,
  # and calculate the error bounds (preventing ymin from falling below 0).
  climate_summary_long <- dplyr::left_join(climate_summary_long, water_errors, by = c("Month_Name", "Water_Metric")) %>%
    dplyr::mutate(
      ymin = pmax(0, Volume - SD),
      ymax = Volume + SD
    )
  
  # _________________________________________________________________________
  # STEP 4: Render Dual-Axis Plot
  # _________________________________________________________________________
  p <- ggplot2::ggplot() +
    # Layer 1: Water Volumes as bars
    ggplot2::geom_bar(data = climate_summary_long, ggplot2::aes(x = Month_Name, y = Volume, fill = Water_Metric), stat = "identity", position = ggplot2::position_dodge(width = 0.9), alpha = 0.85) +
    # Layer 2: Standard Deviations as error bars (bounded at 0)
    ggplot2::geom_errorbar(data = climate_summary_long, ggplot2::aes(x = Month_Name, ymin = ymin, ymax = ymax, group = Water_Metric), position = ggplot2::position_dodge(width = 0.9), width = 0.2, alpha = 0.7)
  
  if (show_temperature) {
    p <- p +
      # Layer 3: Temperature Envelopes (Scaled). 
      # Temperature ranges 20-40C, but Rainfall ranges 0-300mm. We multiply temperature by the 
      # `scale_factor` (e.g., * 7.5) so the line floats visibly in the middle of the chart.
      ggplot2::geom_ribbon(data = climate_summary, ggplot2::aes(x = Month_Name, ymin = pmax(0, Temperature_avg - Temperature_sd) * scale_factor, 
                                                                ymax = (Temperature_avg + Temperature_sd) * scale_factor, group = 1), fill = BWP_PALETTES$climate_ribbon, alpha = 0.15) +
      ggplot2::geom_line(data = climate_summary, ggplot2::aes(x = Month_Name, y = Temperature_avg * scale_factor, color = "Temperature", group = 1), linewidth = 1) +
      # Layer 4: Create the illusion of a secondary axis by mathematically dividing the right-side labels back down by the scale_factor.
      ggplot2::scale_y_continuous(name = expression(bold(paste("Rainfall & ET"[0], " (mm)"))), sec.axis = ggplot2::sec_axis(~. / scale_factor, name = "Temperature (°C)"), expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1), color = ggplot2::guide_legend(order = 2, nrow = 1, breaks = "Temperature", labels = "Mean Temp", override.aes = list(linewidth = 2, shape = NA)))
  } else {
    p <- p +
      ggplot2::scale_y_continuous(name = expression(bold(paste("Rainfall & ET"[0], " (mm)"))), expand = ggplot2::expansion(mult = c(0, 0.05))) +
      ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1), color = "none")
  }
  
  p <- p +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$climate_fill) +
    ggplot2::scale_color_manual(values = BWP_PALETTES$climate_color) +
    ggplot2::labs(x = "Month") +
    theme_manuscript() +
    ggplot2::theme(legend.position = "top", legend.box = "vertical", legend.spacing.y = ggplot2::unit(0.1, "cm"), axis.text = ggplot2::element_text(size = 12, color = "black"))
  
  return(p)
}

#' Plot Crop Coefficient (Kc) Trajectories
#' 
#' @description Visualizes the physiological crop coefficient (Kc) curves across DOY.
#' Implements contiguous growth phase detection to handle gaps and fallow periods smoothly.
#' @param kc_doy_crop_all Dataframe of crop coefficients mapped by day of year (doy).
#' @param KC_FALLOW List defining fallow evaporation constants.
#' @return A `ggplot2` facet-wrapped visualization of Kc curves.
#' @export
plot_crop_coefficient_curves <- function(
    kc_doy_crop_all, 
    KC_FALLOW = list(dry_season = 0.005, kharip = 0.2, doy_switch = 120)
) {
  
  # _________________________________________________________________________
  # STEP 1: Gap Detection & Data Prep
  # _________________________________________________________________________
  kc_doy_crop_all <- kc_doy_crop_all %>%
    # Harmonize all grassland grazing variations to a single "Grass" crop name
    # so they all share the exact same color in the ggplot legend/fill mapping.
    dplyr::mutate(crop_col = dplyr::case_when(
      grepl("Grazed|Grasslands", crop) ~ "Grass",
      grepl("Banana", crop) ~ "Banana",
      grepl("Napier", crop) ~ "Napier",
      grepl("Groundnut", crop) ~ "Groundnut",
      grepl("Okra", crop) ~ "Okra",
      grepl("Tomato", crop) ~ "Tomato",
      grepl("Rice", crop) ~ "Rice",
      TRUE ~ crop
    ))
  
  # _________________________________________________________________________
  # STEP 2a: FALLOW DATA (Baseline Soil Evaporation)
  # _________________________________________________________________________
  # Isolate days where Kc drops to 0 to apply standard fallow shading
  fallow_data <- kc_doy_crop_all %>%
    dplyr::filter(kc == 0 | crop == "No Crop") %>%
    dplyr::mutate(
      kc = dplyr::if_else(doy < KC_FALLOW$doy_switch, KC_FALLOW$dry_season, KC_FALLOW$kharip)
    ) %>%
    dplyr::arrange(crop, doy) %>%
    dplyr::group_by(crop) %>%
    dplyr::mutate(
      is_gap = doy - dplyr::lag(doy, default = dplyr::first(doy)) > 1,
      plot_group = paste(crop, "fallow", cumsum(is_gap), sep = "_")
    ) %>%
    dplyr::ungroup()
  
  # _________________________________________________________________________
  # STEP 2b: CROP DATA (Isolated Growth Phases)
  # _________________________________________________________________________
  # Define contiguous growth phases to physically break lines and ribbons across fallow periods
  crop_data <- kc_doy_crop_all %>%
    dplyr::filter(crop != "No Crop" & kc > 0) %>%
    dplyr::arrange(crop, doy) %>%
    dplyr::group_by(crop) %>%
    dplyr::mutate(
      is_gap = doy - dplyr::lag(doy, default = dplyr::first(doy)) > 1,
      growth_phase = cumsum(is_gap)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(plot_group = paste(crop, "crop", growth_phase, sep = "_"))
  
  # _________________________________________________________________________
  # STEP 3: Rendering
  # _________________________________________________________________________
  ggplot2::ggplot() +
    
    # --- LAYER 1: FALLOW PERIODS ---
    ggplot2::geom_ribbon(data = fallow_data, 
                         ggplot2::aes(x = doy, ymin = 0, ymax = kc, group = plot_group), 
                         fill = "#e6e1d8", alpha = 0.5, color = NA) +
    ggplot2::geom_line(data = fallow_data, 
                       ggplot2::aes(x = doy, y = kc, group = plot_group), 
                       color = "#c4b7a6", linewidth = 0.5) +
    
    # --- LAYER 2: CROP PERIODS ---
    ggplot2::geom_ribbon(data = crop_data, 
                         ggplot2::aes(x = doy, ymin = 0, ymax = kc, fill = crop_col, group = plot_group), 
                         alpha = 0.15, show.legend = FALSE, color = NA) +
    ggplot2::geom_line(data = crop_data, 
                       ggplot2::aes(x = doy, y = kc, color = crop_col, group = plot_group), 
                       linewidth = 1, show.legend = FALSE) +
    
    ggplot2::facet_wrap(~ crop) +
    
    theme_baseR +
    
    ggplot2::labs(
      title = "Generated Crop Coefficient (Kc) Curves - All Types",
      x = "Day of Year (DOY)",
      y = "Crop Coefficient (Kc)"
    ) +
    
    ggplot2::scale_y_continuous(limits = c(0, 1.25)) +
    
    ggplot2::theme(legend.position = "none")
}

#' Plot Crop System Coefficient (Kc) Trajectories
#' 
#' @description Maps the annual and multi-annual cropping systems, incorporating
#' structural fallow soil evaporation constraints during non-crop periods.
#' @param crop_systems_l_kc Dataframe mapping sequential crops to annual rotations.
#' @param KC_FALLOW List defining fallow evaporation constants (default: dry_season = 0.005, kharip = 0.2, doy_switch = 120).
#' @return A comprehensive `ggplot2` grid tracking the system's physiological status across the rotation timeline.
#' @export
plot_crop_system_coefficient_curves <- function(
    crop_systems_l_kc, 
    KC_FALLOW = NULL
) {
  
  # Safety check: if the user forgets to pass the parameters from the CSV, 
  # provide a safe fallback so the plot doesn't crash.
  if (is.null(KC_FALLOW)) {
    warning("KC_FALLOW not provided. Using generic fallback values.")
    KC_FALLOW <- list(dry_season = 0.01, kharip = 0.15, doy_switch = 120)
  }
  
  # _________________________________________________________________________
  # STEP 0: Disambiguate Overlapping Systems & Format Variables
  # _________________________________________________________________________
  crop_systems_l_kc <- crop_systems_l_kc %>%
    dplyr::mutate(
      # Transform numeric rotations (1, 2, 3) into readable factors ("Year 1", etc.)
      n.rotation = factor(paste("Year", n.rotation), 
                          levels = paste("Year", sort(unique(n.rotation)))),
      
      # Dynamically append the month to the 4 identical "Grasslands (Grazed Once)" systems
      # so ggplot puts them on 4 distinct rows instead of overlapping them.
      facet_row = dplyr::if_else(
        as.character(crop.system) == "Grasslands (Grazed Once)",
        paste0("Grasslands (Grazed ", sub("Grazed ", "", rotation), ")"), 
        as.character(crop.system)
      ),
      
      # Harmonize all grassland grazing variations to a single "Grass" crop name
      # so they all share the exact same color in the ggplot legend/fill mapping.
      # and name
      # we should have had a column for the crop name devoid of when/how long it's planted ets
      crop = dplyr::if_else(
        grepl("Grasslands", crop.system) & grepl("Grazed|Grasslands", crop), "Grass",
        dplyr::if_else(grepl("Banana", crop), "Banana",
                       dplyr::if_else(grepl("Napier", crop), "Napier",
                                      dplyr::if_else(grepl("Groundnut", crop), "Groundnut",
                                                     dplyr::if_else(grepl("Okra", crop), "Okra",
                                                                    dplyr::if_else(grepl("Tomato", crop), "Tomato",
                                                                                   dplyr::if_else(grepl("Rice", crop), "Rice",
                                                                                                  crop
                                                                                   ))))))),
      # and so that Grasslands they also share the same name
      rotation = dplyr::if_else(
        grepl("Grasslands", crop.system) & grepl("Grazed|Grasslands", rotation),
        "Grass",
        rotation
      ),
      
      # Create a hidden sorting metric so the Grazing months cascade chronologically 
      # (Aug -> Sept -> Oct -> Nov) rather than alphabetically.
      month_sort = dplyr::case_when(
        rotation == "Grazed 1 Aug" ~ 1,
        rotation == "Grazed 1 Sept" ~ 2,
        rotation == "Grazed 1 Oct" ~ 3,
        rotation == "Grazed 1 Nov" ~ 4,
        TRUE ~ 0
      )
    )
  
  # Extract and lock in the exact top-to-bottom row ordering for the facet grid
  row_levels <- crop_systems_l_kc %>%
    dplyr::select(crop.system, facet_row, month_sort) %>%
    dplyr::distinct() %>%
    dplyr::arrange(crop.system, month_sort, facet_row) %>%
    dplyr::pull(facet_row)
  
  crop_systems_l_kc <- crop_systems_l_kc %>%
    dplyr::mutate(facet_row = factor(facet_row, levels = row_levels))  
  
  # _________________________________________________________________________
  # STEP 1: Smart Panel Labels
  # _________________________________________________________________________
  # Creates a small dataframe specifically for the italicized text in the top left 
  # of each panel. It leaves the label blank if it's redundant to the row header.
  label_df <- crop_systems_l_kc %>%
    dplyr::filter(crop != "No Crop") %>%
    dplyr::group_by(facet_row, n.rotation) %>%
    dplyr::summarise(rotation_name = dplyr::first(rotation), .groups = "drop") %>%
    dplyr::mutate(
      display_name = dplyr::if_else(
        as.character(facet_row) == as.character(rotation_name),
        "", 
        as.character(rotation_name)
      )
    )
  
  # _________________________________________________________________________
  # STEP 2a: FALLOW DATA (System-Level Baseline)
  # _________________________________________________________________________
  # Fallow periods shouldn't care about overlapping placeholder crops. We collapse 
  # everything down to a single maximum Kc per day. If the max Kc is 0, the field is empty.
  fallow_data <- crop_systems_l_kc %>%
    dplyr::group_by(facet_row, n.rotation, doy) %>%
    dplyr::summarise(system_kc = max(kc, na.rm = TRUE), .groups = "drop") %>%
    # Filter down ONLY to days where nothing is growing
    dplyr::filter(system_kc == 0) %>%
    dplyr::mutate(
      # Apply the structural soil evaporation constants based on the Day of Year
      kc = dplyr::if_else(doy < KC_FALLOW$doy_switch, KC_FALLOW$dry_season, KC_FALLOW$kharip)
    ) %>%
    dplyr::arrange(facet_row, n.rotation, doy) %>%
    dplyr::group_by(facet_row, n.rotation) %>%
    dplyr::mutate(
      # Trigger a new polygon (growth_phase) whenever there is a jump in days > 1
      is_gap = doy - dplyr::lag(doy, default = dplyr::first(doy)) > 1,
      # Create an absolute unique ID for every single contiguous block of fallow
      plot_group = paste(facet_row, n.rotation, "fallow", cumsum(is_gap), sep = "_")
    ) %>%
    dplyr::ungroup()
  
  # _________________________________________________________________________
  # STEP 2b: CROP DATA (Isolated Growth Phases)
  # _________________________________________________________________________
  # Here we extract only the actively growing crops. Crucially, we group by `ncrop`
  # to prevent ggplot from accidentally connecting parallel crops (interleaving).
  crop_data <- crop_systems_l_kc %>%
    dplyr::filter(crop != "No Crop" & kc > 0) %>%
    dplyr::arrange(facet_row, n.rotation, ncrop, doy) %>%
    dplyr::group_by(facet_row, n.rotation, ncrop) %>%
    dplyr::mutate(
      # A new phase is triggered if there's a day gap OR if the crop name changes
      is_gap = (doy - dplyr::lag(doy, default = dplyr::first(doy)) > 1) |
        (crop != dplyr::lag(crop, default = dplyr::first(crop))),
      # Create an absolute unique ID for every contiguous block of crop growth
      plot_group = paste(facet_row, n.rotation, ncrop, crop, cumsum(is_gap), sep = "_")
    ) %>%
    dplyr::ungroup()
  
  # _________________________________________________________________________
  # STEP 3: Rendering the Multi-Layer Plot
  # _________________________________________________________________________
  ggplot2::ggplot() +
    
    # LAYER 1: FALLOW PERIODS
    # Uses a subtle, hardcoded earthy tan/gray to fade into the background.
    # color = NA on the ribbon prevents an outline along the y=0 bottom edge.
    ggplot2::geom_ribbon(data = fallow_data, 
                         ggplot2::aes(x = doy, ymin = 0, ymax = kc, group = plot_group), 
                         fill = "#e6e1d8", alpha = 0.5, color = NA) +
    ggplot2::geom_line(data = fallow_data, 
                       ggplot2::aes(x = doy, y = kc, group = plot_group), 
                       color = "#c4b7a6", linewidth = 0.5) +
    
    # LAYER 2: CROP PERIODS
    # Plotted on top of the fallow layer using dynamic categorical colors mapped to 'crop'
    ggplot2::geom_ribbon(data = crop_data, 
                         ggplot2::aes(x = doy, ymin = 0, ymax = kc, fill = crop, group = plot_group), 
                         alpha = 0.15, show.legend = FALSE, color = NA) +
    ggplot2::geom_line(data = crop_data, 
                       ggplot2::aes(x = doy, y = kc, color = crop, group = plot_group), 
                       linewidth = 0.8, show.legend = FALSE) +
    
    # LABELS & THEME
    # Apply the smart labels derived in Step 1
    ggplot2::geom_text(data = label_df, 
                       ggplot2::aes(x = 15, y = 1.45, label = display_name), 
                       size = 2.7, fontface = "italic", color = "#444444", 
                       hjust = 0, vjust = 1) +
    
    # Build the grid using our custom sorted rows and formatted Year columns
    ggplot2::facet_grid(facet_row ~ n.rotation, switch = "y") +
    
    ggplot2::xlab("Day of Year") + 
    theme_manuscript() +
    
    ggplot2::scale_y_continuous(name = expression(bold(paste("Crop Coefficient (K"[c], ")"))), 
                                limits = c(0, 1.55), breaks = c(0, 0.5, 1.0)) +
    
    ggplot2::theme(
      strip.background.y = ggplot2::element_blank(),
      strip.background.x = ggplot2::element_rect(fill = "#f8f9fa", color = "#cccccc", linewidth = 0.4),
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 8, hjust = 1),
      strip.text.x.top = ggplot2::element_text(face = "bold", size = 9, color = "black"),
      panel.spacing = ggplot2::unit(0.2, "lines"),
      panel.border = ggplot2::element_rect(color = "#e0e0e0", fill = NA),
      strip.placement = "outside"
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 3: CROPPING SYSTEM LEVEL (TEMPORAL SUMMARIES) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Process Annual Simulation Water Budgets
#' 
#' @description Extracts and compiles annual summary lists from the simulation engine, 
#' computing mean and standard deviation matrices for subsequent plotting.
#' @param budget_ls List object containing parsed model simulation results.
#' @param crop_systems_df Dataframe associating crop rotation metadata to each model output.
#' @return A list containing `summary_yr_crop` (full dataset) and `summ_qs_cs` (aggregated means).
#' @export
process_annual_budget <- function(budget_ls, crop_systems_df) {
  
  # _________________________________________________________________________
  # STEP 1: Extract and Compile
  # _________________________________________________________________________
  # Extract the $summary.yr dataframe from every list item generated by the engine.
  summary_yr_ls <- lapply(budget_ls, function(x) x$summary.yr)
  
  # Combine them into a single long dataframe and join the crop metadata.
  summary_yr_crop <- dplyr::bind_rows(summary_yr_ls, .id = "rotation") %>%
    dplyr::left_join(crop_systems_df, by = "rotation") %>%
    # Rename internal engine codes (like 'aet') to readable plot labels.
    dplyr::mutate(var.plot = forcats::fct_recode(factor(var), 
                                                 "Precipitation (mm)" = "P", "Precipitation - Runoff (mm)" = "Peff", 
                                                 "Effective Precip Used" = "Peff.used", "Runoff (mm)" = "Q", 
                                                 "AET (mm)" = "aet", "Crop ET (mm)" = "aet.crop", 
                                                 "ET:Effective Rainfall" = "aet.to.Peff", "ET Supply:Demand" = "aet.to.etc.crop", 
                                                 "Blue Water Potential (mm)" = "balance", "Potential ET (mm)" = "et0", 
                                                 "Potential Crop ET (mm)" = "etc", "etc.crop (mm)" = "etc.crop", 
                                                 "Irrigation Requirement (mm)" = "irrigation", "Groundwater Recharge (mm)" = "recharge", 
                                                 "% Runoff" = "runoff.percent", "% Soil Moisture" = "sm.percent"))
  
  # _________________________________________________________________________
  # STEP 2: Aggregate Mean and SD
  # _________________________________________________________________________
  # Because a single crop.system might contain multiple rotations, we average 
  # the previously calculated means and standard deviations across them.
  summ_qs_cs <- summary_yr_crop %>%
    dplyr::group_by(crop.system, Irrigation, var, var.plot) %>%
    dplyr::summarise(
      mean = mean(mean, na.rm = TRUE),
      sd = mean(sd, na.rm = TRUE),
      .groups = "drop"
    )
  
  # _________________________________________________________________________
  # STEP 3: Factor Ordering
  # _________________________________________________________________________
  # We want the charts to display from "Best" to "Worst" BWP performance.
  order_logic <- summ_qs_cs %>% 
    dplyr::filter(var == "balance") %>% 
    dplyr::arrange(mean)
  
  # Lock in that specific descending order into the crop.system factor levels
  summ_qs_cs$crop.system <- factor(summ_qs_cs$crop.system, levels = order_logic$crop.system)
  
  return(list(summary_yr_crop = summary_yr_crop, summ_qs_cs = summ_qs_cs))
}

#' Plot Annual Summary Metrics
#' 
#' @description Maps physical fluxes as horizontal bar charts bounded by their standard deviations.
#' Safely handles negative physical values (like Deficit BWP) to avoid floating bounding errors.
#' @param data Aggregated annual summary dataset (e.g., `summ_qs_cs`).
#' @param vars_to_plot Character vector specifying which variables (labels) should be rendered.
#' @param x_var String denoting the target column to plot on the x-axis (default: "mean").
#' @param title_x String labeling the x-axis (default: "Mean (± SD)").
#' @return A faceted `ggplot2` column chart plotting physical budgets.
#' @export
plot_annual_summary <- function(data, vars_to_plot, x_var = "mean", title_x = "Mean (\u00B1 SD)") {
  
  # _________________________________________________________________________
  # STEP 1: Constrain Error Bounds
  # _________________________________________________________________________
  # Filter and explicitly calculate error bounds before plotting
  plot_data <- data %>% 
    dplyr::filter(var.plot %in% vars_to_plot) %>%
    dplyr::mutate(
      var.plot = factor(var.plot, levels = vars_to_plot),
      # Calculate bounds here to avoid ggplot2 aes() scoping confusion
      error_high = .data[[x_var]] + sd,
      
      # Robust Lower bound logic:
      # If the mean is positive (>= 0), floor the error bar at 0 (e.g., impossible negative rain).
      # If the mean is negative (< 0), let the error bar extend normally downwards (e.g. BWP deficit).
      error_low  = dplyr::if_else(
        .data[[x_var]] >= 0, 
        pmax(.data[[x_var]] - sd, 0), 
        .data[[x_var]] - sd
      )
    )
  
  # _________________________________________________________________________
  # STEP 2: Render Chart
  # _________________________________________________________________________
  # Generate horizontal bar charts
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = crop.system)) +
    ggplot2::facet_grid(. ~ var.plot, scales = "free_x") +
    
    # Plot the main bars
    ggplot2::geom_col(ggplot2::aes(x = .data[[x_var]], fill = Irrigation)) +
    
    # Plot the error bounds using the pre-calculated columns
    ggplot2::geom_errorbar(ggplot2::aes(xmin = error_low, xmax = error_high), 
                           width = 0.2, color = "gray30", na.rm = TRUE) +
    
    ggplot2::scale_fill_manual(values = BWP_PALETTES$irrigation) +    
    ggplot2::labs(x = title_x, y = "Cropping System") +
    theme_baseR + 
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  return(p)
}

#' Process Monthly Summary Over Long-Term Simulations  -- currently unused
#' 
#' @description Computes monthly distribution bounds (2.5%, Median, 97.5% quantiles)
#' over decadal model runs for localized temporal risk analyses.
#' @param budget_ls List object containing parsed model simulation results.
#' @param crop_systems_df Dataframe associating crop rotation metadata to each model output.
#' @return A dataframe representing the synthesized quantile matrix for all months.
#' @export
process_monthly_summary <- function(budget_ls, crop_systems_df) {
  
  # _________________________________________________________________________
  # STEP 1: Internal Quantile Extractor
  # _________________________________________________________________________
  add_clim_mo <- function(x) {
    # Extract the monthly raw dataset
    sum_mo <- x[["sum.mo"]] %>%
      # Invert recharge mathematically so it plots below the 0 axis
      dplyr::mutate(plot.recharge = -recharge,
                    plot.irri = dplyr::if_else(irrigation == 0, NA_real_, irrigation),
                    plot.aet = dplyr::if_else(aet == 0, NA_real_, aet),
                    plot.etc = dplyr::if_else(etc == 0, NA_real_, etc))
    
    # Custom quantile extractor
    quantile_df <- function(v, probs = c(0.025, 0.5, 0.975)) {
      tibble::tibble(quantile = probs, value = quantile(v, probs, na.rm = TRUE))
    }
    
    # Calculate the quantiles for every variable in every month
    clim_mo <- sum_mo %>%
      dplyr::select(-year) %>%
      tidyr::pivot_longer(-month, names_to = "var", values_to = "value") %>%
      dplyr::group_by(var, month) %>%
      dplyr::reframe(quantile_df(value)) %>%
      tidyr::pivot_wider(names_from = "quantile", values_from = "value") %>%
      dplyr::rename(q025 = `0.025`, q975 = `0.975`, median = `0.5`)
    
    x[["clim.mo"]] <- clim_mo
    return(x)
  }
  
  # _________________________________________________________________________
  # STEP 2: Iterate & Consolidate Output
  # _________________________________________________________________________
  # Apply processor across entire simulation list
  budget_updated <- lapply(budget_ls, add_clim_mo)
  clim_mo_ls <- lapply(budget_updated, function(x) x$clim.mo)
  
  # Compile list back into a single frame and attach the metadata names
  clim_crop <- dplyr::bind_rows(clim_mo_ls, .id = "rotation") %>%
    dplyr::left_join(crop_systems_df, by = "rotation")
  
  # Generate the ultimate aggregate averages across the systems
  clim_crop_cs <- clim_crop %>%
    dplyr::group_by(var, crop.system, Irrigation, month) %>%
    dplyr::summarise(dplyr::across(q025:q975, \(x) mean(x, na.rm = TRUE)), .groups = "drop")
  
  return(clim_crop_cs)
}

#' Plot Monthly Blue Water Potential Facets -- currently unused
#' 
#' @description Renders a dual-state ribbon envelope indicating the probability of BWP
#' surplus versus deficit over the course of the growing season.
#' @param clim_crop_cs Quantile matrix data generated by `process_monthly_summary`.
#' @param years_range_text String displaying the years simulated (e.g., "1950 - 2022") for the title.
#' @return A `ggplot2` object displaying facet-wrapped BWP risk envelopes.
#' @export
plot_monthly_bwp <- function(clim_crop_cs, years_range_text) {
  
  # Isolate only the specific 'balance' (BWP) variable for these envelopes
  bwp_data <- clim_crop_cs %>%
    dplyr::filter(var == "balance") %>%
    # Create a logical flag: Is the median BWP currently positive?
    dplyr::mutate(above_axis = median >= 0)
  
  ggplot2::ggplot(bwp_data, ggplot2::aes(x = month, group = 1)) +
    ggplot2::facet_wrap(~crop.system, scales = "fixed") +
    # We draw TWO ribbons. 
    # Ribbon 1 stops at the 0 axis and only draws downward (Deficit tracking)
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pmin(q025, 0), ymax = 0, fill = "Below axis"), alpha = 0.5) +
    # Ribbon 2 stops at the 0 axis and only draws upward (Surplus tracking)
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = pmax(q975, 0), fill = "Above axis"), alpha = 0.5) +
    # Draw the central median line. Color changes dynamically based on the logical above_axis flag
    ggplot2::geom_line(ggplot2::aes(y = median, color = above_axis, group = 1), linewidth = 1) +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$bwp_fill) + 
    ggplot2::scale_color_manual(values = BWP_PALETTES$bwp_color) +          
    ggplot2::scale_x_continuous(breaks = 1:12, labels = as.character(1:12), limits = c(1, 12)) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.4, color = "black") + 
    theme_baseR +
    ggplot2::theme(legend.position = "none", strip.text = ggplot2::element_text(size = 8)) +
    ggplot2::labs(y = "Blue Water Potential (mm) (median + 95 CI)", x = "Month", 
                  title = paste0("Monthly Blue Water Potential for Various Cropping Systems in Anantapur (", years_range_text, ")"))
}
#' Plot Daily Water Budget Diagnostics -- currently unused
#'
#' @description Generates a three-panel diagnostic plot for daily water budget metrics 
#' (precipitation, runoff, irrigation, actual evapotranspiration, recharge, 
#' and crop coefficients) over a specified time period for a specific crop rotation.
#'
#' @param budget_daily_df A data frame containing detailed daily water budget outputs. 
#' Typically generated by compiling the `ts` elements from `get_budget()` and joining 
#' with crop systems metadata (e.g., `budget.daily`).
#' @param target_rotation Character string. The specific rotation ID to plot.
#' @param target_years Numeric vector. The year(s) to include in the plot (default: 2018).
#' @param save_plot Logical. If TRUE, exports the plot as a PDF to the specified output directory.
#' @param output_dir Character string. Directory path to save the PDF if `save_plot` is TRUE.
#'
#' @return A `ggplot` object arranged via `ggpubr::ggarrange` containing the three diagnostic panels.
#' @export
plot_daily_diagnostics <- function(budget_daily_df, target_rotation, target_years = c(2018), 
                                   save_plot = FALSE, output_dir = "figures/water_budgets") {
  
  # 1. Filter for the specific rotation and years
  ts.select <- budget_daily_df %>%
    dplyr::filter(rotation == target_rotation, year %in% target_years)
  
  if (nrow(ts.select) == 0) {
    warning("No data found for the specified rotation and years.")
    return(NULL)
  }
  
  # 2. Extract metadata for titles and filenames
  crop_sys_name <- ts.select$crop.system[1]
  soil_type <- ts.select$soil[1]
  crops <- paste(stats::na.omit(c(ts.select$crop_1[1], ts.select$crop_2[1], ts.select$crop_3[1])), collapse = "_")
  
  crop_sys_title <- paste0("Crop System: ", crop_sys_name, " | Rotation: ", target_rotation, 
                           " on ", toupper(soil_type), " soil")
  
  # 3. Define target variables and pivot to long format
  daily.vars.to.plot <- c("kcr", "aet.to.etc.crop", "P", "Q", "irrigation", "sm.end", "aet", "recharge")
  
  ts.l.select <- ts.select %>%
    dplyr::select(date, year, doy, dplyr::all_of(daily.vars.to.plot)) %>%
    tidyr::pivot_longer(cols = dplyr::all_of(daily.vars.to.plot), names_to = "var", values_to = "value")
  
  # 4. Build the plot panels
  p.daily_1 <- ggplot2::ggplot(ts.l.select %>% base::subset(var %in% c("P", "Q", "irrigation", "sm.end")),
                               ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_line(ggplot2::aes(group = var, color = var), na.rm = TRUE) +
    ggplot2::ylab("mm") + ggplot2::xlab("Year-DOY") +
    ggplot2::scale_x_date(date_labels = "%Y-%m-%d", date_breaks = "6 months") +
    ggplot2::theme_minimal() +
    ggplot2::scale_color_discrete("")
  
  p.daily_2 <- p.daily_1 %+% base::subset(ts.l.select, var %in% c("aet", "recharge"))
  
  p.daily_3 <- p.daily_1 %+% base::subset(ts.l.select, var %in% c("kcr", "aet.to.etc.crop")) +
    ggplot2::ylab("value") +
    ggplot2::ggtitle(paste0(crop_sys_title, "\nWater Budget for ", paste(target_years, collapse = "-")))
  
  # 5. Arrange panels
  p.daily.budget <- ggpubr::ggarrange(p.daily_3, p.daily_1, p.daily_2,
                                      nrow = 3, ncol = 1,
                                      labels = c("A", "B", "C"),
                                      legend = "top")
  
  # 6. Optional File Export
  if (save_plot) {
    if (!dir.exists(here::here(output_dir))) {
      dir.create(here::here(output_dir), recursive = TRUE)
    }
    
    file_name <- paste0("water_budget_SOIL_", soil_type, "_CROPS_", crops, "_", 
                        paste(target_years, collapse = "-"), "_daily.pdf")
    file_path <- here::here(output_dir, file_name)
    
    ggplot2::ggsave(plot = p.daily.budget, file = file_path, width = 9, height = 9)
  }
  
  return(p.daily.budget)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 4: PRODUCTION SYSTEM LEVEL ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Compute Area-Weighted Production System Budgets
#'
#' @description Computes area-weighted water budgets at the Production System
#' level from annual rotation data. It calculates climate risk quantiles and
#' splits specific budget depth and volumetric contributions across land types for ALL budget variables.
#' @param budget_yr_l A long-format dataframe containing annual rotation water budgets.
#' @param ps_by_cs_area A dataframe containing production systems, time periods, crop systems, and area (ha).
#' @param farms_count_df (Optional) A dataframe containing the number of farms per production system to scale to volumetric totals.
#' @return A list containing `ps_budget_summary` (true quantiles), `ps_volumetric_summary` (volumetric metrics for all variables),
#' and `ps_bwp_volumetric` (filtered volumetric footprint targeting the BWP term).
#' @export
get_production_system_budgets <- function(budget_yr_l, ps_by_cs_area, farms_count_df = NULL) {
  
  # Removing productions systems with no area
  ps_by_cs_area <- ps_by_cs_area %>% filter(area_ha > 0)
  # _________________________________________________________________________
  # STEP 1: Land Classification & Data Merge
  # _________________________________________________________________________
  # Join the physical water budgets to the social farm demographic mapping.
  ps_annual_data <- budget_yr_l %>%
    dplyr::select(rotation, crop.system, Irrigation, year, var, value) %>%
    dplyr::left_join(ps_by_cs_area, by = "crop.system", relationship = "many-to-many") %>%
    dplyr::filter(!is.na(sub_production_system)) %>%
    # Directly map land_type from the new land_use_type column (converted to lowercase to match internal plotting logic)
    dplyr::mutate(land_type = tolower(land_use_type))
  
  # _________________________________________________________________________
  # STEP 2: Calculate Area-Weighted Depths (mm)
  # _________________________________________________________________________
  ps_weighted_annual <- ps_annual_data %>%
    dplyr::group_by(production_system, sub_production_system, time_period, Irrigation, year, var, crop.system) %>%
    # Adjust for multiple rotations per year (avoid double-counting land size)
    dplyr::mutate(rotations_count = dplyr::n_distinct(rotation)) %>%
    dplyr::group_by(production_system, sub_production_system, time_period, Irrigation, year, var) %>%
    dplyr::summarise(
      weighted_value = sum(value * (area_ha / rotations_count), na.rm = TRUE) /
        sum(area_ha / rotations_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  # _________________________________________________________________________
  # STEP 3: Calculate Climate Risk Quantiles
  # _________________________________________________________________________
  ps_budget_summary <- ps_weighted_annual %>%
    dplyr::group_by(production_system, sub_production_system, time_period, Irrigation, var) %>%
    dplyr::summarise(
      mean = mean(weighted_value, na.rm = TRUE),
      sd  = sd(weighted_value, na.rm = TRUE),
      p25  = quantile(weighted_value, 0.25, na.rm = TRUE),
      p50  = quantile(weighted_value, 0.50, na.rm = TRUE),
      p75  = quantile(weighted_value, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(var.plot = forcats::fct_recode(factor(var),
                                                 "Precipitation (mm)" = "P", "Precipitation - Runoff (mm)" = "Peff",
                                                 "Effective Precip Used" = "Peff.used", "Runoff (mm)" = "Q",
                                                 "AET (mm)" = "aet", "Crop ET (mm)" = "aet.crop",
                                                 "ET:Effective Rainfall" = "aet.to.Peff", "ET Supply:Demand" = "aet.to.etc.crop",
                                                 "Blue Water Potential (mm)" = "balance", "Potential ET (mm)" = "et0",
                                                 "Potential Crop ET (mm)" = "etc", "etc.crop (mm)" = "etc.crop",
                                                 "Irrigation Requirement (mm)" = "irrigation", "Groundwater Recharge (mm)" = "recharge",
                                                 "% Runoff" = "runoff.percent", "% Soil Moisture" = "sm.percent")) %>%
    dplyr::relocate(var.plot, .after = var)  %>%
    dplyr::arrange(time_period, production_system, sub_production_system)
  
  # _________________________________________________________________________
  # STEP 4: Volumetric Conversion Matrix (mm to m3)
  # _________________________________________________________________________
  ps_total_areas <- ps_by_cs_area %>%
    dplyr::group_by(production_system, sub_production_system, time_period) %>%
    dplyr::summarise(total_system_area = sum(area_ha, na.rm = TRUE), .groups = "drop")
  
  ps_volumetric_summary <- ps_annual_data %>%
    dplyr::group_by(production_system, sub_production_system, time_period, var, land_type, year, crop.system) %>%
    dplyr::mutate(rotations_count = dplyr::n_distinct(rotation)) %>%
    dplyr::group_by(production_system, sub_production_system, time_period, var, land_type, year) %>%
    dplyr::summarise(
      type_volumetric_depth = sum(value * (area_ha / rotations_count), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(ps_total_areas, by = c("production_system", "sub_production_system", "time_period")) %>%
    dplyr::mutate(weighted_contribution = type_volumetric_depth / total_system_area) %>%
    dplyr::group_by(production_system, sub_production_system, time_period, var, land_type) %>%
    dplyr::summarise(mean_contribution = mean(weighted_contribution, na.rm = TRUE), .groups = "drop") %>%
    
    # THE FIX: Calculate the total mean_mm grouped by farm and variable BEFORE pivoting
    dplyr::group_by(production_system, sub_production_system, time_period, var) %>%
    dplyr::mutate(mean_mm = sum(mean_contribution, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    
    # NOW pivot the data. The mean_mm column will safely carry over to every row!
    tidyr::pivot_wider(names_from = land_type, values_from = mean_contribution, values_fill = 0) %>%
    dplyr::left_join(ps_total_areas, by = c("production_system", "sub_production_system", "time_period")) %>%    # UNIT CONVERSION: mm * ha * 10 = m3
    dplyr::mutate(
      mean_m3 = mean_mm * total_system_area * 10
    )
  
  # _________________________________________________________________________
  # STEP 5: Scale to Regional Demographics
  # _________________________________________________________________________
  has_farm_counts <- !is.null(farms_count_df)
  if (has_farm_counts) {
    ps_volumetric_summary <- ps_volumetric_summary %>%
      dplyr::left_join(
        farms_count_df %>% dplyr::select(production_system, sub_production_system, time_period, number_of_farms),
        by = c("production_system", "sub_production_system", "time_period")
      ) %>%
      dplyr::mutate(
        number_of_farms = tidyr::replace_na(number_of_farms, 0),
        total_vol_m3 = mean_m3 * number_of_farms
      ) %>%
      dplyr::select(time_period, production_system, sub_production_system, var, mean_mm,
                    dplyr::any_of(unique(ps_annual_data$land_type)),
                    mean_m3, number_of_farms, total_vol_m3)
  } else {
    ps_volumetric_summary <- ps_volumetric_summary %>%
      dplyr::select(time_period, production_system, sub_production_system, var, mean_mm,
                    dplyr::any_of(unique(ps_annual_data$land_type)), mean_m3)
  }
  
  ps_volumetric_summary <- ps_volumetric_summary %>% 
    dplyr::arrange(time_period, production_system, sub_production_system, var)
  ps_bwp_volumetric <- ps_volumetric_summary %>% 
    dplyr::filter(var == "balance")
  
  return(list(ps_budget_summary = ps_budget_summary,
              ps_volumetric_summary = ps_volumetric_summary,
              ps_bwp_volumetric = ps_bwp_volumetric))
}


#' Plot Socio-Economic Blue Water Potential Divergence
#' 
#' @description Generates a stacked bar chart mapping the farm-level impact of rainfed,
#' irrigated, and pastoral sub-systems on overall regional BWP. Uses dynamic faceting 
#' to automatically scale and separate time periods without hardcoded coordinates.
#' @param ps_bwp_volumetric Dataframe containing land-use partitioned BWP estimates.
#' @return A horizontal `ggplot2` object displaying BWP across demographics and timelines.
#' @export
plot_production_system_bwp <- function(ps_bwp_volumetric) {
  
  # _________________________________________________________________________
  # STEP 1: Format Typology Names
  # _________________________________________________________________________
  # Reverse factor orders so the bars plot neatly top-to-bottom.
  # (No need to manually rename 1950 vs 2022 Pastoralists; faceting handles overlaps natively)
  ps_bwp_volumetric <- ps_bwp_volumetric %>%
    dplyr::mutate(sub_production_system = factor(sub_production_system, levels = rev(unique(sub_production_system)))) %>%
    # Remove land use type that is not any_of(c("rainfed", "irrigated", "grazed"). For example, unused land
    dplyr::filter(dplyr::if_any(dplyr::any_of(c("rainfed", "irrigated", "grazed")), ~ .x != 0))
  # _________________________________________________________________________
  # STEP 2: Pivot for Stacked Barchart
  # _________________________________________________________________________
  # Take the three physical land types and collapse them into a single column so ggplot can build a 'stacked' color bar
  ps_bwp_long <- ps_bwp_volumetric %>%
    dplyr::select(time_period, sub_production_system, rainfed, irrigated, grazed) %>%
    tidyr::pivot_longer(cols = c(rainfed, irrigated, grazed), names_to = "contribution_source", values_to = "bwp_value") %>%
    dplyr::mutate(contribution_source = factor(contribution_source, levels = c("irrigated", "rainfed", "grazed"),
                                               labels = c("irrigated cropped area", "rainfed cropped area", "grazed area")))
  
  # _________________________________________________________________________
  # STEP 3: Render (Dynamic Native Mapping)
  # _________________________________________________________________________
  ggplot2::ggplot() +
    # Layer 1: Stacked Bar columns (mapped directly to y/x to avoid coord_flip complications)
    ggplot2::geom_col(data = ps_bwp_long, ggplot2::aes(y = sub_production_system, x = bwp_value, fill = contribution_source), width = 0.55) +
    # Layer 2: Overlay the net sum (the true 'mean_mm') as a distinct 'X' point shape
    ggplot2::geom_point(data = ps_bwp_volumetric, ggplot2::aes(y = sub_production_system, x = mean_mm, shape = "Net Farm BWP"), size = 2.5, stroke = 1) +
    ggplot2::geom_vline(xintercept = 0, color = "black", linewidth = 0.8) +
    # Place labels on the right
    ggplot2::scale_y_discrete(position = "right") +
    ggplot2::scale_fill_manual(name = "Contribution Source", values = BWP_PALETTES$land_use) +          
    ggplot2::scale_shape_manual(name = NULL, values = c("Net Farm BWP" = 4)) +
    ggplot2::scale_x_continuous(labels = function(x) format(x, big.mark = " ")) +
    ggplot2::labs(x = "Farm Blue Water Potential (mm/year)", y = "Production Systems") +
    # Automatically split 1950 and 2022 into dynamically sized row panels (fixes the overlapping dashed line)
    ggplot2::facet_grid(time_period ~ ., scales = "free_y", space = "free_y", switch = "y") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "gray90"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "black", hjust = 0, vjust = 0.5),
      # Format the right-side Y axis title to sit horizontally at the top like a header
      axis.title.y.right = ggplot2::element_text(angle = 0, face = "bold", vjust = 1.05, hjust = 0, margin = ggplot2::margin(b = 10)),
      # Format the facet strip labels (1950, 2022) to act as the left-side timeline (fixes floating text)
      strip.text.y.left = ggplot2::element_text(angle = 0, face = "bold", size = 14),
      strip.background = ggplot2::element_blank(),
      strip.placement = "outside",
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "gray80"),
      legend.box.margin = ggplot2::margin(l = -100)
    )
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 5: VILLAGE/MICRO-REGION SCALE FLOW DIAGRAMS (SOCIO-ECONOMIC AGGREGATION) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Main Dispatcher for Village Water Balance Sankey Diagrams
#' 
#' @description Checks the incoming time dimensions to decide whether to dispatch
#' the single-panel chart engine or the double-panel comparison engine. Includes
#' smart grouping detection and optional data export.
#' @param ps_bwp_volumetric Dataframe containing BWP estimates.
#' @param ps_by_cs_area Dataframe with production system acreage and metadata.
#' @param cfg Configuration list dictating base visual bounds.
#' @param output_dev Target R graphics device (default `grDevices::cairo_pdf`).
#' @param filename File path output target.
#' @param manuscript_style Logical flag to enforce high-contrast journal formatting.
#' @param save_csv_path Optional file path to save the aggregated figure data.
#' @return Executes a graphical device call.
#' @export
plot_village_bwp <- function(ps_bwp_volumetric, ps_by_cs_area, cfg, output_dev = grDevices::cairo_pdf, filename = "figures/manuscript/Fig.7.pdf", manuscript_style = FALSE, save_csv_path = NULL) {
  
  # 1. Extract mapping safely with timeline bounds
  mapping <- ps_by_cs_area %>%
    dplyr::select(time_period, production_system, dplyr::any_of("report_group_name")) %>%
    dplyr::distinct()
  
  village_bwp_data <- ps_bwp_volumetric %>%
    dplyr::left_join(mapping, by = c("time_period", "production_system"))
  
  # 2. Graceful Fallback: Detect if custom report groupings exist
  group_col <- if ("report_group_name" %in% names(village_bwp_data)) "report_group_name" else "production_system"
  
  # 3. Internal Aggregation & Unit Conversion
  village_bwp_data <- village_bwp_data %>%
    dplyr::mutate(val_converted = round(total_vol_m3 / 10000) * 10) %>%
    dplyr::group_by(time_period, label = .data[[group_col]]) %>%
    dplyr::summarise(val = sum(val_converted, na.rm = TRUE), .groups = "drop") %>%
    dplyr::rename(period = time_period)
  
  # 4. Optional CSV Export
  if (!is.null(save_csv_path)) {
    # Ensure directory exists before writing
    dir.create(dirname(save_csv_path), recursive = TRUE, showWarnings = FALSE)
    write.csv(village_bwp_data, file = save_csv_path, row.names = FALSE)
  }
  
  unique_periods <- unique(village_bwp_data$period)
  num_periods <- length(unique_periods)
  
  # 5. Route to drawing engine
  if (num_periods == 1) {
    get_single_village_plot(village_bwp_data, cfg, output_dev, filename, manuscript_style = manuscript_style)
  } else if (num_periods == 2) {
    get_village_comparison_plot(village_bwp_data, cfg, output_dev, filename, manuscript_style = manuscript_style)
  }
}

#' Draw Double Panel Comparative Village Flow Chart
#' 
#' @description Generates the primary two-panel flow box representing 1950s vs 2022 footprints.
#' @param village_bwp_data Base input dataframe describing inflow and outflow bounds.
#' @param cfg List object controlling graphical layout parameters.
#' @param output_dev Graphics device target.
#' @param filename String controlling output file routing.
#' @param manuscript_style Logical enabling print-safe styling.
#' @return Generates a graphical file.
#' @keywords internal
#' @export
get_village_comparison_plot <- function(village_bwp_data, cfg, output_dev = grDevices::cairo_pdf,  
                                        filename = "figures/manuscript/Fig.6.pdf", manuscript_style = FALSE) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  
  # _________________________________________________________________________
  # STEP 1: Device Configuration
  # _________________________________________________________________________
  # Establish the base R graphics canvas parameters depending on whether it's a PDF or PNG engine
  if (identical(output_dev, grDevices::cairo_pdf)) {
    output_dev(file = filename, width = 10.0, height = 4.5, family = "Helvetica")
  } else {
    output_dev(filename = filename, width = 4000, height = 1750, res = 300, pointsize = 18)
  }
  par(mfrow = c(1, 2), mar = c(1.0, 3.5, 1.5, 7.5), oma = c(0, 0, 1.4, 1))
  
  # _________________________________________________________________________
  # STEP 2: Aggregate Mass Balance
  # _________________________________________________________________________
  unique_periods <- unique(village_bwp_data$period)
  p1 <- unique_periods[1]; p2 <- unique_periods[2]
  df_p1 <- village_bwp_data[village_bwp_data$period == p1, ]
  df_p2 <- village_bwp_data[village_bwp_data$period == p2, ]
  
  tot_p1 <- sum(df_p1$val[df_p1$val >= 0]) - sum(abs(df_p1$val[df_p1$val < 0]))
  tot_p2 <- sum(df_p2$val[df_p2$val >= 0]) - sum(abs(df_p2$val[df_p2$val < 0]))
  max_tot <- max(tot_p1, tot_p2)
  abs_drop <- tot_p1 - tot_p2
  pct_change <- -(abs_drop / tot_p1) * 100
  
  # Internal Helper: Color ramp interpolation for building smooth gradient flows
  get_flow_colors <- function(n, base_colors) { 
    if(n <= 1) return(base_colors[1])
    grDevices::colorRampPalette(base_colors)(n) 
  }
  
  # Internal Helper: Typography split handling for placing large values directly underneath text labels
  draw_split_label <- function(x, y, label, val, adj, is_outflux = FALSE) {
    main_txt <- trimws(sub("\\(.*\\)", "", label))
    bracket_txt <- if (grepl("\\(", label)) regmatches(label, regexpr("\\(.*\\)", label)) else ""
    val_txt <- if(is_outflux) paste0("-", format(abs(val), big.mark=",")) else format(val, big.mark=",")
    line_drop <- 950 ; cex_main  <- 0.65 ; cex_sub   <- 0.55 
    
    text(x, y + line_drop, main_txt, adj = adj, font = 2, cex = cex_main, col = "#111111", xpd = NA)
    if(bracket_txt != "") {
      text(x, y, bracket_txt, adj = adj, font = 1, cex = cex_sub, col = "#555555", xpd = NA)
      text(x, y - line_drop, val_txt, adj = adj, font = 1, cex = cex_main, col = "#777777", xpd = NA)
    } else {
      text(x, y, val_txt, adj = adj, font = 1, cex = cex_main, col = "#777777", xpd = NA)
    }
  }
  
  # _________________________________________________________________________
  # STEP 3: Calculate Bounding Box Dimensions
  # _________________________________________________________________________
  box_xmin <- -10; box_xmax <- box_xmin + cfg$box_width ; box_ymax <- 12000
  fill_y_p1 <- box_ymax * (tot_p1 / max_tot); fill_y_p2 <- box_ymax * (tot_p2 / max_tot)
  plot_xlim <- c(-100, 135) ; plot_ylim <- c(-3000, 25500)
  
  # _________________________________________________________________________
  # STEP 4: Render Panel A (Baseline)
  # _________________________________________________________________________
  df_in_p1 <- df_p1[df_p1$val >= 0, ][order(-df_p1$val[df_p1$val >= 0]), ]
  plot(NULL, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  mtext("A", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)
  
  # Draw the literal Water Tank (Rectangle)
  rect(box_xmin, 0, box_xmax, box_ymax, col = NA, border = BWP_PALETTES$flow_box_line, lwd = 1.5)
  rect(box_xmin, 0, box_xmax, fill_y_p1, col = BWP_PALETTES$flow_box_fill, border = NA)
  text((box_xmin + box_xmax)/2, fill_y_p1 * 0.65, "Blue Water\nPotential", col = "white", font = 2, cex = 0.85)
  text((box_xmin + box_xmax)/2, fill_y_p1 * 0.32, format(tot_p1, big.mark=","), col = "white", font = 1, cex = 1.1)
  
  # Iterate through Inflow volumes and mathematically draw connecting arrows hitting the top of the tank
  n_in_p1 <- nrow(df_in_p1)
  if(n_in_p1 > 0) {
    cols <- get_flow_colors(n_in_p1, BWP_PALETTES$flow_in_p1) 
    y_s  <- seq(16000, 24500, length.out = n_in_p1) 
    x_d  <- seq(box_xmin + 6, box_xmin + (box_xmax - box_xmin)*8/10, length.out = n_in_p1) 
    for(i in 1:n_in_p1) {
      # Rescale numerical volumes into strict pixel line widths (lwd_val)
      lwd_val <- scales::rescale(df_in_p1$val[i], to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(village_bwp_data$val)))
      lines(c(-55, x_d[i], x_d[i]), c(y_s[i], y_s[i], box_ymax + (lwd_val * 75)), lwd = lwd_val, col = cols[i], lend = 1)
      # Draw Arrowheads using the polygon tool
      polygon(c(x_d[i] - (lwd_val * 1.4), x_d[i] + (lwd_val * 1.4), x_d[i]), c(box_ymax + (lwd_val * 75), box_ymax + (lwd_val * 75), box_ymax), col = cols[i], border = NA)
      draw_split_label(-60, y_s[i], df_in_p1$label[i], df_in_p1$val[i], 1)
    }
  }
  text((box_xmin + box_xmax)/2, -2000, paste0(p1, " Archetype Village"), font = 2, cex = 0.95, xpd = NA)
  
  # _________________________________________________________________________
  # STEP 5: Render Panel B (Intensified)
  # _________________________________________________________________________
  df_in_p2  <- df_p2[df_p2$val >= 0, ][order(-df_p2$val[df_p2$val >= 0]), ]
  df_out_p2 <- df_p2[df_p2$val < 0, ][order(abs(df_p2$val[df_p2$val < 0]), decreasing = TRUE), ]
  plot(NULL, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  mtext("B", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)
  
  if (!manuscript_style) {
    # If rendering for Presentation, draw the visually distinct 'deficit drop' gap above the water line
    rect(box_xmin, 0, box_xmax, box_ymax, col = NA, border = BWP_PALETTES$flow_box_line, lwd = 1.5)
    rect(box_xmin, 0, box_xmax, fill_y_p2, col = BWP_PALETTES$flow_box_fill, border = NA)
    rect(box_xmin, fill_y_p2, box_xmax, fill_y_p1, col = BWP_PALETTES$flow_box_diff, border = NA)
    empty_space_mid_y <- fill_y_p2 + ((fill_y_p1 - fill_y_p2) / 2)
    start_x <- box_xmin + (box_xmax - box_xmin) * 0.2
    text_to_show <- paste0("-", format(abs_drop, big.mark=","), " (", round(pct_change), "%)")
    text(start_x, empty_space_mid_y, text_to_show, col = BWP_PALETTES$flow_drop_text, font = 2, cex = 0.80, adj = 0)
    polygon(c(start_x - 12, start_x - 2, start_x - 7), c(empty_space_mid_y + 350, empty_space_mid_y + 350, empty_space_mid_y - 350), col = BWP_PALETTES$flow_drop_text, border = NA)
  } else {
    rect(box_xmin, 0, box_xmax, fill_y_p2, col = NA, border = BWP_PALETTES$flow_box_line, lwd = 1.5)
    rect(box_xmin, 0, box_xmax, fill_y_p2, col = BWP_PALETTES$flow_box_fill, border = NA)
  }
  
  text((box_xmin + box_xmax)/2, fill_y_p2 * 0.65, "Blue Water\nPotential", col = "white", font = 2, cex = 0.85)
  text((box_xmin + box_xmax)/2, fill_y_p2 * 0.32, format(tot_p2, big.mark=","), col = "white", font = 1, cex = 1.1)
  
  # Draw Inflow Arrows (Top)
  n_in_p2 <- nrow(df_in_p2)
  if(n_in_p2 > 0) {
    cols <- get_flow_colors(n_in_p2, BWP_PALETTES$flow_in_p2)
    y_s  <- seq(14000, 24500, length.out = n_in_p2)
    x_d  <- seq(box_xmin + 6, box_xmin + (box_xmax - box_xmin)*8/10, length.out = n_in_p2)
    target_y <- if(!manuscript_style) box_ymax else fill_y_p2 
    for(i in 1:n_in_p2) {
      lwd_val <- scales::rescale(df_in_p2$val[i], to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(village_bwp_data$val)))
      lines(c(-55, x_d[i], x_d[i]), c(y_s[i], y_s[i], target_y + (lwd_val * 75)), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_d[i] - (lwd_val * 1.4), x_d[i] + (lwd_val * 1.4), x_d[i]), c(target_y + (lwd_val * 75), target_y + (lwd_val * 75), target_y), col = cols[i], border = NA)
      draw_split_label(-60, y_s[i], df_in_p2$label[i], df_in_p2$val[i], 1)
    }
  }
  
  # Draw Outflow Arrows (Leaking out of the Right Side of the Tank)
  n_out_p2 <- nrow(df_out_p2)
  if(n_out_p2 > 0) {
    cols <- get_flow_colors(n_out_p2, BWP_PALETTES$flow_out_p2) 
    for(i in 1:n_out_p2) {
      abs_val <- abs(df_out_p2$val[i])
      lwd_val <- scales::rescale(abs_val, to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(abs(village_bwp_data$val))))
      # Calculate the staggering drop heights for each successive outflow line
      x_origin <- box_xmax + 1  
      x_s <- box_xmax + ((n_out_p2 - i) * 8) + 5            
      y_f <- box_ymax + 1000 + (i * cfg$v_stagger_out)      
      x_t <- box_xmax + 40                                  
      y_start <- if(!manuscript_style) fill_y_p2 + ((fill_y_p1 - fill_y_p2) * (i / (n_out_p2 + 1))) else ((fill_y_p2*20/100) * (i / (n_out_p2 + 1)))
      
      lines(c(x_origin, x_s, x_s, x_t), c(y_start, y_start, y_f, y_f), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_t, x_t, x_t + 12), c(y_f - (lwd_val * 60 + 200), y_f + (lwd_val * 60 + 200), y_f), col = cols[i], border = NA)
      draw_split_label(x_t + 15, y_f, df_out_p2$label[i], abs_val, 0, TRUE)
    }
  }
  text((box_xmin + box_xmax)/2, -2000, paste0(p2, " Archetype Village"), font = 2, cex = 0.95, xpd = NA)
  mtext(expression(italic(paste("All units in ", 10^3, " m"^3))), side = 3, line = 0.2, adj = 1, outer = TRUE, cex = 0.9)
  grDevices::dev.off()
}

#' Draw Single Panel Base Village Flow Chart
#' 
#' @description Internal renderer for single-period mass balance charting.
#' @param df Segmented input dataframe.
#' @param cfg List object controlling graphical layout parameters.
#' @param output_dev Graphics device target.
#' @param filename String controlling output file routing.
#' @param manuscript_style Logical flag to enforce high-contrast journal formatting.
#' @return Generates a graphical file.
#' @keywords internal
#' @export
get_single_village_plot <- function(df, cfg, output_dev, filename, manuscript_style = FALSE) {
  # (Note: This standalone function follows the exact same mechanical loop logic as the Panel B outflow 
  # generation above, stripped down to render a single, centered Canvas when only 1 time period is provided).
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  if (identical(output_dev, grDevices::cairo_pdf)) {
    # THE FIX: Scaled up the height slightly to match the spatial ratios of the comparison plot edits
    output_dev(file = filename, width = 6.0, height = 5.25, family = "Helvetica")
  } else {
    output_dev(filename = filename, width = 2400, height = 2100, res = 300, pointsize = 18)
  }
  par(mfrow = c(1, 1), mar = c(1.0, 3.5, 1.5, 7.5), oma = c(0, 0, 1.4, 1))
  
  tot_val <- sum(df$val[df$val >= 0]) - sum(abs(df$val[df$val < 0]))
  period_name <- unique(df$period)[1]
  
  get_flow_colors <- function(n, base_colors) { 
    if(n <= 1) return(base_colors[1])
    grDevices::colorRampPalette(base_colors)(n) 
  }
  
  draw_split_label <- function(x, y, label, val, adj, is_outflux = FALSE) {
    main_txt <- trimws(sub("\\(.*\\)", "", label))
    bracket_txt <- if (grepl("\\(", label)) regmatches(label, regexpr("\\(.*\\)", label)) else ""
    val_txt <- if(is_outflux) paste0("-", format(abs(val), big.mark=",")) else format(val, big.mark=",")
    line_drop <- 950; cex_main <- 0.65; cex_sub <- 0.55 
    text(x, y + line_drop, main_txt, adj = adj, font = 2, cex = cex_main, col = "#111111", xpd = NA)
    if(bracket_txt != "") {
      text(x, y, bracket_txt, adj = adj, font = 1, cex = cex_sub, col = "#555555", xpd = NA)
      text(x, y - line_drop, val_txt, adj = adj, font = 1, cex = cex_main, col = "#777777", xpd = NA)
    } else {
      text(x, y, val_txt, adj = adj, font = 1, cex = cex_main, col = "#777777", xpd = NA)
    }
  }
  
  box_xmin <- -10; box_xmax <- box_xmin + cfg$box_width; box_ymax <- 12000
  plot_xlim <- c(-100, 135); plot_ylim <- c(-3000, 25500)
  
  df_in <- df[df$val >= 0, ][order(-df$val[df$val >= 0]), ]
  df_out <- df[df$val < 0, ][order(abs(df$val[df$val < 0]), decreasing = TRUE), ]
  
  plot(NULL, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  rect(box_xmin, 0, box_xmax, box_ymax, col = NA, border = BWP_PALETTES$flow_box_line, lwd = 1.5)
  rect(box_xmin, 0, box_xmax, box_ymax, col = BWP_PALETTES$flow_box_fill, border = NA)
  text((box_xmin + box_xmax)/2, box_ymax * 0.65, "Blue Water\nPotential", col = "white", font = 2, cex = 0.85)
  text((box_xmin + box_xmax)/2, box_ymax * 0.32, format(tot_val, big.mark=","), col = "white", font = 1, cex = 1.1)
  
  n_in <- nrow(df_in)
  if(n_in > 0) {
    cols <- get_flow_colors(n_in, BWP_PALETTES$flow_in_p1)
    
    # THE FIX: Dropped the top anchor point to 14000 to mirror the spatial spacing fix in the comparison plot
    y_s  <- seq(14000, 24500, length.out = n_in) 
    
    x_d  <- seq(box_xmin + 6, box_xmin + (box_xmax - box_xmin)*8/10, length.out = n_in) 
    for(i in 1:n_in) {
      lwd_val <- scales::rescale(df_in$val[i], to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(df$val)))
      lines(c(-55, x_d[i], x_d[i]), c(y_s[i], y_s[i], box_ymax + (lwd_val * 75)), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_d[i] - (lwd_val * 1.4), x_d[i] + (lwd_val * 1.4), x_d[i]), c(box_ymax + (lwd_val * 75), box_ymax + (lwd_val * 75), box_ymax), col = cols[i], border = NA)
      draw_split_label(-60, y_s[i], df_in$label[i], df_in$val[i], 1)
    }
  }
  
  n_out <- nrow(df_out)
  if(n_out > 0) {
    cols <- get_flow_colors(n_out, BWP_PALETTES$flow_out_single) 
    for(i in 1:n_out) {
      abs_val <- abs(df_out$val[i])
      lwd_val <- scales::rescale(abs_val, to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(abs(df$val))))
      x_origin <- box_xmax + 2  
      x_s <- box_xmax + ((n_out - i) * 8) + 5            
      y_f <- box_ymax + 1000 + (i * cfg$v_stagger_out)  
      x_t <- box_xmax + 40                              
      
      # THE FIX: Added manuscript_style logic mirroring the comparison plot to drop the outflow lines correctly if styling is enabled
      y_start <- if(!manuscript_style) box_ymax * (i / (n_out + 1)) else (box_ymax * 0.20) * (i / (n_out + 1)) 
      
      lines(c(x_origin, x_s, x_s, x_t), c(y_start, y_start, y_f, y_f), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_t, x_t, x_t + 12), c(y_f - (lwd_val * 60 + 200), y_f + (lwd_val * 60 + 200), y_f), col = cols[i], border = NA)
      draw_split_label(x_t + 15, y_f, df_out$label[i], abs_val, 0, TRUE)
    }
  }
  text((box_xmin + box_xmax)/2, -2000, paste0(period_name, " Archetype Village"), font = 2, cex = 0.95, xpd = NA)
  mtext(expression(italic(paste("All units in ", 10^3, " m"^3))), side = 3, line = 0.2, adj = 1, outer = TRUE, cex = 0.9)
  grDevices::dev.off()
}


#' Generate HTML networkD3 Interactive Sankey Chart
#' 
#' @description Maps the absolute volumetric routing of rain through different localized
#' agrosystems toward ultimate net BWP via `networkD3`. 
#' @param ps_volumetric_summary Core volumetric output from socio-economic calculations.
#' @param target_period Timeline evaluating string (e.g. "2022" or "1950s").
#' @param font_size Numeric widget scalar (default: 13).
#' @param margin_left Margin geometry in px (default: 120).
#' @param margin_right Margin geometry in px (default: 30).
#' @return An interactive HTML widget displaying Sankey flow relationships.
#' @export
get_d3_sankey <- function(ps_volumetric_summary, target_period, font_size = 13, margin_left = 120, margin_right = 30) {
  
  target_yr <- stringr::str_extract(as.character(target_period), "\\d+")
  
  # _________________________________________________________________________
  # STEP 1: Extract Period-Specific Data
  # _________________________________________________________________________
  tp_data <- ps_volumetric_summary %>%
    dplyr::filter(stringr::str_detect(as.character(time_period), target_yr)) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(
      total_m3     = sum(total_vol_m3, na.rm = TRUE),
      unused_m3    = sum(unused * (total_vol_m3 / mean_mm), na.rm = TRUE),
      irrigated_m3 = sum(irrigated * (total_vol_m3 / mean_mm), na.rm = TRUE),
      rainfed_m3   = sum(rainfed * (total_vol_m3 / mean_mm), na.rm = TRUE),
      grazed_m3    = sum(grazed * (total_vol_m3 / mean_mm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_m3"), ~ ifelse(is.nan(.) | is.na(.), 0, .)))
  
  # _________________________________________________________________________
  # STEP 2: Formatting & Aesthetic Rounding
  # _________________________________________________________________________
  round_to_10k_in_thousands <- function(val) {
    return(round(val / 10000) * 10)
  }
  
  p_vol         <- round_to_10k_in_thousands(sum(tp_data$total_m3[tp_data$var == "P"], na.rm = TRUE))
  
  # BWP calculation explicitly matches the mathematical rounding order used in Fig 7
  # (Rounding each individual production system before summing, to prevent 1650 vs 1640 drift)
  bwp_components <- ps_volumetric_summary %>%
    dplyr::filter(stringr::str_detect(as.character(time_period), target_yr) & var == "balance") %>%
    dplyr::mutate(val_converted = round(total_vol_m3 / 10000) * 10) 
  
  bwp_vol       <- sum(bwp_components$val_converted, na.rm = TRUE)
  
  aet_row       <- tp_data %>% dplyr::filter(var == "aet")
  aet_tot       <- sum(aet_row$total_m3, na.rm = TRUE)
  
  et_rainfed    <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$rainfed_m3)) else 0
  et_irrigated  <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$irrigated_m3)) else 0
  et_grazed     <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$grazed_m3)) else 0
  et_unused     <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$unused_m3)) else 0
  
  # _________________________________________________________________________
  # STEP 3: Enforce Absolute Mass Balance
  # _________________________________________________________________________
  total_out <- bwp_vol + et_rainfed + et_irrigated + et_grazed + et_unused
  
  # Handle minor rounding artifacts (+/- 10k) naturally across the largest sink
  if (p_vol != total_out && total_out > 0) { 
    et_rainfed <- et_rainfed + (p_vol - total_out) 
  }
  
  # _________________________________________________________________________
  # STEP 4: Build Network Linkages
  # _________________________________________________________________________
  nodes_df <- data.frame(
    name = c(
      paste0("Precipitation|", format(p_vol, big.mark=","), " k m³"),
      paste0("ET: Irrigated Crops|", format(et_irrigated, big.mark=","), " k m³"),
      paste0("ET: Rainfed Crops|", format(et_rainfed, big.mark=","), " k m³"),
      paste0("ET: Grazed Areas|", format(et_grazed, big.mark=","), " k m³"),
      paste0("ET: Unused Areas|", format(et_unused, big.mark=","), " k m³"),
      paste0("Net Recharge |", format(bwp_vol, big.mark=","), " k m³")
    ),
    group = c("Rain", "Irrigated", "Rainfed", "Grazed", "Unused", "BWP")
  )
  
  outflow_vols <- c(et_irrigated, et_rainfed, et_grazed, et_unused, bwp_vol)
  
  outflow_indices <- 1:5 
  
  links_list <- list()
  for (i in seq_along(outflow_vols)) {
    if (outflow_vols[i] > 0) {
      links_list[[i]] <- data.frame(source = 0, target = outflow_indices[i], value = abs(outflow_vols[i]), group = "Flow_Rain")
    }
  }
  
  if (length(links_list) == 0) {
    links_df <- data.frame(source = 0, target = 1, value = 0.001, group = "Flow_Rain")
  } else {
    links_df <- do.call(rbind, links_list)
  }
  
  color_scale <- paste0('d3.scaleOrdinal()
    .domain(', BWP_PALETTES$sankey_domain, ')
    .range(', BWP_PALETTES$sankey_range, ')')
  
  # _________________________________________________________________________
  # STEP 5: Render the D3 Widget
  # _________________________________________________________________________
  sankey <- networkD3::sankeyNetwork(
    Links = links_df, Nodes = nodes_df, Source = "source", Target = "target",
    Value = "value", NodeID = "name", NodeGroup = "group", LinkGroup = "group",
    units = "k m³", fontSize = font_size, nodeWidth = 30, nodePadding = 40, 
    colourScale = color_scale, sinksRight = TRUE, 
    width = 550, height = 450, iterations = 0,
    margin = list(top = 25, left = margin_left, right = margin_right, bottom = 25)
  )
  
  # _________________________________________________________________________
  # STEP 6: Javascript Typography Override
  # _________________________________________________________________________
  sankey <- htmlwidgets::onRender(sankey, '
    function(el, x) {
      d3.select(el).selectAll(".node text")
        .style("fill", "#1a1a1a")
        .style("font-family", "sans-serif")
        .style("font-weight", "600");

      d3.select(el).selectAll(".node text").each(function(d) {
        if (!d.name) return;
        var txt = d3.select(this);
        var parts = d.name.split("|"); 
        if (parts.length > 1) {
          var x_pos = txt.attr("x");
          txt.text(""); 
          txt.append("tspan").attr("x", x_pos).attr("dy", "0.1em").text(parts[0]);
          txt.append("tspan").attr("x", x_pos).attr("dy", "1.3em")
             .style("font-weight", "normal").style("fill", "#555555").text(parts[1]);
        }
      });

      var nodes = d3.select(el).selectAll(".node").data();
      var sourceNode = nodes.find(n => n.name && n.name.startsWith("Precipitation"));

      if (sourceNode) {
        var targetNodes = nodes.filter(n => n.name && !n.name.startsWith("Precipitation"));
        if (targetNodes.length > 0) {
          var maxY = d3.max(targetNodes, n => n.y + n.dy);
          var minY = d3.min(targetNodes, n => n.y);

          var newY = minY + (maxY - minY) / 2 - sourceNode.dy / 2;
          
          sourceNode.y = newY;
          d3.select(el).selectAll(".node")
            .filter(d => d.name && d.name.startsWith("Precipitation"))
            .attr("transform", d => "translate(" + d.x + "," + d.y + ")");

          d3.select(el).selectAll(".link").attr("d", function(d) {
            var curvature = 0.5;
            var x0 = d.source.x + d.source.dx,
                x1 = d.target.x,
                xi = d3.interpolateNumber(x0, x1),
                x2 = xi(curvature),
                x3 = xi(1 - curvature),
                y0 = d.source.y + d.sy + d.dy / 2,
                y1 = d.target.y + d.ty + d.dy / 2;
            
            return "M" + x0 + "," + y0
                 + "C" + x2 + "," + y0
                 + " " + x3 + "," + y1
                 + " " + x1 + "," + y1;
          });
        }
      }
    }
  ')
  
  return(sankey)
}

#' Plot Micro-Regional Disparity Metrics
#' 
#' @description Maps farm count vs. volumetric water consumption footprints using a dual 
#' `patchwork` alignment layout to explore resource inequities across typologies.
#' @param ps_bwp_volumetric Dataframe containing land-use partitioned BWP and farm scaling data.
#' @param ps_by_cs_area Dataframe denoting acreage distributions and pre-calculated `land_use_type`.
#' @param target_period String specifying the timeline to evaluate (default: "2022").
#' @return A composite `patchwork` visualization.
#' @export
plot_farm_bwp_disparity <- function(ps_bwp_volumetric, ps_by_cs_area, target_period = "2022") {
  
  # 1. Extract Typology & Mapping
  mapping <- ps_by_cs_area %>%
    dplyr::filter(time_period == as.character(target_period)) %>%
    dplyr::select(production_system, land_use_type, dplyr::any_of("report_group_name")) %>%
    dplyr::distinct() %>%
    dplyr::group_by(production_system) %>%
    dplyr::slice(1) %>%
    dplyr::ungroup()
  
  clean_data <- ps_bwp_volumetric %>%
    dplyr::filter(time_period == as.character(target_period)) %>%
    dplyr::left_join(mapping, by = "production_system")
  
  # 2. Graceful Fallback
  group_col <- if ("report_group_name" %in% names(clean_data)) "report_group_name" else "production_system"
  
  # 3. Apply Groupings and Text Formatting
  clean_data <- clean_data %>%
    dplyr::mutate(
      label_base = .data[[group_col]],
      # 'label_base' is already perfectly clean from the upstream ETL script!
      typology_tag = stringr::str_remove(land_use_type, "-Driven"),
      display_label = paste0(label_base, " [", typology_tag, "]")
    )
  
  # 4. Safe Aggregation
  agg_data <- clean_data %>%
    dplyr::group_by(display_label) %>%
    dplyr::summarise(
      total = sum(total_vol_m3, na.rm = TRUE),
      farms = if(all(is.na(number_of_farms))) 0 else max(number_of_farms, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      bwp_status = dplyr::if_else(total >= 0, "+BWP (Surplus)", "-BWP (Deficit)", missing = "-BWP (Deficit)")
    ) %>%
    dplyr::arrange(total) %>%
    dplyr::mutate(production_system_fct = factor(display_label, levels = unique(display_label)))
  
  # 5. Patchwork Component Build
  p_count <- ggplot2::ggplot(agg_data, ggplot2::aes(x = production_system_fct, y = farms, fill = bwp_status)) +
    ggplot2::geom_col(width = 0.6, show.legend = FALSE) + 
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$disparity_bwp) +
    ggplot2::labs(x = "Production System", y = "Number of Local Farms", title = paste(target_period, "Demographics")) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black", size = 9),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   panel.grid.major.y = ggplot2::element_blank())
  
  p_bwp <- ggplot2::ggplot(agg_data, ggplot2::aes(x = production_system_fct, y = total / 1000, fill = bwp_status)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$disparity_bwp) +
    ggplot2::labs(x = NULL, y = expression("Total Volumetric BWP (10"^3~"m"^3*"/year)"), title = "Net Watershed Impact") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   axis.ticks.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.y = ggplot2::element_blank(),
                   legend.position = "bottom")
  
  p_combined <- p_count + p_bwp + patchwork::plot_layout(widths = c(1.4, 1))
  return(p_combined)
}