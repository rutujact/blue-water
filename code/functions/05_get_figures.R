
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
# GLOBAL COLOR PALETTE CONFIGURATION (OKABE-ITO ACCESSIBILITY) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This centralized list uses the perceptually uniform, colorblind-safe 
# Okabe-Ito palette. It strictly maps colors to their hydrological impact:
#
# Vermillion (#D55E00)   : Deficit drivers (Irrigated Crops, Negative BWP)
# Sky Blue (#56B4E9)     : Rain-driven systems (Rainfall, Rainfed Crops)
# Bluish-Green (#009E73) : Natural Recharge Buffers (Grazed Areas)
# Dark Blue (#0072B2)    : Deep Physical Water (Actual ET, Net Groundwater)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

BWP_PALETTES <- list(
  # 1. Climate Profile (Supply vs. Demand)
  climate_fill    = c("Rainfall" = "#56B4E9", "ET0" = "#D55E00"),         
  climate_color   = c("Rainfall" = "#0072B2", "ET0" = "#D55E00",          
                      "Temperature_Line" = "#CC79A7"),                    
  climate_ribbon  = "#CC79A7",                                            
  
  # 2. Irrigation Types (Annual Climatology)
  irrigation      = c("Rainfed" = "#56B4E9", "Irrigated" = "#D55E00"),    
  
  # 3. Monthly Blue Water Potential (BWP) Envelopes
  bwp_fill        = c("Below axis" = "#F0E442", "Above axis" = "#56B4E9"), 
  bwp_color       = c("TRUE" = "#0072B2", "FALSE" = "#D55E00"),           
  
  # 4. Socio-Economic Land Use Contributions
  land_use        = c("irrigated cropped area" = "#D55E00",               
                      "rainfed cropped area"   = "#56B4E9",               
                      "grazed area"            = "#009E73"),    
  
  # 5. Farm Disparity Dashboard
  # Add this right below the "land_use" variable in BWP_PALETTES
  farm_category   = c("Irrigated" = "#D55E00", "Rainfed" = "#56B4E9", "Pastoralist" = "#009E73")
  
  
  # 6. Sankey & Network D3 Flows
  sankey_domain   = '["Rain", "Irrigated", "Rainfed", "Grazed", "BWP", "Flow_Rain"]',
  sankey_range    = '["#56B4E9", "#D55E00", "#56B4E9", "#009E73", "#0072B2", "#56B4E9"]',
  
  # 7. Village Flow Charts / Water Tank (Figure 7)
  flow_box_line   = "#0072B2",                              
  flow_box_fill   = "#0072B2",                              
  flow_box_diff   = "#E5F5FA",                              
  flow_drop_text  = "#D55E00",                              
  # INFLOWS: Aggregated Recharge (Monochromatic Sky Blue gradient)
  flow_in_p1      = c("#56B4E9", "#C6E4F9"),                
  flow_in_p2      = c("#56B4E9", "#C6E4F9"),                
  # OUTFLOWS: Irrigated Extraction (Monochromatic Vermillion gradient)
  flow_out_p2     = c("#D55E00", "#F4B88E"),                
  flow_out_single = c("#CC79A7", "#EBB9D3")               
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 1: GLOBAL GRAPHICS THEMES
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Standardized Base Theme for ggplot2 Visualizations
#' @description A clean base theme removing unnecessary grid lines and setting a white panel background.
#' @export
theme_baseR <- ggplot2::theme_bw() +
  ggplot2::theme(
    panel.grid.major = ggplot2::element_blank(), 
    panel.grid.minor = ggplot2::element_blank(), 
    panel.background = ggplot2::element_rect(fill = "white", colour = "black"), 
    strip.background = ggplot2::element_blank()  
  )

#' Graphics Theme for Journal Manuscripts
#' @description Crisp vector, black and white optimized theme matching standard publication criteria.
#' @param base_size Numeric. Base font size for plots. Default is 11.
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
#' @description High-contrast, large-scale typography layouts tailored for projector screens or digital decks.
#' @param base_size Numeric. Base font size for text elements. Default is 14.
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
# SECTION 2: BASELINE AGRO-CLIMATOLOGY & CROP PARAMETERS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Plot Historical Climate Profile
#' @description Generates a publication-grade, dual-axis monthly weather profile tracking average 
#' and quantile intervals ($5\%$ to $95\%$) for Rainfall, Potential Evapotranspiration ($ET_0$), and Temperature.
#' @param climate_summary Dataframe containing columns: Month, Rainfall_avg, Rainfall_q05, 
#' Rainfall_q95, ET0_avg, ET0_q05, ET0_q95, Temperature_avg, Temperature_q05, Temperature_q95.
#' @param scale_factor Numeric. Scaling factor used to align secondary temperature axis. Default 7.5.
#' @return A ggplot object.
#' @export
plot_climate_profile <- function(climate_summary, scale_factor = 7.5) {
  climate_summary <- climate_summary %>%
    dplyr::mutate(Month_Name = forcats::fct_relevel(factor(month.abb[Month], levels = month.abb)))
  
  climate_summary_long <- climate_summary %>%
    dplyr::select(Month_Name, Rainfall_avg, ET0_avg) %>%
    tidyr::pivot_longer(cols = c(Rainfall_avg, ET0_avg), names_to = "Water_Metric", values_to = "Volume") %>%
    dplyr::mutate(Water_Metric = dplyr::recode(Water_Metric, "Rainfall_avg" = "Rainfall", "ET0_avg" = "ET0"))
  
  water_errors <- climate_summary %>%
    dplyr::select(Month_Name, Rainfall_q05, Rainfall_q95, ET0_q05, ET0_q95) %>%
    tidyr::pivot_longer(cols = -Month_Name, names_to = c("Water_Metric", ".value"), names_pattern = "(Rainfall|ET0)_(q05|q95)")
  
  climate_summary_long <- dplyr::left_join(climate_summary_long, water_errors, by = c("Month_Name", "Water_Metric"))
  
  ggplot2::ggplot() +
    ggplot2::geom_bar(data = climate_summary_long, ggplot2::aes(x = Month_Name, y = Volume, fill = Water_Metric), stat = "identity", position = ggplot2::position_dodge(width = 0.9), alpha = 0.85) +
    ggplot2::geom_errorbar(data = climate_summary_long, ggplot2::aes(x = Month_Name, ymin = q05, ymax = q95, group = Water_Metric), position = ggplot2::position_dodge(width = 0.9), width = 0.2, alpha = 0.7) +
    ggplot2::geom_ribbon(data = climate_summary, ggplot2::aes(x = Month_Name, ymin = Temperature_q05 * scale_factor, ymax = Temperature_q95 * scale_factor, group = 1), fill = BWP_PALETTES$climate_ribbon, alpha = 0.15) +
    ggplot2::geom_line(data = climate_summary, ggplot2::aes(x = Month_Name, y = Temperature_avg * scale_factor, color = "Temperature_Line", group = 1), linewidth = 1) +
    ggplot2::scale_y_continuous(name = expression(bold(paste("Rainfall & ET"[0], " (mm)"))), sec.axis = ggplot2::sec_axis(~. / scale_factor, name = "Temperature (°C)"), expand = ggplot2::expansion(mult = c(0, 0.05))) +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$climate_fill) +
    ggplot2::scale_color_manual(values = BWP_PALETTES$climate_color) +
    ggplot2::labs(x = "Month") +
    theme_manuscript() +
    ggplot2::theme(legend.position = "top", legend.box = "vertical", legend.spacing.y = ggplot2::unit(0.1, "cm"), axis.text = ggplot2::element_text(size = 12, color = "black")) +
    ggplot2::guides(fill = ggplot2::guide_legend(order = 1, nrow = 1), color = ggplot2::guide_legend(order = 2, nrow = 1, breaks = "Temperature_Line", labels = "Mean Temp", override.aes = list(linewidth = 2, shape = NA)))
}

#' Plot Crop Coefficient (Kc) Trajectory Curves
#' @description Maps the progressive changes in crop coefficients over the course of the Day of Year (DOY) split across rotations.
#' @param crop_systems_l_kc Dataframe containing columns: doy, Kc, crop, ncrop, and rotation.
#' @return A ggplot object.
#' @export
plot_crop_coefficient_curves <- function(crop_systems_l_kc) {
  ggplot2::ggplot(subset(crop_systems_l_kc, crop != "No Crop"), 
                  ggplot2::aes(x = doy, y = kc)) +
    ggplot2::geom_line(ggplot2::aes(color = crop, group = ncrop), linewidth = 0.8, show.legend = FALSE) +
    ggplot2::facet_wrap(. ~ rotation, ncol = 4, axes = "all_x") +
    ggplot2::xlab("Day of Year") + 
    theme_manuscript() +
    ggplot2::scale_y_continuous(name = expression(bold(paste("Crop Coefficient (K"[c], ")"))), limits = c(0, 1.3), breaks = c(0, 0.5, 1)) +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "transparent", color = "#cccccc", linewidth = 0.4),
                   strip.text = ggplot2::element_text(face = "bold", size = 8.5, color = "black", margin = ggplot2::margin(t = 3, b = 3)))
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 3: CROPPING SYSTEM LEVEL (TEMPORAL CLIMATOLOGY)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Process Annual Simulation Water Budgets
#' @description Combines structural list items from model executions, evaluates standard names, orders system rows 
#' by descending performance, and produces clean long and wide structures.
#' @param budget_ls Named list containing sub-elements with yearly dataframes `$summary.yr`.
#' @param crop_systems_df Mapping dataframe matching column `rotation` to names like `crop.system` and `Irrigation`.
#' @return A list containing `summary_yr_crop` (long format) and `summ_qs_cs` (wide quantile formatted table).
#' @export
process_annual_budget <- function(budget_ls, crop_systems_df) {
  summary_yr_ls <- lapply(budget_ls, function(x) x$summary.yr)
  
  summary_yr_crop <- dplyr::bind_rows(summary_yr_ls, .id = "rotation") %>%
    dplyr::left_join(crop_systems_df, by = "rotation") %>%
    dplyr::mutate(var.plot = forcats::fct_recode(factor(var), 
                                                 "Precipitation (mm)" = "P", "Precipitation - Runoff (mm)" = "Peff", 
                                                 "Effective Precip Used" = "Peff.used", "Runoff (mm)" = "Q", 
                                                 "AET (mm)" = "aet", "Crop ET (mm)" = "aet.crop", 
                                                 "ET:Effective Rainfall" = "aet.to.Peff", "ET Supply:Demand" = "aet.to.etc.crop", 
                                                 "Blue Water Potential (mm)" = "balance", "Potential ET (mm)" = "et0", 
                                                 "Potential Crop ET (mm)" = "etc", "etc.crop (mm)" = "etc.crop", 
                                                 "Irrigation Requirement (mm)" = "irrigation", "Groundwater Recharge (mm)" = "recharge", 
                                                 "% Runoff" = "runoff.percent", "% Soil Moisture" = "sm.percent"))
  
  summ_qs_cs <- summary_yr_crop %>%
    dplyr::group_by(crop.system, Irrigation, var, var.plot, quantile) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value)
  
  order_logic <- summ_qs_cs %>% 
    dplyr::filter(var == "balance") %>% 
    dplyr::arrange(mean)
  
  summ_qs_cs$crop.system <- factor(summ_qs_cs$crop.system, levels = order_logic$crop.system)
  
  return(list(summary_yr_crop = summary_yr_crop, summ_qs_cs = summ_qs_cs))
}

#' Plot Annual Climatology Metrics
#' @description Draws horizontal column charts representing mean performance coupled with 95% Confidence Intervals.
#' @param data Wide summary dataset outputted by `process_annual_budget` (`$summ_qs_cs`).
#' @param vars_to_plot Character vector listing standardized clean string metrics to display.
#' @param x_var Character string referencing the statistical focus metric to plot on X-axis. Default is `"mean"`.
#' @param title_x Character title string label for the X-axis. Default is `"Mean (95 CI)"`.
#' @return A ggplot object.
#' @export
plot_annual_climatology <- function(data, vars_to_plot, x_var = "mean", title_x = "Mean (95 CI)") {
  
  plot_data <- data %>% 
    dplyr::filter(var.plot %in% vars_to_plot) %>%
    dplyr::mutate(var.plot = factor(var.plot, levels = vars_to_plot))
  
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(y = crop.system)) +
    ggplot2::facet_grid(. ~ var.plot, scales = "free_x") +
    ggplot2::geom_col(ggplot2::aes(x = .data[[x_var]], fill = Irrigation)) +
    ggplot2::geom_errorbar(ggplot2::aes(xmax = `0.975`, xmin = `0.025`), width = 0.2, color = "gray30") +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$irrigation) +    
    ggplot2::labs(x = title_x, y = "Cropping System") +
    theme_baseR + 
    ggplot2::theme(legend.title = ggplot2::element_blank())
  return(p)
}

#' Process Monthly Climatology Over Long-Term Simulations
#' @description Maps internal monthly trajectories into precise quantile boundaries and averages values across matching crop profiles.
#' @param budget_ls Named list containing sub-elements with monthly tables `$sum.mo`.
#' @param crop_systems_df Frame matching rotation tags to system metadata labels.
#' @return Dataframe summarizing average quantile points per month for each distinct cropping arrangement.
#' @export
process_monthly_climatology <- function(budget_ls, crop_systems_df) {
  add_clim_mo <- function(x) {
    sum_mo <- x[["sum.mo"]] %>%
      dplyr::mutate(plot.recharge = -recharge,
                    plot.irri = dplyr::if_else(irrigation == 0, NA_real_, irrigation),
                    plot.aet = dplyr::if_else(aet == 0, NA_real_, aet),
                    plot.etc = dplyr::if_else(etc == 0, NA_real_, etc))
    
    quantile_df <- function(v, probs = c(0.025, 0.5, 0.975)) {
      tibble::tibble(quantile = probs, value = quantile(v, probs, na.rm = TRUE))
    }
    
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
  
  budget_updated <- lapply(budget_ls, add_clim_mo)
  clim_mo_ls <- lapply(budget_updated, function(x) x$clim.mo)
  clim_crop <- dplyr::bind_rows(clim_mo_ls, .id = "rotation") %>%
    dplyr::left_join(crop_systems_df, by = "rotation")
  
  clim_crop_cs <- clim_crop %>%
    dplyr::group_by(var, crop.system, Irrigation, month) %>%
    dplyr::summarise(dplyr::across(q025:q975, mean, na.rm = TRUE), .groups = "drop")
  
  return(clim_crop_cs)
}

#' Plot Monthly Blue Water Potential Facets
#' @description Renders multi-panel seasonal envelopes highlighting median tracking boundaries along with 95% CI bands.
#' @param clim_crop_cs Aggregated monthly summary dataframe outputted by `process_monthly_climatology`.
#' @param years_range_text Character string denoting simulation duration limits for text layout labels.
#' @return A ggplot object.
#' @export
plot_monthly_bwp <- function(clim_crop_cs, years_range_text) {
  bwp_data <- clim_crop_cs %>%
    dplyr::filter(var == "balance") %>%
    dplyr::mutate(above_axis = median >= 0)
  
  ggplot2::ggplot(bwp_data, ggplot2::aes(x = month, group = 1)) +
    ggplot2::facet_wrap(~crop.system, scales = "fixed") +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = pmin(q025, 0), ymax = 0, fill = "Below axis"), alpha = 0.5) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = 0, ymax = pmax(q975, 0), fill = "Above axis"), alpha = 0.5) +
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


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 4: PRODUCTION SYSTEM LEVEL (SOCIO-ECONOMIC AGGREGATION)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Compute Area-Weighted Production System Budgets
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
  
  # 1. Combine datasets and classify land types cleanly based on rotation names/irrigation
  ps_annual_data <- budget_yr_l %>%
    dplyr::select(rotation, crop.system, Irrigation, year, var, value) %>%
    dplyr::left_join(ps_by_cs_area, by = "crop.system", relationship = "many-to-many") %>%
    dplyr::filter(!is.na(production_system_period)) %>%
    dplyr::mutate(
      land_type = dplyr::case_when(
        stringr::str_detect(rotation, "(?i)grassland|grazed") | 
          stringr::str_detect(crop.system, "(?i)grassland|grazed") ~ "grazed",
        Irrigation == "Irrigated" ~ "irrigated",
        TRUE ~ "rainfed"
      )
    )
  
  # 2. Compute true area-weighted depth for each production system per year
  ps_weighted_annual <- ps_annual_data %>%
    dplyr::group_by(production_system, time_period, Irrigation, year, var, crop.system) %>%
    dplyr::mutate(rotations_count = dplyr::n_distinct(rotation)) %>%
    dplyr::group_by(production_system, time_period, Irrigation, year, var) %>%
    dplyr::summarise(
      weighted_value = sum(value * (area_ha / rotations_count), na.rm = TRUE) / 
        sum(area_ha / rotations_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 3. Calculate true final Quantiles over the timeline data
  ps_budget_summary <- ps_weighted_annual %>%
    dplyr::group_by(production_system, time_period, Irrigation, var) %>%
    dplyr::summarise(
      mean = mean(weighted_value, na.rm = TRUE),
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
    dplyr::arrange(time_period, production_system)
  
  # 4. Generate the accurate, area-weighted land contribution matrix for ALL Variables
  ps_total_areas <- ps_by_cs_area %>%
    dplyr::group_by(production_system, time_period) %>%
    dplyr::summarise(total_system_area = sum(area_ha, na.rm = TRUE), .groups = "drop")
  
  ps_volumetric_summary <- ps_annual_data %>%
    dplyr::group_by(production_system, time_period, var, land_type, year, crop.system) %>%
    dplyr::mutate(rotations_count = dplyr::n_distinct(rotation)) %>%
    dplyr::group_by(production_system, time_period, var, land_type, year) %>%
    dplyr::summarise(
      type_volumetric_depth = sum(value * (area_ha / rotations_count), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::left_join(ps_total_areas, by = c("production_system", "time_period")) %>%
    dplyr::mutate(weighted_contribution = type_volumetric_depth / total_system_area) %>%
    dplyr::group_by(production_system, time_period, var, land_type) %>%
    dplyr::summarise(mean_contribution = mean(weighted_contribution, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = land_type, values_from = mean_contribution, values_fill = 0) %>%
    dplyr::mutate(
      mean_mm = rowSums(dplyr::across(dplyr::any_of(c("rainfed", "irrigated", "grazed"))), na.rm = TRUE)
    ) %>%
    dplyr::left_join(ps_total_areas, by = c("production_system", "time_period")) %>%
    dplyr::mutate(
      mean_m3 = mean_mm * total_system_area * 10
    )
  
  has_farm_counts <- !is.null(farms_count_df)
  if (has_farm_counts) {
    ps_volumetric_summary <- ps_volumetric_summary %>%
      dplyr::left_join(
        farms_count_df %>% dplyr::select(production_system, time_period, number_of_farms),
        by = c("production_system", "time_period")
      ) %>%
      dplyr::mutate(
        number_of_farms = tidyr::replace_na(number_of_farms, 0),
        total_vol_m3 = mean_m3 * number_of_farms
      ) %>%
      dplyr::select(time_period, production_system, var, mean_mm, 
                    dplyr::any_of(c("rainfed", "irrigated", "grazed")), 
                    mean_m3, number_of_farms, total_vol_m3)
  } else {
    ps_volumetric_summary <- ps_volumetric_summary %>%
      dplyr::select(time_period, production_system, var, mean_mm, 
                    dplyr::any_of(c("rainfed", "irrigated", "grazed")), mean_m3)
  }
  
  ps_volumetric_summary <- ps_volumetric_summary %>% dplyr::arrange(time_period, production_system, var)
  ps_bwp_volumetric <- ps_volumetric_summary %>% dplyr::filter(var == "balance")
  
  return(list(ps_budget_summary = ps_budget_summary, 
              ps_volumetric_summary = ps_volumetric_summary,
              ps_bwp_volumetric = ps_bwp_volumetric))
}

#' Plot Socio-Economic Blue Water Potential Divergence
#' @description Generates a colorblind-safe stacked chart showing components 
#' of different land type splits against net farm water depths.
#' @param ps_bwp_volumetric Evaluated dataframe element tracking volumetric balance fields.
#' @return A ggplot object with horizontal coordinates inverted.
#' @export
plot_socio_economic_bwp <- function(ps_bwp_volumetric) {
  ps_bwp_volumetric <- ps_bwp_volumetric %>%
    dplyr::mutate(production_system = dplyr::if_else(time_period == "1950s" & production_system == "Pastoralists", "Landless pastoralists", production_system)) %>%
    dplyr::mutate(production_system = factor(production_system, levels = rev(unique(production_system)))) 
  
  ps_bwp_long <- ps_bwp_volumetric %>%
    dplyr::select(time_period, production_system, rainfed, irrigated, grazed) %>%
    tidyr::pivot_longer(cols = c(rainfed, irrigated, grazed), names_to = "contribution_source", values_to = "bwp_value") %>%
    dplyr::mutate(contribution_source = factor(contribution_source, levels = c("irrigated", "rainfed", "grazed"),
                                               labels = c("irrigated cropped area", "rainfed cropped area", "grazed area")))
  
  ggplot2::ggplot() +
    ggplot2::geom_col(data = ps_bwp_long, ggplot2::aes(x = production_system, y = bwp_value, fill = contribution_source), width = 0.55) +
    ggplot2::geom_point(data = ps_bwp_volumetric, ggplot2::aes(x = production_system, y = mean_mm, shape = "Net Farm BWP"), size = 2.5, stroke = 1) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.8) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::scale_fill_manual(name = "Contribution Source", 
                               values = BWP_PALETTES$land_use) +          
    ggplot2::scale_shape_manual(name = NULL, values = c("Net Farm BWP" = 4)) +
    ggplot2::scale_y_continuous(labels = function(x) format(x, big.mark = " ")) +
    ggplot2::labs(y = "Farm Blue Water Potential (mm/year)", x = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_line(color = "gray90"),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(color = "black", hjust = 0, vjust = 0.5),
      legend.position = "right",
      legend.background = ggplot2::element_rect(fill = "white", color = "gray80")
    ) +
    ggplot2::geom_vline(xintercept = 15.5, linetype = "dashed", color = "gray50") +
    ggplot2::annotate("text", x = 17, y = -800, label = "1950s", fontface = "bold", hjust = 0) +
    ggplot2::annotate("text", x = 8, y = -800, label = "2022", fontface = "bold", hjust = 0) +
    ggplot2::annotate("text", x = 19, y = 400, label = "       Production Systems", fontface = "bold", hjust = 0) +
    ggplot2::coord_flip(clip = "off")
}

#' Plot Regional Disparity Metrics (Farms Count vs Net Volumetric Footprint)
#' @description Merges side-by-side distribution plots evaluating regional demographic weightings 
#' alongside true absolute volumetric footprints, color-coded by agricultural intensity.
#' @param ps_bwp_volumetric Frame detailing total farm volume totals and counts.
#' @return A patchwork layout merging distribution column frames.
#' @export
plot_farm_bwp_disparity <- function(ps_bwp_volumetric, file_prefix = "Fig6_Farm_Disparity") {
  
  # 1. Clean data and define objective categories
  clean_data <- ps_bwp_volumetric %>%
    dplyr::filter(time_period == "2022") %>%
    dplyr::mutate(
      category = dplyr::case_when(
        stringr::str_detect(production_system, "(?i)fruit|vegetable|rice") ~ "Irrigated",
        stringr::str_detect(production_system, "(?i)rainfed|redgram")      ~ "Rainfed",
        TRUE                                                               ~ "Pastoralist"
      ),
      short_label = stringr::str_remove_all(production_system, "\\s*\\(.*?\\)")
    )
  
  # 2. Force the correct order: Aggregate first, then extract levels
  # This ensures the sorting is based on total_vol_m3, not row order
  level_order <- clean_data %>%
    dplyr::group_by(short_label) %>%
    dplyr::summarise(total = sum(total_vol_m3)) %>%
    dplyr::arrange(total) %>%
    dplyr::pull(short_label)
  
  clean_data <- clean_data %>%
    dplyr::mutate(production_system = factor(short_label, levels = level_order))
  
  # 3. Plotting Demographic Distribution (Left Panel)
  p_count <- ggplot2::ggplot(clean_data, ggplot2::aes(x = production_system, y = number_of_farms, fill = category)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$farm_category) + # <--- Centralized Palette Call
    ggplot2::labs(x = "Production System", y = "Number of Local Farms", title = "Demographic Distribution") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_text(color = "black", size = 9),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   legend.position = "none")
  
  # 4. Plotting Net Watershed Impact (Right Panel)
  p_bwp <- ggplot2::ggplot(clean_data, ggplot2::aes(x = production_system, y = total_vol_m3 / 1000, fill = category)) +
    ggplot2::geom_col(width = 0.6) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = BWP_PALETTES$farm_category) + # <--- Centralized Palette Call
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Total Volumetric BWP (10³ m³/year)", title = "Net Watershed Impact") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.y = ggplot2::element_blank(),
                   plot.title = ggplot2::element_text(face = "bold", size = 11),
                   legend.title = ggplot2::element_blank())
  
  # 5. Combine with Patchwork
  p_combined <- p_count + p_bwp + 
    patchwork::plot_layout(widths = c(1.4, 1), guides = "collect") & 
    ggplot2::theme(legend.position = "bottom")
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # EXPORT FIGURES
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!dir.exists("figures")) { dir.create("figures", recursive = TRUE) }
  
  # Save as PDF
  ggplot2::ggsave(
    filename = paste0("figures/", file_prefix, ".pdf"), 
    plot = p_combined,  
    width = 11, 
    height = 6, 
    units = "in"
  )
  
  # Save as PNG
  ggplot2::ggsave(
    filename = paste0("figures/", file_prefix, ".png"), 
    plot = p_combined,  
    width = 11, 
    height = 6, 
    units = "in", 
    dpi = 300  
  )
  
  return(p_combined)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SECTION 5: LANDSCAPE & VILLAGE SCALE FLOW DIAGRAMS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#' Main Dispatcher for Village Water Balance Sankey Diagrams
#' @description Assesses structural configurations and calls comparative double layouts or singular view layouts.
#' @param village_bwp_collapsed Aggregated long-format mass balance dataframe.
#' @param cfg List configuring chart visual elements (widths, arrow margins, line steps).
#' @param output_dev Graphic rendering function engine. Defaults to `grDevices::cairo_pdf`.
#' @param filename Target string designating output file location pathing. Default is `"figures/manuscript/Fig.6.pdf"`.
#' @param manuscript_style Logical flag adjusting drawing colors to match manuscript print requirements. Default FALSE.
#' @export
plot_village_bwp <- function(village_bwp_collapsed, cfg, output_dev = grDevices::cairo_pdf, filename = "figures/manuscript/Fig.6.pdf", manuscript_style = FALSE) {
  unique_periods <- unique(village_bwp_collapsed$period)
  num_periods <- length(unique_periods)
  if (num_periods == 1) {
    get_single_village_plot(village_bwp_collapsed, cfg, output_dev, filename)
  } else if (num_periods == 2) {
    get_village_comparison_plot(village_bwp_collapsed, cfg, output_dev, filename, manuscript_style = manuscript_style)
  }
}

#' Draw Double Panel Comparative Village Flow Chart
#' @description Builds an intricate, publication-ready two-panel canvas highlighting evolution or shifts across time periods.
#' @param village_bwp_collapsed Frame storing internal aggregated mass values.
#' @param cfg List configuring arrow margins, widths, line steps.
#' @param output_dev Graphic output device constructor function.
#' @param filename Output destination string path.
#' @param manuscript_style Logical flag adjusting visual markers.
#' @keywords internal
get_village_comparison_plot <- function(village_bwp_collapsed, cfg, output_dev = grDevices::cairo_pdf,  
                                        filename = "figures/manuscript/Fig.6.pdf", manuscript_style = FALSE) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  if (identical(output_dev, grDevices::cairo_pdf)) {
    output_dev(file = filename, width = 10.0, height = 4.2, family = "Helvetica")
  } else {
    output_dev(filename = filename, width = 4000, height = 1680, res = 300, pointsize = 18)
  }
  par(mfrow = c(1, 2), mar = c(1.0, 3.5, 1.5, 7.5), oma = c(0, 0, 1.4, 1))
  
  unique_periods <- unique(village_bwp_collapsed$period)
  p1 <- unique_periods[1]; p2 <- unique_periods[2]
  df_p1 <- village_bwp_collapsed[village_bwp_collapsed$period == p1, ]
  df_p2 <- village_bwp_collapsed[village_bwp_collapsed$period == p2, ]
  
  tot_p1 <- sum(df_p1$val[df_p1$val >= 0]) - sum(abs(df_p1$val[df_p1$val < 0]))
  tot_p2 <- sum(df_p2$val[df_p2$val >= 0]) - sum(abs(df_p2$val[df_p2$val < 0]))
  max_tot <- max(tot_p1, tot_p2)
  abs_drop <- tot_p1 - tot_p2
  pct_change <- -(abs_drop / tot_p1) * 100
  
  get_flow_colors <- function(n, base_colors) { 
    if(n <= 1) return(base_colors[1])
    grDevices::colorRampPalette(base_colors)(n) 
  }
  
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
  
  box_xmin <- -10; box_xmax <- box_xmin + cfg$box_width ; box_ymax <- 12000
  fill_y_p1 <- box_ymax * (tot_p1 / max_tot); fill_y_p2 <- box_ymax * (tot_p2 / max_tot)
  plot_xlim <- c(-100, 135) ; plot_ylim <- c(-3000, 25500)
  
  # --- Panel A ---
  df_in_p1 <- df_p1[df_p1$val >= 0, ][order(-df_p1$val[df_p1$val >= 0]), ]
  plot(NULL, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  mtext("A", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)
  
  rect(box_xmin, 0, box_xmax, box_ymax, col = NA, border = BWP_PALETTES$flow_box_line, lwd = 1.5)
  rect(box_xmin, 0, box_xmax, fill_y_p1, col = BWP_PALETTES$flow_box_fill, border = NA)
  text((box_xmin + box_xmax)/2, fill_y_p1 * 0.65, "Blue Water\nPotential", col = "white", font = 2, cex = 0.85)
  text((box_xmin + box_xmax)/2, fill_y_p1 * 0.32, format(tot_p1, big.mark=","), col = "white", font = 1, cex = 1.1)
  
  n_in_p1 <- nrow(df_in_p1)
  if(n_in_p1 > 0) {
    cols <- get_flow_colors(n_in_p1, BWP_PALETTES$flow_in_p1) 
    y_s  <- seq(16000, 24500, length.out = n_in_p1) 
    x_d  <- seq(box_xmin + 6, box_xmin + (box_xmax - box_xmin)*8/10, length.out = n_in_p1) 
    for(i in 1:n_in_p1) {
      lwd_val <- scales::rescale(df_in_p1$val[i], to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(village_bwp_collapsed$val)))
      lines(c(-55, x_d[i], x_d[i]), c(y_s[i], y_s[i], box_ymax + (lwd_val * 75)), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_d[i] - (lwd_val * 1.4), x_d[i] + (lwd_val * 1.4), x_d[i]), c(box_ymax + (lwd_val * 75), box_ymax + (lwd_val * 75), box_ymax), col = cols[i], border = NA)
      draw_split_label(-60, y_s[i], df_in_p1$label[i], df_in_p1$val[i], 1)
    }
  }
  text((box_xmin + box_xmax)/2, -2000, paste0(p1, " Archetype Village"), font = 2, cex = 0.95, xpd = NA)
  
  # --- Panel B ---
  df_in_p2  <- df_p2[df_p2$val >= 0, ][order(-df_p2$val[df_p2$val >= 0]), ]
  df_out_p2 <- df_p2[df_p2$val < 0, ][order(abs(df_p2$val[df_p2$val < 0]), decreasing = TRUE), ]
  plot(NULL, xlim = plot_xlim, ylim = plot_ylim, xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "")
  mtext("B", side = 3, line = 0.5, adj = 0, font = 2, cex = 1.4)
  
  if (!manuscript_style) {
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
  
  n_in_p2 <- nrow(df_in_p2)
  if(n_in_p2 > 0) {
    cols <- get_flow_colors(n_in_p2, BWP_PALETTES$flow_in_p2)
    y_s  <- seq(16000, 24500, length.out = n_in_p2)
    x_d  <- seq(box_xmin + 6, box_xmin + (box_xmax - box_xmin)*8/10, length.out = n_in_p2)
    target_y <- if(!manuscript_style) box_ymax else fill_y_p2 
    for(i in 1:n_in_p2) {
      lwd_val <- scales::rescale(df_in_p2$val[i], to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(village_bwp_collapsed$val)))
      lines(c(-55, x_d[i], x_d[i]), c(y_s[i], y_s[i], target_y + (lwd_val * 75)), lwd = lwd_val, col = cols[i], lend = 1)
      polygon(c(x_d[i] - (lwd_val * 1.4), x_d[i] + (lwd_val * 1.4), x_d[i]), c(target_y + (lwd_val * 75), target_y + (lwd_val * 75), target_y), col = cols[i], border = NA)
      draw_split_label(-60, y_s[i], df_in_p2$label[i], df_in_p2$val[i], 1)
    }
  }
  
  n_out_p2 <- nrow(df_out_p2)
  if(n_out_p2 > 0) {
    cols <- get_flow_colors(n_out_p2, BWP_PALETTES$flow_out_p2) 
    for(i in 1:n_out_p2) {
      abs_val <- abs(df_out_p2$val[i])
      lwd_val <- scales::rescale(abs_val, to = c(cfg$min_arrow_w, cfg$max_arrow_w), from = c(0, max(abs(village_bwp_collapsed$val))))
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
#' @description Draws a standalone static landscape mass balance diagram when comparison timelines are missing.
#' @param df Longitudinal input data subset.
#' @param cfg Visual dimensions configuration settings list.
#' @param output_dev Graphic output engine device.
#' @param filename String layout path pathing.
#' @keywords internal
get_single_village_plot <- function(df, cfg, output_dev, filename) {
  dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE)
  if (identical(output_dev, grDevices::cairo_pdf)) {
    output_dev(file = filename, width = 6.0, height = 5.0, family = "Helvetica")
  } else {
    output_dev(filename = filename, width = 2400, height = 2000, res = 300, pointsize = 18)
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
    y_s  <- seq(16000, 24500, length.out = n_in) 
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
      y_start <- box_ymax * (i / (n_out + 1)) 
      
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
#' @description Compiles an interactive D3-based Javascript Sankey diagram mapping 
#' absolute volumes split from rain inputs to outflux components.
#' @param ps_volumetric_summary Structured long frame compiled by `get_production_system_budgets`.
#' @param target_period Character string identifying the focus slice (e.g. `"2022"`, `"1950s"`).
#' @param font_size Numeric. Typography element baseline size for labels. Default 13.
#' @return An interactive htmlwidget layout structure.
#' @export
get_d3_sankey <- function(ps_volumetric_summary, target_period, font_size = 13) {
  tp_data <- ps_volumetric_summary %>% 
    dplyr::filter(time_period == target_period) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(
      total_m3     = sum(total_vol_m3, na.rm = TRUE),
      irrigated_m3 = sum(irrigated * (total_vol_m3 / mean_mm), na.rm = TRUE),
      rainfed_m3   = sum(rainfed * (total_vol_m3 / mean_mm), na.rm = TRUE),
      grazed_m3    = sum(grazed * (total_vol_m3 / mean_mm), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_m3"), ~ ifelse(is.nan(.) | is.na(.), 0, .)))
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ANNOTATION: CUSTOM ROUNDING LOGIC
  # Goal: Round to nearest 10,000 m³ but express in thousands (k m³ / 10³ m³).
  # Math: (Raw / 10000) rounded * 10
  # Example: 5,127,450 -> 512.745 -> round(513) -> 513 * 10 = 5130 k m³
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  round_to_10k_in_thousands <- function(val) {
    return(round(val / 10000) * 10)
  }
  
  p_vol   = round_to_10k_in_thousands(sum(tp_data$total_m3[tp_data$var == "P"], na.rm = TRUE))
  bwp_vol = round_to_10k_in_thousands(sum(tp_data$total_m3[tp_data$var == "balance"], na.rm = TRUE))
  
  aet_row      <- tp_data %>% dplyr::filter(var == "aet")
  aet_tot      <- sum(aet_row$total_m3, na.rm = TRUE)
  
  et_rainfed   <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$rainfed_m3)) else 0
  et_irrigated <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$irrigated_m3)) else 0
  et_grazed    <- if(aet_tot > 0) round_to_10k_in_thousands(sum(aet_row$grazed_m3)) else 0
  
  # Balance equation to ensure Inflows == Outflows
  # Rainfed acts as the buffer to absorb any minor rounding discrepancies
  total_out <- bwp_vol + et_rainfed + et_irrigated + et_grazed
  if (p_vol != total_out && total_out > 0) { 
    et_rainfed <- et_rainfed + (p_vol - total_out) 
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # EXPLICIT ORDERING: 0=Precip, 1=Irrigated, 2=Rainfed, 3=Grazed, 4=BWP
  nodes_df <- data.frame(
    name = c(
      paste0("Precipitation|", format(p_vol, big.mark=","), " k m³"),
      paste0("ET: Irrigated Crops|", format(et_irrigated, big.mark=","), " k m³"),
      paste0("ET: Rainfed Crops|", format(et_rainfed, big.mark=","), " k m³"),
      paste0("ET: Grazed Areas|", format(et_grazed, big.mark=","), " k m³"),
      paste0("Net Recharge |", format(bwp_vol, big.mark=","), " k m³")
    ),
    group = c("Rain", "Irrigated", "Rainfed", "Grazed", "BWP")
  )
  
  # Match outflow ordering to nodes_df order
  outflow_vols <- c(et_irrigated, et_rainfed, et_grazed, bwp_vol)
  outflow_indices <- 1:4 
  
  links_list <- list()
  for (i in seq_along(outflow_vols)) {
    if (outflow_vols[i] > 0) {
      links_list[[i]] <- data.frame(source = 0, target = outflow_indices[i], value = outflow_vols[i], group = "Flow_Rain")
    }
  }
  links_df <- do.call(rbind, links_list)
  
  # MAP COLORS TO NEW ORDER dynamically
  color_scale <- paste0('d3.scaleOrdinal()
    .domain(', BWP_PALETTES$sankey_domain, ')
    .range(', BWP_PALETTES$sankey_range, ')')
  
  sankey <- networkD3::sankeyNetwork(
    Links = links_df, Nodes = nodes_df, Source = "source", Target = "target",
    Value = "value", NodeID = "name", NodeGroup = "group", LinkGroup = "group",
    units = "k m³", fontSize = font_size, nodeWidth = 30, nodePadding = 40, 
    colourScale = color_scale, sinksRight = TRUE, 
    width = 720, height = 480, iterations = 0,
    margin = list(top = 25, left = 130, right = 140, bottom = 25)
  )
  
  sankey <- htmlwidgets::onRender(sankey, '
    function(el, x) {
      // --- 1. Custom Typography & Multi-line Labels ---
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

      // --- 2. Fix the Straight Top Link (Force Curvature) ---
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