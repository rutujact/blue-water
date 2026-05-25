# ===
# title: "Functions to get Key figures Blue Water Potential (Recharge - Irrigation) for each crop rotation"
# author: "Rutuja Chitra-Tarak"
# assistance: Google Gemini 3
# date: "2026-05-09"
# ===

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# WATER BUDGETING UTILITY FUNCTIONS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add ggplot theme to use ----
theme_baseR <- theme_bw() +
  theme(
    panel.grid.major = element_blank(), # Remove major grid lines
    panel.grid.minor = element_blank(), # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = "black"), # Ensure white background and add a border
    strip.background = element_blank() # Remove background from facet labels if applicable
  )

#' Process Annual Water Budget Data
#' @description Merges budget lists with metadata and prepares climatology summaries.
process_annual_budget <- function(budget_ls, crop_systems_df) {
  # Bind annual summaries and join with crop system metadata
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
  
  # Calculate mean climatology across quantiles
  summ_qs_cs <- summary_yr_crop %>%
    dplyr::group_by(crop.system, Irrigation, var, var.plot, quantile) %>%
    dplyr::summarise(value = mean(value), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = quantile, values_from = value)
  
  # Logic to order cropping systems by the 'balance' (BWP) mean
  order_logic <- summ_qs_cs %>% 
    dplyr::filter(var == "balance") %>% 
    dplyr::arrange(mean)
  
  summ_qs_cs$crop.system <- factor(summ_qs_cs$crop.system, 
                                   levels = order_logic$crop.system)
  
  return(list(summary_yr_crop = summary_yr_crop, summ_qs_cs = summ_qs_cs))
}

#' Plot Annual Climatology
#' @description Generates the faceted column plots for supply/demand or BWP.
plot_annual_climatology <- function(data, vars_to_plot, x_var = "mean", title_x = "Mean (95 CI)") {
  
  plot_data <- data %>% 
    dplyr::filter(var.plot %in% vars_to_plot) %>%
    dplyr::mutate(var.plot = factor(var.plot, levels = vars_to_plot))
  
  p <- ggplot(plot_data, aes(y = crop.system)) +
    facet_grid(. ~ var.plot, scales = "free_x") +
    geom_col(aes(x = .data[[x_var]], fill = Irrigation)) +
    geom_errorbar(aes(xmax = `0.975`, xmin = `0.025`), width = 0.2, color = "gray30") +
    scale_fill_manual(values = c("Rainfed" = "yellow3", "Irrigated" = "skyblue")) +
    labs(x = title_x, y = "Cropping System") +
    theme_baseR + 
    theme(legend.title = element_blank())
  
  return(p)
}

#' Process Monthly Climatology
#' @description Transforms budget data into monthly medians and 95% CI.
process_monthly_climatology <- function(budget_ls, crop_systems_df) {
  
  # Helper to add climatology to each list element
  add_clim_mo <- function(x) {
    sum_mo <- x[["sum.mo"]] %>%
      mutate(plot.recharge = -recharge,
             plot.irri = if_else(irrigation == 0, NA, irrigation),
             plot.aet = if_else(aet == 0, NA, aet),
             plot.etc = if_else(etc == 0, NA, etc))
    
    quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
      tibble(quantile = probs, value = quantile(x, probs, na.rm = TRUE))
    }
    
    clim_mo <- sum_mo %>%
      select(-year) %>%
      pivot_longer(-month, names_to = "var", values_to = "value") %>%
      group_by(var, month) %>%
      reframe(quantile_df(value)) %>%
      pivot_wider(names_from = "quantile", values_from = "value") %>%
      rename(q025 = `0.025`, q975 = `0.975`, median = `0.5`)
    
    x[["clim.mo"]] <- clim_mo
    return(x)
  }
  
  budget_updated <- lapply(budget_ls, add_clim_mo)
  
  # Combine and join with metadata
  clim_mo_ls <- lapply(budget_updated, function(x) x$clim.mo)
  clim_crop <- dplyr::bind_rows(clim_mo_ls, .id = "rotation") %>%
    dplyr::left_join(crop_systems_df, by = "rotation")
  
  # Aggregate by crop system
  clim_crop_cs <- clim_crop %>%
    group_by(var, crop.system, Irrigation, month) %>%
    summarise(across(q025:q975, mean, na.rm = TRUE), .groups = "drop")
  
  return(clim_crop_cs)
}

#' Plot Monthly Blue Water Potential ----
#' @description Generates the faceted monthly BWP plot with dual-color ribbons 
#' (Blue for positive, Red for negative).
plot_monthly_bwp <- function(clim_crop_cs, years_range_text) {
  
  # 1. Prepare and Sort Data by Annual BWP (Sum of Medians)
  bwp_data <- clim_crop_cs %>%
    dplyr::filter(var == "balance") %>%
    dplyr::mutate(above_axis = median >= 0)

  # 2. Generate Plot with Dynamic Group-Based Ribbons
  ggplot(bwp_data, aes(x = month, group = 1)) +
    facet_wrap(~crop.system, scales = "fixed") +
    
    # Ribbon for area below the horizontal axis (y=0)
    geom_ribbon(aes(ymin = pmin(q025, 0), ymax = 0, fill = "Below axis"), alpha = 0.5) +
    # Ribbon for area above the horizontal axis (y=0)
    geom_ribbon(aes(ymin = 0, ymax = pmax(q975, 0), fill = "Above axis"), alpha = 0.5) +
  
    # Central median line matching the ribbon state
    geom_line(aes(y = median, color = above_axis, group = 1), linewidth = 1) +
    
    # Color Configurations matching rutuja.jpg
    scale_fill_manual(values = c("Below axis" = "lightpink", "Above axis" = "skyblue")) +
    # 
    # scale_color_manual(values = c("TRUE" = "skyblue", "FALSE" = "lightpink")) +
    
    # Axes and layout structures
    scale_x_continuous(breaks = 1:12, labels = as.character(1:12), limits = c(1, 12)) +
    geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
    theme_baseR +
    theme(
      legend.position = "none",
      strip.text = element_text(size = 8)
    ) +
    labs(
      y = "Blue Water Potential (mm) (median + 95 CI)", 
      x = "Month",
      title = paste0("Monthly Blue Water Potential for Various Cropping Systems in Anantapur (", years_range_text, ")")
    )
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# VILLAGE WATER BALANCE PLOTTING ENGINE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# --- INTERNAL INDIVIDUAL PLOTTER ---
# This function creates a single year's plot (either 1950 or 2022).
generate_village_plot <- function(df, year_key, global_y_max, cfg, display_labels) {
  
  df_in  <- df %>% filter(val >= 0) %>% arrange(desc(val))
  df_out <- df %>% filter(val < 0) %>% mutate(abs_val = abs(val)) %>% arrange(desc(abs_val)) 
  
  net_balance <- sum(df_in$val) - sum(df_out$abs_val)
  
  wrap_labels <- function(text, width = 24) {
    str_wrap(text, width = width)
  }
  
  box_xmin <- -1
  box_xmax <- box_xmin + cfg$box_width
  box_ymax <- 12000 
  
  box_color <- if(year_key == "1950") "#084594" else "#7f0000"
  box_title <- if(year_key == "1950") "Blue Water Potential" else "Water Deficit\n(Ecological Debt)"
  
  get_colors <- function(n, base_colors) {
    if(n <= 1) return(base_colors[1])
    colorRampPalette(base_colors)(n)
  }
  cols_in  <- get_colors(nrow(df_in), c("#084594", "#a1dab4"))   
  cols_out <- get_colors(nrow(df_out), c("#bd0026", "#fecc5c")) 
  
  # Influx Paths
  make_influx_coords <- function(i) {
    row <- df_in[i, ]
    y_start <- box_ymax + 3500 + (i * cfg$v_stagger_in) 
    x_drop  <- box_xmin + 1.5 + (i * 3) 
    data.frame(
      x = c(box_xmin - 5, x_drop, x_drop),
      y = c(y_start, y_start, box_ymax + 150),
      group = row$label, val = row$val, col = cols_in[i]
    )
  }
  
  # Outflux Paths (Staircase engine honoring vertical terminal alignment)
  make_outflux_coords <- function(i) {
    row <- df_out[i, ]
    x_spine <- box_xmax + ((nrow(df_out) - i) * 2.2)
    y_box_exit <- box_ymax * 0.15 + ((i - 1) * (box_ymax * 0.7 / max(1, nrow(df_out) - 1)))
    y_final_stagger <- box_ymax + 3500 + (i * cfg$v_stagger_out)
    x_end <- box_xmax + 24  # Shared fixed wall coordinate for arrow tips
    
    data.frame(
      x = c(box_xmax - 0.1, x_spine, x_spine, x_end),
      y = c(y_box_exit,  y_box_exit, y_final_stagger, y_final_stagger),
      group = row$label, val = row$abs_val, col = cols_out[i]
    )
  }
  
  paths_in  <- if(nrow(df_in) > 0) do.call(rbind, lapply(seq_len(nrow(df_in)), make_influx_coords)) else data.frame()
  paths_out <- if(nrow(df_out) > 0) do.call(rbind, lapply(seq_len(nrow(df_out)), make_outflux_coords)) else data.frame()
  
  # FIX: Uniform structural coordinates preserve 1:1 box width scaling and stop text truncation
  x_min_limit <- -45
  x_max_limit <- 95
  
  g <- ggplot() +
    # Draw central structural box
    annotate("rect", xmin = box_xmin, xmax = box_xmax, ymin = 0, ymax = box_ymax, 
             fill = box_color, color = "white", linewidth = 0.5) +
    
    # Influx Paths
    geom_path(data = paths_in, aes(x = x, y = y, group = group, color = col, linewidth = val),
              arrow = arrow(length = unit(cfg$arrow_head_sz, "inches"), type = "closed"),
              lineend = "butt", linejoin = "mitre") +
    
    # Outflux Paths
    {if(nrow(paths_out) > 0) geom_path(data = paths_out, aes(x = x, y = y, group = group, color = col, linewidth = val),
                                       arrow = arrow(length = unit(cfg$arrow_head_sz, "inches"), type = "closed"),
                                       lineend = "butt", linejoin = "mitre")} +
    
    # Left Labels (Influx)
    geom_text(data = paths_in %>% group_by(group) %>% slice(1),
              aes(x = x - 2.0, y = y, label = paste0(wrap_labels(group, 28), "\n(", format(val, big.mark=","), ")")), 
              hjust = 1, size = cfg$text_sz_main, fontface = "bold", lineheight = 0.85, color = "#222222") +
    
    # Right Labels (Outflux - perfectly vertically aligned with safe structural margins)
    {if(nrow(paths_out) > 0) geom_text(data = paths_out %>% group_by(group) %>% slice(n()),
                                       aes(x = x + 3.5, y = y, label = paste0(wrap_labels(group, 22), "\n(-", format(val, big.mark=","), ")")), 
                                       hjust = 0, size = cfg$text_sz_out, fontface = "bold", lineheight = 0.85, color = "#99000d")} +
    
    # Central Container Text
    annotate("text", x = (box_xmin + box_xmax)/2, y = 7500, 
             label = box_title, color = "white", fontface = "bold", size = cfg$text_sz_box, lineheight = 0.9) +
    
    annotate("text", x = (box_xmin + box_xmax)/2, y = 4000, 
             label = paste0("bold('", format(net_balance * 1000, big.mark=","), "') ~ bold(m^3)"), 
             parse = TRUE, color = "white", size = cfg$text_sz_box) +
    
    # Timeline Era Axis Indicator
    annotate("text", x = (box_xmin + box_xmax)/2, y = -3500, label = display_labels[[year_key]], 
             size = cfg$text_sz_year, fontface = "bold") +
    
    scale_linewidth(range = c(cfg$min_arrow_w, cfg$max_arrow_w), guide = "none") +
    scale_color_identity() +
    theme_void() + 
    coord_cartesian(xlim = c(x_min_limit, x_max_limit), ylim = c(-5000, global_y_max), clip = "off")
  
  return(g)
}

# --- MASTER WRAPPER FUNCTION (With Person-Year Equivalence) ---
# This is the function you actually call from your Rmd.

get_village_comparison_plot <- function(data_list, display_labels, cfg) {
  
  global_y_max <- map_dbl(data_list, function(df) {
    df_in  <- df %>% filter(val >= 0)
    df_out <- df %>% filter(val < 0)
    
    # Check if either subset is empty and default to 0 rows for calculation
    n_in  <- if(is.null(df_in))  0 else nrow(df_in)
    n_out <- if(is.null(df_out)) 0 else nrow(df_out)
    
    max_in_y  <- 12000 + 3500 + (n_in * cfg$v_stagger_in)
    max_out_y <- 12000 + 3500 + (n_out * cfg$v_stagger_out)
    
    return(max(max_in_y, max_out_y) + 3000)
  }) %>% max()
  
  plots <- imap(data_list, ~generate_village_plot(.x, .y, global_y_max, cfg, display_labels))
  
  pub_caps <- map_chr(names(plots), function(nm) {
    total_bwp_m3 <- (sum(data_list[[nm]]$val[data_list[[nm]]$val >= 0]) - sum(abs(data_list[[nm]]$val[data_list[[nm]]$val < 0]))) * 1000
    prefix <- if(nm == "2022") "B. " else "A. "
    term <- " Blue Water Potential = "
    return(paste0(prefix, nm, " Archetype Village", term, format(total_bwp_m3, big.mark=","), " m³"))
  })
  
  # Symmetric 1:1 distribution preserves matching sizes for both boxes without compression
  final_plot <- (plots[[1]] + labs(caption = pub_caps[1])) + (plots[[2]] + labs(caption = pub_caps[2])) +
    plot_layout(widths = c(1, 1)) & 
    theme(plot.caption = element_text(hjust = 0, size = 18, face = "bold", margin = margin(t = 20)))
  
  return(final_plot)
}