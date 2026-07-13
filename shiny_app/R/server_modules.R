# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R/server_modules.R
# Server Logic for Data Ingestion, Pre-Processing, and Engine Execution
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Module 1: Data Ingestion & Validation ----
data_ingestion_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    app_data <- reactiveValues(
      precip = NULL, et0 = NULL, soil_param = NULL, crop_param = NULL,
      kc_raw = NULL, raw_params = NULL, crop_systems = NULL,
      ps_by_cs_area = NULL, farms_count = NULL, is_loaded = FALSE,
      has_track2 = FALSE
    )
    
    validate_schemas <- function(precip, et0, crop_systems, ps_by_cs_area = NULL, has_track2 = TRUE) {
      if (any(sapply(list(precip, et0, crop_systems), is.null))) {
        stop("Missing critical Track 1 data files. Please ensure files 1 through 7 are uploaded.")
      }
      
      if (!all(c("date", "p") %in% tolower(colnames(precip)))) stop("Precipitation must contain 'date' and 'p' columns.")
      if (any(is.na(precip$p))) stop("Precipitation contains missing (NA) values.")
      if (any(is.na(et0$et0))) stop("ET0 contains missing (NA) values.")
      
      sys_cols <- colnames(crop_systems)
      if (!all(c("crop_1", "crop_2", "crop_3") %in% sys_cols)) {
        stop("Crop systems file must explicitly include 'crop_1', 'crop_2', and 'crop_3' columns.")
      }
      
      if (has_track2) {
        if (is.null(ps_by_cs_area)) stop("Track 2 enabled, but Demographic Area file is missing.")
        area_cols <- colnames(ps_by_cs_area)
        if (!"area_ha" %in% area_cols) stop("Area demographic file must explicitly include the 'area_ha' column.")
      }
      return(TRUE)
    }
    
    observeEvent(input$load_default_data, {
      data_dir <- here::here("data", "input") 
      
      id_notify <- showNotification("Loading repository baseline data... Please wait.", duration = NULL, type = "message")
      
      tryCatch({
        d_precip       <- read_csv(file.path(data_dir, "01_precipitation.csv"), show_col_types = FALSE)
        d_et0          <- read_csv(file.path(data_dir, "02_et0.csv"), show_col_types = FALSE)
        d_soil_param   <- read_csv(file.path(data_dir, "03_soil.parameters.csv"), show_col_types = FALSE)
        d_crop_param   <- read_csv(file.path(data_dir, "04_crop.parameters.csv"), show_col_types = FALSE)
        d_kc_raw       <- read_csv(file.path(data_dir, "05_crop_kc_curve_parameters_table.csv"), show_col_types = FALSE)
        d_raw_params   <- read_csv(file.path(data_dir, "06_fruit_kc_curve_parameters_table.csv"), show_col_types = FALSE)
        d_crop_systems <- read_csv(file.path(data_dir, "07_crop.systems.csv"), show_col_types = FALSE)
        d_ps_by_cs_area<- read_csv(file.path(data_dir, "08_crop_system_area_by_production_system.csv"), show_col_types = FALSE)
        d_farms_count  <- read_csv(file.path(data_dir, "09_no_of_farmers_by_production_system.csv"), show_col_types = FALSE)
        
        validate_schemas(d_precip, d_et0, d_crop_systems, d_ps_by_cs_area, has_track2 = TRUE)
        
        app_data$precip <- d_precip; app_data$et0 <- d_et0; app_data$soil_param <- d_soil_param
        app_data$crop_param <- d_crop_param; app_data$kc_raw <- d_kc_raw; app_data$raw_params <- d_raw_params
        app_data$crop_systems <- d_crop_systems; app_data$ps_by_cs_area <- d_ps_by_cs_area
        app_data$farms_count <- d_farms_count
        app_data$has_track2 <- TRUE
        app_data$is_loaded <- TRUE
        removeNotification(id_notify)
        showNotification("Baseline data successfully validated and loaded.", type = "message")
      }, error = function(e) {
        removeNotification(id_notify)
        showNotification(paste("Validation Error:", e$message), type = "error")
      })
    })
    
    observeEvent(input$load_custom_data, {
      req(input$custom_precip, input$custom_et0, input$custom_soil, input$custom_crop, 
          input$custom_kc, input$custom_systems)
      
      id_notify <- showNotification("Loading and validating custom data... Please wait.", duration = NULL, type = "message")
      
      tryCatch({
        d_precip       <- read_csv(input$custom_precip$datapath, show_col_types = FALSE)
        d_et0          <- read_csv(input$custom_et0$datapath, show_col_types = FALSE)
        d_soil_param   <- read_csv(input$custom_soil$datapath, show_col_types = FALSE)
        d_crop_param   <- read_csv(input$custom_crop$datapath, show_col_types = FALSE)
        d_kc_raw       <- read_csv(input$custom_kc$datapath, show_col_types = FALSE)
        d_crop_systems <- read_csv(input$custom_systems$datapath, show_col_types = FALSE)
        
        if (!is.null(input$custom_fruit)) {
          d_raw_params <- read_csv(input$custom_fruit$datapath, show_col_types = FALSE)
        } else {
          d_raw_params <- NULL
        }
        
        has_t2 <- input$enable_track2
        d_ps_by_cs_area <- NULL
        d_farms_count <- NULL
        
        if (has_t2) {
          req(input$custom_area, input$custom_farms)
          d_ps_by_cs_area <- read_csv(input$custom_area$datapath, show_col_types = FALSE)
          d_farms_count   <- read_csv(input$custom_farms$datapath, show_col_types = FALSE)
        }
        
        validate_schemas(d_precip, d_et0, d_crop_systems, d_ps_by_cs_area, has_track2 = has_t2)
        
        app_data$precip <- d_precip; app_data$et0 <- d_et0; app_data$soil_param <- d_soil_param
        app_data$crop_param <- d_crop_param; app_data$kc_raw <- d_kc_raw; app_data$raw_params <- d_raw_params
        app_data$crop_systems <- d_crop_systems; app_data$ps_by_cs_area <- d_ps_by_cs_area
        app_data$farms_count <- d_farms_count
        app_data$has_track2 <- has_t2
        app_data$is_loaded <- TRUE
        removeNotification(id_notify)
        showNotification("Custom data successfully validated and loaded.", type = "message")
      }, error = function(e) {
        removeNotification(id_notify)
        showNotification(paste("Validation Error:", e$message), type = "error")
      })
    })
    
    return(app_data)
  })
}

# Module 1.5: Pre-Processing Engine ----
pre_processing_server <- function(id, app_data) {
  moduleServer(id, function(input, output, session) {
    
    preproc_data <- reactiveValues(
      climate_summary = NULL, kc_doy_crop_all = NULL,
      crop_rotation_kc_ls = NULL, crop_systems_l_kc = NULL, 
      is_climate_processed = FALSE, is_kc_processed = FALSE
    )
    
    # Render Pre-Processing Button Conditionally
    output$run_preproc_ui <- renderUI({
      if (isTruthy(app_data$is_loaded)) {
        actionButton(session$ns("run_preproc"), "Calculate Kc Curves", icon = icon("cogs"), class = "btn-warning w-100")
      } else {
        tags$button(class = "btn btn-secondary w-100 disabled", disabled = "disabled", icon("cogs"), " Calculate Kc Curves")
      }
    })
    
    observeEvent(app_data$is_loaded, {
      req(app_data$is_loaded)
      tryCatch({
        df_p <- app_data$precip %>% mutate(year = year(date), month = month(date)) %>% group_by(year, month) %>% summarise(P = sum(p, na.rm=TRUE), .groups="drop")
        df_et0 <- app_data$et0 %>% mutate(year = year(date), month = month(date)) %>% group_by(year, month) %>% summarise(ET0 = sum(et0, na.rm=TRUE), .groups="drop")
        
        preproc_data$climate_summary <- df_p %>% left_join(df_et0, by = c("year", "month")) %>%
          group_by(month) %>%
          summarise(
            Rainfall_avg = mean(P, na.rm=TRUE),
            Rainfall_q05 = quantile(P, 0.05, na.rm=TRUE),
            Rainfall_q95 = quantile(P, 0.95, na.rm=TRUE),
            ET0_avg = mean(ET0, na.rm=TRUE),
            ET0_q05 = quantile(ET0, 0.05, na.rm=TRUE),
            ET0_q95 = quantile(ET0, 0.95, na.rm=TRUE),
            .groups = "drop"
          ) %>%
          rename(Month = month) %>%
          mutate(Temperature_avg = NA_real_, Temperature_q05 = NA_real_, Temperature_q95 = NA_real_)
        
        preproc_data$is_climate_processed <- TRUE
      }, error = function(e) {
        showNotification(paste("Climate Processing Error:", e$message), type = "error")
      })
    })
    
    observeEvent(input$run_preproc, {
      req(app_data$is_loaded)
      
      id_notify <- showNotification("Calculating daily Kc curves from phenological stages... Please wait.", duration = NULL, type = "message")
      
      tryCatch({
        biophysical_parameters <- list(grazing_growth_rate = 0.00458333333333333, grazing_drop_kc = 0.20, grazing_max_kc = 0.75, napier_decline_rate_1 = 0.000307692307692308, napier_decline_rate_2 = 0.000153846153846154)
        
        crop.systems.l <- app_data$crop_systems %>%
          select(-Irrigation) %>% pivot_longer(cols = starts_with("crop_"), names_prefix = "crop_", names_to = "ncrop", values_to = "crop") %>%
          left_join(app_data$crop_param, by = "crop")
        
        kc.raw.long <- app_data$kc_raw %>% pivot_longer(cols = -1, names_to = "crop") %>% mutate(crop = str_to_title(gsub("_|\\.", " ", crop, perl = TRUE)))
        
        kc <- kc.raw.long %>% subset(stringr::str_detect(var, stringr::regex("kc", ignore_case = TRUE))) %>% rename(Kc = value, kc_var = var) %>%
          mutate(Kc_var = case_when(grepl("ini", kc_var, ignore.case = TRUE) ~ "Kc_ini", grepl("mid", kc_var, ignore.case = TRUE) ~ "Kc_mid", grepl("end", kc_var, ignore.case = TRUE) ~ "Kc_end", TRUE ~ kc_var))
        
        doy <- kc.raw.long %>% subset(var %in% c("Sowing", "End of ini", "End of Dev", "End of Mid", "Harvest")) %>% rename(doy = value, doy_var = var) %>% mutate(doy = if_else(doy == 365.00, 366.00, doy))
        kc.doy.key <- data.frame(doy_var = c("Sowing", "End of ini", "End of Dev", "End of Mid", "Harvest"), kc_var = c("Kc_ini", "Kc_ini", "Kc_mid", "Kc_mid", "Kc_end"))
        doy <- left_join(doy, kc.doy.key, by = c("doy_var"))
        kc.doy <- left_join(doy, kc, by = c("kc_var", "crop")) %>% arrange(crop, doy, Kc) %>% mutate(doy = as.numeric(doy), Kc = as.numeric(Kc))
        
        kc.doy.crops <- kc.doy %>% select(crop, doy_var, doy, Kc) %>% mutate(doy_var = factor(doy_var, levels = c("Sowing", "End of ini", "End of Dev", "End of Mid", "Harvest"))) %>% arrange(crop, doy, doy_var)
        kc_wide_build <- kc.doy.crops %>% pivot_wider(names_from = doy_var, values_from = c(doy, Kc)) %>%
          rename(Sow = doy_Sowing, E_ini = `doy_End of ini`, E_dev = `doy_End of Dev`, E_mid = `doy_End of Mid`, Harv = doy_Harvest, K_ini = Kc_Sowing, K_mid = `Kc_End of Dev`, K_end = Kc_Harvest)
        
        kc_segmented_ls <- lapply(1:nrow(kc_wide_build), function(i) build_fao_curve(kc_wide_build[i, ]))
        kc_segmented <- bind_rows(kc_segmented_ls) %>% mutate(crop = str_to_title(gsub("_", " ", crop, perl = TRUE)))
        
        gs <- data.frame(doy = 1:366) %>% mutate(crop = "Groundnut Summer", d = doy - 10, kc = if_else(d >= 0 & d <= 104, 1.25*10^(-7)*d^4 - 2.82*10^(-5)*d^3 + 1.8*10^(-3)*d^2 - 2.49*10^(-2)*d + 0.3792, 0))
        gm <- data.frame(doy = 1:366) %>% mutate(crop = "Groundnut Monsoon", d = doy - 182, kc = if_else(d >= 0 & d <= 104, 1.32*10^(-7)*d^4 - 2.99*10^(-5)*d^3 + 1.96*10^(-3)*d^2 - 3.02*10^(-2)*d + 0.4, 0))
        kc_polynomial <- bind_rows(gm, gs) %>% select(-d)
        
        if (!is.null(app_data$raw_params)) {
          raw_p <- app_data$raw_params
          colnames(raw_p) <- stringr::str_to_title(gsub("_|\\.", " ", colnames(raw_p)))
          kc_fruits <- bind_rows(generate_fruit_step("Pomegranate", raw_p), generate_fruit_step("Sweet Lime", raw_p)) 
        } else {
          kc_fruits <- tibble(doy = numeric(), crop = character(), kc = numeric())
        }
        
        no_crop <- tibble(doy = 1:366, crop = "No Crop", kc = 0)
        grassland_base <- kc_segmented %>% filter(crop == "Grasslands") %>% pull(kc)
        growth_rate <- biophysical_parameters$grazing_growth_rate; napier_dec_1 <- biophysical_parameters$napier_decline_rate_1; napier_dec_2 <- biophysical_parameters$napier_decline_rate_2
        graze_drop <- biophysical_parameters$grazing_drop_kc; max_bound <- biophysical_parameters$grazing_max_kc; grassland_sow <- kc_wide_build$Sow[kc_wide_build$crop == "Grasslands"]
        
        grazed_thrice <- generate_grazed_legacy("Grazed Thrice", c(182, 243, 304), grassland_base, growth_rate, graze_drop, max_bound, grassland_sow)
        grazed_1_aug  <- generate_grazed_legacy("Grazed 1 Aug", 213, grassland_base, growth_rate, graze_drop, max_bound, grassland_sow)
        grazed_1_sept <- generate_grazed_legacy("Grazed 1 Sept", 244, grassland_base, growth_rate, graze_drop, max_bound, grassland_sow)
        grazed_1_oct  <- generate_grazed_legacy("Grazed 1 Oct", 274, grassland_base, growth_rate, graze_drop, max_bound, grassland_sow)
        grazed_1_nov  <- generate_grazed_legacy("Grazed 1 Nov", 306, grassland_base, growth_rate, graze_drop, max_bound, grassland_sow)
        napier       <- generate_napier("Napier", 1, napier_dec_1, napier_dec_2)
        napier_short <- generate_napier("Napier Short", 211, napier_dec_1, napier_dec_2)
        
        kc.doy.crop.all <- bind_rows(kc_segmented, kc_polynomial, kc_fruits, no_crop, grazed_thrice, grazed_1_aug, grazed_1_sept, grazed_1_oct, grazed_1_nov, napier, napier_short) %>% arrange(crop, doy)
        preproc_data$kc_doy_crop_all <- kc.doy.crop.all
        
        crop.systems.l.kc <- crop.systems.l %>% select(rotation, n.rotation, crop.system, soil, ncrop, crop, irrigation, rooting_depth_cm) %>%
          left_join(kc.doy.crop.all %>% arrange(doy, crop), by = "crop", relationship = "many-to-many")
        
        preproc_data$crop_systems_l_kc <- crop.systems.l.kc
        preproc_data$crop_rotation_kc_ls <- split(crop.systems.l.kc, crop.systems.l.kc$rotation)
        preproc_data$is_kc_processed <- TRUE
        
        removeNotification(id_notify)
        showNotification("Kc Curves successfully generated!", type = "message")
      }, error = function(e) {
        removeNotification(id_notify)
        showNotification(paste("Kc Pre-processing Error:", e$message), type = "error")
      })
    })
    
    return(preproc_data)
  })
}

# Module 2: Track 1 Engine Execution ----
track1_engine_server <- function(id, app_data, preproc_data) {
  moduleServer(id, function(input, output, session) {
    
    engine_results <- reactiveValues(budget_ls = NULL, summary_yr_budgets = NULL, is_calculated = FALSE)
    
    # Render Engine Button Conditionally
    output$run_engine_ui <- renderUI({
      if (isTruthy(preproc_data$is_kc_processed)) {
        actionButton(session$ns("run_engine"), "Run Biophysical Engine", icon = icon("play"), class = "btn-success w-100")
      } else {
        tags$button(class = "btn btn-secondary w-100 disabled", disabled = "disabled", icon("play"), " Run Biophysical Engine")
      }
    })
    
    # Render Track 1 Download Button Conditionally
    output$download_ui_t1 <- renderUI({
      if (isTruthy(engine_results$is_calculated)) {
        downloadButton(session$ns("download_report_t1"), "Download PDF Report", class = "btn-info w-100")
      } else {
        tags$a(class = "btn btn-secondary w-100 disabled", disabled = "disabled", icon("download"), " Download PDF Report")
      }
    })
    
    observeEvent(input$run_engine, {
      req(app_data$is_loaded, preproc_data$is_kc_processed)
      
      id_notify <- showNotification("Executing parallel Biophysical Engine. This will take several minutes...", duration = NULL, type = "message")
      
      tryCatch({
        p.list <- list(precip = app_data$precip, et0 = app_data$et0, et0.timestep = "daily", CN = 85, soil.input = "P", soil.param = app_data$soil_param, sm.init = 0.5, crop.param = app_data$crop_param, kc_no_crop = list(summer = 0.005, kharip_rabi = 0.2))
        met.forcing.ls <- lapply(as.list(app_data$soil_param$soil), function(x) { get_peff_et0(p = p.list, soil = x) })
        names(met.forcing.ls) <- app_data$soil_param$soil
        
        num_cores <- parallel::detectCores(logical = FALSE)
        safe_cores <- min(3, max(1, num_cores - 1)) 
        cl <- makeCluster(safe_cores, type = "PSOCK")
        
        clusterExport(cl, varlist = c("p.list", "met.forcing.ls", "get_budget", "get_Peff", "get_peff_et0"), envir = environment())
        clusterEvalQ(cl, { library(tidyverse); library(ggpubr) })
        
        budget <- pbapply::pblapply(preproc_data$crop_rotation_kc_ls, get_budget, cl = cl, p = p.list, ts = met.forcing.ls)
        stopCluster(cl)
        
        names(budget) <- names(preproc_data$crop_rotation_kc_ls)
        engine_results$budget_ls <- budget
        engine_results$summary_yr_budgets <- process_annual_budget(budget, app_data$crop_systems)
        engine_results$is_calculated <- TRUE
        
        removeNotification(id_notify)
        showNotification("Engine execution complete! Visualizations are rendering.", type = "message")
      }, error = function(e) {
        removeNotification(id_notify)
        showNotification(paste("Engine execution failed:", e$message), type = "error")
      })
    })
    return(engine_results)
  })
}

# Module 3: Track 2 Engine Execution (Socio-Demographic) ----
track2_engine_server <- function(id, app_data, engine_results) {
  moduleServer(id, function(input, output, session) {
    
    # Render Track 2 Download Button Conditionally
    output$download_ui_t2 <- renderUI({
      if (isTruthy(engine_results$is_calculated)) {
        downloadButton(session$ns("download_report"), "Download PDF Report", class = "btn-info w-100")
      } else {
        tags$a(class = "btn btn-secondary w-100 disabled", disabled = "disabled", icon("download"), " Download PDF Report")
      }
    })
    
    track2_data <- reactive({
      req(engine_results$is_calculated, app_data$is_loaded)
      if (!app_data$has_track2) return(NULL)
      
      budget_yr_ls <- lapply(engine_results$budget_ls, function(x) x$sum.yr)
      budget_yr_l <- data.table::rbindlist(budget_yr_ls, idcol = "rotation") %>%
        tidyr::pivot_longer(cols = c(-rotation, -year), names_to = "var", values_to = "value") %>%
        dplyr::left_join(app_data$crop_systems, by = "rotation")
      
      ps_area <- app_data$ps_by_cs_area %>% mutate(time_period = as.character(time_period))
      farms <- app_data$farms_count %>% mutate(time_period = as.character(time_period))
      farms_clean <- farms %>% group_by(time_period, production_system) %>% summarise(number_of_farms = sum(number_of_farms, na.rm = TRUE), .groups = "drop")
      
      ps_results <- get_production_system_budgets(budget_yr_l = budget_yr_l, ps_by_cs_area = ps_area, farms_count_df = farms_clean)
      
      cs_surface <- app_data$crop_systems %>% select(crop.system, Irrigation) %>% distinct() %>%
        mutate(land_use = case_when(Irrigation == "Irrigated" ~ "Irrigated", str_detect(crop.system, "(?i)grassland|grazed|pasture") ~ "Pastoral", TRUE ~ "Rainfed"))
      
      ps_typology <- ps_area %>% left_join(cs_surface, by = "crop.system") %>%
        group_by(time_period, production_system, land_use) %>% summarise(land_use_area = sum(area_ha, na.rm = TRUE), .groups = "drop_last") %>%
        mutate(total_area_ha = sum(land_use_area, na.rm = TRUE), land_use_pc = round((land_use_area / total_area_ha) * 100, 2)) %>%
        ungroup() %>% select(-land_use_area) %>% pivot_wider(names_from = land_use, values_from = land_use_pc, values_fill = 0) %>%
        mutate(system_typology = case_when(Irrigated >= 35 ~ "Irrigated", Pastoral > Rainfed ~ "Pastoral", TRUE ~ "Rainfed"))
      
      list(ps_bwp_volumetric = ps_results$ps_bwp_volumetric, ps_volumetric_summary = ps_results$ps_volumetric_summary, ps_typology = ps_typology)
    })
    
    return(list(track2_data = track2_data))
  })
}