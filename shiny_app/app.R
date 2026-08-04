# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# app.R
# Main launch script for the Blue Water Toolkit
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("shiny_app", "global.R"))
source(here::here("shiny_app", "R", "ui_modules.R"))
source(here::here("shiny_app", "R", "server_modules.R"))
source(here::here("code", "functions", "06_case_study_formatters.R"))

library(DT)
library(quarto)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER INTERFACE (UI) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- navbarPage(
  title = "The Blue Water Toolkit",
  theme = bs_theme(version = 5, preset = "cerulean"),
  
  header = tags$head(
    tags$script('
      $(function () {
        $("body").tooltip({ selector: \'[data-bs-toggle="tooltip"]\' });
      });
    '),
    tags$style(HTML("
      .tooltip-inner { background-color: #2C3E50; color: #ffffff; }
      .tooltip.bs-tooltip-end .tooltip-arrow::before { border-right-color: #2C3E50; }
    "))
  ),
  
  # Tab 1: Data Setup & Climatology ----
  tabPanel(
    title = "1. Data Setup",
    icon = icon("folder-open"),
    sidebarLayout(
      sidebarPanel(width = 4, h4("Upload Inputs"), data_upload_ui("data_module")),
      mainPanel(
        width = 8,
        div(class = "card mb-4 border-info",
            div(class = "card-body",
                h4("Accounting for the Unaccounted: Farm Social Diversity in Water Budgets", class = "card-title text-info"),
                p(strong("How has the partitioning of our atmospheric water inputs been fundamentally altered by human land use?")),
                p("This toolkit provides a framework for modeling net watershed impacts by incorporating farm social diversity. It calculates Blue Water Potential (BWP) to isolate the absolute net impact of a specific land use decision on the underlying aquifer."),
                tags$ul(
                  tags$li(strong("The Assessment:"), " The calculator identifies which farm topologies act as chronic hydrological 'Net Consumers' via artificial irrigation, and which act as 'Net Contributors' by enabling monsoon precipitation to actively recharge the aquifer."),
                  tags$li(strong("The calculator:"), " The model processes daily meteorological forcing, soil parameters, and crop rotation definitions. It dynamically generates continuous crop coefficient (Kc) curves using standard FAO-56 segments, polynomial equations, and complex grazing regrowth schedules to simulate multi-decadal water fluxes."),
                  tags$li(strong("The Discovery:"), " Scaling BWP by farm typology reveals the deeply unequal distribution of net water extraction across the agricultural landscape. Aggregating these balances to an archetype village illustrates structural tipping points in regional water security.")
                )
            )
        ),
        
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 1: Site Climate", 
                   h4("Monthly Climatology Validation", info_tooltip("Monthly mean (\u00B1 SD) rainfall, temperature, and reference evapotranspiration."), align = "center"),
                   plotOutput("plot_fig1_climate", height = "500px")
          ),
          tabPanel("Data Dictionary",
                   h4("Input Data Requirements", align = "center"),
                   uiOutput("data_dictionary_ui")
          ),
          tabPanel("Raw Data Tables", 
                   br(),
                   selectInput("preview_dataset", "Select Dataset to View:", 
                               choices = c("1. Model Params", "2. Precipitation", "3. ET0", "4. Soil Params", "5. Crop Params", 
                                           "6. Crop Kc Params", "7. Fruit Kc Params", "8. Crop Systems", 
                                           "9. Area by System", "10. Farm Counts")),
                   DTOutput("table_preview")
          )
        )
      )
    )
  ),
  
  # Tab 2: Crop Phenology Module ----
  tabPanel(
    title = "2. Phenology Module",
    icon = icon("seedling"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   h4("Phenology Module", info_tooltip("Generates daily curves integrating multiple modeling approaches: standard FAO-56 segmented linear growth stages, continuous polynomial equations, discrete monthly step-functions, and drop-and-regrowth cycles for grazing.")), 
                   preproc_controls_ui("preproc_module")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 2: All Crop Phenology", 
                   h4("Daily Kc for All Modeled Crops", info_tooltip("Daily crop coefficient (Kc) for all modeled crops. Panels illustrate the temporal variation of Kc across a full calendar year."), align = "center"),
                   plotOutput("plot_fig2_raw_kc", height = "600px")
          ),
          tabPanel("Figure 3: Crop System Phenology", 
                   h4("Daily Kc for Crop Systems (rows) by Rotations (columns)", info_tooltip("Annual Crop Coefficient (Kc) curves for each cropping system. Off-season and 'No Crop' periods are defaulted to a defined seasonal threshold representing bare-soil evaporation."), align = "center"),
                   plotOutput("plot_fig3_kc_curves", height = "1000px")
          )
        )
      )
    )
  ),
  
  # Tab 3: Scale 1 - Core Hydrological Model ----
  tabPanel(
    title = "3. Scale 1: Cropping Systems",
    icon = icon("leaf"),
    sidebarLayout(
      sidebarPanel(width = 4, h4("Model Execution"), scale1_controls_ui("scale1_module")),
      mainPanel(
        width = 8,
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 4: Irrigation Requirement vs Gross Recharge", 
                   h4("Annual irrigation Requirement vs Gross Recharge", info_tooltip("Annual mean (\u00B1 SD) of irrigation requirement for irrigated cropping systems and the gross groundwater recharge for all cropping systems."), align = "center"),
                   plotOutput("plot_fig4_irrigation_recharge")
          ),
          tabPanel("Figure 5: Blue Water Potential", 
                   h4("Blue Water Potential", info_tooltip("Annual mean (\u00B1 SD) of Blue Water Potential (mean annual Net Groundwater Recharge, calculated as Gross Recharge minus Irrigation Requirement) for various cropping systems."), align = "center"),
                   plotOutput("plot_fig5_bwp")
          )
        )
      )
    )
  ),
  
  # Tab 4: Scale 2 - Production Systems ----
  tabPanel(
    title = "4. Scale 2: Production Systems",
    icon = icon("tractor"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   h4("Farm Topology", info_tooltip("Net Consumers (BWP < 0): Irrigation demand exceeds gross recharge. Net Producers (BWP > 0): Fully rainfed or lightly irrigated farms that contribute to the regional Blue Water Commons.")), 
                   scale2_controls_ui("socio_module")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 6: BWP By Farm Type", 
                   h4("Comparative Annual Blue Water Potential Across Production Systems", info_tooltip("Comparative annual BWP across Production Systems (farm types). BWP contributions (+ve or -ve) from Irrigated/Rainfed cropped area or grazed area are mapped by colors. Net BWP contribution of each farm type is identified by a cross."), align = "center"),
                   plotOutput("plot_fig6_bwp_systems", height = "600px")
          )
        )
      )
    )
  ),
  
  # Tab 5: Scale 3 - Agrarian Systems ----
  tabPanel(
    title = "5. Scale 3: Agrarian Systems",
    icon = icon("users"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   h4("Demographic Analysis", info_tooltip("Scaling physical topologies across regional demographic farm counts.")), 
                   scale3_controls_ui("socio_module")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 7: Village BWP Trends", 
                   h4("Annual Blue Water Potential of the Archetype Village", info_tooltip("Annual Blue Water Potential of the archetype village. Highlights the role of often ignored social categories such as Pastoralists who are main Net Contributers of Blue Water."), align = "center"),
                   plotOutput("plot_fig7_village_bwp", height = "500px")
          ),
          tabPanel("Figure 8: Village Water Balance", 
                   h4("Average Annual Water Balance of the Archetype Village", info_tooltip("Average annual water balance of the archetype village. Illustrates the classical representation of aggregated Inflows minus Outflows by land-use, where all land uses are represented as net water consumers."), align = "center"),
                   uiOutput("dynamic_sankey_ui")
          ),
          tabPanel("Figure 9: Farm Disparity", 
                   h4("Micro-Regional Disparity Metrics", info_tooltip("Number of farmers in each Production System and their aggregated Blue Water Potential (BWP) in thousand m³. Highlights how certain social categories disproportionately affect the regional water balance."), align = "center"),
                   uiOutput("dynamic_disparity_ui")
          )
        )
      )
    )
  )
)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SERVER LOGIC ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
server <- function(input, output, session) {
  
  app_data <- data_ingestion_server("data_module")
  preproc_data <- pre_processing_server("preproc_module", app_data)
  calculator_results <- scale1_engine_server("scale1_module", app_data, preproc_data)
  socio_results <- socio_engine_server("socio_module", app_data, calculator_results)  
  
  # --- TAB 1 RENDERING ----
  output$plot_fig1_climate <- renderPlot({
    validate(need(preproc_data$is_climate_processed, "Awaiting Data... Please load baseline or custom data from the sidebar to view the Climatology."))
    plot_climate_profile(preproc_data$climate_summary, show_temperature = FALSE)
  }, res = 96, bg = "white")
  
  output$data_dictionary_ui <- renderUI({
    HTML("
      <div style='padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
      <p>Ensure your CSV files contain the following exact column names. The calculator requires these strict headers to execute the mathematical joins.</p>
      <ul>
        <li><b>01_model_parameters:</b> <code>parameter</code>, <code>value</code>, <code>description</code>.</li>
        <li><b>02_precipitation:</b> <code>date</code> (YYYY-MM-DD), <code>p</code> (mm/day).</li>
        <li><b>03_et0:</b> <code>date</code> (YYYY-MM-DD), <code>et0</code> (mm/day).</li>
        <li><b>04_soil.parameters:</b> <code>soil</code> (ID), <code>whc.mmpercm</code> (Water Holding Capacity), <code>raw</code> (Readily Available Water).</li>
        <li><b>05_crop.parameters:</b> <code>crop</code> (Name), <code>rooting_depth_cm</code> (cm).</li>
        <li><b>06_fao_kc_curve:</b> Columns for each crop defining specific phenological days and Kc values.</li>
        <li><b>08_crop.systems:</b> <code>crop.system</code> (Rotation Name), <code>crop_1</code>, <code>crop_2</code>, <code>crop_3</code> (Sequential crops), <code>rotation</code> (Unique ID).</li>
        <li><b>09_area (Scale 2):</b> <code>time_period</code>, <code>production_system</code> (Farm type), <code>area_ha</code> (hectares).</li>
        <li><b>10_farms (Scale 3):</b> <code>time_period</code>, <code>production_system</code>, <code>number_of_farms</code>.</li>
      </ul>
      </div>
    ")
  })
  
  output$table_preview <- renderDT({
    req(app_data$is_loaded)
    dataset <- switch(input$preview_dataset, 
                      "1. Model Params" = app_data$model_param,
                      "2. Precipitation" = app_data$precip, 
                      "3. ET0" = app_data$et0,
                      "4. Soil Params" = app_data$soil_param,
                      "5. Crop Params" = app_data$crop_param,
                      "6. Crop Kc Params" = app_data$kc_raw,
                      "7. Fruit Kc Params" = app_data$raw_params,
                      "8. Crop Systems" = app_data$crop_systems, 
                      "9. Area by System" = app_data$ps_by_cs_area,
                      "10. Farm Counts" = app_data$farms_count)
    datatable(dataset, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- TAB 2 RENDERING ----
  output$plot_fig2_raw_kc <- renderPlot({
    validate(need(preproc_data$is_kc_processed, "Awaiting Processing... Please click 'Calculate Kc Curves' in the sidebar to generate this visualization."))
    plot_crop_coefficient_curves(preproc_data$kc_doy_crop_all)
  }, res = 96, bg = "white")
  
  output$plot_fig3_kc_curves <- renderPlot({
    validate(need(preproc_data$is_kc_processed, "Awaiting Processing... Please click 'Calculate Kc Curves' in the sidebar to generate this visualization."))
    
    get_param <- function(param_name, is_numeric = TRUE) {
      val <- app_data$model_param$value[app_data$model_param$parameter == param_name]
      if(is_numeric) return(as.numeric(val)) else return(as.character(val))
    }
    
    KC_FALLOW_CONSTANTS <- list(
      dry_season = get_param("kc_dry_season"), 
      kharip = get_param("kc_kharip"), 
      doy_switch = get_param("doy_switch")
    )
    
    plot_crop_system_coefficient_curves(preproc_data$crop_systems_l_kc, KC_FALLOW_CONSTANTS)
  }, res = 96, bg = "white")
  
  # --- TAB 3 RENDERING (Scale 1) ----
  output$plot_fig4_irrigation_recharge <- renderPlot({
    validate(need(calculator_results$is_calculated, "Awaiting Processing... Please run the Core Hydrological Model to view this visualization."))
    vars_sd <- c("Irrigation Requirement (mm)", "Groundwater Recharge (mm)")
    plot_annual_summary(calculator_results$summary_yr_budgets$summ_qs_cs, vars_sd, "mean", "Mean Annual Fluxes (95% CI)")
  }, res = 96, bg = "white")
  
  output$plot_fig5_bwp <- renderPlot({
    validate(need(calculator_results$is_calculated, "Awaiting Processing... Please run the Core Hydrological Model to view this visualization."))
    plot_annual_summary(calculator_results$summary_yr_budgets$summ_qs_cs, "Blue Water Potential (mm)", "mean", "Blue Water Potential (mm)")
  }, res = 96, bg = "white")
  
  # --- TAB 4 RENDERING (Scale 2) ----
  output$plot_fig6_bwp_systems <- renderPlot({
    validate(need(app_data$has_scale2, "Scale 2 Socio-Demographic data was not loaded. Include File 09_area in Tab 1 to unlock these metrics."))
    validate(need(calculator_results$is_calculated, "Awaiting Processing... Please run the Core Hydrological Model on Tab 3 first."))
    req(socio_results$socio_data())
    s_data <- socio_results$socio_data()
    plot_production_system_bwp(s_data$ps_bwp_volumetric)
  }, res = 96, bg = "white")
  
  # --- TAB 5 RENDERING (Scale 3) ----
  output$plot_fig7_village_bwp <- renderPlot({
    validate(need(app_data$has_scale3, "Scale 3 Agrarian System data was not loaded. Include File 10_farms in Tab 1 to unlock these metrics."))
    validate(need(calculator_results$is_calculated, "Awaiting Processing... Please run the Core Hydrological Model on Tab 3 first."))
    req(socio_results$socio_data())
    
    s_data <- socio_results$socio_data()
    
    village.bwp_collapsed <- s_data$ps_bwp_volumetric %>%
      mutate(bwp_thousand_m3 = round(total_vol_m3/10000)*10) %>%
      group_by(time_period, production_system) %>%
      summarise(val = sum(bwp_thousand_m3), .groups = "drop") %>%
      rename(period = time_period, label = production_system)
    
    cfg <- list(box_width = 100, min_arrow_w = 5, max_arrow_w = 30, v_stagger_out = 3000)
    
    tmp_png <- tempfile(fileext = ".png")
    invisible(plot_village_bwp(village_bwp_data = village.bwp_collapsed, cfg = cfg, output_dev = png, filename = tmp_png, manuscript_style = TRUE))
    
    if (file.exists(tmp_png)) {
      img <- png::readPNG(tmp_png)
      par(mar = c(0,0,0,0)) 
      plot.new()
      rasterImage(img, 0, 0, 1, 1)
    }
  }, res = 96, bg = "white")
  
  # Dynamic UI for Figure 8 (Sankey Diagram)
  output$dynamic_sankey_ui <- renderUI({
    validate(need(app_data$has_scale3, "Scale 3 Agrarian System data was not loaded. Include File 10_farms in Tab 1 to unlock these metrics."))
    req(socio_results$socio_data())
    periods <- sort(unique(socio_results$socio_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    col_width <- ifelse(length(periods) > 1, 6, 12) 
    
    cols <- lapply(seq_along(periods), function(i) {
      column(col_width, h5(paste(periods[i], "Archetype"), align = "center"), sankeyNetworkOutput(paste0("plot_village_sankey_", i), height = "450px"))
    })
    do.call(fluidRow, cols)
  })
  
  # Dynamic Plot Rendering for Figure 8
  observe({
    req(socio_results$socio_data(), app_data$has_scale3)
    periods <- sort(unique(socio_results$socio_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    for (i in seq_along(periods)) {
      local({
        my_i <- i
        my_p <- periods[my_i]
        output[[paste0("plot_village_sankey_", my_i)]] <- renderSankeyNetwork({
          s_data <- socio_results$socio_data()
          classical_budget_fig_data <- format_agrarian_sankey_data(s_data$ps_volumetric_summary)
          get_d3_sankey(classical_budget_fig_data, my_p)
        })
      })
    }
  })
  
  # Dynamic UI for Figure 9 (Disparity Plot)
  output$dynamic_disparity_ui <- renderUI({
    validate(need(app_data$has_scale3, "Scale 3 Agrarian System data was not loaded. Include File 10_farms in Tab 1 to unlock these metrics."))
    req(socio_results$socio_data())
    periods <- sort(unique(socio_results$socio_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    cols <- lapply(seq_along(periods), function(i) {
      column(12, h5(paste(periods[i], "Archetype"), align = "center"), plotOutput(paste0("plot_disparity_", i), height = "450px"))
    })
    do.call(fluidRow, cols)
  })
  
  # Dynamic Plot Rendering for Figure 9
  observe({
    req(socio_results$socio_data(), app_data$has_scale3)
    periods <- sort(unique(socio_results$socio_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    for (i in seq_along(periods)) {
      local({
        my_i <- i
        my_p <- periods[my_i]
        output[[paste0("plot_disparity_", my_i)]] <- renderPlot({
          s_data <- socio_results$socio_data()
          
          plot_farm_bwp_disparity(s_data$ps_bwp_volumetric, app_data$ps_by_cs_area, target_period = my_p)
        }, res = 96, bg = "white")
      })
    }
  })
  
  # --- CENTRALIZED HTML QUARTO RENDERER FUNCTION ----
  generate_html_report <- function(file) {
    req(calculator_results$is_calculated)
    id_notify <- showNotification("Rendering full Quarto HTML report. This may take a minute...", duration = NULL, type = "message")
    on.exit(removeNotification(id_notify), add = TRUE)
    
    has_s2 <- app_data$has_scale2
    has_s3 <- app_data$has_scale3
    
    # Read the core Quarto file into memory as a manipulatable template
    qmd_path <- here::here("code", "02_Blue-Water.qmd")
    qmd_lines <- readLines(qmd_path)
    
    # Toggle document logic parameters to match exactly what the user uploaded and processed in Shiny
    qmd_lines <- sub("^HAS_PRODUCTION_SYSTEM\\s*<-.*", paste("HAS_PRODUCTION_SYSTEM <-", has_s2), qmd_lines)
    qmd_lines <- sub("^HAS_AGRARIAN_SYSTEM\\s*<-.*", paste("HAS_AGRARIAN_SYSTEM <-", has_s3), qmd_lines)
    qmd_lines <- sub("^CALCULATE_BUDGET\\s*<-.*", "CALCULATE_BUDGET <- FALSE", qmd_lines) # We bypass computation since Shiny already saved output/01_processed_water_budgets.Rdata
    
    temp_qmd <- here::here("code", "temp_report.qmd")
    writeLines(qmd_lines, temp_qmd)
    
    tryCatch({
      # Render the document using Quarto engine
      quarto::quarto_render(
        input = temp_qmd,
        output_format = "html",
        quiet = TRUE
      )
      
      # Copy final output directly to the user's browser download slot
      temp_html <- here::here("code", "temp_report.html")
      file.copy(temp_html, file)
      
      # Clean up temporary generation files
      unlink(temp_qmd)
      unlink(temp_html)
    }, error = function(e) {
      showNotification(paste("Failed to render report:", e$message), type = "error")
      if(file.exists(temp_qmd)) unlink(temp_qmd)
    })
  }
  
  # Bind Central HTML Downloader to all valid scale buttons
  output$`scale1_module-download_report_s1` <- downloadHandler(filename = function() { paste0("Blue_Water_Toolkit_Report_", Sys.Date(), ".html") }, content = generate_html_report)
  output$`socio_module-download_report_s2` <- downloadHandler(filename = function() { paste0("Blue_Water_Toolkit_Report_", Sys.Date(), ".html") }, content = generate_html_report)
  output$`socio_module-download_report_s3` <- downloadHandler(filename = function() { paste0("Blue_Water_Toolkit_Report_", Sys.Date(), ".html") }, content = generate_html_report)
}

shinyApp(ui = ui, server = server)