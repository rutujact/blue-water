# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# app.R
# Main launch script for the Blue Water Toolkit
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("shiny_app", "global.R"))
source(here::here("shiny_app", "R", "ui_modules.R"))
source(here::here("shiny_app", "R", "server_modules.R"))

if (!require("DT", quietly = TRUE)) install.packages("DT")
library(DT)
library(shinyjs)
library(shinycssloaders)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER INTERFACE (UI) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = "The Blue Water Toolkit",
    theme = bs_theme(version = 5, preset = "flatly"),
    
    # Tab 1: Data Setup & Climatology ----
    tabPanel(
      title = "1. Data Setup",
      icon = icon("folder-open"),
      sidebarLayout(
        sidebarPanel(width = 4, h4("Upload Inputs"), data_upload_ui("data_module")),
        mainPanel(
          width = 8,
          tabsetPanel(
            type = "pills",
            tabPanel("Figure 1: Baseline Climate", 
                     h4("Monthly Climatology Validation", align = "center"),
                     withSpinner(plotOutput("plot_fig1_climate", height = "500px"), color = "#56B4E9")
            ),
            tabPanel("Data Dictionary",
                     h4("Input Data Requirements", align = "center"),
                     uiOutput("data_dictionary_ui")
            ),
            tabPanel("Raw Data Tables", 
                     br(),
                     selectInput("preview_dataset", "Select Dataset to View:", 
                                 choices = c("1. Precipitation", "2. ET0", "3. Soil Params", "4. Crop Params", 
                                             "5. Crop Kc Params", "6. Fruit Kc Params", "7. Crop Systems", 
                                             "8. Area by System", "9. Farm Counts")),
                     DTOutput("table_preview")
            )
          )
        )
      )
    ),
    
    # Tab 2: Kc curve Engine ----
    tabPanel(
      title = "2. Crop Kc Curves (Pre-Processing)",
      icon = icon("seedling"),
      sidebarLayout(
        sidebarPanel(width = 3, h4("Phenology Engine"), preproc_controls_ui("preproc_module")),
        mainPanel(
          width = 9,
          tabsetPanel(
            type = "pills",
            tabPanel("Kc Curves for Individual Crops", 
                     h4("Daily Kc based on Growth Stages", align = "center"),
                     withSpinner(plotOutput("plot_raw_kc", height = "600px"), color = "#56B4E9")
            ),
            tabPanel("Figure 2: Daily Kc for Crop Systems", 
                     h4("Daily Kc for Crop Systems (rows) by Rotations (columns)", align = "center"),
                     withSpinner(plotOutput("plot_fig2_kc_curves", height = "1000px"), color = "#56B4E9")
            )
          )
        )
      )
    ),
    
    # Tab 3: Track 1 - Biophysical Engine ----
    tabPanel(
      title = "3. Biophysical Engine",
      icon = icon("leaf"),
      sidebarLayout(
        sidebarPanel(width = 4, h4("Model Execution"), track1_controls_ui("track1_module")),
        mainPanel(
          width = 8,
          tabsetPanel(
            type = "pills",
            tabPanel("Figure 3: Supply vs Demand", withSpinner(plotOutput("plot_fig3_supply_demand"), color = "#56B4E9")),
            tabPanel("Figure 4: Blue Water Potential", withSpinner(plotOutput("plot_fig4_bwp"), color = "#56B4E9"))
          )
        )
      )
    ),
    
    # Tab 4: Track 2 - Socio-Demographic Impact ----
    tabPanel(
      title = "4. Socio-Demographics",
      icon = icon("users"),
      sidebarLayout(
        sidebarPanel(width = 3, h4("Demographic Analysis"), track2_controls_ui("track2_module")),
        mainPanel(
          width = 9,
          tabsetPanel(
            type = "pills",
            tabPanel("Figure 5: BWP By Farm Type", 
                     h4("Comparative Annual Blue Water Potential Across Production Systems", align = "center"),
                     withSpinner(plotOutput("plot_fig5_bwp_systems", height = "600px"), color = "#56B4E9")
            ),
            tabPanel("Figure 6: Farm Disparity", 
                     h4("Micro-Regional Disparity Metrics", align = "center"),
                     fluidRow(
                       column(6, h5("1950s Archetype", align = "center"), withSpinner(plotOutput("plot_disparity_1950", height = "450px"), color = "#56B4E9")),
                       column(6, h5("2022 Archetype", align = "center"), withSpinner(plotOutput("plot_disparity_2022", height = "450px"), color = "#56B4E9"))
                     )
            ),
            tabPanel("Figure 7: Village BWP Trends", 
                     h4("Annual Blue Water Potential of the Archetype Village", align = "center"),
                     withSpinner(plotOutput("plot_fig7_village_bwp", height = "500px"), color = "#56B4E9")
            ),
            tabPanel("Figure 8: Village Water Balance", 
                     h4("Average Annual Water Balance of the Archetype Village", align = "center"),
                     fluidRow(
                       column(6, h5("1950s Archetype", align = "center"), withSpinner(sankeyNetworkOutput("plot_village_sankey_1950", height = "450px"), color = "#56B4E9")),
                       column(6, h5("2022 Archetype", align = "center"), withSpinner(sankeyNetworkOutput("plot_village_sankey_2022", height = "450px"), color = "#56B4E9"))
                     )
            )
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
  engine_results <- track1_engine_server("track1_module", app_data, preproc_data)
  track2_results <- track2_engine_server("track2_module", app_data, engine_results)
  
  # --- TAB 1 RENDERING ----
  output$plot_fig1_climate <- renderPlot({
    validate(need(preproc_data$is_climate_processed, "Awaiting Data... Please load baseline or custom data from the sidebar to view the Climatology."))
    plot_climate_profile(preproc_data$climate_summary, show_temperature = FALSE)
  }, res = 96, bg = "white")
  
  output$data_dictionary_ui <- renderUI({
    HTML("
      <div style='padding: 15px; background-color: #f8f9fa; border-radius: 5px;'>
      <p>Ensure your CSV files contain the following exact column names. The engine requires these strict headers to execute the mathematical joins.</p>
      <ul>
        <li><b>01_precipitation:</b> <code>date</code> (YYYY-MM-DD), <code>p</code> (mm/day).</li>
        <li><b>02_et0:</b> <code>date</code> (YYYY-MM-DD), <code>et0</code> (mm/day).</li>
        <li><b>03_soil.parameters:</b> <code>soil</code> (ID), <code>whc.mmpercm</code> (Water Holding Capacity), <code>raw</code> (Readily Available Water).</li>
        <li><b>04_crop.parameters:</b> <code>crop</code> (Name), <code>rooting_depth_cm</code> (cm).</li>
        <li><b>05_crop_kc_curve:</b> Columns for each crop defining specific phenological days and Kc values.</li>
        <li><b>07_crop.systems:</b> <code>crop.system</code> (Rotation Name), <code>crop_1</code>, <code>crop_2</code>, <code>crop_3</code> (Sequential crops), <code>rotation</code> (Unique ID).</li>
        <li><b>08_area (Track 2):</b> <code>time_period</code>, <code>production_system</code> (Farm type), <code>area_ha</code> (hectares).</li>
        <li><b>09_farms (Track 2):</b> <code>time_period</code>, <code>production_system</code>, <code>number_of_farms</code>.</li>
      </ul>
      <p><small><i>Note: Extra columns in your files (like 'FORMULE', 'WB', or 'production_system_period') are safely ignored.</i></small></p>
      </div>
    ")
  })
  
  output$table_preview <- renderDT({
    req(app_data$is_loaded)
    dataset <- switch(input$preview_dataset, 
                      "1. Precipitation" = app_data$precip, 
                      "2. ET0" = app_data$et0,
                      "3. Soil Params" = app_data$soil_param,
                      "4. Crop Params" = app_data$crop_param,
                      "5. Crop Kc Params" = app_data$kc_raw,
                      "6. Fruit Kc Params" = app_data$raw_params,
                      "7. Crop Systems" = app_data$crop_systems, 
                      "8. Area by System" = app_data$ps_by_cs_area,
                      "9. Farm Counts" = app_data$farms_count)
    datatable(dataset, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # --- TAB 2 RENDERING ----
  output$plot_fig2_kc_curves <- renderPlot({
    validate(need(preproc_data$is_kc_processed, "Awaiting Processing... Please click 'Calculate Kc Curves' in the sidebar to generate this visualization."))
    plot_crop_coefficient_curves(preproc_data$crop_systems_l_kc)
  }, res = 96, bg = "white")
  
  output$plot_raw_kc <- renderPlot({
    validate(need(preproc_data$is_kc_processed, "Awaiting Processing... Please click 'Calculate Kc Curves' in the sidebar to generate this visualization."))
    ggplot(preproc_data$kc_doy_crop_all, aes(x = doy, y = kc, color = crop)) +
      geom_line(linewidth = 1) + facet_wrap(~ crop) + theme_bw() +
      labs(title = "Generated Crop Coefficient (Kc) Curves - All Types", x = "Day of Year (DOY)", y = "Crop Coefficient (Kc)") +
      theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = "white", colour = "black"), strip.background = element_blank())
  }, res = 96, bg = "white")
  
  # --- TAB 3 RENDERING ----
  output$plot_fig3_supply_demand <- renderPlot({
    validate(need(engine_results$is_calculated, "Awaiting Processing... Please run the Biophysical Engine to view this visualization."))
    vars_sd <- c("Irrigation Requirement (mm)", "Groundwater Recharge (mm)", "Crop ET Supply:Demand")
    plot_annual_climatology(engine_results$summary_yr_budgets$summ_qs_cs, vars_sd, "mean", "Mean Annual Fluxes (95% CI)")
  }, res = 96, bg = "white")
  
  output$plot_fig4_bwp <- renderPlot({
    validate(need(engine_results$is_calculated, "Awaiting Processing... Please run the Biophysical Engine to view this visualization."))
    plot_annual_climatology(engine_results$summary_yr_budgets$summ_qs_cs, "Blue Water Potential (mm)", "mean", "Blue Water Potential (mm)")
  }, res = 96, bg = "white")
  
  # --- TAB 4 RENDERING ----
  output$plot_fig5_bwp_systems <- renderPlot({
    validate(need(app_data$has_track2, "Track 2 Socio-Demographic data was not loaded. Include this data in Tab 1 to unlock these metrics."))
    validate(need(engine_results$is_calculated, "Awaiting Processing... Please run the Biophysical Engine on Tab 3 first."))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    plot_socio_economic_bwp(t2_data$ps_bwp_volumetric)
  }, res = 96, bg = "white")
  
  output$plot_disparity_1950 <- renderPlot({
    validate(need(app_data$has_track2, "Track 2 Socio-Demographic data was not loaded."))
    validate(need(engine_results$is_calculated, ""))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, target_period = "1950s")
  }, res = 96, bg = "white")
  
  output$plot_disparity_2022 <- renderPlot({
    validate(need(app_data$has_track2, ""))
    validate(need(engine_results$is_calculated, ""))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, target_period = "2022")
  }, res = 96, bg = "white")
  
  output$plot_fig7_village_bwp <- renderPlot({
    validate(need(app_data$has_track2, "Track 2 Socio-Demographic data was not loaded."))
    validate(need(engine_results$is_calculated, "Awaiting Processing... Please run the Biophysical Engine on Tab 3 first."))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    
    village.bwp_collapsed <- t2_data$ps_bwp_volumetric %>%
      mutate(bwp_thousand_m3 = round(total_vol_m3/10000)*10) %>%
      select(time_period, production_system, bwp_thousand_m3) %>%
      mutate(production_system = case_when(
        production_system == "Landless pastoralists" ~ "Pastoralists",
        production_system == "Sheep fatteners" ~ "Pastoralists",
        production_system %in% c("Rainfed groundnut growers (with buffaloes)", "Rainfed groundnut growers (with bulls)", "Rainfed groundnut growers (with local cows)") ~ "Groundnut growers (with livestock)",
        production_system %in% c("Rainfed groundnut growers (no livestock)", "Rainfed redgram growers (no livestock)") ~ "Groundnut growers (no livestock)",
        production_system == "Rice growers" ~ "Rice growers", 
        production_system %in% c("Small-scale fruit growers (banana)", "Small-scale fruit growers (grape vines)", "Small-scale fruit growers (pomegranate)", "Small-scale fruit growers (sweet lime)") ~ "Small-scale fruit growers (+ dairy)",
        production_system %in% c("Vegetable growers (no livestock)", "Vegetable growers with dairy farming") ~ "Vegetable growers",
        TRUE ~ as.character(production_system)
      )) %>%
      group_by(time_period, production_system) %>%
      summarise(bwp_thousand_m3 = sum(bwp_thousand_m3), .groups = "drop")
    
    colnames(village.bwp_collapsed) <- c("period", "label", "val")
    cfg <- list(box_width = 100, min_arrow_w = 5, max_arrow_w = 30, v_stagger_out = 3000)
    
    tmp_png <- tempfile(fileext = ".png")
    invisible(plot_village_bwp(village_bwp_collapsed = village.bwp_collapsed, cfg = cfg, output_dev = png, filename = tmp_png, manuscript_style = TRUE))
    
    if (file.exists(tmp_png)) {
      img <- png::readPNG(tmp_png)
      par(mar = c(0,0,0,0)) 
      plot.new()
      rasterImage(img, 0, 0, 1, 1)
    }
  }, res = 96, bg = "white")
  
  output$plot_village_sankey_1950 <- renderSankeyNetwork({
    validate(need(app_data$has_track2, "Track 2 Socio-Demographic data was not loaded."))
    validate(need(engine_results$is_calculated, ""))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    get_d3_sankey(t2_data$ps_volumetric_summary, "1950s")
  })
  
  output$plot_village_sankey_2022 <- renderSankeyNetwork({
    validate(need(app_data$has_track2, ""))
    validate(need(engine_results$is_calculated, ""))
    req(track2_results$track2_data())
    t2_data <- track2_results$track2_data()
    get_d3_sankey(t2_data$ps_volumetric_summary, "2022")
  })
  
  # --- CENTRALIZED PDF GENERATOR FUNCTION ----
  generate_pdf_report <- function(file) {
    req(engine_results$is_calculated)
    id_notify <- showNotification("Generating PDF Report. This may take a few seconds...", duration = NULL, type = "message")
    on.exit(removeNotification(id_notify), add = TRUE)
    
    pdf(file, width = 11, height = 8.5)
    
    print(plot_crop_coefficient_curves(preproc_data$crop_systems_l_kc) + ggtitle("Figure 2: Crop Coefficient Trajectories"))
    
    vars_sd <- c("Irrigation Requirement (mm)", "Groundwater Recharge (mm)", "Crop ET Supply:Demand")
    print(plot_annual_climatology(engine_results$summary_yr_budgets$summ_qs_cs, vars_sd, "mean", "Mean Annual Fluxes (95% CI)") + ggtitle("Figure 3: Annual Supply vs Demand"))
    
    print(plot_annual_climatology(engine_results$summary_yr_budgets$summ_qs_cs, "Blue Water Potential (mm)", "mean", "Blue Water Potential (mm)") + ggtitle("Figure 4: Blue Water Potential"))
    
    if (app_data$has_track2) {
      t2_data <- track2_results$track2_data()
      
      print(plot_socio_economic_bwp(t2_data$ps_bwp_volumetric) + ggtitle("Figure 5: BWP By Farm Type"))
      print(plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, "1950s") + patchwork::plot_annotation(title = "Figure 6: Micro-Regional Disparity Metrics (1950s Archetype)"))
      print(plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, "2022") + patchwork::plot_annotation(title = "Figure 6: Micro-Regional Disparity Metrics (2022 Archetype)"))
      
      village.bwp_collapsed <- t2_data$ps_bwp_volumetric %>%
        mutate(bwp_thousand_m3 = round(total_vol_m3/10000)*10) %>%
        select(time_period, production_system, bwp_thousand_m3) %>%
        mutate(production_system = case_when(
          production_system == "Landless pastoralists" ~ "Pastoralists",
          production_system == "Sheep fatteners" ~ "Pastoralists",
          production_system %in% c("Rainfed groundnut growers (with buffaloes)", "Rainfed groundnut growers (with bulls)", "Rainfed groundnut growers (with local cows)") ~ "Groundnut growers (with livestock)",
          production_system %in% c("Rainfed groundnut growers (no livestock)", "Rainfed redgram growers (no livestock)") ~ "Groundnut growers (no livestock)",
          production_system == "Rice growers" ~ "Rice growers", 
          production_system %in% c("Small-scale fruit growers (banana)", "Small-scale fruit growers (grape vines)", "Small-scale fruit growers (pomegranate)", "Small-scale fruit growers (sweet lime)") ~ "Small-scale fruit growers (+ dairy)",
          production_system %in% c("Vegetable growers (no livestock)", "Vegetable growers with dairy farming") ~ "Vegetable growers",
          TRUE ~ as.character(production_system)
        )) %>%
        group_by(time_period, production_system) %>%
        summarise(bwp_thousand_m3 = sum(bwp_thousand_m3), .groups = "drop")
      
      colnames(village.bwp_collapsed) <- c("period", "label", "val")
      cfg <- list(box_width = 100, min_arrow_w = 5, max_arrow_w = 30, v_stagger_out = 3000)
      invisible(plot_village_bwp(village_bwp_collapsed = village.bwp_collapsed, cfg = cfg, output_dev = png, filename = tempfile(fileext = ".png"), manuscript_style = TRUE))
    }
    dev.off()
  }
  
  # Map the central function to both UI buttons simultaneously
  output$`track1_module-download_report_t1` <- downloadHandler(
    filename = function() { paste0("Blue_Water_Toolkit_Report_", Sys.Date(), ".pdf") },
    content = generate_pdf_report
  )
  
  output$`track2_module-download_report` <- downloadHandler(
    filename = function() { paste0("Blue_Water_Toolkit_Report_", Sys.Date(), ".pdf") },
    content = generate_pdf_report
  )
}

shinyApp(ui = ui, server = server)