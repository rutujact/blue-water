# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# app.R
# Main launch script for the Blue Water Toolkit
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

source(here::here("shiny_app", "global.R"))
source(here::here("shiny_app", "R", "ui_modules.R"))
source(here::here("shiny_app", "R", "server_modules.R"))

if (!require("DT", quietly = TRUE)) install.packages("DT")
library(DT)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# USER INTERFACE (UI) ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ui <- navbarPage(
  title = "The Blue Water Toolkit",
  theme = bs_theme(version = 5, preset = "cosmo"),
  
  # Inject Tooltip JavaScript into the header
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
        
        # --- NEW INTRODUCTORY CARD ---
        div(class = "card mb-4 border-info",
            div(class = "card-body",
                h4("Accounting for the Unaccounted: Farm Social Diversity in Water Budgets", class = "card-title text-info"),
                p(strong("How has the partitioning of our atmospheric water inputs been fundamentally altered by human land use?[cite: 11]")),
                p("This toolkit provides a framework for modeling net watershed impacts by incorporating farm social diversity[cite: 11]. It calculates Blue Water Potential (BWP) to isolate the absolute net impact of a specific land use decision on the underlying aquifer[cite: 11]."),
                tags$ul(
                  tags$li(strong("The Assessment:"), " The engine identifies which farm topologies act as chronic hydrological 'Net Consumers' via artificial irrigation, and which act as 'Net Contributors' by enabling monsoon precipitation to actively recharge the aquifer[cite: 11]."),
                  tags$li(strong("The Engine:"), " The model processes daily meteorological forcing, soil parameters, and crop rotation definitions[cite: 10]. It dynamically generates continuous crop coefficient (Kc) curves using standard FAO-56 segments, polynomial equations, and complex grazing regrowth schedules to simulate multi-decadal water fluxes[cite: 10]."),
                  tags$li(strong("The Discovery:"), " Scaling BWP by farm typology reveals the deeply unequal distribution of net water extraction across the agricultural landscape[cite: 11]. Aggregating these balances to an archetype village illustrates structural tipping points in regional water security[cite: 11].")
                )
            )
        ),
        
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 1: Baseline Climate", 
                   h4("Monthly Climatology Validation", info_tooltip("Monthly baseline metrics showing mean rainfall and reference evapotranspiration (ET0)."), align = "center"),
                   plotOutput("plot_fig1_climate", height = "500px")
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
  
  # Tab 2: Phenology Engine ----
  tabPanel(
    title = "2. Crop Phenology",
    icon = icon("seedling"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   h4("Phenology Engine", info_tooltip("Generates daily curves based on underlying mathematical definitions: standard FAO-56 segmented curves, continuous polynomial equations for rapid growth, and complex linear regrowth rates for pastures.")), 
                   preproc_controls_ui("preproc_module")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          tabPanel("Individual Crop", 
                   h4("Daily Kc curve based on Growth Stages", align = "center"),
                   plotOutput("plot_raw_kc", height = "600px")
          ),
          tabPanel("Figure 2: Daily Kc for Crop Systems", 
                   h4("Daily Kc for Crop Systems (rows) by Rotations (columns)", align = "center"),
                   plotOutput("plot_fig2_kc_curves", height = "1000px")
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
          tabPanel("Figure 3: Supply vs Demand", 
                   h4("Annual Supply vs Demand", info_tooltip("Annual Climatology of Water Supply and Demand. Groundwater Recharge is depicted in negative terms on the water fluxes axis."), align = "center"),
                   plotOutput("plot_fig3_supply_demand")
          ),
          tabPanel("Figure 4: Blue Water Potential", 
                   h4("Blue Water Potential", info_tooltip("Blue Water Potential isolates the absolute net impact of a specific land use decision on the underlying aquifer (Recharge minus Irrigation Requirement)."), align = "center"),
                   plotOutput("plot_fig4_bwp")
          )
        )
      )
    )
  ),
  
  # Tab 4: Track 2 - Socio-Demographic Impact ----
  tabPanel(
    title = "4. Socio-Demographics",
    icon = icon("users"),
    sidebarLayout(
      sidebarPanel(width = 3, 
                   h4("Demographic Analysis", info_tooltip("Net Consumers (Irrigated Farms, BWP < 0): Demand exceeds natural recharge. Net Contributors (Rainfed/Grasslands, BWP > 0): Monsoon precipitation actively recharges the aquifer.")), 
                   track2_controls_ui("track2_module")
      ),
      mainPanel(
        width = 9,
        tabsetPanel(
          type = "pills",
          tabPanel("Figure 5: BWP By Farm Type", 
                   h4("Comparative Annual Blue Water Potential Across Production Systems", info_tooltip("BWP contributions (+ve or -ve) from Irrigated, Rainfed, or Grazed areas within each farm type are mapped by colors. Net BWP contribution is identified by a cross."), align = "center"),
                   plotOutput("plot_fig5_bwp_systems", height = "600px")
          ),
          tabPanel("Figure 6: Farm Disparity", 
                   h4("Micro-Regional Disparity Metrics", info_tooltip("Farm-level demographic composition and net BWP impact. Demographic data reflect total farm counts, while BWP values signify the cumulative water budget aggregated by farm typology."), align = "center"),
                   uiOutput("dynamic_disparity_ui")
          ),
          tabPanel("Figure 7: Village BWP Trends", 
                   h4("Annual Blue Water Potential of the Archetype Village", align = "center"),
                   plotOutput("plot_fig7_village_bwp", height = "500px")
          ),
          tabPanel("Figure 8: Village Water Balance", 
                   h4("Average Annual Water Balance of the Archetype Village", info_tooltip("Notice the structural tipping point: By 2022, the massive evapotranspiration demands of irrigated crops physically constrict long-term aquifer replenishment compared to the 1950s baseline."), align = "center"),
                   uiOutput("dynamic_sankey_ui")
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
        <li><b>05_crop_kc_curve:</b> Columns for each crop defining days identifying growth stages and Kc values.</li>
        <li><b>07_crop.systems:</b> <code>crop.system</code> (Rotation Name), <code>crop_1</code>, <code>crop_2</code>, <code>crop_3</code> (Sequential crops), <code>rotation</code> (Unique ID).</li>
        <li><b>08_area (Track 2):</b> <code>time_period</code>, <code>production_system</code> (Farm type), <code>area_ha</code> (hectares).</li>
        <li><b>09_farms (Track 2):</b> <code>time_period</code>, <code>production_system</code>, <code>number_of_farms</code>.</li>
      </ul>
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
                      "5. FAO Kc Params" = app_data$kc_raw,
                      "6. Monthly Kc Params" = app_data$raw_params,
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
  
  # Dynamic UI for Figure 6
  output$dynamic_disparity_ui <- renderUI({
    req(track2_results$track2_data())
    periods <- sort(unique(track2_results$track2_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    cols <- lapply(seq_along(periods), function(i) {
      column(12, h5(paste(periods[i], "Archetype"), align = "center"), plotOutput(paste0("plot_disparity_", i), height = "450px"))
    })
    do.call(fluidRow, cols)
  })
  
  # Dynamic Plot Rendering for Figure 6
  observe({
    req(track2_results$track2_data())
    periods <- sort(unique(track2_results$track2_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    for (i in seq_along(periods)) {
      local({
        my_i <- i
        my_p <- periods[my_i]
        output[[paste0("plot_disparity_", my_i)]] <- renderPlot({
          t2_data <- track2_results$track2_data()
          plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, target_period = my_p)
        }, res = 96, bg = "white")
      })
    }
  })
  
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
    
    # Restored working PNG tempfile logic for Village BWP
    tmp_png <- tempfile(fileext = ".png")
    invisible(plot_village_bwp(village_bwp_collapsed = village.bwp_collapsed, cfg = cfg, output_dev = png, filename = tmp_png, manuscript_style = TRUE))
    
    if (file.exists(tmp_png)) {
      img <- png::readPNG(tmp_png)
      par(mar = c(0,0,0,0)) 
      plot.new()
      rasterImage(img, 0, 0, 1, 1)
    }
  }, res = 96, bg = "white")
  
  # Dynamic UI for Figure 8 (Sankey Diagram)
  output$dynamic_sankey_ui <- renderUI({
    req(track2_results$track2_data())
    periods <- sort(unique(track2_results$track2_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    # Stack vertically if only 1 period, place side-by-side if multiple periods
    col_width <- ifelse(length(periods) > 1, 6, 12) 
    
    cols <- lapply(seq_along(periods), function(i) {
      column(col_width, h5(paste(periods[i], "Archetype"), align = "center"), sankeyNetworkOutput(paste0("plot_village_sankey_", i), height = "450px"))
    })
    do.call(fluidRow, cols)
  })
  
  # Dynamic Plot Rendering for Figure 8
  observe({
    req(track2_results$track2_data())
    periods <- sort(unique(track2_results$track2_data()$ps_bwp_volumetric$time_period), decreasing = TRUE)
    for (i in seq_along(periods)) {
      local({
        my_i <- i
        my_p <- periods[my_i]
        output[[paste0("plot_village_sankey_", my_i)]] <- renderSankeyNetwork({
          t2_data <- track2_results$track2_data()
          get_d3_sankey(t2_data$ps_volumetric_summary, my_p)
        })
      })
    }
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
      
      # Dynamic PDF generation for available time periods
      periods <- sort(unique(t2_data$ps_bwp_volumetric$time_period), decreasing = TRUE)
      for (p in periods) {
        print(plot_farm_bwp_disparity(t2_data$ps_bwp_volumetric, t2_data$ps_typology, p) + patchwork::plot_annotation(title = paste("Figure 6: Micro-Regional Disparity Metrics (", p, "Archetype)")))
      }
      
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
      
      # Restored working PNG tempfile logic for Village BWP PDF
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