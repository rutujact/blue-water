# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R/ui_modules.R
# User Interface Modules for Scales 1, 2, and 3
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Helper to create a native Bootstrap 5 HTML tooltip ----
info_tooltip <- function(text) {
  tags$span(
    icon("info-circle"), 
    `data-bs-toggle` = "tooltip", 
    `data-bs-placement` = "right",
    title = text, 
    style = "cursor: help; color: #2C3E50; margin-left: 6px; font-size: 0.85em;"
  )
}

# Module: Data Upload UI ----
data_upload_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("By default, the application reads the standardized baseline files directly from the repository's data directory."),
    actionButton(ns("load_default_data"), "Load Repository Data", icon = icon("database"), class = "btn-primary w-100"),
    hr(),
    checkboxInput(ns("use_custom"), "Upload Custom Data", value = FALSE),
    conditionalPanel(
      condition = sprintf("input['%s'] == true", ns("use_custom")),
      
      p(tags$small(em("Note: Uploading custom data will securely update the active baseline CSVs in your local 'data/input/' directory to synchronize with the final HTML report generator."))),
      
      h5("Scale 1: Cropping Systems Data", info_tooltip("Files 1-8 define the hydrological forcing, soil types, and crop phenology.")),
      fileInput(ns("custom_model"), "1. Model Params (01_model_parameters)", accept = ".csv"),
      fileInput(ns("custom_precip"), "2. Precipitation (02_precip)", accept = ".csv"),
      fileInput(ns("custom_et0"), "3. Reference ET0 (03_et0)", accept = ".csv"),
      fileInput(ns("custom_soil"), "4. Soil Params (04_soil)", accept = ".csv"),
      fileInput(ns("custom_crop"), "5. Crop Params (05_crop)", accept = ".csv"),
      fileInput(ns("custom_kc"), "6. FAO Kc Params (06_fao_kc)", accept = ".csv"),
      fileInput(ns("custom_fruit"), "7. Monthly Step Kc Params (07_monthly_kc) [Optional]", accept = ".csv"),
      fileInput(ns("custom_systems"), "8. Crop Systems (08_systems)", accept = ".csv"),
      
      hr(),
      checkboxInput(ns("enable_scale2"), "Include Scale 2 Data (Production Systems)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("enable_scale2")),
        fileInput(ns("custom_area"), "9. Area by Crop System (09_area)", accept = ".csv"),
        
        hr(),
        checkboxInput(ns("enable_scale3"), "Include Scale 3 Data (Agrarian Systems)", value = TRUE),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("enable_scale3")),
          fileInput(ns("custom_farms"), "10. Farm Counts (10_farms)", accept = ".csv")
        )
      ),
      
      br(),
      actionButton(ns("load_custom_data"), "Validate & Load Custom Data", icon = icon("upload"), class = "btn-warning w-100")
    )
  )
}

# Module: Pre-Processing Controls (Crop Phenology Module) ----
preproc_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Click below to dynamically generate the continuous daily Crop Coefficient (Kc) curves. This must be completed before running the Core Hydrological Model."),
    hr(),
    uiOutput(ns("run_preproc_ui"))
  )
}

# Module: Scale 1 Controls (Core Hydrological Model) ----
scale1_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Click below to process the full water budget across all crop rotations and soil types (Scale 1). This multi-decadal simulation will take a few minutes."),
    hr(),
    uiOutput(ns("run_engine_ui")),
    br(), br(),
    uiOutput(ns("download_ui_s1"))
  )
}

# Module: Scale 2 Controls (Production System Level) ----
scale2_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("These visualisations map the hydrological water budgets to socio-economic farm catgories."),
    hr(),
    uiOutput(ns("download_ui_s2"))
  )
}

# Module: Scale 3 Controls (Agrarian System Level) ----
scale3_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("These visualisations scale farm footprints by demographic counts to reveal micro-regional impacts."),
    hr(),
    uiOutput(ns("download_ui_s3"))
  )
}