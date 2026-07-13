# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# R/ui_modules.R
# User Interface Modules for Track 1 and Track 2
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
      
      h5("Biophysical Data (Track 1)", info_tooltip("Files 1-7 define the hydrological forcing, soil types, and crop phenology.")),
      fileInput(ns("custom_precip"), "1. Precipitation (01_precip)", accept = ".csv"),
      fileInput(ns("custom_et0"), "2. Reference ET0 (02_et0)", accept = ".csv"),
      fileInput(ns("custom_soil"), "3. Soil Params (03_soil)", accept = ".csv"),
      fileInput(ns("custom_crop"), "4. Crop Params (04_crop)", accept = ".csv"),
      fileInput(ns("custom_kc"), "5. Crop Kc Params (05_kc)", accept = ".csv"),
      fileInput(ns("custom_fruit"), "6. Fruit Kc Params (06_fruit) [Optional]", accept = ".csv"),
      fileInput(ns("custom_systems"), "7. Crop Systems (07_systems)", accept = ".csv"),
      
      hr(),
      checkboxInput(ns("enable_track2"), "Include Socio-Demographic Data (Track 2)", value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] == true", ns("enable_track2")),
        fileInput(ns("custom_area"), "8. Area by System (08_area)", accept = ".csv"),
        fileInput(ns("custom_farms"), "9. Farm Counts (09_farms)", accept = ".csv")
      ),
      
      actionButton(ns("load_custom_data"), "Validate & Load Custom Data", icon = icon("upload"), class = "btn-warning w-100")
    )
  )
}

# Module: Pre-Processing Controls (Kc Engine) ----
preproc_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Click below to dynamically generate the continuous daily Crop Coefficient (Kc) curves. This must be completed before running the Biophysical Engine."),
    hr(),
    uiOutput(ns("run_preproc_ui"))
  )
}

# Module: Track 1 Controls (Biophysical Engine) ----
track1_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("Click below to process the full regional water budget across all crop rotations and soil types. This multi-decadal simulation will take a few minutes."),
    hr(),
    uiOutput(ns("run_engine_ui")),
    br(), br(),
    uiOutput(ns("download_ui_t1"))
  )
}

# Module: Track 2 Controls (Socio-Hydrological Engine) ----
track2_controls_ui <- function(id) {
  ns <- NS(id)
  tagList(
    p("These visualisations map the physical water budgets to specific socio-economic farm topologies and historical demographic data."),
    hr(),
    uiOutput(ns("download_ui_t2"))
  )
}