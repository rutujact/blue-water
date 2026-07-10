# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# global.R ----
# Foundational setup for the Blue Water Toolkit Shiny App
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1. Load Required Libraries ----
library(shiny)
library(bslib)
library(shinyWidgets)         
library(tidyverse)
library(data.table)
library(lubridate)   
library(stringr)
library(scales)                    
library(ggpubr)
library(patchwork)
library(RColorBrewer)    
library(networkD3)
library(htmlwidgets)             
library(parallel)
library(pbapply)                  
library(here)

# 2. Source Core Engine Functions ----
source(here::here("code", "functions", "01_get_kc.R"))
source(here::here("code", "functions", "02_get_Peff.R"))
source(here::here("code", "functions", "03_expand_kc.R"))
source(here::here("code", "functions", "04_get_budget.R"))
source(here::here("code", "functions", "05_get_figures.R"))

# 3. Define Global Biophysical Constants ----
biophysical_parameters <- list(
  grazing_growth_rate   = 0.00458333333333333,
  grazing_drop_kc       = 0.20,
  grazing_max_kc        = 0.75,
  napier_decline_rate_1 = 0.000307692307692308,
  napier_decline_rate_2 = 0.000153846153846154
)