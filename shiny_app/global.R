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
library(DT)
library(quarto)


# 2. Source Core Engine Functions ----
source(here::here("code", "functions", "01_get_kc.R"))
source(here::here("code", "functions", "02_get_Peff.R"))
source(here::here("code", "functions", "03_get_budget.R"))
source(here::here("code", "functions", "04_get_figures.R"))
source(here::here("code", "functions", "05_case_study_formatters.R"))