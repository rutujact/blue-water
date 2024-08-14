# ---
# title: "Run crop water budget function"
# subtitle: ""
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-12"
# ---
rm(list = ls())
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse)
pacman::p_load(ggpubr)
if (!dir.exists("figures/crop_kc_curves")) {dir.create("figures/crop_kc_curves", showWarnings = FALSE)}
if (!dir.exists("figures/water_budgets")) {dir.create("figures/water_budgets", showWarnings = FALSE)}

#~~~~~~~~~~~~~~~
# Get Input Data ----
#~~~~~~~~~~~~~~~
rain <- read.csv(file = "data-raw/rainfall.csv", header = TRUE) # Provide date in a standard unambiguous format: YYYY-MM-DD
et0.week.imd <- read.csv(file = paste0("data/et0.week.imd.csv"), header = TRUE) # year, week (0-52), et0
et0.daily <- read.csv(file = "data-raw/et0.csv", header = TRUE) # Provide date in a standard unambiguous format: YYYY-MM-DD

crop.param <- read.csv(file = "data/crop.parameters.csv", header = TRUE)
kc.doy.crops <- read.csv(file = "data/Kc_curve_parameters_by_crop_rotation.csv", header = TRUE)
soil.param <- read.csv(file = "data/soil.parameters.csv", header = TRUE)

#~~~~~~~~~~~~~~~
# Source Functions ----
#~~~~~~~~~~~~~~~
source("code/2.0_function_get_Peff.R")
source("code/3.0_function_get_et0.R")
source("code/4.0_function_get_kc.R")
source("code/5.0_function_get_budget.R")

#~~~~~~~~~~~~~~~
# Provide Input Parameters ----
#~~~~~~~~~~~~~~~
# Crop options
# unique(crop.param$crop)
# [1] "No_crop"        "Blackeyedbeans" "Horsegram"      "FieldBeans"     "FingerMillet"  
# [6] "Maize 15"       "FieldBeans 15"  "Ginger"         "Coconut"        "FORMULE"       
# [11] "Cabbage"        "Tomato"         "Tomato_Irri"    "Cucumber"       "Rice"          
# [16] "Maize"          "Banana"         "Areca"          "Beans_Irri"  
p.list <- list(rain = rain,
               crop.param = crop.param,
               soil.param = soil.param,
               # et0.timestep = "weekly", # weekly/daily
               #et0.input = et0.week.imd, # For "weekly": et0.week.imd
               et0.timestep = "daily", # weekly/daily
               et0.input = et0.daily, # For "daily": et0.daily
               kc.doy.crops = kc.doy.crops,
               # crops.vec = c("no_crop"),
               crops.vec = c("FieldBeans", "Horsegram"), # See crops options above 
               kc_no_crop = list(summer = 0.005, kr = 0.2),
               soil.type = "red",
               CN = 85,
               sm.init = 0.5)
#~~~~~~~~~~~~~~~
# Run functions ----
#~~~~~~~~~~~~~~~
met.forcing <- get_peff_et0(p = p.list)
budget <- get_budget(p = p.list, ts = met.forcing)
