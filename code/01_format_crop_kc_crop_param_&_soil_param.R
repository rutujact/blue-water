# ---
# title: "Prepare data from Laurent's Crop Water Budget in Excel"
# subtitle: ""
# author: "Rutuja Chitra-Tarak"
# date: "2024-07-18"
# output: html_document
# ---
rm(list = ls())
if (!require("pacman")) install.packages("pacman"); library(pacman)
pacman::p_load(tidyverse)
if (!dir.exists("data/processed")) {dir.create("data/processed", showWarnings = FALSE)}
# Chapter 6 - ETc - Single crop coefficient (Kc)
# https://www.fao.org/4/X0490E/x0490e0b.htm
# https://www.researchgate.net/figure/FAO-segmented-crop-coefficient-curve-and-four-growing-stages-after-Allen-et-al-1998_fig5_225419080
# literature/FAO-segmented-crop-coefficient-curve-and-four-growing-stages-after-Allen-et-al-1998.pdf
# "Kc_ini" value continues from "Sowing date" to "End of ini" 
# "Kc_mid" value continues from "End of Dev" to "End of Mid"
# "Kc_end" is the value at "Harvest date"

# Get parameters for FAO Segmented Crop Coefficient (Kc) curve by crop of each rotation ----
Kc.raw <- readxl::read_xlsx("data/raw/Kc Billikere final_mod.xlsx",
                                   sheet = "Crop cycles Kc",
                                   range = "A1:AX15")

# Wrangle raw data to prepare Kc parameter table by DOY----
# Remove columns with all NAs
Kc.raw <- Kc.raw[, !as.logical(sapply(Kc.raw, function(x)all(is.na(x))))]
Kc.raw <- Kc.raw[,-2]
# Remove extra row
Kc.raw <- Kc.raw[-2,]

# Header edits
colnames(Kc.raw)[1] <- "var"
colnames(Kc.raw)[2] <- "FORMULE"
# Remove header extras
cols.to.change <- colnames(Kc.raw)[stringr::str_detect(colnames(Kc.raw), "[...]")]

colnames(Kc.raw)[stringr::str_detect(colnames(Kc.raw), "[...]")] <- 
  gsub("...[0-9]*$", "", cols.to.change, perl = TRUE)

# To add crop name to header
colnames(Kc.raw)[-c(1,2)] <- paste0(colnames(Kc.raw)[-c(1,2)], ".", Kc.raw[1, c(-1,-2)])

# Remove extra row with crop name
Kc.raw <- Kc.raw[-1,]
# Change name of "[values as used in the generic] formula" to pattern
colnames(Kc.raw)[2] <- "Rotation 0.Kc_formule"

# Separate crops by Rotation
Kc.raw.long <- Kc.raw %>% 
  pivot_longer(cols = starts_with("Rotation"), 
                          names_pattern = "(Rotation )([0-9]{1,2}).(Kc_)(.*)",
                          names_to = c("text", "rotation", "Kc", "crop")) %>%
  select(-text, -Kc)
# unique(Kc.raw$var)
# [1] "Sowing date"  "Kc_ini"       "Ini_duration" "End of ini"   "Dev_duration"
# [6] "End of Dev"   "Kc_mid"       "Mid_duration" "End of Mid"   "Kc_end"      
# [11] "End_duration" "Harvest date"
# Only select rows with Kc values
Kc <- Kc.raw.long %>% 
  subset(var %in% Kc.raw$var[stringr::str_detect(Kc.raw$var, "Kc")]) %>%
  rename(Kc = value, Kc_var = var)
  # pivot_wider(names_from = c(crop, rotation),
  #             names_glue = "{crop}_{rotation}",
  #             values_from = value)

# Only select rows with DOY values
doy <- Kc.raw.long %>% 
  subset(var %in% c("Sowing date", "End of ini", "End of Dev", 
                    "End of Mid", "Harvest date")) %>%
  rename(doy = value, doy_var = var) %>%
# IF a doy is 365, it should be 366 to include leap years:
  mutate(doy = if_else(doy == "365", "366", doy))

Kc.doy.key <- data.frame(doy_var = c("Sowing date", "End of ini", "End of Dev", "End of Mid", "Harvest date"),
                           Kc_var = c("Kc_ini", "Kc_ini", "Kc_mid", "Kc_mid", "Kc_end"))

doy <- left_join(doy, Kc.doy.key, by = c("doy_var"))
Kc.doy <- left_join(doy, Kc, by = c("Kc_var", "crop", "rotation")) %>%
  arrange(crop, rotation, doy, Kc) %>%
  mutate(doy = as.numeric(doy), Kc = as.numeric(Kc))

# This is FAO Kc Parameter Table by DOY by Crop and Rotation----
Kc.doy.df <- Kc.doy %>% select(rotation, crop, doy_var, doy, Kc) %>%
  unite(crop.rotation, c("crop", "rotation"), sep = "_", remove = FALSE) %>%
  arrange(crop.rotation, doy, Kc) #%>%
  # mutate(irrigation = if_else(grepl("Iri", crop.rotation, ignore.case = TRUE), 
  #                             "Irrigated", 
  #                             if_else(grepl("Irri", crop.rotation, ignore.case = TRUE), 
  #                                     "Irrigated", 
  #                                     "Rainfed")))
write.csv(Kc.doy.df, file = "data/processed/Kc_curve_parameters_by_crop_rotation.csv", row.names = FALSE)

# Format & Save Crop Parameters (rooting depth, irrigation, season)----
param.raw <- readxl::read_xlsx("data/raw/Model Crop Rotations Water budget Billikere_estimated rotations_effective rainfall.xlsx",
                            sheet = "CROP paprameter")

param.raw <- param.raw[, !as.logical(sapply(param.raw, function(x)all(is.na(x))))]
param.raw <- param.raw %>% 
  filter(!if_all(everything(), is.na)) %>% 
  filter(!if_all(2:4, is.na))
crop.param <- param.raw 
# > colnames(param.raw)
# [1] "CROP"               "irrig =1 rainfed=2" "rooting depth (cm)"
# [4] "season"
colnames(crop.param) <- c("crop", "irrigation", "rooting_depth_cm", "season")
crop.param <- crop.param %>%
  mutate(irrigation = case_match(as.character(irrigation), "1" ~ "irrigated", "2" ~ "rainfed", .default = NA))
match.param.crop <- pmatch(tolower(unique(Kc.doy.df$crop)), tolower(crop.param$crop))
# Crops found in param
tolower(crop.param$crop)[match.param.crop]
# Crops not found in param
tolower(unique(Kc.doy.df$crop))[is.na(match.param.crop)]
# "millet"
write.csv(crop.param, file = "data/processed/crop.parameters.csv", row.names = FALSE)

# Save Soil Parameters (soil.type, whc, raw)----
soil.param <- data.frame(soil.type = c("red", "black"),
           whc.mmpercm = c(1.2, 1.7), # mm/cm soil
           raw = c(0.4, 0.4)) # %
write.csv(soil.param, file = "data/processed/soil.parameters.csv", row.names = FALSE)
