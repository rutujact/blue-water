# ---
# title: "Load data and apply Crop Waterbudget functions"
# subtitle: ""
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-11"
# ---

# kc.doy.df <- read.csv(file = "data/Kc_curve_parameters_by_crop_rotation.csv", header = TRUE)

# List by Crop.Rotation
kc.doy.ls <- split(kc.doy.df, kc.doy.df$crop.rotation)

# "Prepare Kc curve climatology and recycle to the entire forcing met time series"
source("code/1.0_function_get_kc.R")
# kc.key <- kc.doy.ls[[3]] # Does not have doy 1 or 366
# kc.key <- kc.doy.ls[[1]] Has doy 1 and 366
# ts <- get_kc(kc.key = kc.key, from = as.Date("2024-01-01"),
#           to = as.Date(as.Date("2024-12-01")))
# plot(x = ts$doy, y = ts$kc)
kc.crops <- lapply(kc.doy.ls, get_kc)
# Output list 
