# ---
# title: "Function to expand kc curve time series"
# subtitle: "Recycle kc curve climatology (by doy) to the entire forcing met time series"
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-11"
# input: (1) A data frame of kc by doy
#            data.frame(doy_var = c("Sowing date", "End of ini", "End of Dev", "End of Mid", "Harvest date"),
#                       kc_var = c("kc_ini", "kc_ini", "kc_mid", "kc_mid", "kc_end")):
#            class=data.frame(doy=numeric, kc=numeric), class=data.frame, dim=[5, 2], 
#        (2) beginning date as.Date(), class=date, length=1 
#        (3) end date as.Date() for the met data time series, class=date, length=1
# output: data.frame(date=Date, doy=numeric, kc=numeric), class=data.frame, dim=[366, 3], 
# example:
# source("code/03_function_expand_kc.R")
# ts <- expand_kc(kc.key = kc.key, from = as.Date("2024-01-01"),
#           to = as.Date(as.Date("2024-12-01")))
# plot(x = ts$doy, y = ts$kc)
# kc.crops <- lapply(kc.doy.ls, expand_kc)
# Output list 
# ---
expand_kc <- function(kc.doy, 
                   from = as.Date("1951/01/01"),
                   to = as.Date("2023/12/31")) {
  # Example Data
  # kc.doy.df <- read.csv(file = "data/processed/kc_curve_parameters_by_crop_rotation.csv", header = TRUE)
  # 
  # # List by Crop.Rotation
  # kc.doy.ls <- split(kc.doy.df, kc.doy.df$crop.rotation)
  # 
  # kc.key <- kc.doy.ls[[3]] # Does not have doy 1 or 366
  # kc.key <- kc.doy.ls[[1]] # Has doy 1 and 366
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  df <- kc.key[, c("doy", "kc")]
  df$doy <- as.numeric(df$doy)
  df$kc <- as.numeric(df$kc)
  # Build kc climatology----
  
  # Recycle kc climatology for the full forcing data dates----
  ts <- data.frame(date = as.Date(seq(from, to, by = '1 day')))
  ts$doy <-
    as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), format = "%j"))
  ts <- left_join(ts, clim, by = "doy")
  ts<-ts[,c("date","doy","kc")]
  
  return(list(ts = ts, kc.key = kc.key, clim = clim, phase = phase, actions = actions))
}