# ---
# title: "Function to get kc curve time series"
# subtitle: "Prepare kc curve climatology (by doy) and recycle to the entire forcing met time series"
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-11"
# input: (1) A data frame of kc curve parameters, with doy and kc values corresponding to 
#            data.frame(doy_var = c("Sowing date", "End of ini", "End of Dev", "End of Mid", "Harvest date"),
#                       kc_var = c("kc_ini", "kc_ini", "kc_mid", "kc_mid", "kc_end")):
#            class=data.frame(doy=numeric, kc=numeric), class=data.frame, dim=[5, 2], 
#        (2) beginning date as.Date(), class=date, length=1 
#        (3) end date as.Date() for the met data time series, class=date, length=1
# output: data.frame(date=Date, doy=numeric, kc=numeric), class=data.frame, dim=[366, 3], 
# example:
# source("code/4.0_function_get_kc.R")
# ts <- get_kc(kc.key = kc.key, from = as.Date("2024-01-01"),
#           to = as.Date(as.Date("2024-12-01")))
# plot(x = ts$doy, y = ts$kc)
# kc.crops <- lapply(kc.doy.ls, get_kc)
# Output list 
# ---
get_kc <- function(kc.key, 
                   from = as.Date("1951/01/01"),
                   to = as.Date("2023/12/31")) {
  # Example Data
  # kc.doy.df <- read.csv(file = "data/kc_curve_parameters_by_crop_rotation.csv", header = TRUE)
  # 
  # # List by Crop.Rotation
  # kc.doy.ls <- split(kc.doy.df, kc.doy.df$crop.rotation)
  # 
  # kc.key <- kc.doy.ls[[3]] # Does not have doy 1 or 366
  # kc.key <- kc.doy.ls[[1]] # Has doy 1 and 366
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  # For plotting phases :----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  phase <- kc.key[, c("doy_var", "doy")] %>%
    mutate(phase.mid.doy = doy + round((lead(doy) - doy)/2),
           phase = case_match(doy_var, 
                              "Sowing date" ~ "Initial phase", 
                              "End of ini" ~ "Developmental phase",
                              "End of Dev" ~ "Mid Season phase",
                              "End of Mid" ~ "Late Season phase",
                              .default = NA)) %>%
    select(phase, phase.mid.doy) %>%
    subset(!is.na(phase.mid.doy))
  
  actions <- kc.key[, c("doy_var", "doy")] %>%
    subset(doy_var %in% c("Sowing date", "Harvest date")) %>%
    mutate(actions = case_match(doy_var, 
                              "Sowing date" ~ "Sowing", 
                              "Harvest date" ~ "Harvest",
                              .default = NA)) %>%
    select(actions, doy)
    
  #~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  df <- kc.key[, c("doy", "kc")]
  df$doy <- as.numeric(df$doy)
  df$kc <- as.numeric(df$kc)
  # If doy = 1 OR doy = 366 absent,
  # Provide kc values as 0 for the entire period before the sowing date and after the harvest date----
 
  # if doy 1 absent: 
  while(!any(df$doy == 1)) { 
    # any() is TRUE if any one doy matches 1.
    # So, only when no doy matches 1, 
    # any() is FALSE and thus !any() is TRUE and 
    # only then while() condition is met and the following expression is evaluated
    pre.sowing <- data.frame(doy = c(1, c(df$doy[1]-1)),
                            kc = rep(0, 2))
    df <- rbind(pre.sowing, df)
  } 
  # if doy 366 absent:
  while(!any(df$doy == 366)) {
    post.harvest <- data.frame(doy = c(c(df$doy[nrow(df)]+1), 366),
                               kc = rep(0, 2))
      
    df <- rbind(df, post.harvest)
  }

  df <- df[order(df$doy),]
  
  # Build kc curve climatology----
  df$slope <- df$intercept <- NA
  for(i in 2:nrow(df)){
    df$slope[i] <- (df$kc[i] - df$kc[i-1])/(df$doy[i] - df$doy[i-1])
    df$intercept[i] <- df$kc[i] - df$doy[i]*df$slope[i]
  }
  # Build kc climatology----
  clim <- data.frame(doy = as.numeric(seq(1, 366)))
  clim$kc <- NA
  for(i in 2:nrow(df)){
    if (i == nrow(df)) {
      rows <- which(clim$doy <= df$doy[i] & clim$doy >= df$doy[i-1])
    } else {
      rows <- which(clim$doy < df$doy[i] & clim$doy >= df$doy[i-1])
    }
    clim$kc[rows] <- (clim$doy[rows]*df$slope[i]) + df$intercept[i]
  }
  
  # pdf(paste0("figures/crop_kc_curves/", toupper(kc.key$crop[1]),".pdf"), width = 7, height = 5)
  # plot(1, type="n", xlab = "DOY", ylab = "kc", xlim=c(0, 366), 
  #      ylim=c(0, (max(clim$kc)*1.1)),
  #      main = paste0(toupper(kc.key$crop[1]), "\nSegmented Crop Coefficient Curve"))
  # lines(x = clim$doy, y = clim$kc, type = "l")
  # segments(x0 = kc.key$doy, y0 = 0, x1 = kc.key$doy, y1 = kc.key$kc, 
  #          cex = .5, col = "blue", lty = "dotted")
  # points(x = kc.key$doy, y = kc.key$kc, 
  #          cex = .5, col = "blue")
  # text(x = kc.key$doy, y = (kc.key$kc)*1.05, labels = kc.key$doy, 
  #     cex = 0.8, col = "blue")
  # text(x = phase$phase.mid.doy, y = 0.02, labels = phase$phase, 
  #      cex = 0.8, col = "dark red", srt = 90, adj = 0)
  # text(x = actions$doy + c(-7, 7), y =  0.02, labels = actions$actions, 
  #      cex = 0.8, col = "magenta", srt = 90, adj = 0)
  # dev.off()
  # Recycle kc climatology for the full forcing data dates----
  ts <- data.frame(date = as.Date(seq(from, to, by = '1 day')))
  ts$doy <-
    as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), format = "%j"))
  ts <- merge(ts, clim, by = "doy")
  ts<-ts[,c("date","doy","kc")]

  return(ts)
}