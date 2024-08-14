# ---
# title: "Function to get aet and other water budget time series"
# subtitle: "Calculate ks based on kcr and etcr"
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-12"
# input: (1) Peff time series, output from 2.0_function_get_Peff.R: 
#            data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric), class=data.frame, dim=n,4 
#        (2) Et0 time series, output from source("code/3.0_function_get_et0.R")
#        (3) Rotation crops vector
#        (4) Kc time series, output from source("code/4.0_function_get_kc.R")
#        (5) Soil type, class=character, length=1 # any case allowed (Capital/title/lower) 
#        (6) soil parameter data.frame, data.frame(soil.type = c("red", "black"), whc = c(1.2, 1.7), raw = c(0.4, 0.4)) 
#           soil.param <- read.csv(file = "data/soil.parameters.csv", header = TRUE)
#        (7) Initial SM = for example, 0.5 of WHC

# Some functions (get_Peff, get_et0) that won't vary by specified crop rotation 
# are invoked outside of get_aet, via get_peff_et0. 
# This could be important for speed when get_aet is invoked multiple times for different crop rotations 

# output: data.frame(date=Date, P=numeric, Q=numeric, Peff=numeric, ks = numeric), class=data.frame, dim=n,3 
# example:
# source("code/5.0_function_get_budget.R")

# ---
# TO DO: Crumbs..
# 1. Find appropriate CROP parameters by Irrigation and Season
# 2. update output above 

#~~~~~~~~~~~~~~~
# Function 1 of 2:  get_peff_et0 ----
#~~~~~~~~~~~~~~~
get_peff_et0 <- function(p = p.list) {
  # #~~~~~~~~~~~~~~~
  # ## source functions ----
  # #~~~~~~~~~~~~~~~
  # source("code/2.0_function_get_Peff.R")
  # source("code/3.0_function_get_et0.R")
  
  #~~~~~~~~~~~~~~~
  ## Get Peff ----
  #~~~~~~~~~~~~~~~
  ts <- get_Peff(rain = p$rain, CN = p$CN, soil.type = p$soil.type)
  
  #~~~~~~~~~~~~~~~
  ## Get et0 ----
  #~~~~~~~~~~~~~~~  
  if (p$et0.timestep == "weekly") {
    et0 <- get_et0(p$et0.input)
  } else {
    if (p$et0.timestep == "daily") {
      et0 <- p$et0.input
    }
  }
  # Join et0 with ts-----
  colnames(et0) <- tolower(colnames(et0))
  et0$date <- as.Date(et0$date)
  ts <- left_join(ts, et0[, c("date", "et0")], by = "date")
  return(ts)
}

#~~~~~~~~~~~~~~~
# Function 2 of 2:  get_budget ----
#~~~~~~~~~~~~~~~
get_budget <- function(p = p.list, ts = met.forcing) {
  # #~~~~~~~~~~~~~~~
  # ## source functions ----
  # #~~~~~~~~~~~~~~~
  # source("code/4.0_function_get_kc.R")
  
  #~~~~~~~~~~~~~~~
  ## Get kc and kcr ----
  #~~~~~~~~~~~~~~~
  crop.param.ls <- split(p$crop.param, p$crop.param$crop)
  names(crop.param.ls) <- tolower(names(crop.param.ls))
  crops.vec <- tolower(p$crops.vec)
  if ("no_crop" %in% crops.vec) {
    kcr <- data.frame(date = ts$date) 
    kcr$doy <- as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), 
                                 format = "%j"))
    kcr$kcr <- NA
    kcr$kcr[kcr$doy < 120] <- p$kc_no_crop[["summer"]]
    kcr$kcr[kcr$doy >= 120] <- p$kc_no_crop[["kr"]]
    # "no_crop" normally should be rainfed, but just in case:
    if (crop.param.ls[[crops.vec]]$irrigation == "rainfed") {
      kcr$irrigated <- FALSE
    } else {
      if (crop.param.ls[[crops.vec]]$irrigation == "irrigated") {
      kcr$irrigated <- TRUE
      }
    }
  } else {
    kc.doy.ls <- split(kc.doy.crops, kc.doy.crops$crop.rotation)
    names(kc.doy.ls) <- tolower(names(kc.doy.ls))
    crops.rotation.vec <- tolower(p$crops.rotation.vec)
    kc.doy.ls.select <- kc.doy.ls[crops.rotation.vec]
    kc.crops.ls <- lapply(kc.doy.ls.select, get_kc, 
                       from = ts$date[1], to = ts$date[nrow(ts)])
    # Has a list for each crop. Converting to a joint data-frame
    kc.crops.df <- bind_rows(kc.crops.ls, .id = "crop") %>%
      pivot_wider(names_from = crop, values_from = kc)
    ## Summing up kc of all crop columns:
    kcr <- data.frame(date = kc.crops.df$date, doy = kc.crops.df$doy,
                      kcr = rowSums(kc.crops.df[, !colnames(kc.crops.df) 
                                                %in% c("date", "doy")]))
    kcr <- kcr[order(kcr$date), ]
    # Does any of the specified crops need irrigation:
    irri.needed <- any(unlist(lapply(crop.param.ls[crops.vec], 
                      function(x) {return(x$irrigation)})) == "irrigated")
    # If irri.needed, then is a crop present, that is, is kcr positive?
    if (irri.needed) {
      kcr$irrigated[kcr$kcr > 0] <- TRUE
      kcr$irrigated[kcr$kcr == 0] <- FALSE
    } else {
      kcr$irrigated <- FALSE
    }
  }
 
  # Join kcr with ts----
  ts <- left_join(ts, kcr, by = "date")
  # Potential et crop cycle
  ts$etc <- ts$et0*ts$kcr
  
  #~~~~~~~~~~~~~~~
  ## Get ks and aet ----
  #~~~~~~~~~~~~~~~  
  soil.param <- p$soil.param
  soil.param$soil.type <- tolower(soil.param$soil.type)
  psoil <- split(soil.param, soil.param$soil.type)
  
  soil.type <- tolower(p$soil.type)
  max.rooting.depth <- max(as.numeric(unlist(lapply(crop.param.ls[crops.vec], 
                                   function(x) {return(x$rooting_depth_cm)}))))
  whc <- psoil[[soil.type]]$whc.mmpercm*max.rooting.depth
  raw <- psoil[[soil.type]]$raw
  
  # Dynamic variables -- these are dependent on daily status of other variables  
  ts$sm.hold <- NA
  ts$recharge <- NA
  ts$sm.mid <- NA
  ts$ks <- NA
  ts$aet <- NA
  ts$sm <- NA
  ts$irrigation <- NA
  # Add a progress bar
  # pb = txtProgressBar(min = 0, max = nrow(ts), initial = 0, style = 3)
  # t1 = Sys.time()
  # Write Dynamic 
  for(i in 1:nrow(ts)) {
    # setTxtProgressBar(pb,i)
    if (i == 1) {
      ts$sm.hold[i] <- whc * p$sm.init
    } else {
      ts$sm.hold[i] <- ts$sm[i-1] + ts$Peff[i]
    }
    if(ts$sm.hold[i] > whc) {
      ts$recharge[i] <- ts$sm.hold[i] - whc
      ts$sm.mid[i] <- whc
    } else {
      ts$recharge[i] <- 0
      ts$sm.mid[i] <- ts$sm.hold[i]
    }
    
    if(ts$sm.mid[i] >= whc*raw) {
      ts$ks[i] <- 1
    } else {
      ts$ks[i] <- ts$sm.mid[i]/(whc*(1-raw))
    }
    # Potential et crop cycle given soil moisture
    ts$etcs[i] <- ts$etc[i]*ts$ks[i]
    # AET crop cycle can only be the fraction of etcs that's available in sm.mid
    ts$aet[i] <- min(ts$sm.mid[i], ts$etcs[i], na.rm = TRUE) 
    # sm is updated by removing AET, and adding irrigation when irrigated and soil under water-stress
    # Why isn't irrigation given before/during AET drawn?----
    if (ts$irrigated[i] & ((ts$sm.mid[i] - ts$aet[i]) < whc*raw)) {
      # If irrigated (crop present and crop is under irrigation) 
      # AND soil is water-stressed, then irrigated to whc
      ts$sm[i] <- whc 
    } else {
      # Else only ET is removed
      ts$sm[i] <- max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
    }
  }
  # t2 = Sys.time()
  # t2-t1
  # Extent of Irrigation does not have dependent variables, so can be calculated outside loop
  ts$irrigation <- ts$sm - (ts$sm.mid - ts$aet)
  ts$sm.percent <- ts$sm/whc*100
  # Find out crop et supply:demand, aet:etc for crop duration, that is, when kc > 0
  if ("no_crop" %in% crops.vec) {
    ts$etc.crop <- NA
    ts$aet.crop <- NA
    ts$aet.to.etc.crop <- NA
  } else {
    ts$etc.crop[ts$etc == 0] <- 0
    ts$etc.crop[ts$etc > 0] <- ts$etc[ts$etc > 0]
    ts$aet.crop[ts$etc == 0] <- 0
    ts$aet.crop[ts$etc > 0] <- ts$aet[ts$etc > 0]
    ts$aet.to.etc.crop[ts$etc.crop == 0] <- NA
    ts$aet.to.etc.crop[ts$etc.crop != 0] <- ts$aet.crop[ts$etc.crop != 0]/ts$etc.crop[ts$etc.crop != 0]
  }
  ts$aet.to.P <- min(ts$aet/ts$Peff, NA)
  ts$runoff.percent <- min(ts$Q/ts$P*100, NA)
  ts$balance <- ts$recharge - ts$irrigation
  ts$year <- as.numeric(format(ts$date, format = "%Y"))
  ts$month <- as.numeric(format(ts$date, format = "%m"))
  ts <- ts %>% select(date, year, month, doy, P, Q, Peff,
                      et0, etc, aet, etcs, etc.crop, aet.crop, 
                      recharge, irrigation, balance, runoff.percent, aet.to.P, everything())
  # > colnames(ts)
  # [1] "date"            "year"            "month"           "doy"             "P"              
  # [6] "Q"               "Peff"            "et0"             "etc"             "aet"            
  # [11] "etcs"            "etc.crop"        "aet.crop"        "recharge"        "irrigation"     
  # [16] "balance"         "runoff.percent"  "aet.to.P"        "kcr"             "irrigated"      
  # [21] "sm.hold"         "sm.mid"          "ks"              "sm"              "sm.percent"     
  # [26] "aet.to.etc.crop"
  
  write.csv(ts, file = paste0("data/water_budget_SOIL_", p$soil.type, 
                              "_CROPS_", paste(p$crops.vec, collapse = "_"), "_daily.csv"), row.names = FALSE)
  
  #~~~~~~~~~~~~~~
  # Summary stats----
  #~~~~~~~~~~~~~~
  # Yearly
  sum.yr <- ts %>% group_by(year) %>%
    summarise_at(vars(P:balance), sum, na.rm = TRUE) %>%
    mutate(aet.to.P = aet/Peff, runoff.percent = min(Q/P*100, NA))
  mean.yr <- ts %>% group_by(year) %>%
    summarise_at(vars(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE)
  sum.yr <-  sum.yr %>% left_join(mean.yr, by = "year")
  write.csv(sum.yr, file = paste0("data/water_budget_SOIL_", p$soil.type, 
                              "_CROPS_", paste(p$crops.vec, collapse = "_"), "_yearly.csv"), row.names = FALSE)
  # Monthly
  sum.mo <- ts %>% 
    group_by(year, month) %>%
    summarise_at(vars(P:balance), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(aet.to.P = aet/Peff, runoff.percent = Q/P*100)
  mean.mo <- ts %>% group_by(year, month) %>%
    summarise_at(vars(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE, .groups = "drop") %>%
    ungroup()
  sum.mo <-  sum.mo %>% left_join(mean.mo, by = c("year", "month"))
  clim.mo <- sum.mo %>%
    select(-year) %>%
    group_by(month) %>%
    summarise_all(mean, na.rm = TRUE)
    
  #~~~~~~~~~~~~~~
  # Plot Water Budget ----
  #~~~~~~~~~~~~~~
  crop.sys.title <- paste0("Crop System: ", 
                       paste(toupper(p$crops.vec), collapse = " + "), 
                       " on ", toupper(p$soil.type), " soil")
  # Daily ----
  daily.vars.to.plot <- c("kcr", "aet.to.etc.crop", "P", "Q", "irrigation", "sm", "aet", "recharge", "aet.to.etc.crop")
  years.to.plot <- c(2022, 2023)
  ts.l <- ts %>%
    pivot_longer(cols = -c(date, year, doy), names_to = "var", values_to = "value") %>%
    subset(var %in% daily.vars.to.plot)
  ts.l.select <- ts.l %>% subset(year %in% years.to.plot)
  p.daily_1 <- 
    ggplot(ts.l.select %>% subset(var %in% c("P", "Q", "irrigation", "sm")), 
           aes(x = date, y = value)) +
    geom_line(aes(group = var, color = var)) +
    ylab("mm") + xlab("Year-DOY") +
    scale_x_date(date_labels = "%Y-%j", date_breaks = "6 months") +
    theme_minimal() +
    scale_color_discrete("")
  p.daily_2 <- p.daily_1 %+% subset(ts.l.select, var %in% c("aet", "recharge"))
  p.daily_3 <- p.daily_1 %+% subset(ts.l.select, var %in% c("kcr", "aet.to.etc.crop")) +
    ylab("value") + 
    ggtitle(paste0(crop.sys.title, "\n", 
                   "Water Budget for ", paste(years.to.plot, collapse = "-")))
  p.daily.budget <- ggarrange(p.daily_3, p.daily_1, p.daily_2, 
                              nrow = 3, ncol = 1,
                              labels = c("A", "B", "C"),
                              # common.legend = TRUE, 
                              legend = "top")
  # Save plot
  ggsave(plot = p.daily.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", p$soil.type, 
               "_CROPS_", paste(p$crops.vec, collapse = "_"), "_", paste(years.to.plot, collapse = "-"),"_daily.pdf"), 
        width = 9, height = 9)
  ggsave(plot = p.daily.budget, file = paste0("figures/water_budgets/water_budget_daily.jpg"), 
         width = 9, height = 9)  
  # Plot yearly ----
  sum.yr.l <- sum.yr %>% 
    pivot_longer(cols = -c(year), names_to = "var", values_to = "value")
  yearly.vars.to.plot <- c("aet.to.etc.crop", "aet.to.P", "balance", "aet", "P", "Q", "recharge", "irrigation", "sm.percent")
  sum.yr.l.select <- sum.yr.l %>% subset(var %in% yearly.vars.to.plot)
  sum.yr.l.select$var_f = factor(sum.yr.l.select$var, 
                                 levels = c("aet.to.etc.crop", "aet.to.P", "balance", "aet", "P", "Q", "recharge", "irrigation", "sm.percent"))
  p.yearly.budget <- ggplot(sum.yr.l.select, 
                            aes(x = year, y = value)) +
    facet_grid(var_f ~ ., scales = "free_y") +
    geom_col(aes(fill = var_f), show.legend = FALSE) +
    ylab("Value") + xlab("Year") +
    ggtitle(paste0(crop.sys.title, "\n", "Water Budget")) +
    theme_minimal()
  ggsave(plot = p.yearly.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", p$soil.type, 
                    "_CROPS_", paste(p$crops.vec, collapse = "_"), "_yearly.pdf"), width = 9, height = 9)
  ggsave(plot = p.yearly.budget, file = paste0("figures/water_budgets/water_budget_yearly.jpg"), width = 9, height = 9)

  # Plot monthly----
  clim.mo.l <- clim.mo %>%
    pivot_longer(cols = -c(month), names_to = "var", values_to = "value")
  monthly.vars.to.plot <- c("aet.to.etc.crop", "aet.to.P", "balance", "Q", "runoff.percent", "aet", "etc", "P", "recharge", "irrigation", "sm.percent")
  clim.mo.l.select <- clim.mo.l %>% subset(var %in% monthly.vars.to.plot)
  clim.mo.l.select$var_f = factor(clim.mo.l.select$var,
                                 levels = c("aet.to.etc.crop", "aet.to.P", "balance", "Q", "runoff.percent", "aet", "etc", "P", "recharge", "irrigation", "sm.percent"))
  p.monthly.budget <- ggplot(clim.mo.l.select,
                            aes(x = month, y = value)) +
    facet_grid(var_f ~ ., scales = "free_y") +
    geom_col(aes(fill = var_f), show.legend = FALSE, width = 0.7) +
    ylab("Value") + xlab("Month") +
    ggtitle(paste0(crop.sys.title, "\n", "Water Budget")) +
    theme_minimal()
  ggsave(plot = p.monthly.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", p$soil.type,
                                               "_CROPS_", paste(p$crops.vec, collapse = "_"), "_monthly.pdf"), width = 5, height = 9)
  ggsave(plot = p.monthly.budget, file = paste0("figures/water_budgets/water_budget_monthly.jpg"), width = 5, height = 9)
  
  p.monthly.runoff <- ggplot(clim.mo.l.select %>% 
                               subset(var_f %in% c("P", "Q")),
                             aes(x = month, y = value)) +
    geom_col(aes(fill = var_f), width = 0.7, position = position_dodge()) +
    geom_line(data = clim.mo.l.select %>% 
                 subset(var %in% c("irrigation", "runoff.percent", "etc")), 
               aes(col = var)) +
    geom_point(data = clim.mo.l.select %>% 
                 subset(var %in% c("irrigation", "runoff.percent", "etc")), 
               aes(col = var)) +
    scale_fill_ordinal("") +
    scale_color_discrete("") +
    ylab("mm") + xlab("Month") +
    ggtitle(paste0(crop.sys.title, "\n", "Water Budget")) +
    theme_minimal()
  ggsave(plot = p.monthly.runoff, file = paste0("figures/water_budgets/water_budget_SOIL_", p$soil.type,
                                                "_CROPS_", paste(p$crops.vec, collapse = "_"), "_runoff_overlaid_monthly.pdf"), width = 5, height = 3)
  ggsave(plot = p.monthly.runoff, file = paste0("figures/water_budgets/water_budget_runoff_overlaid_monthly.jpg"), width = 5, height = 3)
  
  # summary(ts)
  # plot(x = ts$date, y = ts$Peff)
  return(list(ts, sum.yr))
}
