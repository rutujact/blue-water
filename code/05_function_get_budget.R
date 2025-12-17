# ---
# title: "Function to get aet and other water budget time series"
# subtitle: "Calculate ks based on kcr and etcr"
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-12"

# input from p.list:
# p.list <- list(
#                rain = rain,
#                crop.param = crop.param,
#                soil.param = soil.param,
#                # et0.timestep = "weekly", # weekly/daily
#                # et0.input = et0.week.imd, # For "weekly", choose: et0.week.imd
#                et0.timestep = "daily", # weekly/daily
#                et0.input = et0.daily, # For "daily", choose: et0.daily
#                kc.doy.crops = kc.doy.crops,
#                kc_no_crop = list(summer = 0.005, kr = 0.2),
#                soil.type = "red", # Or "black"
#                CN = 85,
#                sm.init = 0.5)

#        (1) Precipitation time series: data.frame(date=Date, p=numeric), class=data.frame, dim=n,2 
#        (2) Crop Parameters: data.frame(crop=character, irrigation=character, rooting_depth=numeric), class=data.frame, dim=n,3
#            crop.param <- read.csv(file = "data/processed/crop.parameters.csv", header = TRUE)
#        (3) soil parameter data.frame, data.frame(soil.type = c("red", "black"), whc = c(1.2, 1.7), raw = c(0.4, 0.4)) 
#            soil.param <- read.csv(file = "data/processed/soil.parameters.csv", header = TRUE)
#        (6) kc.doy.crops
#        (8) Soil type, class=character, length=1 # any case allowed (Capital/title/lower) 
#        (9) Initial SM = for example, 0.5 of WHC

# (1) function: get_peff_et0()
# Some functions (get_Peff, get_et0) that won't vary by specified crop rotation 
# are invoked outside of get_aet, via get_peff_et0. 
# This could be important for speed when get_aet is invoked multiple times for different crop rotations 
# input: p=p.list
#        (1) Peff time series, output from get_Peff(), source(code/2.0_function_get_Peff.R): 
#            output: data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric), class=data.frame, dim=n,4 
#            input: p$rain, p$CN, p$soil.type from p.list
#        (2) et0 time series, output from get_et0(), source("code/3.0_function_get_et0.R")
#            output: data.frame(date=Date, year=numeric, week=numeric, et0=numeric), class=data.frame, dim= [from - to + 1),4]

# output: data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric, et0 = numeric), class=data.frame, dim=n,5 

# (2) function: get_budget()
# input: cs = a list of crop.systems, each of data.frame(rotation = numeric,  crop = character, crop.rotation = character), class=data.frame, dim=n,3
#        p=p.list, ts = met.forcing (output from get_peff_et0())

# a. Build kc and kcr time series, output from source("code/4.0_function_get_kc.R")
# b. Get ks and aet
# c. Calculate Dynamic water states and fluxes
# d. Calculate non-dynamic water budget variables
# e. Calculate summary stats and daily, monthly, yearly and summary of yearly
# f. Plot graphs
# output: list(ts = ts, sum.yr = sum.yr, summary.yr = summary.yr, sum.mo = sum.mo, clim.mo = clim.mo))

# source("code/5.0_function_get_budget.R")

#~~~~~~~~~~~~~~~
# Function 1 of 2:  get_peff_et0 ----
#~~~~~~~~~~~~~~~
get_peff_et0 <- function(p = p.list) {
  # #~~~~~~~~~~~~~~~
  # ## source functions ----
  # #~~~~~~~~~~~~~~~
  # source("code/02_function_get_Peff.R")
  # source("code/03_function_get_et0.R")
  
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
get_budget <- function(cs, #= crop.system.ls[[1]], 
                       p = p.list, 
                       ts = met.forcing,
                       get_kc = get_kc) {
  # #~~~~~~~~~~~~~~~
  # ## source functions ----
  # #~~~~~~~~~~~~~~~
  # source("code/4.0_function_get_kc.R")
  
  #~~~~~~~~~~~~~~~
  ## Get kc and kcr ----
  #~~~~~~~~~~~~~~~
  crop.param.ls <- split(p$crop.param, p$crop.param$crop)
  names(crop.param.ls) <- tolower(names(crop.param.ls))
  crops.vec <- tolower(cs$crop)
  # Split into no_crop and crop vec
  no.crop.vec <- tolower(cs$crop.rotation)[tolower(cs$crop.rotation) == "no_crop"]
  # no.crop.irri.vec <- tolower(cs$Irrigation)[tolower(cs$crop.rotation) == "no_crop"]
  crop.rotation.vec <- tolower(cs$crop.rotation)[tolower(cs$crop.rotation) != "no_crop"]
  # crop.irri.vec <- tolower(cs$Irrigation)[tolower(cs$crop.rotation) != "no_crop"]
  
  kc.doy.ls <- split(p$kc.doy.crops, tolower(p$kc.doy.crops$crop.rotation))
  # Select Crops Kc parameters specific to Crop Rotations
  kc.doy.ls.select <- kc.doy.ls[crop.rotation.vec]
  kc.crops.ls <- lapply(kc.doy.ls.select, get_kc, 
                     from = ts$date[1], to = ts$date[nrow(ts)])
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Adding irrigation need by crop ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  kc.crops.irri.ls <- lapply(names(kc.crops.ls), function(crop.rotation) {
    element <- kc.crops.ls[[crop.rotation]]
    # Does the crop need irrigation ----
    irri <- cs$Irrigation[tolower(cs$crop.rotation) == crop.rotation]
    crop.irri <- paste(crop.rotation, "irri", sep = "_")
    element$ts[, crop.irri] <- FALSE
    # If irri.needed, then is a crop present, that is, is kcr positive?
    if (irri == "irrigated") {
      kc_positive <- element$ts$kc > 0
      element$ts[, crop.irri][kc_positive] <- TRUE
    }
    return(element$ts[, crop.irri])
    }) |> setNames(names(kc.crops.ls))
  kc.crops.irri <- bind_rows(lapply(kc.crops.irri.ls, function(x) {x}))
  # Irrigation status TRUE across any crops? ----
  ts$irri.needed <- apply(kc.crops.irri, 1, any)
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calculating total Rotation kcr ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Has a list for each crop. Converting to a joint data-frame
  kc.crops.df <- bind_rows(lapply(kc.crops.ls, function(x) x[["ts"]]), 
                           .id = "crop") %>%
    pivot_wider(names_from = crop, values_from = kc)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Adding Kc for bare ground when crop Kc zero.   
  ## Coconut is coded differently 
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Coconut + other crops is coded differently: total Kcr = 0.5* Kc_coconut + 0.5*(Kc_crop2 + Kc_crop3)
  # Other Crop.System: total Kcr = sum(Kc_crop1 + Kc_crop2 + Kc_crop3)
  
  # Any other crops besides coconut present?
  non.coco.crops <- setdiff(crops.vec, c("coconut", "no_crop")) 
  
  if(any(crops.vec == "coconut") & (length(non.coco.crops) > 0)) {
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # CASE 1: Coconut & other crops ----
    #~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # Is crop kc, for the non-coco crops, zero on any day?
    coco.column <-  names(kc.crops.df)[pmatch("coconut", names(kc.crops.df))]
    non.coco.kcr.sum <- round(rowSums(kc.crops.df[, !colnames(kc.crops.df) 
                                                  %in% c("date", "doy", coco.column)], na.rm = TRUE))
    any.zero.day <- any(non.coco.kcr.sum == 0)
    
    if(any.zero.day) {
      # print("CASE 1: Coconut & Annual Crops")
      kc.crops.df$no_crop <- NA
      kc.crops.df <- kc.crops.df %>%
        mutate(no_crop = if_else(non.coco.kcr.sum == 0, 
                                 if_else(doy < 120,
                                         p$kc_no_crop[["summer"]], p$kc_no_crop[["kr"]]), 0))
    }
    #In this case for the total kcr, give 0.5 weight to the coco kc and half to the remainder kc
    ## Summing up kc of all crop columns:
    kcr <- data.frame(date = kc.crops.df$date, doy = kc.crops.df$doy,
                      kcr = 0.5*rowSums(kc.crops.df[, colnames(kc.crops.df) 
                                                    %in% coco.column], na.rm = TRUE) +
                        0.5*rowSums(kc.crops.df[, !colnames(kc.crops.df) 
                                                %in% c("date", "doy", coco.column)], na.rm = TRUE)) %>%
      arrange(date)
  } else {
      # print("CASE 2: Annual Crops or Dense coconut")
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      # CASE 3: Annual Crops or Dense Coconut ----
      #~~~~~~~~~~~~~~~~~~~~~~~~~~~
      all.crops.kcr.sum <- rowSums(kc.crops.df[, !colnames(kc.crops.df) %in% c("date", "doy")], na.rm = TRUE)
      any.zero.day <- any(all.crops.kcr.sum == 0)
      
      if(any.zero.day) {
        kc.crops.df <- kc.crops.df %>%
          mutate(no_crop = if_else(all.crops.kcr.sum == 0, if_else(doy < 120, 
                                                                  p$kc_no_crop[["summer"]], p$kc_no_crop[["kr"]]), 0))
      }
      ## Summing up kc of all crop columns:
      kcr <- data.frame(date = kc.crops.df$date, doy = kc.crops.df$doy,
                        kcr = rowSums(kc.crops.df[, !colnames(kc.crops.df) 
                                                  %in% c("date", "doy")], na.rm = TRUE)) %>%
        arrange(date)
  }
  
  # Join kcr with ts----
  ts <- left_join(ts, kcr, by = "date")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Potential et crop cycle ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts$etc <- ts$et0*ts$kcr
  
  #~~~~~~~~~~~~~~~
  ## Get ks and aet depending on soil moisture ----
  #~~~~~~~~~~~~~~~  
  soil.param <- p$soil.param
  soil.param$soil.type <- tolower(soil.param$soil.type)
  psoil <- split(soil.param, soil.param$soil.type)
  
  soil.type <- tolower(p$soil.type)
  max.rooting.depth <- max(as.numeric(unlist(lapply(crop.param.ls[crops.vec], 
                                   function(x) {return(x$rooting_depth_cm)}))))
  whc <- psoil[[soil.type]]$whc.mmpercm*max.rooting.depth
  raw <- psoil[[soil.type]]$raw
  #~~~~~~~~~~~~~~~
  # Dynamic variables -- these are dependent on daily status of other variables  
  #~~~~~~~~~~~~~~~  
  ts$sm.hold <- NA
  ts$recharge <- NA
  ts$sm.mid <- NA
  ts$ks <- NA
  ts$aet <- NA
  ts$sm <- NA
  ts$irrigation <- NA
  # Does any of the specified crops need irrigation ----
  irri.needed <- any(tolower(cs$Irrigation) == "irrigated")
  # Add a progress bar
  # pb = txtProgressBar(min = 0, max = nrow(ts), initial = 0, style = 3)
  # t1 = Sys.time()
  #~~~~~~~~~~~~~~~  
  # Calculate Dynamic water states and fluxes----
  #~~~~~~~~~~~~~~~  
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
    if(ts$sm.mid[i] >= whc*raw) { # raw = readily available water
      ts$ks[i] <- 1
    } else {
      ts$ks[i] <- ts$sm.mid[i]/(whc*raw)
    }
    # ET crop cycle given soil moisture
    ts$aet[i] <- ts$etc[i]*ts$ks[i] # Column ET in Excel

    # sm is updated by removing aet from sm.mid (recharge already removed)
    # and updated to WHC If irrigation applied and soil under water-stress
          # Why isn't irrigation given before/during aet drawn?----
    if(ts$irri.needed[i]) {
      # Is soil below whc*raw? 
      irri.need.test <- (ts$sm.mid[i] - ts$aet[i]) < whc*raw
      # then irrigated to whc
      if((!is.na(irri.need.test) || !is.null(irri.need.test)) & irri.need.test) {
        ts$sm[i] <- whc
      } else {
        # Else only ET is removed (recharge already removed when sm.mid calculated)
        ts$sm[i] <- max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
      }
    } else {
      # Else only ET is removed (recharge already removed when sm.mid calculated)
      ts$sm[i] <- max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
    }
  }
  # t2 = Sys.time()
  # t2-t1
  #~~~~~~~~~~~~~~~  
  # Calculate non-dynamic water budget variables----
  #~~~~~~~~~~~~~~~  
  # Extent of Irrigation does not have dependent variables, so can be calculated outside loop
  ts$irrigation <- ts$sm - (ts$sm.mid - ts$aet)
  ts$sm.percent <- ts$sm/whc*100
  # Find out crop et supply:demand, aet:etc for the crop duration, that is, when kc > 0
  ts$etc.crop <- NA; ts$aet.crop <- NA; ts$aet.to.etc.crop <- NA
  ts$etc.crop[ts$etc == 0] <- 0; ts$etc.crop[ts$etc > 0] <- ts$etc[ts$etc > 0]
  ts$aet.crop[ts$etc == 0] <- 0; ts$aet.crop[ts$etc > 0] <- ts$aet[ts$etc > 0]
  ts$aet.to.etc.crop[ts$etc.crop == 0] <- NA
  ts$aet.to.etc.crop[ts$etc.crop != 0 & !is.na(ts$etc.crop)] <- ts$aet.crop[ts$etc.crop != 0 & !is.na(ts$etc.crop)]/ts$etc.crop[ts$etc.crop != 0 & !is.na(ts$etc.crop)]
  
  ts$aet.to.Peff <- min(ts$aet/ts$Peff, NA)
  ts$runoff.percent <- min(ts$Q/ts$P*100, NA)
  ts$balance <- ts$recharge - ts$irrigation
  ts$year <- as.numeric(format(ts$date, format = "%Y"))
  ts$month <- as.numeric(format(ts$date, format = "%m"))
  ts <- ts %>% select(date, year, month, doy, P, Q, Peff,
                      et0, etc, aet, etc.crop, aet.crop, 
                      recharge, irrigation, balance, runoff.percent, aet.to.Peff, everything())
  # > colnames(ts)
  # [1] "date"            "year"            "month"           "doy"             "P"              
  # [6] "Q"               "Peff"            "et0"             "etc"             "aet"            
  # [11]           "etc.crop"        "aet.crop"        "recharge"        "irrigation"     
  # [16] "balance"         "runoff.percent"  "aet.to.Peff"      "kcr"             "irrigated"      
  # [21] "sm.hold"         "sm.mid"          "ks"              "sm"              "sm.percent"     
  # [26] "aet.to.etc.crop"
  
  # write.csv(ts, file = paste0("output/water_budgets/water_budget_SOIL_", p$soil.type, 
  #                             "_CROPS_", paste(cs$crop, collapse = "_"), "_daily.csv"), row.names = FALSE)
  
  #~~~~~~~~~~~~~~
  # Summary stats----
  #~~~~~~~~~~~~~~
  # Yearly
  sum.yr <- ts %>% group_by(year) %>%
    summarise_at(vars(P:balance), sum, na.rm = TRUE) %>%
    mutate(aet.to.Peff = aet/Peff, runoff.percent = min(Q/P*100, NA))
  mean.yr <- ts %>% group_by(year) %>%
    summarise_at(vars(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE)
  sum.yr <-  sum.yr %>% left_join(mean.yr, by = "year")
  # write.csv(sum.yr, file = paste0("output/water_budgets/water_budget_SOIL_", p$soil.type, 
  #                             "_CROPS_", paste(cs$crop, collapse = "_"), "_yearly.csv"), row.names = FALSE)
  # 
  # Summary at annual scale:
  # When the functions in .fns return a data frame, you typically get a
  # "packed" data frame back
  quantile_df <- function(x, probs = c(0.025, 0.5, 0.975)) {
    tibble(quantile = probs, value = quantile(x, probs, na.rm = TRUE))
  }
  # Use .unpack to automatically expand these packed data frames into their
  # individual columns
  summary.mean <- sum.yr %>%
    pivot_longer(-year, names_to = "var", values_to = "value") %>%
    group_by(var) %>%
    summarise(value = mean(value, na.rm = TRUE)) %>%
    mutate(quantile = "mean")
  
  summary.yr <- sum.yr %>% 
    pivot_longer(-year, names_to = "var", values_to = "value") %>%
    group_by(var) %>%
    reframe(quantile_df(value)) %>%
    as.data.frame() %>%
    mutate(quantile = as.character(quantile)) %>%
    bind_rows(summary.mean) %>% 
    arrange(var)
    
  # write.csv(summary.yr, file = paste0("output/water_budgets/water_budget_SOIL_", p$soil.type, 
  #                                 "_CROPS_", paste(cs$crop, collapse = "_"), "_yrly.summary.csv"), row.names = FALSE)
  
  # Monthly
  sum.mo <- ts %>% 
    group_by(year, month) %>%
    summarise_at(vars(P:balance), sum, na.rm = TRUE) %>%
    ungroup() %>%
    mutate(aet.to.Peff = aet/Peff, runoff.percent = Q/P*100)
  mean.mo <- ts %>% group_by(year, month) %>%
    summarise_at(vars(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE, .groups = "drop") %>%
    ungroup()
  sum.mo <-  sum.mo %>% left_join(mean.mo, by = c("year", "month"))
  clim.mo <- sum.mo %>%
    select(-year) %>%
    pivot_longer(-month, names_to = "var", values_to = "value") %>%
    group_by(var, month) %>%
    reframe(quantile_df(value)) %>%
    as.data.frame() %>%
    pivot_wider(names_from = "quantile", values_from = "value")
  # ~~~~~~~~~~~~~~
  # Plot Water Budget ----
  #~~~~~~~~~~~~~~
  crop.sys.title <- paste0("Crop System: ",
                       paste(toupper(cs$crop), collapse = " + "),
                       " on ", toupper(p$soil.type), " soil")
  # Plot Daily ----
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
               "_CROPS_", paste(cs$crop, collapse = "_"), "_", paste(years.to.plot, collapse = "-"),"_daily.pdf"),
        width = 9, height = 9)
  # Plot yearly ----
  sum.yr.l <- sum.yr %>%
    pivot_longer(cols = -c(year), names_to = "var", values_to = "value")
  yearly.vars.to.plot <- c("aet.to.etc.crop", "aet.to.Peff", "balance", "aet", "P", "Q", "recharge", "irrigation", "sm.percent")
  sum.yr.l.select <- sum.yr.l %>% subset(var %in% yearly.vars.to.plot)
  sum.yr.l.select$var_f = factor(sum.yr.l.select$var,
                                 levels = c("aet.to.etc.crop", "aet.to.Peff", "balance", "aet", "P", "Q", "recharge", "irrigation", "sm.percent"))
  p.yearly.budget <- ggplot(sum.yr.l.select,
                            aes(x = year, y = value)) +
    facet_grid(var_f ~ ., scales = "fixed") +
    geom_line(aes(col = var_f), show.legend = FALSE) +
    geom_smooth(span = 0.2, method = 'loess', formula = 'y ~ x') +
    # geom_smooth(se = FALSE, method = lm, formula = y ~ splines::bs(x, 3), span = 0.3) +
    ylab("Value") + xlab("Year") +
    ggtitle(paste0(crop.sys.title, "\n", "Water Budget")) +
    theme_minimal()
  ggsave(plot = p.yearly.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", p$soil.type,
                    "_CROPS_", paste(cs$crop, collapse = "_"), "_yearly.pdf"), width = 9, height = 9)

  # ~~~~~~~~~~~~~~
  # Return ----
  # ~~~~~~~~~~~~~~
  return(list(ts = ts, sum.yr = sum.yr, summary.yr = summary.yr, 
              sum.mo = sum.mo, clim.mo = clim.mo,
              kcr = kcr[1:366,], kc.crops.df = kc.crops.df[1:366,]))
}
