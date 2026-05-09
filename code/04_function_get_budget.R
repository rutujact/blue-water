# ---
# title: "Main function to get Blue Water Potential (Recharge - Irrigation) for each crop rotation"
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-12"
# Modified: "2026-05-09"
# ---

#~~~~~~~~~~~~~~~
# Input-output expectations and formats for Functions get_budget() and get_peff_et0() ---- 
#~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~
## Input from p.list are used in both functions below ----
#~~~~~~~~~~~~~~~
# p.list <- list(precip = precip,
#                et0 = et0,
#                et0.timestep = et0.timestep, # weekly/daily
#                CN = 85, # To calculate Peff, Reference Curve Number (CN)
#                soil.input = "P", # "P"/Peff". Peff is calculated in either case
#                soil.param = soil.param,
#                sm.init = 0.5,
#                crop.param = crop.param,
#                kc_no_crop = list(summer = 0.005, kharip_rabi = 0.2))

#        (1) Precipitation time series: data.frame(date=Date, p=numeric), class=data.frame, dim=n,2 
#            precip <- read.csv(file = "data/processed/01_precipitation.csv", header = TRUE) 
#            Provide date in a standard unambiguous format: YYYY-MM-DD, Precipitation in mm
#        (2) et0, daily or weekly, time series: data.frame(date=Date, et0=numeric), class=data.frame, dim=n,2 
#            et0 <- read.csv(file = "data/processed/02_et0.csv", header = TRUE) # Provide date in a standard unambiguous format: YYYY-MM-DD
#        (3) et0.timestep = "daily", # weekly/daily, atomic vector, class = character, dim=NULL
#        (4) Reference Curve Number CN = 85: Used in function get_peff() run via get_peff_et0: atomic vector, class = numeric, dim=NULL
#        (5) soil.input = "P", # "P"/Peff" # whether soil input should be Precip or Peff = Precip - Runoff; Peff is calculated in either case
#        (6) soil parameter, data.frame(soil = c(1,2), soil.type = c("RED LOAMY", "GREY CLAYEY"), whc.mmpercm = c(1.7, 1.8), raw = c(0.5, 0.5)), class=data.frame, dim=n,4; soil.type can be given in any case  
#            soil.param <- read.csv(file = "data/processed/03_soil.parameters.csv", header = TRUE)
#        (7) Initial SM, For example, 0.5 of WHC: sm.init <- 0.5, atomic vector, class = numeric, dim = NULL
#        (8) Crop Parameters: data.frame(crop.ID=numeric, crop=character, irrigation=character, rooting_depth_cm=numeric), class=data.frame, dim=n,4
#            crop.param <- read.csv(file = "data/processed/04_crop.parameters.csv", header = TRUE)
#        (9) Kc values to be used when land fallow depending on season (summer: doy < 120; kharip_rabi: doy >=120)
#            kc_no_crop = list(summer = 0.005, kharip_rabi = 0.2),

#~~~~~~~~~~~~~~~
## (1) function: get_peff_et0() ----
#~~~~~~~~~~~~~~~
# Some functions (get_Peff, get_et0) that won't vary by specified crop rotation 
# are invoked outside of get_budget, via get_peff_et0. 
# This could be important for speed when get_budget is invoked multiple times for different crop rotations 
# input: p=p.list, soil = as.list(soil.param$soil)
#        (1) Peff time series, output from get_Peff(), source(code/2.0_function_get_Peff.R): 
#            output: data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric), class=data.frame, dim=n,4 
#            input: p$precip, p$CN from p.list
#        (2) et0 time series, output from get_et0(), source("code/3.0_function_get_et0.R")
#            output: data.frame(date=Date, year=numeric, week=numeric, et0=numeric), class=data.frame, dim= [from - to + 1),4]
#            input: p$et0.timestep ("weekly" or "daily) from p.list
# output: List of n = length(soil.param$soil) data.frames:
#         data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric, et0 = numeric), class=data.frame, dim=n,5 

#~~~~~~~~~~~~~~~
## (2) function: get_budget() ----
#~~~~~~~~~~~~~~~
# input: cs = a list of a data-frame for each crop.system:
#             data.frame(crop.system.ID = numeric, crop.system = character, 
#             Irrigation = character, n.rotation = numeric, rotation = numeric, rotation.ID = numeric,  
#             soil = numeric, crop_1, crop_2, crop_3 = character, crop1_crop2_crop3 = character, WB = character), class=data.frame, dim=n,12
#         p = p.list, 
#        ts = met.forcing.ls : List of n = length(soil.param$soil) data.frames, output from get_peff_et0(): 
#             data.frame(date=Date, P=numeric,  Q=numeric,  Peff=numeric, et0 = numeric), class=data.frame, dim=n,5 )

#~~~~~~~~~~~~~~~
## What happens in get_budget()? ----
#~~~~~~~~~~~~~~~
# a. Get daily kcr = sum of crop ET coefficient, made non-zero for fallow times
# c. Calculate Dynamic water states and fluxes 
#    to (1) get daily soil moisture ET coefficient, ks, after removing recharge from soil 
#       (2) to get AET = ks*kcr
# d. Calculate non-dynamic water budget variables such as
#       (1) daily irrigation supplied
#       (2) daily NPWR = Recharge - Irrigation
# e. Calculate aet to etc (= pet) and aet to P supply:demand,
# e. Calculate summary stats: daily, monthly, yearly outputs and monthly & yearly climatology
# f. Plot graphs
# output: list(ts = ts, sum.yr = sum.yr, summary.yr = summary.yr, sum.mo = sum.mo, clim.mo = clim.mo))

# source("code/5.0_function_get_budget.R")

#~~~~~~~~~~~~~~~
# Function 1 of 2:  get_peff_et0() ----
#~~~~~~~~~~~~~~~
get_peff_et0 <- function(p = p.list, soil = soil) {
  # ~~~~~~~~~~~~~~~
  # Functions sourced externally ----
  # ~~~~~~~~~~~~~~~
  # source("code/02_function_get_Peff.R")
  # source("code/03_function_get_et0.R")
  
  #~~~~~~~~~~~~~~~
  # Get Peff ----
  #~~~~~~~~~~~~~~~
  ts <- get_Peff(precip = p$precip, CN = p$CN, soil = soil)
  
  #~~~~~~~~~~~~~~~
  # Get et0 ----
  #~~~~~~~~~~~~~~~  
  if (p$et0.timestep == "weekly") {
    et0 <- get_et0(p$et0)
  } else {
    if (p$et0.timestep == "daily") {
      et0 <- p$et0
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
get_budget <- function(cs, #=crop.rotation.kc.ls, 
                       p, # = p.list, 
                       ts # = met.forcing.ls
                       ) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 1. Choose meteorological forcing for the rotations' soil type ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
  soil <- cs$soil[1]
  ts <- met.forcing.ls[[soil]]
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 2. If crop is irrigation dependent, is the crop present, that is, is crop et0 coefficient kcr positive? ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cs.ncrop <- cs[, c("doy", "kc", "crop", "ncrop", "irrigation")]
  cs.ncrop <- cs.ncrop %>% mutate(kc.irri = if_else(irrigation == "irrigated" & kc > 0, 
                                         TRUE, FALSE))
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 3. kcr = Sum of crop et0 coefficient, kc, across crops ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  cs.kcr.irri <- cs.ncrop %>%
    group_by(doy) %>%         
    dplyr::summarise(irri.needed = any(kc.irri),
           kcr = sum(kc), .groups = "drop") %>%
    ungroup()
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 4. To allow for fallow land ET, make kcr non-zero when sum(kc) == 0 ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  summer.kc <- p$kc_no_crop[["summer"]]; kharip_rabi.kc <- p$kc_no_crop[["kharip_rabi"]]
  
  cs.kcr.irri$sum.kc <- cs.kcr.irri$kcr
  cs.kcr.irri$kcr[cs.kcr.irri$kcr == 0 & cs.kcr.irri$doy < 120] <- summer.kc;
  cs.kcr.irri$kcr[cs.kcr.irri$kcr == 0 & cs.kcr.irri$doy >= 120] <- kharip_rabi.kc
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 5. Join kcr with meteorological forcing----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts$doy <-
    as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), format = "%j"))
  
  ts <- left_join(ts, cs.kcr.irri, by = "doy", relationship = "many-to-many")
  
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 6. Potential ET (mm) ----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ts$etc <- ts$et0*ts$kcr # Potential et0 * Sum of Crop et0 coefficients, adjusted for fallow land 
  
  #~~~~~~~~~~~~~~~
  # 7. Get input soil parameters that influence soil moisture dependent ET coefficient ks ----
  #~~~~~~~~~~~~~~~  
  soil.param <- p$soil.param
  psoil <- split(soil.param, soil.param$soil)

  max.rooting.depth <- max(cs$rooting_depth_cm)
  whc <- psoil[[soil]]$whc.mmpercm*max.rooting.depth
  raw <- psoil[[soil]]$raw
  
  soil.type <- soil.param$soil.type[soil.param$soil == soil]
  
  #~~~~~~~~~~~~~~~
  # 8. Set Soil Precip Input depending on Runoff De/Activation ----
  #~~~~~~~~~~~~~~~
  # If soil.input = "P", Peff.used = P
  # else if soil.input = "Peff", Peff.used = Peff = P - Q
  if (p$soil.input == "P") {
    ts$Peff.used <- ts$P
    ts$Q <- 0
  } else {
    ts$Peff.used <- ts$Peff
  }
  
  #~~~~~~~~~~~~~~~
  # 9. Initiate Dynamic variables -- these are dependent on daily status of other variables ---- 
  #~~~~~~~~~~~~~~~  
  ts$sm.hold <- NA
  ts$recharge <- NA
  ts$sm.mid <- NA
  ts$ks <- NA
  ts$aet <- NA
  ts$sm.end <- NA
  ts$irrigation <- NA
  
  # Add a progress bar
  # pb = txtProgressBar(min = 0, max = nrow(ts), initial = 0, style = 3)
  # t1 = Sys.time()
  
  #~~~~~~~~~~~~~~~  
  # 10. Calculate Dynamic water states and fluxes----
  #~~~~~~~~~~~~~~~  
  for(i in 1:nrow(ts)) {
    # setTxtProgressBar(pb,i)
    #~~~~~~~~~~~~~~~  
    ## 10.1 Soil Moisture with Precip Input is sm.hold----
    #~~~~~~~~~~~~~~~ 
    if (i == 1) {
      ts$sm.hold[i] <- whc * p$sm.init
    } else {
      ts$sm.hold[i] <- ts$sm.end[i-1] + ts$Peff.used[i]
    }
    #~~~~~~~~~~~~~~~  
    # 10.2 Water greater than WHC is removed as Recharge from sm.hold. Remaining is sm.mid----
    #~~~~~~~~~~~~~~~ 
    if(ts$sm.hold[i] > whc) {
      ts$recharge[i] <- ts$sm.hold[i] - whc
      ts$sm.mid[i] <- whc
    } else {
      ts$recharge[i] <- 0
      ts$sm.mid[i] <- ts$sm.hold[i]
    }
    #~~~~~~~~~~~~~~~  
    # 10.3 Depending on readily available water in sm.mid define soil moisture coefficient, ks, for aet ----
    #~~~~~~~~~~~~~~~
    if(ts$sm.mid[i] >= whc*raw) { # raw = readily available water
        ts$ks[i] <- 1
      } else {
        ts$ks[i] <- ts$sm.mid[i]/(whc*raw)
      }
    # But if no soil moisture, ks is zero. 
    # Adding a separate condition rather than adding a second if_else statement in the above condition 
    if (ts$sm.mid[i] == 0) {
      ts$ks[i] <- 0
    }
    #~~~~~~~~~~~~~~~ 
    # 10.4 Calculate AET based on soil moisture dependent coefficient, ks and crop etc (=PET) ----
    #~~~~~~~~~~~~~~~ 
    
    ts$aet[i] <- ts$etc[i]*ts$ks[i] # Column ET in Excel
    
    #~~~~~~~~~~~~~~~ 
    # 10.5 sm.end is sm.mid - aet (recharge already removed)----
    # 10.6 And sm.end is refilled to WHC if irrigation is applied and soil under water-stress ----
    #~~~~~~~~~~~~~~~ 
    if(ts$irri.needed[i]) {
      # Is soil below whc*raw? 
      irri.need.test <- (ts$sm.mid[i] - ts$aet[i]) < whc*raw
      # then irrigated to whc
      if((!is.na(irri.need.test) || !is.null(irri.need.test)) & irri.need.test) {
        ts$sm.end[i] <- whc
      } else {
        # Else only ET is removed (recharge already removed when sm.mid calculated)
        ts$sm.end[i] <- max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
      }
    } else {
      # Else only ET is removed (recharge already removed when sm.mid calculated)
      ts$sm.end[i] <- max(ts$sm.mid[i] - ts$aet[i], 0, na.rm = TRUE) 
    }
  }
  # t2 = Sys.time()
  # t2-t1
  
  #~~~~~~~~~~~~~~~  
  # 11. Calculate non-dynamic water budget variables----
  #~~~~~~~~~~~~~~~  
  # Extent of Irrigation does not have dependent variables, so can be calculated outside loop
  #~~~~~~~~~~~~~~~  
  ## 11.1 Irrigation is the difference, if any, between sm.end of the day and it's value after removing aet, which may be the same if no irrigation applied, or soil was not water-stressed ----
  #~~~~~~~~~~~~~~~  
  ts$irrigation <- ts$sm.end - (ts$sm.mid - ts$aet)
  ts$sm.percent <- ts$sm.end/whc*100
  
  #~~~~~~~~~~~~~~~ 
  # Get potential and actual crop ET, while crop was present (sum.kc > 0)
  ## 11.2 Get crop et supply:demand, aet:etc, for the crop duration ----
  #~~~~~~~~~~~~~~~ 
  ts$etc.crop <- ts$etc; ts$aet.crop <- ts$aet; ts$aet.to.etc.crop <- NA
  rows.crop.kc.zero <- which(ts$sum.kc == 0)
  if (length(rows.crop.kc.zero) != 0) {
    ts$etc.crop[rows.crop.kc.zero] <- 0
    ts$aet.crop[rows.crop.kc.zero] <- 0;
  }
  ts$aet.to.etc.crop[ts$etc.crop == 0] <- NA
  valid.rows <- ts$etc.crop != 0 & !is.na(ts$etc.crop)
  ts$aet.to.etc.crop[valid.rows] <- ts$aet.crop[valid.rows]/ts$etc.crop[valid.rows]
  
  #~~~~~~~~~~~~~~~ 
  ## 11.3 Get aet:Peff, Percent Runoff, Net Recharge (R -Irri) ----
  #~~~~~~~~~~~~~~~ 
  ts$aet.to.Peff <- ts$aet/ts$Peff; ts$aet.to.Peff[ts$Peff == 0] <- NA
  ts$runoff.percent <- ts$Q/ts$P*100; ts$runoff.percent[ts$P == 0] <- NA
  ts$balance <- ts$recharge - ts$irrigation
  ts$year <- as.numeric(format(ts$date, format = "%Y"))
  ts$month <- as.numeric(format(ts$date, format = "%m"))
  ts <- ts %>% select(date, year, month, doy, P, Q, Peff, Peff.used,
                      et0, etc, aet, etc.crop, aet.crop, 
                      recharge, irrigation, balance, runoff.percent, aet.to.Peff, everything())
  # > colnames(ts)
  # [1] "date"            "year"            "month"           "doy"             "P"              
  # [6] "Q"               "Peff"            "et0"             "etc"             "aet"            
  # [11]           "etc.crop"        "aet.crop"        "recharge"        "irrigation"     
  # [16] "balance"         "runoff.percent"  "aet.to.Peff"      "kcr"             "irrigated"      
  # [21] "sm.hold"         "sm.mid"          "ks"              "sm.end"              "sm.percent"     
  # [26] "aet.to.etc.crop"
  
  # write.csv(ts, file = paste0("output/water_budgets/water_budget_SOIL_", p$soil, 
  #                             "_CROPS_", paste(cs.ncrop$crop[1:3], collapse = "_"), "_daily.csv"), row.names = FALSE)
  
  #~~~~~~~~~~~~~~
  # 12 Summary stats----
  #~~~~~~~~~~~~~~
  ## 12.1 Yearly ----
  #~~~~~~~~~~~~~~
  sum.yr <- ts %>% group_by(year) %>%
    dplyr::summarise(across(P:balance, sum, na.rm = TRUE), .groups = "drop") %>%
    mutate(aet.to.Peff = aet/Peff, runoff.percent = Q/P*100)
  mean.yr <- ts %>% group_by(year) %>%
    dplyr::summarise(across(c(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE), .groups = "drop")
  sum.yr <-  sum.yr %>% left_join(mean.yr, by = "year")
  # write.csv(sum.yr, file = paste0("output/water_budgets/water_budget_SOIL_", p$soil, 
  #                             "_CROPS_", paste(cs.ncrop$crop[1:3], collapse = "_"), "_yearly.csv"), row.names = FALSE)
  
  #~~~~~~~~~~~~~~  
  ## 12.2 Annual Climatology ----
  #~~~~~~~~~~~~~~
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
    dplyr::summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(quantile = "mean")
  
  summary.yr <- sum.yr %>% 
    pivot_longer(-year, names_to = "var", values_to = "value") %>%
    group_by(var) %>%
    dplyr::reframe(quantile_df(value)) %>%
    as.data.frame() %>%
    mutate(quantile = as.character(quantile)) %>%
    dplyr::bind_rows(summary.mean) %>% 
    arrange(var)
    
  # write.csv(summary.yr, file = paste0("output/water_budgets/water_budget_SOIL_", soil.type, 
  #                                 "_CROPS_", paste(cs.ncrop$crop[1:3], collapse = "_"), "_yrly.summary.csv"), row.names = FALSE)
  #~~~~~~~~~~~~~~  
  ## 12.3 Monthly ----
  #~~~~~~~~~~~~~~
  sum.mo <- ts %>% 
    group_by(year, month) %>%
    dplyr::summarise(across(P:balance, sum, na.rm = TRUE), .groups = "drop") %>%
    ungroup() %>%
    mutate(aet.to.Peff = aet/Peff, runoff.percent = Q/P*100)
  
  #~~~~~~~~~~~~~~  
  ## 14.4 Monthly Climatology ----
  #~~~~~~~~~~~~~~
  mean.mo <- ts %>% group_by(year, month) %>%
    dplyr::summarise(across(c(sm.percent, aet.to.etc.crop), mean, na.rm = TRUE), 
                            .groups = "drop") %>%
    ungroup()
  sum.mo <-  sum.mo %>% left_join(mean.mo, by = c("year", "month"))
  clim.mo <- sum.mo %>%
    select(-year) %>%
    pivot_longer(-month, names_to = "var", values_to = "value") %>%
    group_by(var, month) %>%
    dplyr::reframe(quantile_df(value)) %>%
    as.data.frame() %>%
    pivot_wider(names_from = "quantile", values_from = "value")
  # ~~~~~~~~~~~~~~
  # 13 Plot Water Budget ----
  #~~~~~~~~~~~~~~
  crop.sys.title <- paste0("Crop System: ", cs$crop.system[1], " | Rotation: ", cs$rotation[1],
                       " on ", toupper(soil.type), " soil")
  # ~~~~~~~~~~~~~~
  ## 13.1 Plot Daily ----
  # ~~~~~~~~~~~~~~
  daily.vars.to.plot <- c("kcr", "aet.to.etc.crop", "P", "Q", "irrigation", "sm.end", "aet", "recharge", "aet.to.etc.crop")
  years.to.plot <- c(2018)
  ts.l <- ts %>%
    pivot_longer(cols = -c(date, year, doy), names_to = "var", values_to = "value") %>%
    subset(var %in% daily.vars.to.plot)
  ts.l.select <- ts.l %>% subset(year %in% years.to.plot)
  p.daily_1 <-
    ggplot(ts.l.select %>% subset(var %in% c("P", "Q", "irrigation", "sm.end")),
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
  ggsave(plot = p.daily.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", soil.type,
               "_CROPS_", paste(cs.ncrop$crop[1:3], collapse = "_"), "_", paste(years.to.plot, collapse = "-"),"_daily.pdf"),
        width = 9, height = 9)
  # ~~~~~~~~~~~~~~
  ## 13.2 Plot yearly ----
  # ~~~~~~~~~~~~~~
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
  ggsave(plot = p.yearly.budget, file = paste0("figures/water_budgets/water_budget_SOIL_", soil.type,
                    "_CROPS_", paste(cs.ncrop$crop[1:3], collapse = "_"), "_yearly.pdf"), width = 9, height = 9)

  # ~~~~~~~~~~~~~~
  # 14. Return ----
  # ~~~~~~~~~~~~~~
  return(list(ts = ts, sum.yr = sum.yr, summary.yr = summary.yr, 
              sum.mo = sum.mo, clim.mo = clim.mo))
}
