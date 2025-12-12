# ---
# title: "Function to get et0 daily time series from IMDs weekly average et0 data"
# subtitle: "et0 = Reference Evapotranspiration. Assign average weekly data to each day of the week.
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-11"
# input: (1) A data frame of IMD average weekly et0 (mm/day):
#            class=data.frame(year=numeric, week=numeric, et0=numeric), dim=[n,3], 
# output: data.frame(date=Date, year=numeric, week=numeric, et0=numeric), class=data.frame, dim= [from - to + 1),4],
# example:
# source("code/03_function_get_ET0.R")
# et0.week.imd <- read.csv(file = paste0("data/raw/et0.week.imd.csv"), header = TRUE)
# ts <- get_ET0(et0.week.imd = et0.week.imd)
# plot(x = ts$doy, y = ts$et0)
# ---

get_et0 <- function(et0.week.imd = et0.week.imd) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Creating a sample IMD dataset----
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # year.from = 1970
  # year.to = 2023
  # dates <- seq(as.Date(paste0(year.from, "-01-01")), 
  #                      as.Date(paste0(year.to, "-12-31")), by = "weeks")
  ## Week of the year as decimal number (00–53) using Sunday as the first day 1 of 
  ## the week (and typically with the first Sunday of the year as day 1 of week 1). 
  ## The US convention.
  # imd.years <-
  #   as.numeric(format(strptime(dates, format = "%Y-%m-%d"), format = "%Y"))
  # imd.weeks <-
  #   as.numeric(format(strptime(dates, format = "%Y-%m-%d"), format = "%U"))
  # et0.week.imd <- data.frame(year = imd.years,
  #                            week = imd.weeks,
  #                            et0 = rnorm(length(imd.weeks), mean = 6, sd = 1))
  # write.csv(et0.week.imd, file = paste0("data/processed/et0.week.imd.csv"), row.names = FALSE)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  df <- et0.week.imd
  colnames(df) <- tolower(colnames(df))
  df$year <- as.numeric(df$year)
  df$week <- as.numeric(df$week)
  df$et0 <- as.numeric(df$et0)
  year.from <- unique(df$year)[1]
  year.to <- unique(df$year)[length(unique(df$year))]
  
  # Create a daily time series corresponding to IMD weekly time series----
  ts <- data.frame(date = as.character(seq(as.Date(paste0(year.from, "-01-01")), 
                                           as.Date(paste0(year.to, "-12-31")), 
                                           by = "1 day")))
  
  # Week of the year as decimal number (00–53) using Sunday as the first day 1 of 
  # the week (and typically with the first Sunday of the year as day 1 of week 1). 
  # The US convention.
  ts$week <-
    as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), format = "%U"))
  ts$year <-
    as.numeric(format(strptime(ts$date, format = "%Y-%m-%d"), format = "%Y"))
  ts <- merge(ts, df, by = c("year", "week"))
  ts <- ts[order(ts$date),]
  ts<-ts[c("date","year","week","et0")]
  # plot(x = ts$doy, y = ts$et0)
  return(ts)
}