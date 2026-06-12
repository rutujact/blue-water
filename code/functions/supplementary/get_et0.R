# ==============================================================================
# title: "IMD Weekly Evapotranspiration Daily Disaggregation Engine"
# author: "Rutuja Chitra-Tarak"
# description: Transforms coarse-grain aggregate weekly IMD reference 
#              evapotranspiration (ET0) records into standard flat-file daily
#              timeseries arrays by aligning calendar-week indexes.
# input: et0.week.imd - Data frame with 'year', 'week', and numeric 'et0' blocks.
# output: A clean daily timeseries data frame containing standardized variables:
#         'date', 'year', 'week', and expanded 'et0'.
# ==============================================================================

#' Convert Weekly IMD Reference Evapotranspiration Records to Daily Resolutions
#' 
#' @param et0.week.imd A data.frame containing raw weekly metadata records with three specific parameters: \code{year} (numeric), \code{week} (numeric index 0-53), and \code{et0} (numeric average weekly mm/day rate).
#' 
#' @return A continuous daily data.frame spanning from January 1st of the initial year through December 31st of the terminal year, featuring columns: \code{date} (Date), \code{year} (numeric), \code{week} (numeric US standard week registration index), and \code{et0} (daily isolated mm rate variable).
#' @export
get_et0 <- function(et0.week.imd) {
  
  # 1. Type verification and Guarding
  if (!is.data.frame(et0.week.imd)) {
    stop("Input parameter 'et0.week.imd' must be passed as a valid data.frame container.")
  }
  
  df <- et0.week.imd
  colnames(df) <- tolower(colnames(df))
  
  required_cols <- c("year", "week", "et0")
  if (!all(required_cols %in% colnames(df))) {
    stop("Input dataframe columns must include: 'year', 'week', and 'et0' fields.")
  }
  
  # Ensure strict numerical variable definitions to prevent string casting errors
  df$year <- as.numeric(df$year)
  df$week <- as.numeric(df$week)
  df$et0  <- as.numeric(df$et0)
  
  year_from <- unique(df$year)[1]
  year_to   <- unique(df$year)[length(unique(df$year))]
  
  if (is.na(year_from) || is.na(year_to)) {
    stop("Timeline extraction error: Could not deduce valid boundary years from input column arrays.")
  }
  
  # 2. Build continuous daily calendar tracking infrastructure
  start_date <- as.Date(paste0(year_from, "-01-01"))
  end_date   <- as.Date(paste0(year_to, "-12-31"))
  
  ts <- data.frame(
    date = seq(start_date, end_date, by = "1 day"),
    stringsAsFactors = FALSE
  )
  
  # 3. Map precise US Standard Week Calendar Intervals (%U: Sunday tracking standard index)
  ts$week <- as.numeric(format(ts$date, "%U"))
  ts$year <- as.numeric(format(ts$date, "%Y"))
  
  # 4. Bind structural profiles using explicit namespace merging operations
  ts <- ts %>%
    dplyr::left_join(df, by = c("year", "week"))
  
  # Handle potential unmapped calendar gaps elegantly to prevent downstream NaN contamination
  if (any(is.na(ts$et0))) {
    ts$et0[is.na(ts$et0)] <- 0
  }
  
  # 5. Clean, order, and restrict structural matrix profiles
  ts <- ts[order(ts$date), ]
  ts <- ts[, c("date", "year", "week", "et0")]
  
  return(ts)
}