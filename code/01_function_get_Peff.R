# ---
# title: "Function to get Effective Rainfall (Precip-Runoff) time series"
# subtitle: ""
# author: "Rutuja Chitra-Tarak"
# date: "2024-08-12"
# input: (1) Precipitation time series: 
#            data.frame(date=Date, p=numeric), class=data.frame, dim=n,2 
#        (2) CN value, class=numeric, length=1
#        (3) Soil type, class=character, length=1 
# output: data.frame(date=Date, P=numeric, Q=numeric, Peff=numeric), class=data.frame, dim=n,3 
# example:
# source("code/02_function_get_Peff.R")
# ---
get_Peff <- function(rain, CN = 85, soil.type = "red") {
  rain <- read.csv(file = "data/raw/rainfall.csv", header = TRUE)
  colnames(rain) <- tolower(colnames(rain))
  ts <- rain[, c("date", "p")]
  colnames(ts)[2] <- "P"
  ts$date <- as.Date(ts$date)
  ts$P <- as.numeric(ts$P)
  # Reference parametrisation Curve number
  # Reddy K. S., Manoranjan Kumar., Rao, K.V., Maruthi, V., Reddy, B.M.K., Umesh B., Ganesh
  # Babu R., Srinivasa Reddy K., Vijayalakshmi and Venkateswarlu, B. (2012). Farm Ponds: A
  # Climate Resilient Technology for Rainfed Agriculture; Planning, Design and Construction.
  # Central Research Institute for Dryland Agriculture, Santoshnagar, Saidabad, Hyderabad-
  # 500059, Andhra Pradesh, India. 60p
  
  # cn = 25400 / c(254 + s) 
  
  S <- c( 25400 / CN) - 254
  
  ts$Q <- NA
  
  if (tolower(soil.type) == "black") {

    thresh <- 0.2*S
    
  } else if (tolower(soil.type) == "red") {

    thresh <- 0.3*S
  }
  
  # If P is below a threshold, Q is zero
  P.below.thresh <- c(ts$P < thresh)
  ts$Q[P.below.thresh] <- 0
  
  # If P is above the threshold, Q is based on a formula
  # 𝑄= (𝑃 − thresh)^2/(𝑃 - thresh + S)
  
  ts$Q[!P.below.thresh] <- 
    c(ts$P[!P.below.thresh] - thresh)^2/ c(ts$P[!P.below.thresh] - thresh + S)
  
  ts$Peff <- ts$P - ts$Q
  
  write.csv(ts, file = paste0("data/processed/10_Peffective_", soil.type, ".csv"), row.names = FALSE)
  # summary(ts)
  # plot(x = ts$date, y = ts$Peff)
  return(ts)
}
