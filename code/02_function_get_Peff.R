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
  rain <- read.csv(file = "data-raw/rainfall.csv", header = TRUE)
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
  
  # cn = 25400 / c( 254 + s) 
  S <- c( 25400 / CN) - 254
  
  ts$Q <- NA
  
  if (tolower(soil.type) == "black") {
    
    # Below a threshold P, Q is zero
    thresh <- 0.2*S
    P.below.thresh <- c(ts$P < thresh)
    ts$Q[P.below.thresh] <- 0
    
    # Above the threshold P, Q is based on a formula
    # 𝑄= (𝑃− 0.2*𝑆)^2/(𝑃+ 0.8𝑆)
    
    ts$Q[!P.below.thresh] <- 
      c(ts$P[!P.below.thresh] - thresh)^2/ c(ts$P[!P.below.thresh] + 0.8*S)
    
  } else if (tolower(soil.type) == "red") {
    # Below a threshold P, Q is zero
    thresh <- 0.3*S
    P.below.thresh <- c(ts$P < thresh)
    ts$Q[P.below.thresh] <- 0
      
    # Above the threshold P, Q is based on a formula
    # 𝑄= (𝑃− 0.3*𝑆)^2/(𝑃+ 0.7𝑆)
      
    ts$Q[!P.below.thresh] <- 
      c(ts$P[!P.below.thresh] - thresh)^2/ c(ts$P[!P.below.thresh] + 0.7*S)
  }
  ts$Peff <- ts$P - ts$Q
  write.csv(ts, file = paste0("data/Peffective_", soil.type, ".csv"), row.names = FALSE)
  # summary(ts)
  # plot(x = ts$date, y = ts$Peff)
  return(ts)
}
