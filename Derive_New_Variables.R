Derive_Add_Variables <- function(Dataa) {
  Dataa$Twc <- 13.12 + 0.6215 * Dataa$AT - 11.37 * Dataa$WS^0.16 + 0.3965 * Dataa$AT * Dataa$WS^0.16
  Dataa$Tap <- Dataa$AT + 0.348 * Dataa$RH + 0.7 * Dataa$WS + 0.7 * Dataa$SR * (Dataa$WS - 10)^(-1) - 4.25
  
  #Dataa$Tap <- 2.653 + (0.994*Dataa$AT) + 0.0153*(Dataa$DP)^2
 # Tap <-  AT + 0.348 *  (0.01*RH*6.105)#*exp((17.27*AT)/(237.7+AT))) #- 0.7 *  WS + 0.7 *  SR * ( WS + 10) #^(-1) - 4.25
  
  Dataa$HI <- -42.379 + 2.04901523 * Dataa$AT - 6.83783e-3 * Dataa$AT^2 + Dataa$RH * (10.14333127 + 0.22475541 * Dataa$AT + 1.22874e-3 * Dataa$AT^2) + Dataa$RH^2 * (8.5282e-4 + 1.99e-6 * Dataa$AT^2)
  Dataa$ER <- 0.1396 + -3.019e-3 * Dataa$RH + -1.2109e-3 * Dataa$AT + 1.626e-5 * Dataa$RH^2 + 8.224e-5 * Dataa$AT^2 + 0.1842 * Dataa$SR + 0.5 * Dataa$SR * (Dataa$RH * -1.095e-3 + Dataa$AT * 3.655e-3) + -4.442e-3 * Dataa$SR^2
  Dataa$CP <- 20.52 * Dataa$WS^0.42 * (36.5 - Dataa$AT)
  Dataa$HD <- Dataa$AT + 0.5555 * (6.11 * exp(5417.753 * (273.16)^(-1) * (273.16 + Dataa$DP)^(-1)) - 10)
  Dataa$TE <- 37 - ((0.68 - 0.0014 * Dataa$RH + (1.76 + 1.4 * Dataa$WS^0.75)^(-1))^(-1) * (37 - Dataa$AT) - 0.29 * Dataa$AT * (1 - 0.01 * Dataa$RH))
  return(Dataa)
}
  
 