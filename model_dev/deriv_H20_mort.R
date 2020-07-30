##WATER STRESS MORTALITY FUNCTION##

#The relationship between soil matric potential (MPa) and seedling mortality rates is derived in this script from: 
#Engelbrecht, B. M. J., & Kursar, T. A. (2003). 
#Comparative drought-resistance of seedlings of 28 species of co-occurring tropical woody plants. 
#Oecologia, 136(3), 383–393. 
#https://doi.org/10.1007/s00442-003-1290-8
rm(list = rm())
gc()
library(tidyverse)

path_to_observational_data <- "~/cloud/gdrive/rec_submodel/data/observations/"
engelbrecht_mort_data <- read_csv(paste0(path_to_observational_data,"engelbrecht_mort_data.csv"))
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")

#pedotransfer function based on data provided in Engelbrecht et al. 2003
PTF_func <- function(sgwc){
  
  sgwc_T  <- c(0.78, 0.47, 0.34)
  matric_T <- c(0, -0.5, -3.5)
  d <- data.frame(sgwc_T = sgwc_T, matric_T = matric_T)
  #plot(sgwc_T,matric_T)
  
  sgwc_T2 <- sgwc_T^2
  PTF <- lm(matric_T ~ sgwc_T + sgwc_T2)
  
  
  matric <- coef(PTF)[3]*sgwc^2 + coef(PTF)[2]*sgwc + coef(PTF)[1]
  return(matric) #returns matric potential
}


#Step 1. Deriving the soil water retention curve at the experimental plots in Engelbrecht and Kursar, 2003

# sgwc_T  <- c(0.78, 0.47, 0.34)
# matric_T <- c(0, -0.5, -3.5)
# d <- data.frame(sgwc_T = sgwc_T, matric_T = matric_T)
# #plot(sgwc_T,matric_T)
# 
# sgwc_T2 <- sgwc_T^2
# PTF <- lm(matric_T ~ sgwc_T + sgwc_T2)



#Step 2. Replicating the time series data for soil gravimetric water content
weeks <- c(4,8,10,12,14,16,18,20,22)
sgwc <- c(0.48, 0.41,0.39,0.35,0.37,0.34,0.33,0.33,0.38)

#plot(weeks, sgwc)

#Step 3. Converting the soil moisture time series data to a matric potential (mm water suction)

matric <- PTF_func(sgwc = sgwc) * 1e5
#plot(weeks, matric)

#creating a dataframe to store the moisture and (later) mortality data
ts_wk <- data.frame(week = weeks, matric = matric)

#adding the day of the experiment
ts_wk$day <- ts_wk$week*7

#Wilting points. Wilting points were determined from Figure 2 in Engelbrecht and Kursar, 2003
thresh.xx <- c(ts_wk$matric[2], ts_wk$matric[6])
names(thresh.xx) <- c("latedi","latedt") 

#Determing matric potential on each day
matric_days <- predict(loess(matric~weeks), newdata = seq(from = 4, to = 22, length = 127))
days <- 1:127 + 27
ts_days <- data.frame(day = days, matric = matric_days)
plot(days, matric_days / 1e5, ylab = "soil matric potential (Mpa)", 
     main = "Soil moisture observations \n Engelbrecht drought experiment")



#defining a function to create deficit days for the 22 week period of the experiment
def_func <- function(soil_moist, thresh.x = thresh.xx[PFT], window){
  thresh <- thresh.x
  def <- (abs(thresh) - abs(soil_moist))*-1
  no_def <- def < 0 
  def[no_def] <- 0
  deficit_days <- c()
  for(i in 1:length(def)){
    deficit_days[i] <- ifelse(i < window, sum(def[1:i]), sum(def[(i-window):i]))
  }
  return(deficit_days)
}



#plot(def_func(thresh.x = thresh.xx[PFT], soil_moist = matric_days, window = 126))

#creating deficit days for the drought intolerant PFTs over the 18 week period when soil moisture was measured.
deficit_days_DT <- def_func(soil_moist = matric_days, thresh.x = thresh.xx["latedt"], window = 18*7)[126]
deficit_days_DI <- def_func(soil_moist = matric_days, thresh.x = thresh.xx["latedi"], window = 18*7)[126]


dr_mort_DI <- engelbrecht_mort_data %>%
  filter(PFT == "DI") %>%
  summarise(mort_rate = mean(pct_mort_drought))

dr_mort_DT <- engelbrecht_mort_data %>%
  filter(PFT == "DT") %>%
  summarise(mort_rate = mean(pct_mort_drought))

#fitting a linear model of mortality as a function of deficit days for DT and DI PFTs
DI_dr_data <- data.frame(def_days = c(0,deficit_days_DI), mort = c(0, dr_mort_DI$mort_rate))
DI_dr_lm <- lm(data = DI_dr_data, formula = mort~def_days)

DT_dr_data <- data.frame(def_days = c(0,deficit_days_DT), mort = c(0, dr_mort_DT$mort_rate))
DT_dr_lm <- lm(data = DT_dr_data, formula = mort~def_days)

P1H20 <- rep(c(coef(DI_dr_lm)[2], coef(DT_dr_lm)[2]),2) 
names(P1H20) <- pft_names

P2H20 <- rep(c(coef(DI_dr_lm)[1], coef(DT_dr_lm)[1]),2) #note P2H20 is essentially zero
names(P2H20) <- pft_names


print("P1H20 is:")
print(P1H20)