
#this munge script cleans raw data and creates time series, dataframes, and variables that are used in the toy model

#creating a time series of precipitation data at BCI for an average year.
monthly_rain_bci_c <- monthly_rain_bci$Average[-13]
daily_rain <- monthly_rain_bci_c/30.4

daily_rain_4_one_year <- c()
for(i in 1:length(daily_rain)){
  
  temp <- rep(daily_rain[i],30) 
  daily_rain_4_one_year <- append(daily_rain_4_one_year, temp)
  
}
daily_rain_4_one_year <- append(daily_rain_4_one_year, daily_rain_4_one_year[356:360])


#cleaning the PFT data and creating lists of early PFTs vs. late PFTs.
pfts$sp <- paste0(pfts$g," ",pfts$s)
names(pfts)[9] <- "Latin"
sp_code <- bci.spptable[,c(1,2)]
pfts <- merge(sp_code, pfts, by = "Latin")[,-c(3,4)]
Early <- pfts[pfts$pft == "e",]$sp
Late <- pfts[pfts$pft == "l",]$sp



###############ALLOMETRY#####################

#Determining the height of the seedling layer
#Seedling height (cm) is the mean seedling height in all of the date 1s for each PFT. Early vs. Late
Hght_E <- sdbci %>%
  filter(sp == Early) %>%
  select(sp, hght1) %>%
  summarise(H = mean(hght1)/10)

#species used to calc height for early PFTs, n = 12
sp_E <- sdbci %>%
  filter(sp == Early) %>%
  select(sp) %>%
  unique(.)

#height of late PFTs in cm
Hght_L <- sdbci %>%
  filter(sp == Late) %>%
  select(sp, hght1) %>%
  summarise(H = mean(hght1)/10)

#species used for late PFT, n = 29
sp_L <- sdbci %>%
  filter(sp == Late) %>%
  select(sp) %>%
  unique(.)


#STEP 2. Determining relationship between height and the basal diameter of each seedling from data from Breugal et. al.
#adding species names to the breug data
breug <- merge(breug, breug_codes)

#adding the bci codes to the breug data
breug <- merge(sp_code,breug, by = "Latin")

#taking out the basal diameter values above 8.
Hgt <- breug %>%
  select(sp.x, var, mean) %>%
  filter(var == "H")

BD <- breug %>%
  select(sp.x, var, mean) %>%
  filter(var == "BD")

dfBD_H <- data.frame(H = Hgt$mean, BD = BD$mean)

BD_H_mod <- lm(data = dfBD_H, BD ~ H)

#plot(Hgt$mean, BD$mean, xlab = "height (m)", ylab = "basal diameter (cm)", main = "Basal diameter vs. height; n = 49 sp.")
#abline(BD_H_mod)

#this function is not PFT specific. Only four species overlap. 
Hgh2BD <- function(H){
  BD <- coef(BD_H_mod)[2]*H + coef(BD_H_mod)[1]
  return(BD)
}

sp_overlap <- unique(breug$Latin) %in% pfts$Latin
sp_overlap_sp <- unique(breug$Latin)[sp_overlap]
sp_overlap_sp %in% pfts$Latin
pfts[pfts$Latin == "Brosimum alicastrum",]

#determining the basal diamter
BDe <- Hgh2BD(H = Hght_E/100)
BDl <- Hgh2BD(H = Hght_L/100)

#STEP 3. determining the height of a 1 cm size class individual from the below paper:

#Hubbell, S. P., & Condit, R. (1995). Diameter, Height, Crown, and Age Relationships in Eight Neotropical Tree Species. Ecology.

#writing down parameters for ln-ln relationship between dbh (mm) and H (m) found in the Hubbell and Condit paper (1995) above.
#parameters for species that are a proxy for late PFT
slope_l <- mean(0.684, 0.736) # mean of Prioria copaifera and Faramea occidentalis
int_l <- mean(-0.327, -0.492) # mean of Prioria copaifera and Faramea occidentalis

#parameters for species that is a proxy for early PFT
slope_e <- 0.640 #used data for this is Ocotea whitei
int_e <- -0.209

#this is the dbh to height allometric equation for those above species. The species parameters were in the middle of the range, so should be pretty representative. This data does include lower diameter individuals (i.e. 1 cm dbh). Its for the most common species on bci, but this could probably be improved by adding using a paratmers based off of canopy species when they are at a young age.

dbh2H <- function(PFT = "early", dbh_mm){
  ifelse(test = PFT =="early",
         lnH <- log(dbh_mm) * slope_e + int_e,
         lnH <- log(dbh_mm) * slope_l + int_l)
  return(exp(lnH))
}


Hgt_1_cm_e <- dbh2H(PFT = "early", dbh_mm = 10) #Height in the 1 cm size class for the early PFT is 3.54 m 
Hgt_1_cm_l <- dbh2H(PFT = "late", dbh_mm = 10) #Height in the 1 cm size class for the late PFT is 3.48


#STEP 4. Converting height of the 1 cm size class to basal diameter of the 1 cm size class using the equation from:
#King, D. A. (2015). Allometry of saplings and understorey trees of a panamanian forest, 4(1), 27–32.
#As an input this allometric equation takes H in meters. The output: diameter (cm) at 1/10th the tree height (m).

slp <- mean(c(0.813,0.755,0.670)) #this is the average of the parameter values for three canopy / subcanopy species: Alseis blackiana, Virola sebifera, Trichilia tuberculata

King_allom <- function(H){
  lnTD <-  log(H) * slp - 0.1
  return(exp(lnTD))
}

King_allom(H = 3.5)

BD_1_cm_e <- King_allom(H = Hgt_1_cm_e)
BD_1_cm_l <- King_allom(H = Hgt_1_cm_l)


#STEP 5, determining the amount of growth that the seedlings have to put on in units of basal diameter is
BD_delta_e <- BD_1_cm_e - BDe
BD_delta_l <- BD_1_cm_l - BDl

#STEP 6, determining the relationship between absolute light value and radial growth using Cecropia obtusifolia for early and Trophus racemosa for late (both of these species reach the canopy and have trees with dbh in the high 30s cm in the bci forest dynamics plot.



tropra <- bci.full %>%
  select(sp, dbh) %>%
  filter(sp == "tropra") %>%
  na.omit(.)

max(tropra$dbh) #max dbh in bci census data from tropra is 38.2 cm


#determining the amount of carbon in each seedling
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)

#BD in cm to biomass (kg)
#see the func.R file to see or change this function

seedling_BD2biomass <- function(BD){
  C <- c()
  for(i in 1:4){
    C[i] <- cole_table$a[i]*(BD^(cole_table$b[i]))
  }
  return(sum(C)*1000)
}


seedling_BD2biomass(2)


#calculating the amount of biomass in each seedling
#using the allometric equations in Cole, T. G., & Ewel, J. J. (1995). Allometric equations for four valuable tropical tree species. Ecology, 229, 351–360. https://doi.org/10.1016/j.foreco.2006.04.017
#because they include equations specifically for individuals that are too small to have a dbh.
#using Cordia alliodora for all PFTs (sample for equations included height ranges at small as 10 cm tall.)
Z0_seedling_e <- seedling_BD2biomass(BD = BDe)
Z0_seedling_l <- seedling_BD2biomass(BD = BDl)

#instead of the biomass values above I'm going to use the following prescribed parameters that I'm estimating based on what FATES uses for the amount of carbon in their smallest size class(0.37 dbh). These params are the amount of carbon in seedlings rather than the amount of biomass..

Z0_seedling_e <- 35
Z0_seedling_l <- 40




#cleaning the bci soil moisture data and creating 2 soil moisture datasets. 1) A full year of daily soil moisture content in matric potential from site 9 of the Lutz water catchment area in 2013 (a non-ENSO year), and 1998 a strong ENSO year.


date <- strptime(as.character(bci_soil_moisture$Date), format = "%m/%d/%Y")

date <- as.Date(date)

bci_soil_moisture$Date <- date

site_avgs <- bci_soil_moisture %>% #These are the average soil grav water contents (by dry) at each of the 10 sample locations 
  filter(Site == "LUTZ *", Depth.cm. == "0-10")%>%#, #Date >= "2013-1-1" & Date <= "2013-12-31") %>%
  group_by(Sample.) %>%
  summarise(mean(H2O_by_dry...))


#SOIL MOISTURE DATA FOR 2013








bci_soil_mois_2013_site9 <- bci_soil_moisture %>% 
  filter(Site == "LUTZ *", Depth.cm. == "0-10", Date >= "2013-1-1" & Date <= "2013-12-31", Sample. == 9) %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, H2O_by_dry...)


bci_sm_2013_Lutz_9 <- predict(loess(data = bci_soil_mois_2013_site9, H2O_by_dry...~day), newdata = seq(from = 1, to = 365))

bci_sm_2013_Lutz_9[1:2] <- bci_sm_2013_Lutz_9[3]
bci_sm_2013_Lutz_9[365] <- bci_sm_2013_Lutz_9[364]


PTF_func <- function(sgwc){
  
  sgwc_T  <- c(0.78, 0.47, 0.34)
  matric_T <- c(0, -0.5, -3.5)
  d <- data.frame(sgwc_T = sgwc_T, matric_T = matric_T)
  #plot(sgwc_T,matric_T)
  
  sgwc_T2 <- sgwc_T^2
  PTF <- lm(matric_T ~ sgwc_T + sgwc_T2)
  
  
  matric <- coef(PTF)[3]*sgwc^2 + coef(PTF)[2]*sgwc + coef(PTF)[1]
  return(matric)
}



bci_sm_2013_Lutz_9 <- PTF_func(sgwc = (bci_sm_2013_Lutz_9/100))
pos <- bci_sm_2013_Lutz_9 >= 0 
bci_sm_2013_Lutz_9[pos] <- 0


#SOIL MOISTURE DATA FOR 1998

bci_soil_mois_1998_site9 <- bci_soil_moisture %>% 
  filter(Site == "LUTZ *", Depth.cm. == "0-10", Date >= "1998-1-1" & Date <= "1998-12-31", Sample. == 9) %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, H2O_by_dry...)

bci_sm_1998_Lutz_9 <- predict(loess(data = bci_soil_mois_1998_site9, H2O_by_dry...~day), newdata = seq(from = 1, to = 365))

bci_sm_1998_Lutz_9[1:7] <- bci_sm_1998_Lutz_9[8]
bci_sm_1998_Lutz_9[356:365] <- bci_sm_1998_Lutz_9[354]

bci_sm_1998_Lutz_9 <- PTF_func(sgwc = (bci_sm_1998_Lutz_9/100))
pos <- bci_sm_1998_Lutz_9 >= 0 
bci_sm_1998_Lutz_9[pos] <- 0


#CLEANING AND ORGANIZING THE BCI PRECIP DATA


date_pre <- strptime(as.character(bci_precip$Date), format = "%m/%d/%Y")

date_pre <- as.Date(date_pre)

bci_precip$Date <- date_pre

#for a normal year 2013
bci_precip_2013 <- bci_precip %>% 
  filter(Date >= "2013-1-1" & Date <= "2013-12-31") %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, rain_mm)

negvals <- bci_precip_2013$rain_mm < 0
bci_precip_2013$rain_mm[negvals] <- 0 

#for the el nino year - 1998
bci_precip_1998 <- bci_precip %>% 
  filter(Date >= "1998-1-1" & Date <= "1998-12-31") %>%
  mutate(day = format(Date, "%j")) %>%
  select(day, rain_mm)

negvals <- bci_precip_1998$rain_mm < 0
bci_precip_1998$rain_mm[negvals] <- 0 











