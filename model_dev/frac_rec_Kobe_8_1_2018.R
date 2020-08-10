#The goal of this script is to represent the relationship between light and seedling transition rates into the 1 cm size class. 
#The function is "frac_rec_Kobe" and using light dependent growth rates to determine recruitment rates. 
#The first part of this script is all allometric calculations for the purpose of converting growth rates to recruitment rates over a chosen time window. 
#The second part determines the relationship between light and seedling transition rates into the 1 cm size class using light dependent growth rates as an intermediary step.


#sourcing Kobe 1999 growth parameters
source("growth_params_Kobe_1999.R")#this creates values for the standard deviations associated with each growth parameter


#function to calculate the fraction of the seed pool recruiting into the 1 cm size class each day.
frac_rec <- function(light = 5e6, t_window.x = rec_window, PFT = "early", seedlings_C = 750000, N_smp.x = N_smp, p1e.x = P1rec[1], p1l.x = P1rec[2], p2e.x = P2rec[1], p2l.x = P2rec[2], g_param = c(1.00, 1.00)){
  
  #ifelse(test = light < 50, yes = t_window <- t_window.x, no = t_window <- t_window.x * 0.7)
  
  pct_light <- (light / 15750113) * 100
  
  names(g_param) <- c("early", "late")
  
 # ifelse(test = PFT == "early" & pct_light > 1, yes =  t_window <- t_window.x / (log(pct_light)), no = t_window <- t_window.x)
  
  t_window <- t_window.x
  
  names(t_window) <- c("early", "late")
  
  ifelse(PFT == "early", 
         BD_start <- 1.3,
         BD_start <- 1.46)
  
  
  ifelse(PFT == "early",
         BD_finish <- 2.32,
         BD_finish <- 2.29)
  
  BD_start <- as.numeric(BD_start)
  
  ifelse(PFT == "early", 
         seedlings_N <- seedlings_C / Z0_seedling_e, #assuming seedling of basal diameter 1.3 cm. if early PFT
         seedlings_N <- seedlings_C / Z0_seedling_l) #assuming seedling of basal diameter 1.46 cm if late PFT.
  

  #pct_light <-   #the corresponding percent light (percent of full sun) level at la selva during Kobe 1999 study 
  
  p1e <- p1e.x
  p1e_sd <- p1l_se * p1e
  
  p1l <- p1l.x
  p1l_sd <- (p1l_se * p1l) / 2
  
  p2e <- p2e.x
  p2e_sd <- p2e_se * p2e
  
  p2l <- p2l.x
  p2l_sd <- (p2l_se * p2l) /2
  
  
  ifelse(PFT == "early", p1 <- p1e, p1 <- p1l)
  ifelse(PFT == "early", p2 <- p2e, p2 <- p2l)
  ifelse(PFT == "early", p1_sd <- p1e_sd, p1_sd <- p1l_sd)
  ifelse(PFT == "early", p2_sd <- p2e_sd, p2_sd <- p2l_sd)

  
  p1 <- rnorm(mean = p1, sd = p1_sd*SDmultiplier, n = N_smp.x)
  
  p2 <- rnorm(mean = p2, sd = p2_sd*SDmultiplier, n = N_smp.x)
  
  G_L <- c()
  for(i in 1:length(p1)){
    G_L[i] <- (p1[i] * pct_light) / ((p1[i]/p2[i]) + pct_light)
  }
  
  Rad_start <- (BD_start * 10) / 2 #starting radius in mm
  
  Rad_after_1_yr <- (Rad_start) * (g_param[PFT] + G_L)^12
  
  BD_after_1_yr <- (Rad_after_1_yr/10) * 2
  
  BD_end_of_window <- (((BD_after_1_yr - BD_start) / 12) * t_window[PFT]) + BD_start
  
  #a distribution of the radii of the stems (mm) after growth for the number of months in the window. 
  
  frac_recruiting_per_t_window <- sum(BD_end_of_window >= BD_finish) / N_smp.x
  
  daily_rec_rate <- frac_recruiting_per_t_window / (t_window[PFT]*30.4)
  
  return(daily_rec_rate)
}





#deciding how to set the time window
#maximum amout of joules per day at 100% light 
max_light <- max(input_vars$FSDS)

bci_light_mean <- mean(input_vars$FSDS)


out <- c()
for(i in 1:500){
out[i] <- frac_rec(bci_light_mean, PFT = "early", g_param = c(1,1), N_smp = 100, t_window.x = c(9,9))*80000/Z0 * 365
}
mean(out)


0.0035*80000/Z0 * 365



#################################parameter tuning##########################
#################################parameter tuning##########################
lx <- 1:100
lx.x <- 15750113 * lx / 100 
frac_Recruiting <- c()

for(i in 1:100){
  frac_Recruiting[i] <- frac_rec(light = lx.x[i], t_window.x = c(8,6), N_smp = 500, PFT = "early", p1e.x = 0.7618, p2e.x = 0.0099, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.00,1))
}

data.frame(pct = lx, fracR = frac_Recruiting)[1:10,]

plot(lx.x, frac_Recruiting)

#################################parameter tuning##########################
#################################parameter tuning##########################




frac_rec(light = 1e6,PFT = "early")[c(1:10),]

frac_recs <- c()
row_num <- 1
for(i in (20e6* (1:10)/10)){
frac_recs[row_num] <- frac_rec(light = i, PFT = "early")
row_num <- row_num +1
}









debug(frac_rec)

frac_rec(light = 20e6 *0.02)

#recruitment
#BENCHMARKING
#creating benchmarking data, late PFT
l  <- 1:100

a <- log10(0.011*25*0.9) 
b <- 0.23

Rs <- 10^(a + b * log10(l)) * 400
plot(l,Rs, main = "Benchmark: Recruits per ha per year; Late PFT" ,xlab = "relative irradiance", ylab = "number of recruits")

#late_rec_bench <- Rs

#recruitment
#PARAMETERIZATION, late
lx <- 1:100
N_recs <- c()
frac <- c()
  for(i in 1:length(lx)){
    N_recs[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "late", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07, 1.0)) * 25000 / (Z0*0.4) * 365
    frac[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "late", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07,1.0))
  }
 
plot(lx, frac*5*30)
plot(lx, N_recs/365, main = "Recruits per ha per year; Late PFT" ,xlab = "relative irradiance", ylab = "number of recruits")







#early
#BENCHMARKING early recruitment with ruger model
l  <- 1:100
a <- log10(0.011*25*0.1) 
b <- 1.082

Rse <- 10^(a + b * log10(l)) * 400
plot(l,Rse, main = "Recruits per ha per year; Early PFT" ,xlab = "relative irradiance", ylab = "number of recruits")

#early_rec_bench <- Rse

#CORRECT PARAMETERIZATION earlylx <- 1:100
N_recs <- c()
frac <- c()
for(i in 1:length(lx)){
  N_recs[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "early", N_smp = 50, p1e.x = 0.7618, p2e.x = 0.0039, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07, 1.0)) * 25000 / (Z0*0.4) * 365
  frac[i] <- frac_rec(light = lx[i], t_window = c(5,8), PFT = "early", N_smp = 50, p1e.x = 0.7618, p2e.x = 0.0039, p1l.x = 0.2211, p2l.x = 0.0145, g_param = c(1.07,1.0))
}

plot(lx, frac*5*30)
plot(lx, N_recs, main = "Recruits per ha per year; Early PFT" ,xlab = "relative irradiance", ylab = "number of recruits")










#debug(frac_rec)


#frac_rec(light = 80, t_window = c(2,2), PFT = "early", N_smp = 50, p1e.x = 0.718, p2e.x = 0.0043, p1l.x = 0.2211, p2l.x = 0.0145, g_param = 1.05)




#frac_rec(light = 3, t_window = c(2,2), PFT = "late", N_smp = 200) * 25000 / (Z0*0.4) * 365



#exploring different parameterizations on the light dependence of growth


#early
light <- seq(from = 1, to = 100, length = 100)
p1 <- 0.718
p2 <- 0.004
G_L <- (p1 * light) / ((p1/p2) + light)
Radial_growth <- (5 * (1.0 + G_L)^12) - 0.7
plot(light, Radial_growth)

#late
light <- seq(from = 1, to = 100, length = 100)
p1 <- 0.2211
p2 <- 0.0145
G_L <- (p1 * light) / ((p1/p2) + light)
Radial_growth <- (5 * (1.0 + G_L)^12) - 0.7
plot(light, Radial_growth)






G_L[i] <- (p1[i] * light) / ((p1[i]/p2[i]) + light)

#1.375



























