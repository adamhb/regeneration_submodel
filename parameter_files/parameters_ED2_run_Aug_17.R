#PFT parameters 
pft_names <- c("LD_DI", "LD_DT", "ST_DI", "ST_DT")
percent_light <- 0.02
model_area <- 10000 #area in square meters
#avg_SMP <- -27423.26#-60326 
Z0 <- 160 #g C of new recruit, to match the 1 cm recruit in ED2

#these are the default parameters based off of BCI FDP data
Dmax <- c(895, 861, 545, 557) #maximum diamater (mm)
names(Dmax) <- pft_names
frac_repro <- c(0.1,0.1,0.1,0.1)#the fraction of NPP that gets allocated to reproduction
names(frac_repro) <- pft_names
seed_frac <- 0.5 #the fraction of reproductive carbon that gets allocated to seeds
k <- 0.0125


# a_rec <- c(4.117201e-06, 4.117201e-06, 3.154737e-06, 3.154737e-06)
# names(a_rec) <- pft_names
# b_rec <- c(3.421052e-10,  3.421052e-10, -4.552948e-10, -4.552948e-10) 
# names(b_rec) <- pft_names
# c_rec <- c(-3.619412e-05, -3.619412e-05,  7.320845e-05,  7.320845e-05) 
# names(c_rec) <- pft_names

mult <- "none"
#a_rec <- c(-5.551113, -5.551113, -5.139195, -5.139195)
#a_rec <- c(2.811169e-06, 2.811169e-06, 7.257800e-06, 7.257800e-06) 
#a_rec <- c(2.8e-06, 2.8e-06, 7.26e-06, 7.26e-06) LATEST VALUES


#a_rec <- c(rep(a_rec_com_LD,2),rep(a_rec_com_ST,2))
#a_rec <- c(3.201329e-08, 3.201329e-08, 8.961700e-08, 8.961700e-08) * 100
#a_rec <- c(5.6e-06, 5.6e-06, 7.26e-06, 7.26e-06)
#a_rec <- c(-5.852143,-5.852143,-5.440225,-5.440225) * mult
#a_rec <- c(3.131735e-08, 3.131735e-08, 8.961700e-08, 8.961700e-08) * 100 (this is latest prior)
a_rec <- rep(rep(18e-6,4)) / 2 # the 1/2 is tuning
names(a_rec) <- pft_names
b_rec <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_rec) <- pft_names
  
#seed bank and emergence
decay_rate <- 0.51 #the annual decay rate of the seedbank
#beta_emerg <- c(0.074, 0.056, 0.03617053, 0.07876964)
a_emerg <- rep(0.004, 4) #the average daily fraction of the seedbank that moves to the seedling pool (annual average), was 0.5/365
names(a_emerg) <- pft_names
b_emerg <- c(1.2,1.2, 0.8, 0.8) #the soil moisture response parameter for emergence
names(b_emerg) <- pft_names
W_emerg <- 14 #the window over which SMP anomalies are calculated for seedling emergence

#background_seedling_mort <- default_background_seedling_mort #see script called background_seedling_mort.R for derivation
#background_seedling_mort <- c(0.15, 0.15, 0.15, 0.15) # this changed from original parameter file from May 2018 after AHB reran the analysis scripts on the Comita/Johnson data
background_seedling_mort <- c(0.4150849, 0.1670488, 0.1723011, 0.1302036)
names(background_seedling_mort) <- pft_names

#seedling mort-H20
# P1H20 <- c(4.975e-08, 5.067e-08, 4.975e-08, 5.067e-08)
# names(P1H20) <- pft_names
# P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
# names(P2H20) <- pft_names

a.H20 <- c(1.036938e-16, 4.070565e-17, 1.036938e-16, 4.070565e-17) 
b.H20 <- c(-5.532319e-10, -6.390757e-11, -5.532319e-10, -6.390757e-11) 
c.H20 <- c(3.523348e-04, 1.268992e-05, 3.523348e-04, 1.268992e-05) 

names(a.H20) <- pft_names
names(b.H20) <- pft_names
names(c.H20) <- pft_names
DD_thresh <- rep(c(4.6e6,1.4e6),2)
names(DD_thresh) <- pft_names
window.x <- 18*7 #the number of days over which to calculate the moisture deficit
thresh.xx <- rep(c(-175912.9, -251995.7),2)
names(thresh.xx) <- pft_names


#P1H20 <- c(4.97e-08, 5.07e-08, 4.97e-08, 5.07e-08)
#names(P1H20) <- pft_names
#P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
#names(P2H20) <- pft_names
#thresh.xx <- c(-167973.2, -350000.0, -167973.2, -350000.0) #the water moisture threshold (mm of head of water) when plants start to stress
#names(thresh.xx) <- pft_names


#seedling light mort
# P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
# names(P1light_mort) <- pft_names
# P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
# names(P2light_mort) <- pft_names
# LD_light_thresh <- 18.98 # from Kobe
#Z0_seedling <- c(35,35,40,40)
#names(Z0_seedling) <- pft_names


#seedling light mort param for light mort with simple negative exponential

P1light_mort <- c(-0.010673455, -0.010673455, -0.003168996, -0.003168996)
P2light_mort <- c(-4.217788, -4.217788, -7.142556, -7.142556)

names(P1light_mort) <- pft_names
names(P2light_mort) <- pft_names


#initial conditions
seedbank_0 = 0# 22750 #the initial mass of carbon in the seedbank (g C)
seedpool_0 = 0#12000 #the initial mass of carbon in the seedling pool (g C)
litter_0 = 0 # 10000

plot_input_vars <- "Y"