#PFT parameters 
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")


#site params and tuning params
#tuning param
a_rec <- rep(0.0004,4)
names(a_rec) <- pft_names
#site and scenario params
#avg_precip <- 71 #precipitation in mm over two weeks (the annual average)
avg_SMP <- -60326 #
avg_l <- 105 #the average total solar radiation load (MJ per m2) at the forest floor over 6 months (annual average); 61 is the value that was used previously

#transition from seedling to adult recruit
#a_rec <- a_rec_default #this is the daily beta rec default from liza comitas data

b_rec <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_rec) <- pft_names
Z0 <- 160 #g C of new recruit, to match the 1 cm recruit in ED2

#these are the default parameters based off of BCI FDP data
Dmax <- c(895, 861, 545, 557) #maximum diamater (mm)
names(Dmax) <- pft_names
frac_repro <- c(0.1,0.1,0.1,0.1)#the fraction of NPP that gets allocated to reproduction
names(frac_repro) <- pft_names
seed_frac <- 0.5 #the fraction of reproductive carbon that gets allocated to seeds

#seed bank and emergence
decay_rate <- 0.51 #the annual decay rate of the seedbank
#beta_emerg <- c(0.074, 0.056, 0.03617053, 0.07876964)
a_emerg <- rep(0.5/365, 4) #the average daily fraction of the seedbank that moves to the seedling pool (annual average)
names(a_emerg) <- pft_names
b_emerg <- c(1.05,1.05, 1, 1) #the precipitation response parameter for emergence
names(b_emerg) <- pft_names

#background_seedling_mort <- default_background_seedling_mort #see script called background_seedling_mort.R for derivation
background_seedling_mort <- c(0.15, 0.15, 0.15, 0.15) # this changed from original parameter file from May 2018 after AHB reran the analysis scripts on the Comita/Johnson data
names(background_seedling_mort) <- pft_names

#seedling mort-H20
P1H20 <- c(4.975e-08, 5.067e-08, 4.975e-08, 5.067e-08)
names(P1H20) <- pft_names
P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
names(P2H20) <- pft_names

#P1H20 <- c(4.97e-08, 5.07e-08, 4.97e-08, 5.07e-08)
#names(P1H20) <- pft_names
#P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
#names(P2H20) <- pft_names
thresh.xx <- c(-167973.2, -350000.0, -167973.2, -350000.0) #the water moisture threshold (mm of head of water) when plants start to stress
names(thresh.xx) <- pft_names
window.x <- 18*7 #the number of days over which to calculate the moisture deficit

#seedling light mort
P1light_mort <- c(0.752, 0.752, 0.0241, 0.0241)
names(P1light_mort) <- pft_names
P2light_mort <- c(0.1368, 0.1368, 0.0404, 0.0404)
names(P2light_mort) <- pft_names
Z0_seedling <- c(35,35,40,40)
names(Z0_seedling) <- pft_names




#initial conditions
seedbank_0 = 0# 22750 #the initial mass of carbon in the seedbank (g C)
seedpool_0 = 0#12000 #the initial mass of carbon in the seedling pool (g C)
litter_0 = 0 # 10000

plot_input_vars <- "Y"