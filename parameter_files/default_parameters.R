
#########################
#user-defined parameters#
#########################

pft_names <- c("LD_DI", "LD_DT", "ST_DI", "ST_DT")
percent_light <- 0.02
model_area <- 10000 #area in square meters
Z0 <- 160 #g C of new recruit, to match the 1 cm recruit in ED2
plot_input_vars <- "Y" #do you want to plot submodel output?
photoblastic_germ_rate_modifier_switch <- T
####################
#initial conditions#
####################

seedbank_0 = 0 #the initial mass of carbon in the seed bank (g C)
seedpool_0 = 0 #the initial mass of carbon in the seedling pool (g C)
litter_0 = 0 # the initial mass of carbon in the litter pool (g C)


#########################
###submodel parameters###
#########################

#reproductive allocation
Dmax <- c(1295, 1775, 942, 1127) #maximum diamater (mm) was 895,861,545,557 before I fixed the weighted average issue
RA_0 <- c(-3.1380, -2.4607, -2.6518, -2.6171)
names(RA_0) <- pft_names
RA_1 <- c(0.0058, 0.0059, 0.0042, 0.0049)
names(RA_1) <- pft_names
F_repro <- c(0.1,0.1,0.1,0.1)#the fraction of carbon for growth and reproduction that gets allocated to reproduction
names(F_repro) <- pft_names
F_seed <- 0.5 #the fraction of reproductive carbon that is seed
k <- 0.0125 #shape parameter relating dbh to reproductive probability

#seed bank dynamics
S_decay <- 0.51 #the annual decay rate of the seedbank
a_emerg <- rep(0.007, 4) #was 0.004 the average daily fraction of the seedbank that moves to the seedling pool (annual average), was previously 0.5/365
names(a_emerg) <- pft_names
b_emerg <- c(1.2,1.2, 0.8, 0.8) + 0.4 #the soil moisture response parameter for emergence
names(b_emerg) <- pft_names
W_emerg <- 28
emerg_thresh <- -15744.65
b0_light_germ <- c(0.5171172,0.5171172,NA,NA)
names(b0_light_germ) <- pft_names
b1_light_germ <- c(0.1729696,0.1729696,NA,NA)
names(b1_light_germ) <- pft_names
l_crit <- 70 #was 22.3

#light-based seedling mortality
a.ML <- c(-0.015096011, -0.015096011, -0.004465246, -0.004465246)
#b.ML <- c(-4.217788, -4.217788, -7.142556, -7.142556)
b.ML <- c(-3.760108,   -3.760108,   -7.148243,   -7.148243)
W_ML <- 64
names(a.ML) <- pft_names
names(b.ML) <- pft_names

#moisture-based seedling mortality
a.MH20 <- c(1.036938e-16, 4.070565e-17, 1.036938e-16, 4.070565e-17) 
b.MH20 <- c(-5.532319e-10, -6.390757e-11, -5.532319e-10, -6.390757e-11) 
c.MH20 <- c(3.523348e-04, 1.268992e-05, 3.523348e-04, 1.268992e-05) 
names(a.MH20) <- pft_names
names(b.MH20) <- pft_names
names(c.MH20) <- pft_names
psi_crit <- rep(c(-175912.9, -251995.7),2)
names(psi_crit) <- pft_names
W_psi <- 18*7 
MDDs_crit <- rep(c(4.6e6,1.4e6),2) #this parameter avoids negative values of mortality to be calculated from the quadratic relationship between MDDs and M_H2O
names(MDDs_crit) <- pft_names

#background seedling mortality
#M_background <- c(0.4150849, 0.1670488, 0.1723011, 0.1302036)
M_background <- c(0.1739748, 0.1834359, 0.1893379, 0.1085371) 
names(M_background) <- pft_names
#light-based seed to sapling transition rate
# a_TR <- rep(rep(18e-6,4)) #this parameter can be divided by 2 to have recruitment better match the BCI forest dynamics plot data
# names(a_TR) <- pft_names
# b_TR <- c(1.0653, 1.0653, 0.8615, 0.8615)
# names(b_TR) <- pft_names
a_TR <- c(rep(1.771e-5,2),rep(4.085e-5,2)) #this parameter can be divided by 2 to have recruitment better match the BCI forest dynamics plot data
names(a_TR) <- pft_names
b_TR <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_TR) <- pft_names
W_TR <- 183


############################
###default ED2 parameters###
############################
C2B <- 2 #carbon to biomass ratio used in ED2
seed_rain <- 0.01 #kg C per month per m2 ./init/ed_params.f90:3278/3279
F_repro_ED2 <- 0.3
seedling_mortality <- 0.95 # mortality rate in every time step. Taken from ED2 code: ./init/ed_params.f90:2089


#########################################
#record the paramater values of this run#
#########################################
paramsOFrun <- tibble(
  param_names = c("Dmax", "F_repro","k", 
                "F_seed","S_decay","a_emerg", "b_emerg", 
                "a.ML","b.ML","W_ML", 
                "a.MH20","b.MH20","c.MH20","psi_crit","W_psi",
                "M_background",
                "a_TR","b_TR","W_TR", 
                "percent_light","start_date", "end_date","driver_data",
                "gitCommit"),
  param_vals = c(paste0(Dmax, collapse = ","),paste0(F_repro, collapse = ","), k,
                F_seed, S_decay, paste0(a_emerg, collapse = ","), paste0(b_emerg, collapse = ","), 
                paste0(a.ML, collapse = ","), paste0(b.ML, collapse = ","), W_ML,
                paste0(a.MH20, collapse = ","),paste0(b.MH20, collapse = ","),paste0(c.MH20, collapse = ","),paste0(psi_crit, collapse = ","),W_psi, 
                paste0(M_background, collapse = ","),
                paste0(a_TR, collapse = ","), paste0(b_TR, collapse = ","), W_TR,
                percent_light, start_date, end_date, basename(driver_data_path),
                system("git rev-parse HEAD", intern=TRUE)) 
  )


##########
#scratch##
##########


# a_rec <- c(4.117201e-06, 4.117201e-06, 3.154737e-06, 3.154737e-06)
# names(a_rec) <- pft_names
# b_rec <- c(3.421052e-10,  3.421052e-10, -4.552948e-10, -4.552948e-10) 
# names(b_rec) <- pft_names
# c_rec <- c(-3.619412e-05, -3.619412e-05,  7.320845e-05,  7.320845e-05) 
# names(c_rec) <- pft_names


#a_rec <- c(-5.551113, -5.551113, -5.139195, -5.139195)
#a_rec <- c(2.811169e-06, 2.811169e-06, 7.257800e-06, 7.257800e-06) 
#a_rec <- c(2.8e-06, 2.8e-06, 7.26e-06, 7.26e-06) LATEST VALUES

#background_seedling_mort <- default_background_seedling_mort #see script called background_seedling_mort.R for derivation
#background_seedling_mort <- c(0.15, 0.15, 0.15, 0.15) # this changed from original parameter file from May 2018 after AHB reran the analysis scripts on the Comita/Johnson data



#seedling mort-H20
# P1H20 <- c(4.975e-08, 5.067e-08, 4.975e-08, 5.067e-08)
# names(P1H20) <- pft_names
# P2H20 <- c(-3.93e-17, -2.45e-17, -3.93e-17, -2.45e-17)
# names(P2H20) <- pft_names






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




#a_rec <- c(rep(a_rec_com_LD,2),rep(a_rec_com_ST,2))
#a_rec <- c(3.201329e-08, 3.201329e-08, 8.961700e-08, 8.961700e-08) * 100
#a_rec <- c(5.6e-06, 5.6e-06, 7.26e-06, 7.26e-06)
#a_rec <- c(-5.852143,-5.852143,-5.440225,-5.440225) * mult
#a_rec <- c(3.131735e-08, 3.131735e-08, 8.961700e-08, 8.961700e-08) * 100 (this is latest prior)
