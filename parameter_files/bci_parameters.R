
#########################
#user-defined parameters#
#########################

pft_names <- c("LD_DI", "LD_DT", "ST_DI", "ST_DT")
#percent_light <- 0.02
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
#see Table 1 in Hanbury-Brown et al., in prep for parameter definitions

#reproductive allocation
b_RA <- c(-3.1380, -2.4607, -2.6518, -2.6171)
names(b_RA) <- pft_names
a_RA <- c(0.0058, 0.0059, 0.0042, 0.0049)
names(a_RA) <- pft_names
F_repro <- c(0.1,0.1,0.1,0.1)#the fraction of carbon for growth and reproduction that gets allocated to reproduction
names(F_repro) <- pft_names
F_seed <- 0.5 * 0.89 #the fraction of reproductive carbon that is seed; was .82


#seed bank dynamics
S_decay <- 0.51 #the annual decay rate of the seedbank
a_emerg <- rep(0.0003,4)
names(a_emerg) <- pft_names
b_emerg <- c(1.2,1.2, 0.8, 0.8) + 0.4 #the soil moisture response parameter for emergence
names(b_emerg) <- pft_names
W_emerg <- 14 
emerg_thresh <- -15744.65 # mm H20 suction
l_crit <- 0.656455142231947

#light based-seedling mortality
a.ML <- c(-0.033231702, -0.033231702, -0.009897694, -0.009897694)
b.ML <- c(-3.836400, -3.836400, -7.154063, -7.154063) 
names(a.ML) <- pft_names
names(b.ML) <- pft_names
W_ML <- 64

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
MDDs_crit <- rep(c(4.6e6,1.4e6),2)
names(MDDs_crit) <- pft_names

#background seedling mortality
M_background <- c(0.1739748, 0.1834359, 0.1893379, 0.1085371) 
names(M_background) <- pft_names


#light-based seed to sapling transition rate
a_TR <- c(0.010300421, 0.010300421, 0.007002215, 0.007002215) 
names(a_TR) <- pft_names
b_TR <- c(1.0653, 1.0653, 0.8615, 0.8615)
names(b_TR) <- pft_names


############################
###ED2 parameters###
############################
C2B <- 2 #carbon to biomass ratio used in ED2
seed_rain <- 0.01 #kg C per month per m2 ./init/ed_params.f90:3278/3279
F_repro_ED2 <- 0.1
seedling_mortality <- 0.96 # mortality rate in every time step. Taken from ED2 code: ./init/ed_params.f90:2089

#########################################
#record the paramater values of this run#
#########################################
source('parameter_files/param_names.R')
clps <- function(x){paste0(eval(as.name(x)), collapse = ",")}
model_params_of_run <- unlist(purrr::map(.x = param_names, .f = clps))

paramsOFrun <- tibble(
  param_names = c(param_names,
                  "percent_light","start_date", "end_date","driver_data",
                  "gitCommit"),
  param_vals = c(model_params_of_run,
                 percent_light, start_date, end_date, basename(driver_data_path),
                 system("git rev-parse HEAD", intern=TRUE)) 
)

