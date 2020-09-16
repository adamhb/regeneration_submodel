
#ED2 parameters
C2B <- 2 #carbon to biomass ratio used in ED2
seed_rain <- 0.01 #kg C per month per m2 ./init/ed_params.f90:3278/3279
F_repro_ED2 <- 0.3

#the default value for this is 0.95
seedling_mortality <- 0.95 # mortality rate in every time step. Taken from ED2 code: ./init/ed_params.f90:2089


#the below allometric functions were taken from the ED2 code to determine the amount of carbon required to make 1 new recuirt with dbh of 1 cm
#sapling height to dbh function
h2dbh <- function(h, #height (m) 
                  b1Ht = (0.37 * log(10)),
                  b2Ht = 0.64) {
  exp((log(h)-b1Ht)/b2Ht) #returns dbh (cm)
}
#a sapling of 2.345 m in height has a 1 cm dbh; h2dbh(2.345) = 1 cm

#dbh to biomass of leaves
dbh2bl <- function(dbh, #cm
                   b1Bl = 0,
                   b2Bl = 0){
  b1Bl / C2B * dbh ^ b2Bl + 0.08 # returns mass of carbon in leaves (Kg C / plant) ./utils/allometry.f90:167
} #the C2B term here (2) appears to modify this allometric relationship to return units of C rather than units of biomass (which I assume is what the equation was originally designed to do)


#dbh to alive biomass function. It appears that small trees don't have dead biomass in ED2, /init/ed_params.f90:2727
dbh2ba <- function(dbh, # 
                   q = 1,
                   b1Bs_small = 0,
                   b2Bs_small = 0){
  dbh2bl(dbh) * (1 + q) #* 2 #NOTE I TOOK OUT THIS x2 BECAUSE IT SEEMS THAT THE C2B CONVERSION NEVER HAPPENS FOR THESE VERY SMALL TREES BECAUSE THE B1BL PARAM IN THE LEAVE BIOMASS EQUATION IS 0 FOR SMALL TREES
}
                  
#recruitment rate per m2 per ha per year
#bseed units coming out of ED2 appears to be KgC / m2 according to ed_state_vars.f90 L11590
#use bseed_PFT in this function
# ED2_recruitment_bseeds <- function(bseeds, min_dbh = 1){ #bseeds (KgC / m2); dbh (cm)
#   ((bseeds + seed_rain) * (1 - seedling_mortality) / dbh2ba(min_dbh)) #seed_rain adds 0.1 kg of C of seed every timestep 
# } #function returns # of recruits / m2 for the given timestep
#0.5 KgC of seed per m2 = 781.25 recruits per ha


# ED2_recruitment <- function(NPPseed, min_dbh = 1){ #input is npp to seed per pft per m2 per day (gC)
#   #C_gr <- NPPseed / 0.3
#   ((NPPseed + (seed_rain * 1000 / 30.4)) * (1 - seedling_mortality) / (dbh2ba(min_dbh) * 1000)) #seed_rain adds 0.1 kg of C of seed every timestep 
# }
#ED2_recruitment(NPPseed = 0.195) 



ED2_recruitment <- function(NPPseed, min_dbh = 1){ #input is npp to seed per pft per m2 per day (gC)
  C_gr <- NPPseed / 0.3
  ((C_gr * F_repro_ED2) + (seed_rain * 1000 / 30.4)) * (1 - seedling_mortality) / (dbh2ba(min_dbh) * 1000) #seed_rain adds 0.01 kg of C of seed every timestep 
}







