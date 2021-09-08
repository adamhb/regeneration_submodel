#This is the primary script that drives the Tree Recruitment Scheme
#However, to set up a run, the user needs to use one of the scripts in the 'run' folder
#The functions that represent the environmentally sensitive processes are in 'model/process_funcs.R'

print(paste("Running regeneration submodel",Sys.time()))

#Generating moisture deficit days for the time series for the input data
water_def <- c()
for(PFT in pft_names){
  water_def <- append(water_def, def_func(soil_moist = input_data[input_data$pft == PFT,]$SMP, psi_crit.x = psi_crit[PFT], window = W_psi))
}

#Adding moisture deficit days to the input data
input_data$water_def <- water_def

#Applying the H20 mortality function to the time series. 
#This can be done here because the mortality rate from  
input_data <- input_data %>%
  mutate(H20_mort_rate = base::mapply(FUN = H20_mort, deficit_days = input_data$water_def, pft.x = input_data$pft))

#If tree was in negative carbon balance, then reproductive carbon allocation is zero
input_data <- input_data %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (input_data$N_co), co_dbh_ind = (input_data$dbh), PFT = input_data$pft)) %>% #Adding the "effective fraction" of NPP that gets allocated to reproduction in each time step. Note that this is calculated within cohorts in the clean_input/prep_driver_data... script which is used to calculate reproductive carbon. It is recalculated here at the pft level as a diagnostic variable. 
  mutate_at(.tbl = .,.vars = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft) 

#Calculate ED2's predictions of recruitment
if(emulate_ED2 == T){
  input_data <- input_data %>%
    mutate(ED2_R = ED2_recruitment(NPPseed = nppseed_pft_day * model_area))
}


#This converts solar radiation (PAR)
#From Joules per day at TOC
#To forest floor PAR MJ per day
if(patch_run_type != "many"){
  input_data <- input_data %>%
  mutate(light = FSDS * percent_light / 1e6)
  print(paste0("Running at ",percent_light * 100,"% light"))
}

if(patch_run_type == "many"){
  input_data <- input_data %>%
    mutate(light = FSDS * lightZ0 / 1e6)
  print(paste0("Running at ",mean(input_data$lightZ0) * 100,"% light"))
}



#The following nested for loop iterates through the pfts and timesteps of the run
#It integrates the state variables
#with the fluxes calculated in each time step.

output <- list() #variable to hold the output
j <- 1 #j is a loop counter for pft

for(PFT in pft_names){ #start pft loop
    input_vars <- input_data %>% filter(pft == PFT)
    
    #pfts
    PFT_record <- c()
    
    #seed bank dynamics
    seedbank <- c() #seed bank state variable (g C)
    frac_emerging <- c() #fraction of the seed bank emerging in each time step; recorded as a history variable for diagnostics
    
    #seedling pool dynamics
    seedpool <- c() #seedling pool state variable
    light_mort_rate <- c() #diagnostic variable
    frac_rec.t <- c() #diagnostic variable; the fraction of the seedling pool that recruits in each time step 
    
    #Recruitment and litter pool
    R <- c() #flux of recruits out of the seedling pool in each time step
    N <- c() #cumulative number of recruits (since the beginning of the simulation) for each time step
    litter <- c() #litter state variable
    
    #keep a record of the current pft
    PFT_record[1] <- PFT
    
    #initialize state variables and flux variables
    seedbank[1] <- seedbank_0
    seedpool[1] <- seedpool_0
    frac_emerging[1] <- 0 
    light_mort_rate[1] <- 0
    frac_rec.t <- 0
    R[1] <- 0
    N[1] <- 0
    litter[1] <- litter_0
   
    #Iterating through the timesteps of the run. 'i' is a loop counter for the model timestep.
    #Allocation dynamics are captured in the input data script because they don't rely on state variables tracked by the Tree Recruitment Scheme
    #They can be calculated from the input data alone. 
    for(i in 1:(nrow(input_vars)-1)){ #start time step loop
      
      #recording the PFT
      PFT_record[i+1] <- PFT
  
      #Seedbank Dynamics
      seedbank[i+1] <- seedbank[i] %>%
        - (S_decay/365 * seedbank[i]) %>%
        - emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                  no = input_vars$SMP[i])), 
                     seedbank.x = seedbank[i],
                     light.xx = input_vars$light[i])$C_emerg %>%
        + (F_seed * input_vars$c_repro[i])
      
      #Recording the fraction of the seedbank emerging in each time step as a history variable for diagnostic purposes.
      frac_emerging[i+1] <- emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                                    no = input_vars$SMP[i])), 
                                       seedbank.x = seedbank[i],
                                       light.xx = input_vars$light[i])$frac_emerg
      
      #Seedling Pool Dynamics    
      seedpool[i+1] <- seedpool[i] %>%
        + emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), 
                                                  yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                  no = input_vars$SMP[i])), 
                      seedbank.x = seedbank[i],
                      light.xx = input_vars$light[i])$C_emerg %>%
        - ((light_mort(light = ifelse(test = i > W_ML, yes = sum(input_vars$light[(i-W_ML):i] +0.0001), no = input_vars$light[i]*W_ML + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        - (input_vars$H20_mort_rate[i] * seedpool[i]) %>%
        - (seedpool[i]*M_background[PFT]/365) %>%
        - (rec_func(l = ifelse(test = i > W_ML, yes = mean(input_vars$light[(i-W_ML):i] +0.0001), no = mean(input_vars$light[(i+W_ML):i]) + 0.0001),
                    seedpool.x = seedpool[i], 
                    SMP.x = input_vars$SMP[i])$C_rec)
      
      #recording the seedling mortality rate from light for diagnostic purposes
      light_mort_rate[i+1] <- (light_mort(light = ifelse(test = i > W_ML, yes = sum(input_vars$light[(i-W_ML):i] +0.0001), no = input_vars$light[i]*W_ML + 0.00001), seedpool.x = seedpool[i]))
      
      #recording the fraction of the seedling pool that recruits each time step for diagnostic purposes 
      frac_rec.t[i+1] <- rec_func(l = ifelse(test = i > W_ML, yes = mean(input_vars$light[(i-W_ML):i] +0.0001), no = mean(input_vars$light[(i+W_ML):i]) + 0.0001), seedpool.x = seedpool[i],
                                  SMP.x = input_vars$SMP[i])$frac_rec
    
    
      #Recruitment and litter pool dynamics
      R[i+1] <- rec_func(l = ifelse(test = i > W_ML, yes = mean(input_vars$light[(i-W_ML):i] +0.0001), no = mean(input_vars$light[(i+W_ML):i]) + 0.0001), seedpool.x = seedpool[i],
                         SMP.x = input_vars$SMP[i])$N_rec
      
      N[i+1] <- N[i] %>%
        + (R[i+1])
      
      
      litter[i+1] <- litter[i] %>%
        + ((1-F_seed) * input_vars$c_repro[i]) %>%
        + (S_decay/365 * seedbank[i]) %>%
        + ((light_mort(light = ifelse(test = i > W_ML, yes = sum(input_vars$light[(i-W_ML):i] +0.0001), no = input_vars$light[i]*W_ML + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        + (seedpool[i] * M_background[PFT]/365) %>%
        + (H20_mort(deficit_days = input_vars$water_def[i], pft.x = PFT)*seedpool[i]) 
      
      
    }
    
  
    #packaging the output from one PFT into a dataframe
    output[[j]] <- data.frame(PFT_record = PFT_record,
                              seedbank = seedbank, 
                              frac_emerging = frac_emerging,
                              #carbon_emerging = carbon_emerging,
                              seedpool = seedpool,
                              light_mort_rate = light_mort_rate,
                              frac_rec.t = frac_rec.t,
                              R = R, 
                              N = N,
                              litter = litter) %>% cbind(input_vars)
    
    print(paste0("PFT ",j," of 4 is done!"))  
    j <- j+1
    
    
  } #end model loop

#combining all pft data together
full_output <- rbind(output[[1]], output[[2]], output[[3]], output[[4]])

print(paste("Run finished successfully!",Sys.time()))

























































