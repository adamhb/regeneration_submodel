
#source("parameter_files/parameters.R")

print(paste("Running regeneration submodel",Sys.time()))

############functions###############
#the probability that an individual is of reproductive status as a function of dbh (mm)
prob_repro <- function(k = 0.0125, size_mm, Dmax){
  y <- 1 / (1 + exp(-k*(size_mm - 0.5*Dmax)))
  return(y)
}


efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, Dmax = Dmax[PFT]) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * frac_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}


#seedling emergence
emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
  
  log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
  
  frac_emerg <- 10^log10_frac_emerg 
  if(frac_emerg > 0.07){frac_emerg <- 0.07}
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}


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



H20_mort <- function(deficit_days, pft.x){
  PFT <- pft.x
  mort_rate <- deficit_days * P1H20[PFT] #+ P2H20[PFT] P2H20 is essentially zero in the observational analysis of Engelbrecht et al. 2003
  return(mort_rate/(window.x))
}


#light-based seedling mortality
light_mort <- function(light = 5000000*60, seedpool.x = 750000){
  
  
  pct_light <- (light / (15750113 * 90 / 1e6)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
  
  seedlings_N <- seedpool.x / Z0_seedling[PFT]
  
  A <- P1light_mort[PFT]
  B <- P2light_mort[PFT]
  
  ifelse((test = PFT == "latedi" | PFT == "latedt" | (PFT == "earlydi" & pct_light <= 18.98) | (PFT == "earlydt" & pct_light <= 18.98)), 
         yes = Ml <- A * exp(-B*pct_light),
         no = Ml <- A * exp(-B*18.98))
  
  Pm_yr <- 1 - exp(-Ml*3)
  
  Pm_day <- Pm_yr / 90 # why did I divide by 90 here instead of 365, check Kobe paper on this
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day)
}



#recruitment subroutine function
#inputs: l = light in MJ per square meter at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

rec_func <- function(a_rec.x = a_rec[PFT], b_rec.x = b_rec[PFT], l, avg_l.x = avg_l, seedpool.x, SMP.x = avg_SMP){
  
  log10_frac_rec <- log10(a_rec.x) + b_rec.x*log10(l/avg_l) 
  
  frac_rec <- (10^log10_frac_rec) #the fraction of the seedling pool recruiting per day
  
  if(SMP.x < thresh.xx[PFT]){
    frac_rec <- 0
  }
  
  C_rec <- frac_rec * seedpool.x
  
  N_rec <- C_rec / Z0
  
  out <- list(frac_rec,C_rec, N_rec)
  
  names(out) <- c("frac_rec", "C_rec", "N_rec")
  return(out)
}


#generating water deficit values for the time series
water_def <- c()
for(PFT in pft_names){
  water_def <- append(water_def, def_func(soil_moist = input_data[input_data$pft == PFT,]$SMP, thresh.x = thresh.xx[PFT], window = window.x))
}



#adding water deficit to the input data
input_data$water_def <- water_def

#applying the H20 mortality function to the time series
input_data <- input_data %>%
  mutate(H20_mort_rate = base::mapply(FUN = H20_mort, deficit_days = input_data$water_def, pft.x = input_data$pft))


input_data <- input_data %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (input_data$N_co), co_dbh_ind = (input_data$dbh), PFT = input_data$pft)) %>% #adding the "effective fraction" of NPP that gets allocated to reproduction in each time step
  mutate(c_repro = e_frac * CgANDr * model_area) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare). Because NPP is input in units of per m2
  mutate_at(.tbl = .,.vars = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft) 

if(emulate_ED2 == T){
  input_data <- input_data %>%
    mutate(ED2_R = ED2_recruitment(NPPseed = nppseed_pft_day * model_area))
}


if(patch_run_type != "many"){
  input_data <- input_data %>%
  mutate(light = FSDS * percent_light / 1e6) #this converts solar radiation in Joules per day at TOC to solar radiation at the forest floor in MJ per per day
}

if(patch_run_type == "many"){
  input_data <- input_data %>%
    mutate(light = FSDS * lightZ0 / 1e6)
}

#str(input_data)

output <- list()
j <- 1

for(PFT in pft_names){
    
    
    input_vars <- input_data %>% filter(pft == PFT)
    
    #pfts
    PFT_record <- c()
    
    #seed bank dynamics
    seedbank <- c()
    frac_emerging <- c()
    carbon_emerging <- c()
    
    #seedling pool dynamics
    seedpool <- c()
    light_mort_rate <- c()
    frac_rec.t <- c()
    
    #recruitment and litter pool
    R <- c()
    N <- c()
    litter <- c()
    
    
    #initializing
    #pfts
    #lPFT[1] <- PFT.xxx 
    #dPFT[1] <- dPFT.xxx
    PFT_record[1] <- PFT
    
  
    #seedbank dynamics
    seedbank[1] <- seedbank_0
    frac_emerging[1] <- 0 
    carbon_emerging[1] <- 0
    
    #seedling pool dynamics
    seedpool[1] <- seedpool_0
    light_mort_rate[1] <- 0
    frac_rec.t <- 0
    
    #recruitment and litter pool
    R[1] <- 0
    N[1] <- 0
    litter[1] <- litter_0
   
    
    for(i in 1:(nrow(input_vars)-1)){
      
      #recording PFTs
      PFT_record[i+1] <- PFT
      
      
      #allocation dynamics are captured above, outside the for loops, because they don't rely on previous time steps
      
     
      #seedbank dynamics
      seedbank[i+1] <- seedbank[i] %>%
        - (decay_rate/365 * seedbank[i]) %>%
        - emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg %>%
        + (seed_frac * input_vars$c_repro[i])
      
      frac_emerging[i+1] <- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$frac_emerg
      
      carbon_emerging[i+1] <- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg
      
      #seedling pool dynamics
      
      #if(i == 3653){browser()}
      
      seedpool[i+1] <- seedpool[i] %>%
        + (emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg)  %>%
        - ((light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        - (input_vars$H20_mort_rate[i] * seedpool[i]) %>%
        - (seedpool[i]*background_seedling_mort[PFT]/365) %>%
        - (rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001),
                    seedpool.x = seedpool[i], 
                    SMP.x = input_vars$SMP[i])$C_rec)
      
      light_mort_rate[i+1] <- (light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i]))
      
      frac_rec.t[i+1] <- rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001), seedpool.x = seedpool[i],
                                  SMP.x = input_vars$SMP[i])$frac_rec
    
    
      
      
      #recruitment and litter pool dynamics
      R[i+1] <- rec_func(l = ifelse(test = i > 183, yes = sum(input_vars$light[(i-183):i] +0.0001), no = sum(input_vars$light[(i+183):i]) + 0.0001), seedpool.x = seedpool[i],
                         SMP.x = input_vars$SMP[i])$N_rec
      
      N[i+1] <- N[i] %>%
        + (R[i+1])
      
      
      litter[i+1] <- litter[i] %>%
        + ((1-seed_frac) * input_vars$c_repro[i]) %>%
        + (decay_rate/365 * seedbank[i]) %>%
        + ((light_mort(light = ifelse(test = i > 90, yes = sum(input_vars$light[(i-90):i] +0.0001), no = input_vars$light[i]*90 + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        + (seedpool[i] * background_seedling_mort[PFT]/365) %>%
        + (H20_mort(deficit_days = input_vars$water_def[i], pft.x = PFT)*seedpool[i]) 
      
      
    }
    
  
    
    output[[j]] <- data.frame(PFT_record = PFT_record,
                              seedbank = seedbank, 
                              frac_emerging = frac_emerging,
                              carbon_emerging = carbon_emerging,
                              seedpool = seedpool,
                              light_mort_rate = light_mort_rate,
                              frac_rec.t = frac_rec.t,
                              R = R, 
                              N = N,
                              litter = litter) %>% cbind(input_vars)
    
    print(paste0("PFT ",j," of 4 is done!"))  
    j <- j+1
    
    
  }


full_output <- rbind(output[[1]], output[[2]], output[[3]], output[[4]])

print(paste("Run finished successfully!",Sys.time()))

























































