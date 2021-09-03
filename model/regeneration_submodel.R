
#source("parameter_files/parameters.R")

print(paste("Running regeneration submodel",Sys.time()))


############functions###############
#the probability that an individual is of reproductive status as a function of dbh (mm)
# prob_repro <- function(k.x = k, size_mm, Dmax){
#   y <- 1 / (1 + exp(-k.x*(size_mm - 0.5*Dmax)))
#   return(y)
# }
# 
# 
# efrac <- function(N, co_dbh_ind, PFT){
#   N_repro <- prob_repro(size_mm = co_dbh_ind, Dmax = Dmax[PFT]) * N
#   fraction_reproductive <- N_repro / N
#   e_frac <- fraction_reproductive * F_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
#   return(e_frac)
# }



#seedling emergence
# emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.x = avg_SMP, avg_SMP.x = avg_SMP, seedbank.x){
#   
#   log10_frac_emerg <- log10(a) + b*log10(abs(avg_SMP.x)/abs(SMP.x)) 
#   
#   frac_emerg <- 10^log10_frac_emerg 
#   #if(frac_emerg > 0.07){frac_emerg <- 0.07}
#   
#   C_emerg <- frac_emerg * seedbank.x
#   
#   out <- list(frac_emerg, C_emerg)
#   names(out) <- c("frac_emerg", "C_emerg")
#   return(out)
# }


photoblastic_germ_rate_modifier <- function(l_crit.x = l_crit, 
                                            light.x){ #understory light in current time step (MJ m-2 -day)
  
  germ_rate_modifier <- light.x / (light.x + l_crit.x) #rate modifier functional form is from (Bonan, 2019, p. 56, Fig. 4.3, panel b)
  if(photoblastic_germ_rate_modifier_switch == T & PFT %in% c("LD_DI","LD_DT")){
    return(germ_rate_modifier)
  } else{
    return(1)
  }
}



# emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.2.to.0.wks.ago, SMP.4.to.2.wks.ago, seedbank.x, light.xx){
#   
#   if(input_vars$SMP[i] < emerg_thresh){
#     frac_emerg <- 0
#   } else {
#   
#   log10_frac_emerg <- log10(a) + b*log10(abs(SMP.4.to.2.wks.ago)/abs(SMP.2.to.0.wks.ago)) 
#   
#   frac_emerg <- (10^log10_frac_emerg) *  photoblastic_germ_rate_modifier(l_crit.x = l_crit, light.x = light.xx * 1e6)
#                                                                           
#   #if(frac_emerg > 0.07){frac_emerg <- 0.07}
#   
#   }
#   
#   C_emerg <- frac_emerg * seedbank.x
#   
#   out <- list(frac_emerg, C_emerg)
#   names(out) <- c("frac_emerg", "C_emerg")
#   return(out)
# }

# emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.2.to.0.wks.ago, seedbank.x, light.xx){
#   
#   wet_index <- 1 / (SMP.2.to.0.wks.ago * -1 / 1e5)
#   
#   if(SMP.2.to.0.wks.ago < emerg_thresh){
#     frac_emerg <- 0
#   } else {
#     
#     frac_emerg <- (a * wet_index^b)  *  photoblastic_germ_rate_modifier(l_crit.x = l_crit, light.x = light.xx * 1e6)
#     
#     #if(frac_emerg > 0.07){frac_emerg <- 0.07}
#     
#   }
#   
#   C_emerg <- frac_emerg * seedbank.x
#   
#   out <- list(frac_emerg, C_emerg)
#   names(out) <- c("frac_emerg", "C_emerg")
#   return(out)
# }


emerg_func <- function(a = a_emerg[PFT], b = b_emerg[PFT], SMP.2.to.0.wks.ago, seedbank.x, light.xx){
  
  wet_index <- 1 / (SMP.2.to.0.wks.ago * -1 / 1e5)
  
  if(SMP.2.to.0.wks.ago < emerg_thresh){
    frac_emerg <- 0
  } else {
    
    frac_emerg <- (a * wet_index^b)  *  photoblastic_germ_rate_modifier(l_crit.x = l_crit, light.x = light.xx)
    
    #if(frac_emerg > 0.07){frac_emerg <- 0.07}
    
  }
  
  C_emerg <- frac_emerg * seedbank.x
  
  out <- list(frac_emerg, C_emerg)
  names(out) <- c("frac_emerg", "C_emerg")
  return(out)
}




def_func <- function(soil_moist, psi_crit.x = psi_crit[PFT], window){
  def <- (abs(psi_crit.x) - abs(soil_moist))*-1
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
  
  daily_mort_rate <- a.MH20[PFT] * deficit_days^2 + b.MH20[PFT] * deficit_days + c.MH20[PFT]
  
  if(deficit_days < MDDs_crit[PFT]){
    daily_mort_rate <- 0
  }
  
  return(daily_mort_rate)
}


#light-based seedling mortality
# light_mort <- function(light = 5000000*60, seedpool.x = 750000){
# 
#   pct_light <- (light / (15750113 * 90 / 1e6)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
# 
#   #seedlings_N <- seedpool.x / Z0_seedling[PFT]
# 
#   A <- P1light_mort[PFT]
#   B <- P2light_mort[PFT]
# 
#   ifelse((test = PFT == "ST_DI" | PFT == "ST_DT" | (PFT == "LD_DI" & pct_light <= LD_light_thresh) | (PFT == "LD_DT" & pct_light <= LD_light_thresh)),
#          yes = Ml <- A * exp(-B*pct_light),
#          no = Ml <- A * exp(-B*LD_light_thresh))
# 
#   Pm_yr <- 1 - exp(-Ml*3)
# 
#   Pm_day <- Pm_yr / 90 # why did I divide by 90 here instead of 365, check Kobe paper on this
# 
#   #N_mort <- Pm_day * seedlings_N
# 
#   #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
# 
#   return(Pm_day)
# }



#####second light mort function with no plateauing########## 

# light_mort <- function(light = 5000000*60, seedpool.x = 750000){
# 
#   pct_light <- (light / (15750113 * 90 / 1e6)) * 100 #the percent RI equivalent at Kobe's site in Costa Rica
# 
#   #seedlings_N <- seedpool.x / Z0_seedling[PFT]
# 
#   A <- P1light_mort[PFT]
#   B <- P2light_mort[PFT]
# 
#   Ml <- A * exp(-B*pct_light)
# 
#   Pm_yr <- 1 - exp(-Ml*3)
# 
#   Pm_day <- Pm_yr / 90 # why did I divide by 90 here instead of 365, check Kobe paper on this
# 
#   #N_mort <- Pm_day * seedlings_N
# 
#   #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
# 
#   return(Pm_day)
# }

#third light mort (simple negative exponential)

light_mort <- function(light = 90, seedpool.x = 1){ #input: cumulative light (MJ m-2) at seedling layer over prior 64 days
  
  #browser()
  a.ML.x <- a.ML[PFT]
  b.ML.x <- b.ML[PFT]
  
  Pm_day <- exp(a.ML.x * light + b.ML.x)
  
  #N_mort <- Pm_day * seedlings_N
  
  #frac_mort <- (N_mort * Z0_seedling[PFT]) / seedpool.x
  
  return(Pm_day) #output: daily mortality rate
}




#recruitment subroutine function
#inputs: l = light in MJ per square meter at the forest floor over the prior 6 months
##average light at the forest floor over any 6 month period


#output
#provides the number of recruits per day

# rec_func <- function(a_rec.x = a_rec[PFT],
#                       b_rec.x = b_rec[PFT],
#                       l,
#                       seedpool.x,
#                       SMP.x = avg_SMP,
#                       c_rec.x = c_rec[PFT]){
#   
#   #frac_rec <- predict(object = STm2, newdata = tibble(light = l))
#   
#   frac_rec <- a_rec.x * l + b_rec.x * l^2 + c_rec.x
#   
#   if(SMP.x < thresh.xx[PFT]){
#     frac_rec <- 0
#   }
#   
#   C_rec <- frac_rec * seedpool.x
#   
#   N_rec <- C_rec / Z0
#   
#   out <- list(frac_rec,C_rec, N_rec)
#   
#   names(out) <- c("frac_rec", "C_rec", "N_rec")
#   
#   return(out) 
# }



rec_func <- function(a_TR.x = a_TR[PFT], b_TR.x = b_TR[PFT], l, SMP.x, seedpool.x){
  
  frac_rec <- a_TR.x * l^b_TR.x
  
  if(SMP.x < psi_crit[PFT]){
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
  water_def <- append(water_def, def_func(soil_moist = input_data[input_data$pft == PFT,]$SMP, psi_crit.x = psi_crit[PFT], window = W_psi))
}



#adding water deficit to the input data
input_data$water_def <- water_def

#applying the H20 mortality function to the time series
input_data <- input_data %>%
  mutate(H20_mort_rate = base::mapply(FUN = H20_mort, deficit_days = input_data$water_def, pft.x = input_data$pft))



input_data <- input_data %>%
  mutate(e_frac = base::mapply(FUN = efrac, N = (input_data$N_co), co_dbh_ind = (input_data$dbh), PFT = input_data$pft)) %>% #adding the "effective fraction" of NPP that gets allocated to reproduction in each time step
  #mutate(c_repro = e_frac * CgANDr * model_area) %>%  #calculating the carbon allocated to reproduction in each daily timestep for the whole model area (1 hectare). Because NPP is input in units of per m2
  mutate_at(.tbl = .,.vars = vars(c_repro), .funs = function(x){ifelse(x < 0, 0, x)}) %>% 
  arrange(., day,pft) 

if(emulate_ED2 == T){
  input_data <- input_data %>%
    mutate(ED2_R = ED2_recruitment(NPPseed = nppseed_pft_day * model_area))
}


if(patch_run_type != "many"){
  input_data <- input_data %>%
  mutate(light = FSDS * percent_light / 1e6)
  print(paste0("Running at ",percent_light * 100,"% light"))#this converts solar radiation in Joules per day at TOC to solar radiation at the forest floor in MJ per day
}

if(patch_run_type == "many"){
  input_data <- input_data %>%
    mutate(light = FSDS * lightZ0 / 1e6)
  print(paste0("Running at ",mean(input_data$lightZ0) * 100,"% light"))
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
    #carbon_emerging <- c()
    
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
    #carbon_emerging[1] <- 0
    
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
        - (S_decay/365 * seedbank[i]) %>%
        #- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg %>%
        - emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                  no = input_vars$SMP[i])), 
                     seedbank.x = seedbank[i],
                     light.xx = input_vars$light[i])$C_emerg %>%
        + (F_seed * input_vars$c_repro[i])
      
      #frac_emerging[i+1] <- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$frac_emerg
      
      frac_emerging[i+1] <- emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                                    no = input_vars$SMP[i])), 
                                       seedbank.x = seedbank[i],
                                       light.xx = input_vars$light[i])$frac_emerg
      
      #carbon_emerging[i+1] <- emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg
      
      #seedling pool dynamics
      
      #if(i == 3653){browser()}
      
      seedpool[i+1] <- seedpool[i] %>%
        #+ (emerg_func(SMP.x = (ifelse(test= i > 14, yes = mean(input_vars$SMP[(i-13):i]), no = input_vars$SMP[i])), seedbank.x = seedbank[i])$C_emerg)  %>%
        + emerg_func(SMP.2.to.0.wks.ago = (ifelse(test= i > round(W_emerg), yes = mean(input_vars$SMP[(i-round(W_emerg)):i]), 
                                                  no = input_vars$SMP[i])), 
                     seedbank.x = seedbank[i],
                     light.xx = input_vars$light[i])$C_emerg %>%
        - ((light_mort(light = ifelse(test = i > W_ML, yes = sum(input_vars$light[(i-W_ML):i] +0.0001), no = input_vars$light[i]*W_ML + 0.00001), seedpool.x = seedpool[i])) * seedpool[i]) %>%
        - (input_vars$H20_mort_rate[i] * seedpool[i]) %>%
        - (seedpool[i]*M_background[PFT]/365) %>%
        - (rec_func(l = ifelse(test = i > W_ML, yes = mean(input_vars$light[(i-W_ML):i] +0.0001), no = mean(input_vars$light[(i+W_ML):i]) + 0.0001),
                    seedpool.x = seedpool[i], 
                    SMP.x = input_vars$SMP[i])$C_rec)
      
      light_mort_rate[i+1] <- (light_mort(light = ifelse(test = i > W_ML, yes = sum(input_vars$light[(i-W_ML):i] +0.0001), no = input_vars$light[i]*W_ML + 0.00001), seedpool.x = seedpool[i]))
      
      frac_rec.t[i+1] <- rec_func(l = ifelse(test = i > W_ML, yes = mean(input_vars$light[(i-W_ML):i] +0.0001), no = mean(input_vars$light[(i+W_ML):i]) + 0.0001), seedpool.x = seedpool[i],
                                  SMP.x = input_vars$SMP[i])$frac_rec
    
    
      
      
      #recruitment and litter pool dynamics
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
    
    
  }


full_output <- rbind(output[[1]], output[[2]], output[[3]], output[[4]])

print(paste("Run finished successfully!",Sys.time()))

























































