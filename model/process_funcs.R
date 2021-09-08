#This script contains the functions that represent the
#environmentally sensitive regeneration processes.
#See the methods section of Hanbury-Brown et al., in prep for a description of each process.

#########################################
#####reproductive allocation#############
#########################################
prob_repro <- function(size_mm,PFT.x){
  a_RA.x <- a_RA[PFT.x]
  b_RA.x <- b_RA[PFT.x]
  y <- (exp(b_RA.x+a_RA.x*size_mm) / (1 + exp(b_RA.x+a_RA.x*size_mm)))
  return(y)
}

efrac <- function(N, co_dbh_ind, PFT){
  N_repro <- prob_repro(size_mm = co_dbh_ind, PFT.x = PFT) * N
  fraction_reproductive <- N_repro / N
  e_frac <- fraction_reproductive * F_repro[PFT] 
  return(e_frac)
}

#################################################
##############seedling emergence#################
#################################################

#modifies seedling emergence based on light for photoblastic germinators
photoblastic_germ_rate_modifier <- function(l_crit.x = l_crit, 
                                            light.x){ #understory light in current time step (MJ m-2 -day)
  
  germ_rate_modifier <- light.x / (light.x + l_crit.x) #rate modifier functional form is from (Bonan, 2019, p. 56, Fig. 4.3, panel b)
  if(photoblastic_germ_rate_modifier_switch == T & PFT %in% c("LD_DI","LD_DT")){
    return(germ_rate_modifier)
  } else{
    return(1)
  }
}


#predicts seedling emergence as a function of light
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


#light-based seedling mortality
light_mort3 <- function(light = 90, seedpool.x = 1){
  
  a.ML.x <- a.ML[PFT]
  b.ML.x <- b.ML[PFT]
  
  Pm_day <- exp(a.ML.x * light +  b.ML.x)
  
  return(Pm_day)
}
light_mort <- light_mort3


#moisture-based seedling mortality

#Function to accumulate moisture deficit days.
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

#Function to calculate seedling mortality as a function of moisture deficit days 
H20_mort <- function(deficit_days, pft.x){
  PFT <- pft.x
  
  daily_mort_rate <- a.MH20[PFT] * deficit_days^2 + b.MH20[PFT] * deficit_days + c.MH20[PFT]
  
  if(deficit_days < MDDs_crit[PFT]){
    daily_mort_rate <- 0
  }
  
  return(daily_mort_rate)
}


#light-based seedling to sapling transition (i.e. recruitment)
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








