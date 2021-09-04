

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
  e_frac <- fraction_reproductive * F_repro[PFT] #frac repro for now is just a fixed percent of NPP (10%), need better data to get better numbers for this
  return(e_frac)
}

#################################################
##############seedling emergence#################
#################################################
photoblastic_germ_rate_modifier <- function(l_crit.x = l_crit, 
                                            light.x){ #understory light in current time step (MJ m-2 -day)
  
  germ_rate_modifier <- light.x / (light.x + l_crit.x) #rate modifier functional form is from (Bonan, 2019, p. 56, Fig. 4.3, panel b)
  if(photoblastic_germ_rate_modifier_switch == T & PFT %in% c("LD_DI","LD_DT")){
    return(germ_rate_modifier)
  } else{
    return(1)
  }
}



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
