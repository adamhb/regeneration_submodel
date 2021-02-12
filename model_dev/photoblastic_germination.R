#the critical PAR level for photoblasitic germination used as a shape parameter in the germination rate modifier
l_crit <- 22.3 # umol m-2 s-1; Pearson et al., 2002

photoblastic_germ_rate_modifier <- function(l_crit.x = l_crit, #this functional form matches observations from (find this obs)
         median_TOC_light = median(input_vars$FSDS), #median TOC light (J m-2 -day)
         light.x){ #understory light in current time step (J m-2 -day)
  
  median_TOC_light_umol_s <- (median_TOC_light * 4.6) / (3600 * 12) # assuming 12 hour day 
  understory_light_umol_s <- (light.x * 4.6) / (3600 * 12)
    
  x <- understory_light_umol_s/median_TOC_light_umol_s
  x_prime <- l_crit/median_TOC_light_umol_s
  
    #relative resource amount (% of median TOC light)
  germ_rate_modifier <- x/(x + x_prime) #rate modifier functional form is from (Bonan, 2019, p. 56, Fig. 4.3, panel b)
  if(photoblastic_germ_rate_modifier_switch == T & PFT %in% c("LD_DI","LD")){
    return(germ_rate_modifier)
  } else{
    return(1)
  }
}

ul <- seq(from = 300000, to = 5e6, length.out = 200)
plot(x = ul, y = photoblastic_germ_rate_modifier(light.x = ul))


