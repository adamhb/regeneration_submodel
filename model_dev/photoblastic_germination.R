#the critical PAR level for photoblasitic germination used as a shape parameter in the germination rate modifier
l_crit <- 70 # umol m-2 s-1; The light level in a small gap, Pearson et al., 2002
mean_bci_TOC <- 16.7 # MJ m-2 day-1
mean_bci_understory <- mean_bci_TOC * 0.02 #mean understory light at BCI MJ m-2 day-1
mean_bci_20pct_gap <- mean_bci_TOC * 0.2
print(paste("l_crit =",l_crit))

photoblastic_germ_rate_modifier <- function(l_crit.x = l_crit, #this functional form matches observations from (find this obs)
         median_TOC_light = median(input_vars$FSDS), #median TOC light (J m-2 -day)
         light.x){ #understory light in current time step (J m-2 -day)
  
  median_TOC_light_umol_s <- (median_TOC_light * 4.6) / (3600 * 12) # assuming 12 hour day 
  understory_light_umol_s <- (light.x * 4.6) / (3600 * 12)
    
  x <- understory_light_umol_s/median_TOC_light_umol_s
  x_prime <- l_crit/median_TOC_light_umol_s
  
    #relative resource amount (% of median TOC light)
  germ_rate_modifier <- x/(x + x_prime) #rate modifier functional form is from (Bonan, 2019, p. 56, Fig. 4.3, panel b)
  if(photoblastic_germ_rate_modifier_switch == T & PFT %in% c("LD_DI","LD_DT")){
    return(germ_rate_modifier)
  } else{
    return(1)
  }
}

#germ data
ul <- seq(from = 0.6 * mean_bci_understory, to = 1.1 * mean_bci_20pct_gap, length.out = 200) * 1e6 #to convert from MJ to Joules

germ_data <- tibble()
for(p in pft_names){
  PFT <- p
  tmp <- tibble(light = ul,
                rate_mod = photoblastic_germ_rate_modifier(light.x = ul,median_TOC_light = mean_bci_TOC * 1e6),
                pft = PFT)
  germ_data <- rbind(germ_data,tmp)
}


photoblastic_germ_fig <- germ_data %>%
  ggplot(aes(x = light / 1e6, y = rate_mod, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  #annotate(geom = "text", x = 0, y = 0.035, label = "d", size = subplot_heading_size) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "emergence rate modifier") +
  xlab(expression(paste("Forest floor PAR [MJ m"^"-2"," day"^"-1","]"))) +
  labs(title = "Photoblastic germination") +
  theme_minimal() +
  geom_vline(xintercept = mean_bci_understory, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_20pct_gap, linetype = "dashed") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  multipanel_theme

print("made photoblastic_germ_fig")
