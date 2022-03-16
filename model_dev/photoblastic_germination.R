source('model/process_funcs.R')
source('utils/supporting_funcs.R')
#Step 1. calculate the value of par_crit for BCI. par_crit is the critical light threshold
#for photoblastic germination
##############################################################################################
#we use par in a small canopy gap as the threshold for photoblastic germinators.
par_in_gap <- 3 #mol m2-1 day-1 (from Fig. 1 in Pearson et al., 2002)
mol_photons_per_joule <- 4.57 / 1e6  #Garcia-Rodriguez et al., 2020

l_crit <- par_in_gap / mol_photons_per_joule * megajoules_per_joule #joules of par m2-1 day-1
print(paste("par crit = ",l_crit, "meagjoules m2-1 day-1"))

# Pearson TRH, Burslem DFRP, Mullins CE, Dalling JW. 2002. 
#Germination ecology of neotropical pioneers: Interacting effects of environmental conditions and seed size. 
#Ecology 83: 2798–2807.
 
# García-Rodríguez A, García-Rodríguez S, Díez-Mediavilla M, Alonso-Tristán C. 2020. Photosynthetic Active 
#Radiation, Solar Irradiance and the CIE Standard Sky Classification. Applied Sciences 10.
##############################################################################################

#Step 2. generate germination/emergence data for visualization purposes
##############################################################################################
mean_bci_TOC <- summary(input_data$FSDS)[3] / 1e6
mean_bci_understory <- mean_bci_TOC * 0.02 #mean understory light at BCI MJ m-2 day-1
mean_bci_20pct_gap <- mean_bci_TOC * 0.2

ul <- seq(from = 0.6 * mean_bci_understory, to = 1.1 * mean_bci_20pct_gap, length.out = 200)

germ_data <- tibble()
for(p in pft_names){
  PFT <- p
  tmp <- tibble(light = ul,
                rate_mod = photoblastic_germ_rate_modifier(light.x = ul),
                pft = PFT)
  germ_data <- rbind(germ_data,tmp)
}

photoblastic_germ_fig <- germ_data %>%
  ggplot(aes(x = light, y = rate_mod, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  #annotate(geom = "text", x = 0, y = 0.035, label = "d", size = subplot_heading_size) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "Emergence rate \n modifier") +
  xlab(expression(atop("Forest floor light", paste("[MJ PAR m"^"-2"," day"^"-1","]")))) +
  labs(title = "Light & seedling emergence") +
  theme_minimal() +
  geom_vline(xintercept = mean_bci_understory, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_20pct_gap, linetype = "dashed") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  multipanel_theme

print("made photoblastic_germ_fig")
#####################################################################################