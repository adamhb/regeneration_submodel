source("create_output/figure_formatting.R")
source("parameter_files/parameters_ED2_run_benchmarking.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"


#site params
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")

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



light_mort_rates <- c()
lights <- 30:2081 
pft <- c()
for(p in pft_names){
  for(i in 1:length(lights)){
    PFT <- p
    light.xxx <- lights[i]
    tmp <- light_mort(light = light.xxx, seedpool.x = 1)
    light_mort_rates <- append(light_mort_rates, tmp) 
    pft <- append(pft, p) 
  }
}


mean_bci <- 52.2 # MJ of light in prior 3 months
mean_bci_TOC <- 1512 

viz_light_mort <- tibble(pft = pft,
       lights = rep(lights,4),
       light_mort_rates = light_mort_rates) %>%
  ggplot(aes(x = lights, y = light_mort_rates * 30, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "monthly light-based \n seedling mortality rate") +
  xlab(label = expression(paste("cum. light in prior 3 months ","(MJ m"^"-2",")"))) +
  labs(title = "Light-based seedling mortality") +
  geom_vline(xintercept = mean_bci, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  theme_minimal() +
  multipanel_theme

viz_light_mort


makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light_mort")


