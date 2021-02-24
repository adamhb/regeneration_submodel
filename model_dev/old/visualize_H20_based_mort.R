source("runs/ED2_SMP_ENSO.R")
source("create_output/figure_formatting.R")
source("parameter_files/parameters_ED2_run_benchmarking.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/model_functions/"


#site params
pft_names <- c("earlydi", "earlydt", "latedi", "latedt")

H20_mort <- function(deficit_days, pft.x){
  PFT <- pft.x
  mort_rate <- deficit_days * P1H20[PFT] #+ P2H20[PFT] P2H20 is essentially zero in the observational analysis of Engelbrecht et al. 2003
  return(mort_rate/(window.x))
}

water_def <- c()
for(PFT in pft_names){
  water_def <- append(water_def, def_func(soil_moist = input_data[input_data$pft == PFT,]$SMP, thresh.x = thresh.xx[PFT], window = window.x))
}

H20_mort_rates <- c()
water_defs <- seq(from = 0, to = summary(water_def)[6], length.out = 100)
pft <- c()
for(p in pft_names){
  for(i in 1:length(water_defs)){
    PFT <- p
    water_def.xxx <- water_defs[i]
    tmp <- H20_mort(deficit_days = water_def.xxx, pft.x = p)
    H20_mort_rates <- append(H20_mort_rates, tmp) 
    pft <- append(pft, p) 
  }
}


mean_bci <- 52.2 # MJ of light in prior 3 months
mean_bci_TOC <- 1512 

viz_H20_mort <- tibble(pft = pft,
                         water_def = rep(water_defs,4),
                         H20_mort_rate = H20_mort_rates) %>%
  ggplot(aes(x = water_def / 1e5, y = H20_mort_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "solid","dashed","dashed"))+
  ylab(label = "monthly moisture-based \n seedling mortality rate") +
  xlab(label = "moisture deficit days") +
  labs(title = "moisture-based seedling mortality") +
  # geom_vline(xintercept = mean_bci, linetype = "dotted") +
  # geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  theme_minimal() +
  multipanel_theme


makePNG(fig = viz_H20_mort, path_to_output.x = path_to_output, file_name = "viz_H20_mort")



moisture_def_fig <- ggplot(data = full_output, aes(x = day, y = SMP/1e5)) +
  labs(title = "Moisture deficit days") +
  theme_classic()+
  geom_smooth(size = 1.8, color = "black", se = FALSE)+
  #smooth_line +
  #year_axis_def +
  scale_x_continuous(limits = c(1400,1600))+
  ylab(expression(paste("Soil matric potential (MPa)")))+
  xlab(bquote('day of simulation'))+
  theme(legend.position = "none") +
  geom_hline(yintercept = thresh.xx[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  geom_hline(yintercept = thresh.xx[2]/1e5, size = 1.8, color = "darkolivegreen4")+
  annotate(geom = "text", x = 1435, y = -2, label = "threshold for \n drought intolerant pft", size = 5) +
  annotate(geom = "text", x = 1430, y = -3.2, label = "threshold for \n drought tolerant pft", size = 5) +
  annotate(geom = "text", x = 1505, y = -2.1, label = "deficit days \n accumulate \n here", size = 5) +
  multipanel_theme 
  

moisture_def_fig

makePNG(fig = moisture_def_fig, path_to_output.x = path_to_output, file_name = "viz_def_days")






