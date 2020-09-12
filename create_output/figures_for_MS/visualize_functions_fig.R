source("create_output/figure_formatting.R")

#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"

source('parameter_files/parameters_ED2_run_Aug_17.R')

source2('model/regeneration_submodel.R',start = 9,end = 209) #source the model functions

#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100

multipanel_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          strip.text.x = element_text(size = 20),
                          legend.title = element_blank (),
                          axis.title.x = element_text (size = 16), # change the axis title
                          axis.title.y = element_text (size = 16),
                          axis.title.y.right = element_text (size = 16, color = pft.cols[2]),
                          axis.text.x = element_text (size = 14, colour = "black"),
                          axis.text.y = element_text (size = 14, colour = "black"),
                          legend.position = "none")


###############################################
#############        repro alloc                ##########

sizes <- 1:1000 
pft <- c()

for(i in pft_names){
  pft <- append(pft,rep(i,length(sizes)))
}


F_repro_fig <- tibble(F_alloc = c(efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[1]),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[2]),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[3]),
                   efrac(N = 1000, co_dbh_ind = sizes, PFT = pft_names[4])),
       PFT = pft,
       dbh = rep(sizes,4)) %>%
  ggplot(aes(x = dbh, y = F_alloc, color = PFT, linetype = PFT)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "solid","solid","dashed")) +
  ylab(expression(atop(paste("fraction of ",C[g+r]),"allocated to reproduction"))) +
  labs(title = "Allocation to reproduction") +
  multipanel_theme +
        #legend.key.size = unit(2,"line")) +
  theme(legend.position = c(0.8,0.4), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size=8)))
         
  

#makePNG(fig = F_repro_fig, path_to_output.x = path_to_output, file_name = "F_repro_fig")

############33
############################emergence

emerg <- c()
SMP_windows <- seq(from = -5000, to = 0, length.out = 100)
pft <- c()
for(p in pft_names){
  for(i in 1:length(SMP_windows)){
    PFT <- p
    SMP_window <- SMP_windows[i]
    tmp <- emerg_func(SMP.x = SMP_window, seedbank.x = 1)$frac_emerg
    emerg <- append(emerg, tmp) 
    pft <- append(pft, p) 
  }
}


viz_emerg <- tibble(pft = pft,
                    SMP = rep(SMP_windows,4),
                    emerg = emerg) %>%
  ggplot(aes(x = SMP / 1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "mean SMP (Mpa) \n in prior two weeks") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme

#makePNG(fig = viz_emerg, path_to_output.x = path_to_output, file_name = "visualize_emergence")


############
###light-based mort
###############3




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

light_mort_data <- tibble(pft = pft,
                         lights = rep(lights,4),
                         light_mort_rates = light_mort_rates)


lm <- light_mort_data %>%
  filter(lights == round(mean_bci)) %>%
  pull(light_mort_rates)

print(paste0("light mort for the LD PFT is ", round(lm[1] /lm[3],1), " times greater than for the ST pft at mean light"))

viz_light_mort <- light_mort_data %>%
  ggplot(aes(x = lights, y = light_mort_rates * 30, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "monthly light-based \n seedling mortality rate") +
  xlab(label = expression(paste("cum. light in prior 3 months ","(MJ m"^"-2",")"))) +
  labs(title = "Light-based seedling mortality") +
  geom_vline(xintercept = mean_bci, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  annotate(geom = "text", x = 700, y = 0.15, label = "BCI mean \n at smallest cohort", size = 5) +
  geom_segment(aes(x = 350, y = 0.12, xend = 70, yend = 0.05),
                  arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  #theme_minimal() +
  multipanel_theme


#makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light_mort")




#transition rate
light_regimes <- 20:1000
light_rec_data <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    annual_transition_rate =
      rec_func(l = light_regimes,
               a_rec.x = a_rec[PFT], 
               b_rec.x = b_rec[PFT], 
               seedpool.x = 1, 
               SMP.x = -50000)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
  )
  
  light_rec_data <- rbind(light_rec_data,temp)  
}


lt <- light_rec_data %>%
  filter(light == round(92)) %>%
  pull(annual_transition_rate)

lt[1]/lt[4]

light_rec_fig <- light_rec_data %>%
  #mutate(pft = case_when(
  #  pft == "earlydi" ~ "early",
  #  pft == "latedi" ~ "late"
  #)) %>%
  ggplot(aes(x = light, y = annual_transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition prob.") +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(atop("cum. solar rad. at seedling layer", paste("(MJ m"^"-2"," in prior 6 months)")))) +
  labs(title = "Seedling to sapling transition") +
  geom_vline(xintercept = 92, linetype = "dashed") +
  #annotate("text", x = 170, y = 0.15, label = paste0("avg_l = ", avg_l), size = 25) +
  annotate("text", x = 250, y = 0.1, label = "mean light \n at BCI seedling layer", size = 5) +
  geom_vline(xintercept = 920, linetype = "dashed") +
  annotate("text", x = 800, y = 0.032, label = "mean light \n in small gap \n (20% TOC)", size = 5) +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  multipanel_theme 


#makePNG(light_rec_fig, path_to_output.x = path_to_output, file_name = "transition_vs_light")




# 
# full_output %>%
#   mutate_at(.vars = "SMP",.funs = function(x){x*3}) %>%
#   select(day, SMP) %>%
#   write_csv(path = "temp/SMP_data_for_deficit_graph.csv")


SMP_data <- read_csv('temp/SMP_data_for_deficit_graph.csv')

moisture_def_fig <- ggplot(SMP_data, aes(x = day, y = SMP/1e5)) +
  labs(title = "") +
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
  annotate(geom = "text", x = 1575, y = -1.3, label = "DI \n threshold", size = 5) +
  annotate(geom = "text", x = 1575, y = -3.1, label = "DT \n threshold", size = 5) +
  annotate(geom = "text", x = 1485, y = -2.2, label = "deficit days \n grow \n here", size = 4) +
  multipanel_theme +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=5))


moisture_def_fig

#makePNG(fig = moisture_def_fig, path_to_output.x = path_to_output, file_name = "viz_def_days")



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
  labs(title = "Moisture-based \n seedling mortality") +
  # geom_vline(xintercept = mean_bci, linetype = "dotted") +
  # geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  multipanel_theme


#makePNG(fig = viz_H20_mort, path_to_output.x = path_to_output, file_name = "viz_H20_mort")


