#source('runs/ED2_BASE.R')


source("create_output/figure_formatting.R")
#visualize efrac
path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"
source2('parameter_files/bci_params_default_ED2.R',start = 1,end = 80)
source2('model/regeneration_submodel.R',start = 9,end = 221) #source the model functions

#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100

multipanel_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                          strip.text.x = element_text(size = 18),
                          legend.title = element_blank (),
                          axis.title.x = element_text (size = 14), # change the axis title
                          axis.title.y = element_text (size = 14),
                          axis.title.y.right = element_text (size = 14, color = pft.cols[2]),
                          axis.text.x = element_text (size = 12, colour = "black"),
                          axis.text.y = element_text (size = 12, colour = "black"),
                          legend.position = "none")

subplot_heading_size <- 12
###############################################
#############        repro alloc        #######
###############################################

sizes <- 1:1300 
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
  scale_linetype_manual(values=c("solid", "solid","solid","solid")) +
  ylab(expression(atop(paste("fraction of ",C[g+r]),"allocated to reproduction"))) +
  #annotate(geom = "text", x = 0, y = 0.1, label = "a", size = subplot_heading_size) +
  labs(title = "Allocation to reproduction") +
  theme_minimal() +
  multipanel_theme +
        #legend.key.size = unit(2,"line")) +
  theme(legend.position = c(0.2,0.8), legend.text = element_text(size = 20), legend.title = element_text(size = 20)) +
  guides(color = guide_legend(override.aes = list(size=8)))
         
#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100
#makePNG(fig = F_repro_fig, path_to_output.x = path_to_output, file_name = "F_repro_fig")



##########################
########emergence#########
##########################
source2(file = "runs/ED2_BASE.R",start = 5,end = 45)

SMPv <- full_output %>%
  filter(pft == "ST_DT") %>%
  pull(SMP)
length(SMPv)

smp4.2 <- c()
smp2.0 <- c()
for(i in 28:length(SMPv)){
  smp4.2 <- append(smp4.2,mean(SMPv[(i-27):(i-14)]))
  smp2.0 <- append(smp2.0,mean(SMPv[(i-13):i]))
}

smpDF <- tibble(SMP = SMPv[28:7065],
       smp4.2 = smp4.2,
       smp2.0 = smp2.0,
       day = 28:7065) %>%
  mutate(delta = abs(smp4.2)/abs(smp2.0))


day <- c()
SMPf <- c()
smp4.2 <- c()
smp2.0 <- c()
deltaf <- c()
emerg <- c()
pft <- c()

for(p in pft_names){
  for(i in 1:nrow(smpDF)){
    day <- append(day,smpDF$day[i])
    SMPf <- append(SMPf,smpDF$SMP[i])
    smp4.2 <- append(smp4.2,smpDF$smp4.2[i])
    smp2.0 <- append(smp2.0,smpDF$smp2.0[i])
    deltaf <- append(deltaf,smpDF$delta[i])
    PFT <- p
    tmp <- emerg_func(SMP.2.to.0.wks.ago = smpDF$smp2.0[i],
                      SMP.4.to.2.wks.ago = smpDF$smp4.2[i],
                      seedbank.x = 1)$frac_emerg
    emerg <- append(emerg, tmp) 
    pft <- append(pft, p) 
    #print(paste(i/nrow(smpDF) * 100,"% done"))
  }
}

PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100

multipanel_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                                            strip.text.x = element_text(size = 18),
                                            legend.title = element_blank (),
                                            axis.title.x = element_text (size = 14), # change the axis title
                                            axis.title.y = element_text (size = 14),
                                            axis.title.y.right = element_text (size = 14, color = pft.cols[2]),
                                            axis.text.x = element_text (size = 12, colour = "black"),
                                            axis.text.y = element_text (size = 12, colour = "black"),
                                            legend.position = "none")


viz_emerg <- tibble(day = day,
       SMP = SMPf,
       delta = deltaf,
       smp4.2 = smp4.2,
       smp2.0 = smp2.0,
       pft = pft,
       emerg = emerg) %>%
  mutate(pct_increase = (abs(smp4.2) - abs(smp2.0)) / abs(smp4.2)) %>%
  #filter(pft == "LD_DI") %>%
  ggplot(aes(x = pct_increase, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  #annotate(geom = "text", x = -0.5, y = 0.05, label = "b", size = subplot_heading_size) +
  scale_x_continuous(limits = c(-0.5,1)) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "fraction of \n seedbank emerging") +
  xlab(label = "% change in SMP \n in prior month") +
  labs(title = "Seedling emergence") +
  theme_minimal() +
  multipanel_theme

#path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"
#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100
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
  xlab(expression(paste("cum. light in prior 3 mo. [MJ m"^"-2","]"))) +
  #annotate(geom = "text", x = 1550, y = 0.3, label = "c", size = subplot_heading_size) +
  scale_x_continuous(limits = c(0,1700)) +
  labs(title = "Light-based seedling mortality") +
  geom_vline(xintercept = mean_bci, linetype = "dotted") +
  geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  annotate(geom = "text", x = 700, y = 0.15, label = "BCI mean \n seedling layer", size = 5) +
  geom_segment(aes(x = 350, y = 0.12, xend = 70, yend = 0.05),
                  arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  theme_minimal() +
  multipanel_theme


#makePNG(fig = viz_light_mort, path_to_output.x = path_to_output, file_name = "viz_light_mort")





#################
#transition rate#
#################

light_regimes <- 20:1000
light_rec_data <- tibble()

#for(i in pft_names[c(1,3)]){
for(i in pft_names){
  PFT <- i
  temp <- tibble(
    transition_rate =
      rec_func(l = light_regimes,
               a_TR.x = a_TR[PFT],
               b_TR.x = b_TR[PFT],
               SMP.x = 0,
               seedpool.x = 1)$frac_rec,
    pft = rep(i,length(light_regimes)),
    light = light_regimes
  )
  
  light_rec_data <- rbind(light_rec_data,temp)  
}

lt <- light_rec_data %>%
  filter(light == round(92)) %>%
  pull(transition_rate)

light_rec_fig <- light_rec_data %>%
  #mutate(pft = case_when(
  #  pft == "earlydi" ~ "early",
  #  pft == "latedi" ~ "late"
  #)) %>%
  ggplot(aes(x = light, y = transition_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values = c("solid","dashed","solid","dashed")) +
  ylab(label = "monthly seedling to \n sapling transition") +
  #xlab(label = "cumulative solar radiation at seedling layer \n (MJ accumulated in prior 6 months)") +
  xlab(expression(paste("cum. light in prior 6 mo. [MJ m"^"-2","]"))) +
  labs(title = "Recruitment") +
  #annotate(geom = "text", x = 0, y = 0.77, label = "f", size = subplot_heading_size) +
  geom_vline(xintercept = 92, linetype = "dotted") +
  #annotate("text", x = 170, y = 0.15, label = paste0("avg_l = ", avg_l), size = 25) +
  annotate("text", x = 250, y = 0.7, label = "BCI mean \n seedling layer", size = 5) +
  geom_vline(xintercept = 920, linetype = "dotted") +
  annotate("text", x = 850, y = 0.4, label = "BCI mean \n small gap \n (20% TOC)", size = 5) +
  #geom_vline(xintercept = 61) +
  #annotate("text", x = 61, y = 0.06, label = "original avg_l value") +
  theme_minimal() +
  multipanel_theme 


#makePNG(light_rec_fig, path_to_output.x = path_to_output, file_name = "transition_vs_light")


###############################################
###########moisture-based seedling mortality###
###############################################

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
  geom_hline(yintercept = psi_crit[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  #annotate(geom = "text", x = 1500, y = -0.5, label = "e", size = subplot_heading_size) +
  geom_hline(yintercept = psi_crit[2]/1e5, size = 1.8, color = "darkolivegreen4")+
  annotate(geom = "text", x = 1575, y = psi_crit[2]/1e5, label = "DI \n threshold", size = 5) + 
  annotate(geom = "text", x = 1575, y = psi_crit[1]/1e5, label = "DT \n threshold", size = 5) +
  annotate(geom = "text", x = 1485, y = -2.2, label = "deficit days \n grow \n here", size = 4) +
  multipanel_theme +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=5))

#makePNG(fig = moisture_def_fig, path_to_output.x = path_to_output, file_name = "viz_def_days")

source2("runs/ED2_ENSO.R",start = 18,end = 42)
source2("model/regeneration_submodel.R",start = 225,end = 228)

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


H20_mort_data <- tibble(pft = pft,
       water_def = rep(water_defs,4),
       H20_mort_rate = H20_mort_rates)


path_to_output <- "~/cloud/gdrive/rec_submodel/output/forMS/"
#png options
PNGheight = 11/3
PNGwidth = 4.5 #usually 8
PNGunits = "in"
PNGres = 100

multipanel_theme <- theme_minimal() + theme(plot.title = element_text(hjust = 0.5, size = 18),
                                            strip.text.x = element_text(size = 18),
                                            legend.title = element_blank (),
                                            axis.title.x = element_text (size = 14), # change the axis title
                                            axis.title.y = element_text (size = 14),
                                            axis.title.y.right = element_text (size = 14, color = pft.cols[2]),
                                            axis.text.x = element_text (size = 12, colour = "black"),
                                            axis.text.y = element_text (size = 12, colour = "black"),
                                            legend.position = "none")


viz_H20_mort <- H20_mort_data %>%
  ggplot(aes(x = water_def / 1e5, y = H20_mort_rate * 30.4, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  #annotate(geom = "text", x = 0, y = 0.035, label = "d", size = subplot_heading_size) +
  scale_linetype_manual(values=c("solid", "solid","dashed","dashed"))+
  ylab(label = "monthly moisture-based \n seedling mortality rate") +
  xlab(label = "moisture deficit days [Mpa days]") +
  labs(title = "Moisture-based \n seedling mortality") +
  theme_minimal() +
  # geom_vline(xintercept = mean_bci, linetype = "dotted") +
  # geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  multipanel_theme


#makePNG(fig = viz_H20_mort, path_to_output.x = path_to_output, file_name = "viz_H20_mort")

viz_funcs <- plot_grid(F_repro_fig,viz_emerg,viz_light_mort,
          viz_H20_mort, moisture_def_fig, light_rec_fig, nrow = 2,labels = "auto")

makePNG(fig = viz_funcs, path_to_output.x = path_to_output, file_name = "viz_funcs",height = 11/1.5,width = 4.5*3)


