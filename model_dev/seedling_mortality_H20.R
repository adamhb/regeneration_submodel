
#The functional relationship between soil matric potential (MPa) and seedling mortality rates is derived in this script 
#from Engelbrecht and Kursar (2003).

#Parameters for seedling drought stress mortality are derived in this script 


#Load supporting functions
source("utils/supporting_funcs.R")
#load the regeneration process functions
source("model/process_funcs.R")
#Run the script that assigns species to plant functional types
source("benchmarking/assigning_pfts.R")

#Load seedling mortality observations from Engelbrecht and Kursar's manipulative drought experiment.
#These observations have been tabulated by Adam Hanbury-Brown using Figures in Engelbrecht and Kursar (2003),
#Engelbrecht et al., 2005, and Engelbrecht et al., 2007
engelbrecht_mort_data <- read_csv(paste0(path_to_observational_data,"engelbrecht_mort_data.csv"))
seedling_mort_over_time <- read_csv(paste0(path_to_observational_data,"engelbrecht_seedling_mort_over_time.csv"),
                                    col_names = c("week","pct_indvls_alive","engelbrecht_id","treatment","Latin","N_start"))
moisture_stress_points <- read_csv(paste0(path_to_observational_data,"engelbrecht_wilt_50.csv"))
eng_start_date <- dmy("18-12-2000")

#Engelbrecht BMJ, Kursar TA, Tyree MT. 2005. Drought effects on seedling survival in a tropical moist forest. Tree: 312–321.
#Engelbrecht BMJ, Comita LS, Condit R, Kursar T, Tyree MT, Turner BL, Hubbell SP. 2007. Drought sensitivity shapes species distribut
#ion patterns in tropical forests. Nature 447: 80–82.
#Engelbrecht, B. M. J., & Kursar, T. A. (2003). 
#Comparative drought-resistance of seedlings of 28 species of co-occurring tropical woody plants.Oecologia, 136(3), 383–393. https://doi.org/10.1007/s00442-003-1290-8


#determining psi_crit for the species in the drought experiment
moisture_stress_points_pft <- moisture_stress_points %>%
  select(-pft) %>%
  left_join(d_indices, by = "Latin") %>%
  mutate(pft = case_when(
    d_index < 14.2 ~ "DT",
    d_index > 33.35 ~ "DI"
  )) %>%
  drop_na() %>%
  group_by(pft) %>%
  summarise(mean_wilt_50_week = mean(week_50_slight_wilt), n = length(week_50_slight_wilt)) %>%
  mutate(day = round(as.numeric((eng_start_date + mean_wilt_50_week * 7) - eng_start_date)))
  

#Step 1. Pedotransfer function based on data provided in Engelbrecht et al. 2003
PTF_func <- function(sgwc){ #input: soil gravimetric water content
  
  sgwc_T  <- c(0.78, 0.47, 0.34)
  matric_T <- c(0, -0.5, -3.5)
  d <- data.frame(sgwc_T = sgwc_T, matric_T = matric_T)
  #plot(sgwc_T,matric_T)
  
  sgwc_T2 <- sgwc_T^2
  PTF <- lm(matric_T ~ sgwc_T + sgwc_T2)
  
  matric <- coef(PTF)[3]*sgwc^2 + coef(PTF)[2]*sgwc + coef(PTF)[1]
  return(matric) #returns matric potential
}


#Step 2. Replicating the time series data for soil gravimetric water content
weeks <- c(4,8,10,12,14,16,18,20,22)
sgwc <- c(0.48, 0.41,0.39,0.35,0.37,0.34,0.33,0.33,0.38)
#plot(weeks, sgwc)

#Step 3. Converting the soil moisture time series data to a matric potential (mm water suction)
matric <- PTF_func(sgwc = sgwc) * 1e5 #converting Megapascals (Mpa) to mm of H20 suction
#plot(weeks, matric)

#creating a dataframe to store the moisture and (later) mortality data
ts_wk <- data.frame(week = weeks, matric = matric)

#adding the day of the experiment
ts_wk$day <- ts_wk$week*7

#determing matric potential on each day
matric_days <- predict(loess(matric~weeks), newdata = seq(from = 4, to = 22, length = 127))
days <- 1:127 + 27
ts_days <- data.frame(day = days, matric = matric_days)

#determining the matric potential at psi_crit
pft_names <- c("LD_DI", "LD_DT", "ST_DI", "ST_DT")
psi_crit <- ts_days %>% filter(day %in% c(moisture_stress_points_pft$day)) %>% pull(matric) %>% rep(.,2)
names(psi_crit) <- pft_names
print("psi crit:")
print(psi_crit)

#plotting matric potential during the experiment
plot(days, matric_days / 1e5, ylab = "soil matric potential (Mpa)", 
     main = "Soil moisture observations \n Engelbrecht drought experiment")


#calculating moisture deficit days for the 22 week period of the experiment
DDs <- tibble(DDs = append(def_func(soil_moist = matric_days, psi_crit.x = psi_crit["LD_DT"], window = 18*7),
                           def_func(soil_moist = matric_days, psi_crit.x = psi_crit["LD_DI"], window = 18*7)),
              pft = c(rep("DT",127),rep("DI",127)),
              day = rep(1:127,2))

pfts_nov_2018 <- pfts_nov_2018 %>% mutate_at(.vars = "Latin", .funs = as.character)
#pfts_nov_2018 <- read_csv(file = "benchmarking/pft_assignments.csv")

# missing_sp <- seedling_mort_over_time %>%
#   left_join(pfts_nov_2018, by = "Latin") %>% 
#   filter(is.na(pft) == T) %>%
#   pull(Latin) %>% unique()

#calculating daily seedling mortality rates over Engelbrecht's manipulative drought experiment
seedling_mort_over_time1 <- seedling_mort_over_time %>%
  left_join(pfts_nov_2018, by = "Latin") %>%
  mutate(pft=ifelse(Latin=="Pouteria unilocularis","ST_DT",pft)) %>%
  filter(Latin != "Hymenaea courbaril") %>%
  mutate(pft = case_when(
    pft %in% c("ST_DI", "LD_DI") ~ "DI",
    pft %in% c("ST_DT", "LD_DT") ~ "DT"
  )) %>%
  select(-sp) %>%
  mutate_at(.vars = "week", .funs = round) %>%
  spread(key = treatment, value = pct_indvls_alive) %>%
  mutate(Ndry = round(N_start * (dry/100)), Nwet = round(N_start * (wet/100))) %>%
  arrange(Latin,week) %>%
  mutate(int_name = paste0(lag(week),"_",week)) %>%
  mutate(int.length = (week - lag(week)) * 7) %>%
  mutate(N.dead.dry = lag(Ndry) - Ndry,
         N.dead.wet = lag(Nwet) - Nwet) %>%
  mutate(mort.rate.dry = N.dead.dry / lag(Ndry),
         mort.rate.wet = N.dead.dry / lag(Nwet)) %>%
  mutate(mort.rate.drought = mort.rate.dry - mort.rate.wet) %>%
  drop_na() %>%
  mutate(M_daily_sp = mort.rate.drought / int.length) %>%
  group_by(pft,int_name) %>%
  summarise(M_daily = median(M_daily_sp),
            mean_N = mean(Ndry)) %>%
  filter(M_daily >= 0, int_name != "22_0")


#assigning each of Engelbrecht's species to PFTs
seedling_mort_over_time %>%
  left_join(pfts_nov_2018, by = "Latin") %>%
  mutate(pft=ifelse(Latin=="Pouteria unilocularis","ST_DT",pft)) %>%
  filter(Latin != "Hymenaea courbaril") %>%
  mutate(pft = case_when(
    pft %in% c("ST_DI", "LD_DI") ~ "DI",
    pft %in% c("ST_DT", "LD_DT") ~ "DT"
  )) %>% select(engelbrecht_id, pft) %>% distinct()


#getting start and end week for each interval of the manipulative drought experiment
x <- str_split(seedling_mort_over_time1$int_name,pattern = "_")
starts <- unlist(lapply(x, `[[`, 1))
ends <- unlist(lapply(x, `[[`, 2))
eng_start_date <- dmy("18-12-2000")


#cleaning the data for statistical analysis 
seedling_mort_over_time2 <- seedling_mort_over_time1 %>%
  ungroup() %>%
  mutate(start.week = as.numeric(starts), end.week = as.numeric(ends)) %>%
  mutate(start_date = eng_start_date + start.week * 7,
         end_date = eng_start_date + end.week * 7) %>%
  mutate(st_day = as.numeric(start_date - eng_start_date),
         end_day = as.numeric(end_date - eng_start_date)) %>%
  arrange(start_date) %>%
  rename(day = end_day) %>%
  left_join(DDs, by = c("pft","day")) %>%
  drop_na()


#graph showing moisture deficit days and mortality over time
mortANDDD_overTime <- seedling_mort_over_time2 %>%
  ggplot(aes(day,DDs,color = pft)) +
  geom_point(size = 5) +
  geom_line(mapping = aes(day, M_daily * 2631856462)) + #scaling factor to get mortality and moisture deficit days on the same graph
  geom_point(mapping = aes(day, M_daily * 2631856462)) +
  scale_y_continuous(
        "deficit days",
        sec.axis = sec_axis(~ . / 2631856462, name = "daily drought-induced mortality rate")
      ) +
  adams_theme

#dividing the data into drought tolerant and drought intolerant PFTs
DI_mort_data <- seedling_mort_over_time2 %>%
  filter(pft == "DI",
         day < 120) 
DT_mort_data <- seedling_mort_over_time2 %>%
  filter(pft == "DT",
         day < 120) %>%
  mutate_at(.vars = c("M_daily","DDs"), .funs = function(x){x+0.00000000001})


#fitting quadratic statistical models that predict seedling mortality as a function of moisture deficit days
DT_mod_Quad <- lm(data = DT_mort_data, formula = M_daily ~ DDs + I(DDs^2))
summary(DT_mod_Quad)

pred_data <- tibble(DDs = seq(0,1.1e7,length.out = 100))
pred_data$quad <- predict(DT_mod_Quad, newdata = pred_data)

DI_mort_data <- seedling_mort_over_time2 %>%
  filter(pft == "DI",
         day < 120) 


DI_mod_Quad <- lm(data = DI_mort_data, formula = M_daily ~ DDs + I(DDs^2))
summary(DT_mod_Quad)

pred_data_DI <- tibble(DDs = seq(0,1.1e7,length.out = 100))

pred_data_DI$quad <- predict(DI_mod_Quad, newdata = pred_data_DI)



#plotting seedling mortality predictions with observations
#drought tolerant PFT
DT_MDDs_vs_M <- DT_mort_data %>%
  ggplot(aes(DDs * 1e-5,M_daily* 1e3)) +
  geom_point(shape = 2, size = 3) +
  #geom_line(data = pred_data, mapping = aes(DDs,logLin)) +
  geom_line(data = pred_data %>% filter(DDs > 1e6), 
            mapping = aes(DDs * 1e-5,quad * 1e3), linetype = "dashed") +
  scale_y_continuous(limits = c(0,1.3)) +
  #geom_vline(xintercept = 1.4, linetype = "dotted") +
  xlab(label = "Moisture deficit days [MPa days]") +
  ylab(label = expression(paste("daily mortality rate [x 10"^"-3","]"))) +
  adams_theme +
  theme(legend.position = c(0.25,0.75))

#makePNG(fig = DT_MDDs_vs_M, file_name = "DT_MDDs_vs_Mort", path_to_output.x = paste0(path_to_output,"forMS/"),height = 4, width = 6, units = "in",res = 600)

#drought intolerant pft
DI_MDDs_vs_M <- DI_mort_data %>%
  ggplot(aes(DDs * 1e-5,M_daily * 1e3)) +
  geom_point(shape = 2, size = 3) +
  #geom_line(data = pred_data, mapping = aes(DDs,logLin)) +
  geom_line(data = pred_data_DI %>% filter(DDs > 1e6), 
            mapping = aes(DDs * 1e-5,quad * 1e3), linetype = "dashed") +
  scale_y_continuous(limits = c(0,13)) +
  #geom_vline(xintercept = 1.4, linetype = "dotted") +
  xlab(label = "Moisture deficit days [MPa days]") +
  ylab(label = expression(paste("Daily mortality rate [x 10"^"-3","]"))) +
  adams_theme +
  theme(legend.position = c(0.25,0.75))
  # theme(legend.position = c(0.25,0.75)) +
  # guides(shape = "legend")

MDDsVsM <- cowplot::plot_grid(DI_MDDs_vs_M, DT_MDDs_vs_M, labels = c("(a)","(b)"))
#makePNG(fig =MDDsVsM, file_name = "MDDsVsM", path_to_output.x = paste0(path_to_output,"forMS/"),height = 5, width = 10, units = "in",res = 600)
#makePNG(fig = DI_MDDs_vs_M, file_name = "DI_MDDs_vs_Mort", path_to_output.x = paste0(path_to_output,"forMS/"),height = 4, width = 6, units = "in",res = 600)

#parameters for the H20 mort function
a.H20 <- rep(c( coef(DI_mod_Quad)[3], coef(DT_mod_Quad)[3] ), 2)
b.H20 <- rep(c( coef(DI_mod_Quad)[2], coef(DT_mod_Quad)[2] ), 2)
c.H20 <- rep(c( coef(DI_mod_Quad)[1], coef(DT_mod_Quad)[1] ), 2)
names(a.H20) <- pft_names
names(b.H20) <- pft_names
names(c.H20) <- pft_names

print("a.H20:")
print(a.H20)
print("b.H20:")
print(b.H20)
print("c.H20:")
print(c.H20)


MDDs_crit <- rep(c(4.6e6,1.4e6),2) #this parameter is derived from visualizing the quadratic relationship
#where mortality is 0 or < 0 up until a certain point.
names(MDDs_crit) <- pft_names
print("MDDs_crit:")
print(MDDs_crit) #in units of m of H20 suction


##########################################################
##visualizing the concept of moisture deficit days########
##########################################################

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


#generating data to visualize the relationship between moisture deficit days and seedling mortality

#create the pft col for the predictions data
pft_col <- c()
for(p in c("LD_DT","ST_DT","LD_DI","ST_DI")){
  tmp <- rep(p,nrow(pred_data))
  pft_col <- append(pft_col,tmp)
}

data_for_H20_mort_fig <- pred_data %>%
  rbind(pred_data) %>%
  rbind(pred_data_DI) %>%
  rbind(pred_data_DI) %>%
  mutate(pft = pft_col) %>%
  mutate(M = case_when(
    DDs < 1e6 | quad < 0 ~ 0,
    DDs >= 1e6 & quad >= 0 ~ quad
  )) %>%
  mutate(pft = factor(pft,levels = pft_names))


Fig_seedling_mort_H20 <- data_for_H20_mort_fig %>%
  ggplot(aes(x = DDs / 1e5, y = M, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  scale_color_manual(values = pft.cols) +
  #annotate(geom = "text", x = 0, y = 0.035, label = "d", size = subplot_heading_size) +
  scale_linetype_manual(values=c("solid", "solid","dashed","dashed"))+
  ylab(label = "Daily moisture stress \n seedling mortality rate") +
  xlab(label = "Moisture deficit days [MPa days]") +
  labs(title = "Moisture & \n seedling mortality") +
  theme_minimal() +
  # geom_vline(xintercept = mean_bci, linetype = "dotted") +
  # geom_vline(xintercept = mean_bci_TOC, linetype = "dotted") +
  # annotate(geom = "text", x = mean_bci + 400, y = 0.09, label = "BCI mean \n at smallest cohort", size = 5) +
  # annotate(geom = "text", x = mean_bci_TOC, y = 0.09, label = "BCI mean \n TOC", size = 5) +
  multipanel_theme

print("made Fig_seedling_mort_H20")
