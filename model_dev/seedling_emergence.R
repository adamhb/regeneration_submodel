source("utils/supporting_funcs.R")
source('model/process_funcs.R')

##################################################################################
#Calibration of a_emerg, b_emerg, and psi_emerg to observations of seedling emergence at BCI

#psi_emerg is the minimum soil matric potential (SMP) required for emergence. To capture observations that 
#seedlings do not emerge in the dry season at BCI (Garwood, 1983), we used the minimum wet season SMP simulated 
#by ED2-hydro under observed meterology (2008-2014) as the value for psi_emerg. 
#This stops emergence during BCI’s dry season.



#Step 1.
#a_emerg is derived from germination data (Pearson et al., 2020) 
#under non-limting light and moisture.
#The seedling emergence function is then used to find a value of a_emerg
#(Eqn 4) in Hanbury-Brown et al., in review.

germ_data <- read_csv(paste0(path_to_observations,"Pearson_et_al_2002_Fig2_data.csv")) 
daily_germ_rate <- germ_data %>% pull(germ) %>% mean() %>% `/` (100*60) #60 day germination trial

#solve for a_emerg using Eqn 4 with the mean wetness index at BCI (excluding the dry season)
#wetness index is 1 / (-1 * SMP) where SMP has units of MPa. mean wetness index at BCI (excluding dry season)
#is ~15
a_emerg_deriv <- round(daily_germ_rate / (15^1.2),4) 
print(paste("the value of a_emerg is" , a_emerg_deriv))


#Step 2. Calculate the median soil matric potential on each day of the year
source('runs/ED2_BASE.R')
SMP_frac_emerg <- full_output %>%
  select(yr:date,SMP,frac_emerging,seedbank) %>%
  mutate_at(.vars = "SMP",.funs = function(x){x/1e5}) %>%
  mutate(doy = strftime(date, format = "%j")) %>%
  group_by(doy) %>%
  summarise(n = length(SMP),
            SMP_med = median(SMP),
            fe_med = median(frac_emerging),
            seedbank_med = median(seedbank),
            SMP_sd = sd(SMP,na.rm = T)) %>%
  mutate(wetness = 1/-SMP_med) %>%
  mutate_at(.vars = "doy",.funs = as.numeric) %>%
  mutate(seeds_emerging = seedbank_med * fe_med)


#Step. 2b. Below we visualize seasonal soil moisture and emergence in the TRS using a range values for
#b_emerg.
#This is used to find parameters for b_emerg that roughly match the
#observations of seasonal seedling emergence by Garwood,1983

#Garwood NC. 1983. Seed Germination in a Seasonal Tropical Forest in Panama: A Community Study. Ecological 
#Monographs 53: 159–181.


#b_emerg is calibrated to Garwood's observations such that the timing of seasonal
#seedling emergence matches her observations. See Garwood (1983) Fig. 7. 

#values of b_emerg = 1.2 / 1.6 (shade tolerant / light demanding respectively)
#produces the below graph that roughly matches observations by Garwood

seasonal_emergence_graph <- ggplot(data = SMP_frac_emerg,
                                   mapping = aes(doy,wetness)) +
  smooth_line +
  geom_line(data = SMP_frac_emerg,mapping = aes(doy,seeds_emerging/5.37)) +
  ylab(expression(paste("soil mosture [1/-SMP (MPa)]"))) +
  xlab(bquote('day of year')) +
  scale_y_continuous(limits = c(0,30))+
  #labs(title = paste("b_emerg:",b_emerg[c(1,3)])) +
  scale_y_continuous(sec.axis = sec_axis(~. *5.37, name = "emergence [g per day ha-1]")) +
  adams_theme +
  theme(axis.title.y = element_text(colour = "blue"),
        axis.text.y = element_text(colour = "blue"),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.right = element_text(colour = "black"))

print("made seasonal_emergence_graph")

makePNG(fig = seasonal_emergence_graph, path_to_output.x = paste0(path_to_output,"model_dev_figs/"),
        file_name = "seasonal_emergence_graph")

#Step. 2c Determine the wet season by looking at inflection points in soil mositure throughout the year.
#using the seasonal_emergence_graph above

#Step 3. Calculate the minimum soil moisture in the wet season
#and use this value for psi_emerg
wetness <- SMP_frac_emerg %>%
  mutate(moisture_period = case_when(
    doy < 135 ~ "dry",
    doy %in% 135:165 ~ "trans",
    doy > 165 ~ "wet"
  )) %>%
  group_by(moisture_period) %>%
  summarise(min_wetness_per_period = min(wetness),
            mean_wetness_per_period = mean(wetness)) 

emerg_thresh <- 1/-(wetness$min_wetness_per_period[3]) * 1e5 #mm H20 suction

print(paste("psi_emerg is",emerg_thresh,"mm H20 suction (which equals MPa * 10^5)")) #mm H20 suction


#######################################
#######visualize emergence#############
#######################################
emerg_data <- tibble()
SMPrange <- read_csv(file = 'temp/SMPrange.csv')
SMPrange <- SMPrange$SMPrange

for(p in pft_names){
  
  PFT <- p
  tmp <- tibble(SMP = SMPrange) %>%
    mutate(pft = p)
  
  emergs <- c()
  for(i in 1:length(SMPrange)){
    
    emerg_rate_i <- emerg_func(a = a_emerg[PFT], 
                               b = b_emerg[PFT], 
                               SMP.2.to.0.wks.ago = tmp$SMP[i],
                               #SMP.4.to.2.wks.ago = tmp$SMP2,
                               seedbank.x = 1,
                               light.xx = 16.7)$frac_emerg
    emergs <- append(emergs,emerg_rate_i)
  }
  tmp <- tmp %>% mutate(emerg = emergs)
  emerg_data <- rbind(emerg_data,tmp)
}


#create figure
emerg_vs_SMP_fig <- emerg_data %>%
  drop_na() %>%
  #mutate(delta = (abs(SMP2) - abs(SMP1)) / abs(SMP2) ) %>%
  ggplot(aes(x = SMP/1e5, y = emerg, color = pft, linetype = pft)) +
  geom_line(size = 2) +
  #annotate(geom = "text", x = -0.5, y = 0.05, label = "b", size = subplot_heading_size) +
  scale_x_continuous(limits = c(-0.25,0)) +
  scale_y_continuous(limits = c(0,0.25)) +
  scale_color_manual(values = pft.cols) +
  scale_linetype_manual(values=c("solid", "dashed","solid","dashed"))+
  ylab(label = "daily fraction of \n seedbank emerging") +
  xlab(label = "mean soil matric potential \n [MPa] in prior two weeks") +
  labs(title = "Moisture & \n seedling emergence") +
  theme_minimal() +
  multipanel_theme
emerg_vs_SMP_fig

print("made emerg_vs_SMP_fig")
