str(full_output)

SMP_MPa_g <- ggplot(data = full_output, aes(x = as.Date(date), y = SMP/1e5)) +
  labs(title = "Soil Matric Potential (MPa)") +
  theme_classic()+
  geom_line(size = 1.8, color = "black")+
  #smooth_line +
  year_axis +
  ylab(expression(paste("SMP (MPa)")))+
  xlab(bquote('year'))+
  adams_theme +
  theme(legend.position = "none") +
  geom_hline(yintercept = psi_crit[1]/1e5, size = 1.8, color = "darkolivegreen2")+
  geom_hline(yintercept = psi_crit[2]/1e5, size = 1.8, color = "darkolivegreen4")#

#source('create_output/figures_for_MS/benchmark_figure_BCI_2008to2014.R')
SMP_frac_emerg <- full_output %>%
  select(yr:date,SMP,frac_emerging,seedbank) %>%
  mutate_at(.vars = "SMP",.funs = function(x){x/1e5}) %>%
  mutate(doy = strftime(date, format = "%j")) %>%
  group_by(doy) %>%
  summarise(n = length(SMP),
            SMP_med = median(SMP),
            fe_med = median(frac_emerging),
            SMP_sd = sd(SMP,na.rm = T)) %>%
  mutate(wetness = 1/-SMP_med) %>%
  mutate_at(.vars = "doy",.funs = as.numeric)

SMP_frac_emerg$fe_med %>% summary()

#soil moisture
ggplot(data = SMP_frac_emerg,
       mapping = aes(doy,wetness)) +
  smoother_line +
  geom_line(data = SMP_frac_emerg,mapping = aes(doy,fe_med*1905)) +
  ylab(expression(paste("soil mosture [1/-SMP (MPa)]"))) +
  xlab(bquote('day of year')) +
  adams_theme
  
#calculate SMP threshold for emergence
wetness <- SMP_frac_emerg %>%
  mutate(moisture_period = case_when(
    doy < 135 ~ "dry",
    doy %in% 135:165 ~ "trans",
    doy > 165 ~ "wet"
  )) %>%
  group_by(moisture_period) %>%
  summarise(med_wetness_per_period = min(wetness)) 

emerg_thres <- 1/-(wetness$med_wetness_per_period[3]) #MPa
print(paste("moisture emergence threshold is",emerg_thres*1e5,"mm H20 suction (which equals Mpa * 10^5)")) #mm H20 suction




