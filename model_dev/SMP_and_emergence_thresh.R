source('create_output/figures_for_MS/benchmark_figure_BCI_2008to2014.R')
source("create_output/figure_formatting.R")
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

#SMP_frac_emerg$fe_med %>% summary()


#calculate SMP threshold for emergence. 
#This is the minimum daily median soil moisture in the wet season at BCI (day 135 to 365)
#The wet season was determined visually by looking at inflection points in soil mositure throughout the year.
wetness <- SMP_frac_emerg %>%
  mutate(moisture_period = case_when(
    doy < 135 ~ "dry",
    doy %in% 135:165 ~ "trans",
    doy > 165 ~ "wet"
  )) %>%
  group_by(moisture_period) %>%
  summarise(med_wetness_per_period = min(wetness)) 

emerg_thres <- 1/-(wetness$med_wetness_per_period[3]) * 1e5 #mm H20 suction
print(paste("moisture emergence threshold is",emerg_thres,"mm H20 suction (which equals Mpa * 10^5)")) #mm H20 suction


#visualize soil moisture and emergence 
emergence_graph <- ggplot(data = SMP_frac_emerg,
       mapping = aes(doy,wetness)) +
  smooth_line +
  geom_line(data = SMP_frac_emerg,mapping = aes(doy,seeds_emerging/5.37)) +
  ylab(expression(paste("soil mosture [1/-SMP (MPa)]"))) +
  xlab(bquote('day of year')) +
  scale_y_continuous(limits = c(0,30))+
  #labs(title = paste("b_emerg:",b_emerg[c(1,3)])) +
  scale_y_continuous(sec.axis = sec_axis(~. *5.37, name = "emergence (g per day)")) +
  adams_theme +
  theme(axis.title.y = element_text(colour = "blue"),
        axis.title.y.right = element_text(colour = "black"))

makePNG(fig = emergence_graph, path_to_output.x = paste0(path_to_output,"model_dev_figs/"),file_name = "emergence_vs_moisture")


