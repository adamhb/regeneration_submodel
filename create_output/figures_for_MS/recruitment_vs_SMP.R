source("utils/supporting_funcs.R")
from_new_data <- T
print("creating recruitment vs. SMP figure...")


if(file.exists('temp/SMP_summary_data.csv') == F | from_new_data == T){
  print("need to run ED2_run_soil_moisture_demo.R")
  source("runs/ED2_run_soil_moisture_demo.R")
}else(print("making figure with prior run's data"))

#import benchmarking data
bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")

source('benchmarking/bci_soil_moisture_obs.R')
source("create_output/figure_formatting.R")

# bench4graph <- bench %>%
#   filter(date > start_date,
#          date < end_date) %>%
#   group_by(pft) %>%
#   summarise(R_bench = mean(rec_rate)) %>%
#   mutate(SMP = Lutz.smp) %>%
#   mutate(model = "BCI obs.")

bench4graph <- bench %>%
  mutate(year = substring(text = as.character(date), first = 1, last = 4)) %>%
  filter(date > start_date,
         date < end_date) %>%
  group_by(pft,int) %>%
  summarise(BCI_obs = mean(rec_rate),
            start_dateB = min(date),
            end_dateB = max(date),
            date = mean(date)) %>%
  mutate(model = "BCI obs.") %>%
  filter(date > as.Date(as.numeric(as.Date(start_date)) + 365*3, origin = "1970-01-01")) %>%
  group_by(pft) %>% summarise(R = mean(BCI_obs), se_R = sd(BCI_obs)/sqrt(length(BCI_obs))) %>%
  mutate(SMP = Lutz.smp) %>%
  mutate(model = "BCI obs.") 




summary_data <- read_csv('temp/SMP_summary_data.csv')
summary_data %>%
  filter(pft %in% c("ST_DT","LD_DT") )


se_df <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, SMP_avg) %>%
  rename(submodel = R_sd, ED2 = R_sd_ED2) %>%  
  gather(submodel:ED2, key = "model", value = "sd") 

pd <- position_dodge(0.003)
psize <- 5
axis_size <- 20
title_size <- 25



Rvsmp_TRS <- summary_data %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  left_join(se_df, by = c("model","SMP_avg","pft")) %>%
  filter(model != "ED2") %>%
  mutate(model = case_when(
    model == "submodel" ~ "TRS",
    FALSE ~ model
  )) %>%
  ggplot(mapping = aes(x = SMP_avg/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = psize, stroke = 1) +
  geom_point(data = bench4graph, mapping = aes(x = SMP/1e5, y = R, color = pft), size = psize, stroke = 2) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, show.legend = F) +
  scale_shape_manual(values = c(10,24,21)) +
  scale_color_manual(values = pft.cols) +
  scale_x_continuous(limits = c(-2.5,0)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = lseq(from = 1,to = 280,length.out = 7), breaks = lseq(from = 1,to = 300,length.out = 7)) + 
  ylab(expression(paste('N recruits [ha'^'-1','yr'^'-1',"]"))) +
  #ylab(expression(paste('N recruits ha'^'-1','yr'^'-1'))) + # indv. ha"^"-1", "yr"^"-1", ")"))) +
  #xlab(paste0("20-yr mean patch-level \n soil matric potential (MPa)")) +
  adams_theme +
  #theme(legend.position = c(0.2,0.65)) +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  guides(color = guide_legend(override.aes = list(shape = 15)))


Rvsmp_ED2 <- summary_data %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  left_join(se_df, by = c("model","SMP_avg","pft")) %>%
  filter(model != "submodel") %>%
  ggplot(mapping = aes(x = SMP_avg/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = psize, stroke = 1) +
  geom_point(data = bench4graph, mapping = aes(x = SMP/1e5, y = R, color = pft), size = psize, stroke = 2) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, show.legend = F) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_color_manual(values = pft.cols) +
  scale_x_continuous(limits = c(-2.5,0)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), 
                     labels = lseq(from = 1,to = 250,length.out = 7), 
                     breaks = lseq(from = 1,to = 300,length.out = 7)) + 
  ylab(expression(paste('N recruits ha'^'-1','yr'^'-1'))) + # indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab(paste0("20-yr mean patch-level \n soil matric potential [MPa]")) +
  adams_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  #theme(legend.position = c(0.2,0.65)) +
  guides(color = guide_legend(override.aes = list(shape = 15)))

rvssmp <- plot_grid(Rvsmp_TRS, Rvsmp_ED2,
                         rel_widths = c(2,2), labels = c("(c)","(d)"), label_x = -0.02, label_size = 24)

rvssmp
rvssmp2 <- ggdraw(add_sub(rvssmp, "20-yr mean patch-level \n soil matric potential [MPa]",
                          vpadding=grid::unit(1,"lines"),
                          size = axis_size ))

PNGwidth <- 9
makePNG(fig = rvssmp2, path_to_output.x = paste0(path_to_output,"forMS/"), file_name = "rec_vs_smp")



#simple statistics on summary data
DI_end <- summary_data %>%
  filter(R_avg != 0, pft %in% c("LD_DI","ST_DI"), patch == 5) %>% pull(R_avg) %>% mean()

DI_start <- summary_data %>%
  filter(R_avg != 0, pft %in% c("LD_DI","ST_DI"), patch == 1) %>% pull(R_avg) %>% mean()

DT_end <- summary_data %>%
  filter(R_avg != 0, pft %in% c("LD_DT","ST_DT"), patch == 5) %>% pull(R_avg) %>% mean()

DT_start <- summary_data %>%
  filter(R_avg != 0, pft %in% c("LD_DT","ST_DT"), patch == 1) %>% pull(R_avg) %>% mean()


DI_diff <- (DI_end - DI_start) / DI_start
DT_diff <- (DT_end - DT_start) / DT_start
# makePNG(fig = rec_vs_smp, 
#         path_to_output.x = path_to_MS_figures, 
#         file_name = "rec_vs_SMP")


# rec_vs_smp_full_axis <- soil_moisture_data %>%
#   ggplot(mapping = aes(x = soil_moisture/1e5, y = R * 365, color = pft, shape = model)) +
#   geom_point(size = 2.5, stroke = 1, alpha = 1) +
#   adams_theme +
#   scale_shape_manual(values = rep(c(21,24),2)) +
#   scale_color_manual(values = pft.cols) +
#   #scale_y_log10() +
#   #scale_y_continuous(limits = c(0,50)) +
#   ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
#   xlab(paste0("soil matric potential (avg. over prior ",soil_moisture_period," days")) +
#   #scale_y_continuous(limits = c(0,20))
#   labs(title = paste0(start_date,"--",end_date,"\n",
#                     "driver: ",basename(driver_data_path),"\n",
#                     "light (%): ", percent_light * 100,"\n",
#                     "run name:" , run_name))
# 
# makePNG(fig = rec_vs_smp_full_axis, path_to_output.x = path_to_this_run_output, file_name = paste0("02_rec_vs_SMP_full_axis_",basename(driver_data_path)))
# 
# 
# 
# SMP_b <- input_data %>%
#   filter(pft == "earlydi") %>%
#   ggplot(aes(x = date, y = SMP / 1e5)) +
#   geom_line() +
#   ylab(label = "SMP (MPa)")
# 
# makePNG(fig = SMP_b, path_to_output.x = path_to_this_run_output, file_name = paste0("01_SMP",basename(driver_data_path)))
# 
# 
# # 
# 
# rec_vs_smp_normalized <- soil_moisture_data %>%
#   ggplot(mapping = aes(x = soil_moisture/1e5, y = R * 365, color = pft, shape = model)) +
#   geom_point(size = 2.5, stroke = 1, alpha = 1) +
#   adams_theme +
#   scale_shape_manual(values = rep(c(21,24),2)) +
#   scale_color_manual(values = pft.cols) +
#   ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
#   xlab(paste0("soil matric potential (avg. over prior ",soil_moisture_period," days")) +
#   #scale_y_continuous(limits = c(0,1e-8))
#   labs(title = paste0(start_date,"--",end_date,"\n",
#                       "driver: ",basename(driver_data_path),"\n",
#                       "light (%): ", percent_light * 100,"\n",
#                       "run name:" , run_name))
# 
# makePNG(fig = rec_vs_smp_normalized, path_to_output.x = path_to_this_run_output, file_name = paste0("02_rec_vs_SMP_full_axis_",basename(driver_data_path)))
print("finished making recruitment vs. SMP figure!")