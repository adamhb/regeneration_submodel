from_new_data <- F
print("creating recruitment vs. SMP figure...")


if(file.exists('temp/SMP_summary_data.csv') == F | from_new_data == T){
  print("need to run ED2_run_soil_moisture_demo.R")
  source("runs/ED2_run_soil_moisture_demo.R")
}else(print("making figure with prior run's data"))

#import benchmarking data
bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")

source('benchmarking/bci_soil_moisture_obs.R')
source("create_output/figure_formatting.R")

bench4graph <- bench %>%
  filter(date > start_date,
         date < end_date) %>%
  group_by(pft) %>%
  summarise(R_bench = mean(rec_rate)) %>%
  mutate(SMP = Lutz.smp) %>%
  mutate(model = "BCI obs.")

summary_data <- read_csv('temp/SMP_summary_data.csv')

se_df <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, SMP_avg) %>%
  rename(submodel = R_sd, ED2 = R_sd_ED2) %>%  
  gather(submodel:ED2, key = "model", value = "sd") 

pd <- position_dodge(0.003)
psize <- 5
axis_size <- 20
title_size <- 25

rec_vs_smp <- summary_data %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  left_join(se_df, by = c("model","SMP_avg","pft")) %>%
  ggplot(mapping = aes(x = SMP_avg/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = psize, stroke = 1) +
  geom_point(data = bench4graph, mapping = aes(x = SMP/1e5, y = R_bench, color = pft, shape = model), size = psize, stroke = 2) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, show.legend = F) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_color_manual(values = pft.cols) +
  scale_x_continuous(limits = c(-4,0)) +
  scale_y_continuous(trans=scales::pseudo_log_trans(base = 10), labels = lseq(from = 1,to = 300,length.out = 7), breaks = lseq(from = 1,to = 300,length.out = 7)) + 
  ylab(expression(paste('N recruits ha'^'-1','yr'^'-1'))) + # indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab(paste0("20-yr mean patch-level \n soil matric potential (MPa)")) +
  #labs(title = "Moisture-sensitive recruitment \n among patches (2% TOC light)") +
  #annotate(geom = "text", x = -3, y = 30, label = "recruitment failure \n under chronically dry conditions", size = 4) +
  #geom_segment(aes(x = -3.5, y = 20, xend = -3.5, yend = 3),
  #             arrow = arrow(length = unit(0.5, "cm")), color = "black") +
  adams_theme +
  guides(color = guide_legend(override.aes = list(shape = 15)))
  
makePNG(fig = rec_vs_smp, 
        path_to_output.x = path_to_MS_figures, 
        file_name = "rec_vs_SMP")


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