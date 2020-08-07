
source("create_output/figure_formatting.R")
#import benchmarking data
# bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")
# bench4graph <- bench %>%
#   filter(date > start_date,
#          date < end_date) %>%
#   group_by(pft) %>%
#   summarise(R_bench = mean(rec_rate)) %>%
#   mutate(light = 3) %>%
#   mutate(model = "BCI obs.")

#time stamp
model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

#create folder to store output
path_to_this_run_output <- paste0(path_to_output,run_name,"_rec_vs_SMP_",sub(pattern = " ", replacement = "",x = model_run_time_stamp),"/")

dir.create(path = path_to_this_run_output)

# summary_data %>%
#   group_by(patch)%>%
#   summarise(adf = mean(SMP_avg))



summary_data <- read_csv('temp/SMP_summary_data.csv')

se_df <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, SMP_avg) %>%
  rename(submodel = R_sd, ED2 = R_sd_ED2) %>%  ###############################
  gather(submodel:ED2, key = "model", value = "sd") 

pd <- position_dodge(0.03)


rec_vs_smp <- summary_data %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%
  gather(c(submodel,ED2), key = "model", value = "R") %>%
  left_join(se_df, by = c("model","SMP_avg","pft")) %>%
  ggplot(mapping = aes(x = SMP_avg/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = pft.cols) +
  ylab(expression(paste('N recruits ha'^'-1','yr'^'-1'))) + # indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab(paste0("mean soil matric potential (MPa)")) #+
  # labs(title = paste0(start_date,"--",end_date,"\n",
  #                   "driver: ",basename(driver_data_path),"\n",
  #                   "light (%): ", percent_light * 100,"\n",
  #                   "run name:" , run_name))

rec_vs_smp


makePNG(fig = rec_vs_smp, 
        path_to_output.x = path_to_this_run_output, 
        file_name = paste0("SMP_failure_points",
                           basename(driver_data_path)))


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
