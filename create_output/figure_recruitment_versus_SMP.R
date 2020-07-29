
source("create_output/figure_formatting.R")

#time stamp
model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

#create folder to store output
path_to_this_run_output <- paste0(path_to_output,run_name,"_rec_vs_SMP_",sub(pattern = " ", replacement = "",x = model_run_time_stamp),"/")

dir.create(path = path_to_this_run_output)

rec_vs_smp <- soil_moisture_data %>%
ggplot(mapping = aes(x = soil_moisture/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = pft.cols) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab(paste0("soil matric potential (avg. over prior ",soil_moisture_period," days")) +
  scale_y_continuous(limits = c(0,20)) +
  labs(title = paste0(start_date,"--",end_date,"\n",
                    "driver: ",basename(driver_data_path),"\n",
                    "light (%): ", percent_light * 100,"\n",
                    "run name:" , run_name))

makePNG(fig = rec_vs_smp, path_to_output.x = path_to_this_run_output, file_name = paste0("03_rec_vs_SMP_",basename(driver_data_path)))


rec_vs_smp_full_axis <- soil_moisture_data %>%
  ggplot(mapping = aes(x = soil_moisture/1e5, y = R * 365, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = pft.cols) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab(paste0("soil matric potential (avg. over prior ",soil_moisture_period," days")) +
  #scale_y_continuous(limits = c(0,20))
  labs(title = paste0(start_date,"--",end_date,"\n",
                    "driver: ",basename(driver_data_path),"\n",
                    "light (%): ", percent_light * 100,"\n",
                    "run name:" , run_name))

makePNG(fig = rec_vs_smp_full_axis, path_to_output.x = path_to_this_run_output, file_name = paste0("02_rec_vs_SMP_full_axis_",basename(driver_data_path)))



SMP_b <- input_data %>%
  filter(pft == "earlydi") %>%
  ggplot(aes(x = date, y = SMP / 1e5)) +
  geom_line() +
  ylab(label = "SMP (MPa)")

makePNG(fig = SMP_b, path_to_output.x = path_to_this_run_output, file_name = paste0("01_SMP",basename(driver_data_path)))


# 
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
