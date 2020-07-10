
source("create_output/figure_formatting.R")

rec_vs_smp <- soil_moisture_data %>%
ggplot(mapping = aes(x = soil_moisture, y = R * 365, color = pft, shape = model)) +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  adams_theme +
  scale_shape_manual(values = rep(c(21,24),2)) +
  scale_color_manual(values = pft.cols) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("soil matric potential (avg. over prior 4 months)") +
  #scale_y_continuous(limits = c(0,900))
  labs(title = paste0(start_date,"--",end_date,"\n",
                      "driver: ",basename(driver_data_path),"\n",
                      "light (%): ", percent_light * 100))

makePNG(fig = rec_vs_smp, path_to_output.x = paste0(path_to_output,"/forMS/"), file_name = "rec_vs_SMP")
