print("making recruitment versus light figure...")

source("create_output/figure_formatting.R")

model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")


Rvsl <- summary_data %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%
  gather(submodel:ED2, key = "model", value = "R") %>%
  ggplot(aes(x = mean_pct_light, y = R * 365, color = pft, shape = model)) +
  #geom_point() +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  scale_color_manual(values = pft.cols) +
  scale_shape_manual(values = rep(c(21,24),2)) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light (%)") +
  plot_title +
  adams_theme

png(paste0(path_to_output,"forMS/","R_variation_with_light_",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
print(Rvsl)
dev.off()


print("successfully made recruitment versus light figure")
