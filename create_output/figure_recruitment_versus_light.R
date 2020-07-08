print("making recruitment versus light figure...")

source("create_output/figure_formatting.R")

model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")


Rvsl <- summary_data %>%
  ggplot(aes(x = mean_pct_light, y = R_avg * 365, color = pft)) +
  #geom_point() +
  geom_point(size = 2.5, stroke = 1, alpha = 0.5) +
  scale_color_manual(values = pft.cols) +
  scale_shape_manual(values = rep(c(21,24),2)) +
  ylab(expression(paste('avg. rec. rate'," (# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light (%)") +
  adams_theme

png(paste0(path_to_output,"forMS/","R_variation_with_light_",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
print(Rvsl)
dev.off()

print("successfully made recruitment versus light figure")
