from_new_data <- TRUE

if(file.exists("temp/recruitment_vs_light.csv") == FALSE | from_new_data == T){
  source('runs/ED2_run_light_demo.R')
}else{
  summary_data <- read_csv("temp/recruitment_vs_light.csv")
}

print("making recruitment versus light figure...")

source("create_output/figure_formatting.R")

#import benchmarking data
bench <- read_csv("benchmarking/bci_rec_benchmarks_long.csv")



pd <- position_dodge(0.001) # move them .05 to the left and right

se_df <-summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_sd, ED2 = R_sd_ED2) %>%  ###############################
  gather(submodel:ED2, key = "model", value = "sd") 


pd <- position_dodge(0.003)

yrs_in_analysis <- (as.numeric(substr(end_date,start = 1,stop = 4)) - as.numeric(substr(start_date,start = 1,stop = 4))) - 2


bench4graph <- bench %>%
  filter(date > start_date,
         date < end_date) %>%
  group_by(pft) %>%
  summarise(R_bench = mean(rec_rate)) %>%
  mutate(light = 2) %>%
  mutate(model = "BCI obs.")



Rvsl <- summary_data %>%
  dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
  rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
  gather(submodel:ED2, key = "model", value = "R") %>% 
  left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
  mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
  ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
  #geom_point() +
  geom_point(size = 2.5, stroke = 1, alpha = 1) +
  geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = psize) +
  geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd) +
  scale_color_manual(values = pft.cols) +
  scale_shape_manual(values = c(10,21,24)) +
  scale_x_log10() +
  #scale_y_log10() +
  rec.y.axis +
  #ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
  xlab("light at seedling layer (% TOC)") +
  geom_vline(xintercept = 46, linetype = "dotted") +
  labs(title = "Light-sensitive recruitment") +
  #plot_title +
  adams_theme 
  #annotate("segment", x = 2, xend = 0.1, y = 15, yend = 25, colour = "pink", size=3, alpha=0.6, arrow=arrow())

png(paste0(path_to_output,"forMS/","R_variation_with_light_both_models",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
print(Rvsl)
dev.off()


# Rvsl_just_submodel <- summary_data %>%
#   dplyr::select(pft, R_avg, R_avg_ED2, R_sd, R_sd_ED2, mean_pct_light) %>%
#   rename(submodel = R_avg, ED2 = R_avg_ED2) %>%  ###############################
#   gather(submodel:ED2, key = "model", value = "R") %>% 
#   left_join(se_df, by = c("model","mean_pct_light","pft")) %>%
#   mutate_at(.vars = "sd", .funs = function(x){x / sqrt(yrs_in_analysis)}) %>%
#   filter(model == "submodel") %>%
#   ggplot(aes(x = mean_pct_light * 100, y = R * 365, color = pft, shape = model)) +
#   #geom_point() +
#   geom_point(size = 2.5, stroke = 1, alpha = 1) +
#   geom_point(data = bench4graph, mapping = aes(x = light, y = R_bench, color = pft, shape = model), size = 5) +
#   geom_errorbar(aes(ymin= (R * 365) - (sd * 365), ymax = (R * 365) + (sd * 365)), width=0, position = pd) +
#   scale_color_manual(values = pft.cols) +
#   scale_shape_manual(values = rep(c(10,24),2)) +
#   scale_x_log10() +
#   ylab(expression(paste('recruitment rate ',"(# indv. ha"^"-1", "yr"^"-1", ")"))) +
#   xlab("percent light at seedling layer (% TOC)") +
#   geom_vline(xintercept = 46, linetype = "dotted") +
#   #plot_title +
#   adams_theme
# 
# png(paste0(path_to_output,"forMS/","R_variation_with_light_just_submodel",model_run_time_stamp,".png"), height=PNGheight, width=PNGwidth, units=PNGunits, res = PNGres)
# print(Rvsl_just_submodel)
# dev.off()


print("successfully made recruitment versus light figure")